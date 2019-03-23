import logging
import os
import traceback
import re
# Local modules
from fortls.jsonrpc import path_to_uri, path_from_uri
from fortls.parse_fortran import fortran_file, process_file, get_paren_level, \
    get_var_stack, climb_type_tree, expand_name, get_line_context
from fortls.objects import find_in_scope, find_in_workspace, get_use_tree, \
    set_keyword_ordering, MODULE_TYPE_ID, SUBROUTINE_TYPE_ID, FUNCTION_TYPE_ID, \
    CLASS_TYPE_ID, INTERFACE_TYPE_ID, SELECT_TYPE_ID
from fortls.intrinsics import get_intrinsic_keywords, load_intrinsics, \
    set_lowercase_intrinsics

log = logging.getLogger(__name__)
# Global regexes
FORTRAN_EXT_REGEX = re.compile(r'^\.F(77|90|95|03|08|OR|PP)?$', re.I)
INT_STMNT_REGEX = re.compile(r'^[ ]*[a-z]*$', re.I)
SCOPE_DEF_REGEX = re.compile(r'[ ]*(MODULE|PROGRAM|SUBROUTINE|FUNCTION)[ ]+', re.I)
END_REGEX = re.compile(r'[ ]*(END)( |MODULE|PROGRAM|SUBROUTINE|FUNCTION|TYPE|DO|IF|SELECT)?', re.I)


def init_file(filepath, pp_defs):
    #
    file_obj = fortran_file(filepath)
    err_str = file_obj.load_from_disk()
    if err_str is not None:
        return None, err_str
    #
    try:
        _, file_ext = os.path.splitext(os.path.basename(filepath))
        if file_ext == file_ext.upper():
            file_ast = process_file(file_obj, True, pp_defs=pp_defs)
        else:
            file_ast = process_file(file_obj, True)
    except:
        log.error("Error while parsing file %s", filepath, exc_info=True)
        return None, 'Error during parsing'
    file_obj.ast = file_ast
    return file_obj, None


def get_line_prefix(pre_lines, curr_line, iChar):
    """Get code line prefix from current line and preceeding continuation lines"""
    if (curr_line is None) or (iChar > len(curr_line)) or (curr_line.startswith('#')):
        return None
    prepend_string = ''.join(pre_lines)
    curr_line = prepend_string + curr_line
    iChar += len(prepend_string)
    line_prefix = curr_line[:iChar].lower()
    # Ignore string literals
    if (line_prefix.count("'") % 2 == 1) or (line_prefix.count('"') % 2 == 1):
        return None
    return line_prefix


class LangServer:
    def __init__(self, conn, debug_log=False, settings={}):
        self.conn = conn
        self.running = True
        self.root_path = None
        self.fs = None
        self.all_symbols = None
        self.workspace = {}
        self.obj_tree = {}
        self.source_dirs = []
        self.excl_paths = []
        self.excl_suffixes = []
        self.post_messages = []
        self.pp_defs = {}
        self.streaming = True
        self.debug_log = debug_log
        # Intrinsic (re-loaded during initialize)
        self.statements, self.keywords, self.intrinsic_funs, self.intrinsic_mods = load_intrinsics()
        # Get launch settings
        self.nthreads = settings.get("nthreads", 4)
        self.notify_init = settings.get("notify_init", False)
        self.symbol_include_mem = settings.get("symbol_include_mem", True)
        self.sync_type = settings.get("sync_type", 1)
        self.autocomplete_no_prefix = settings.get("autocomplete_no_prefix", False)
        self.lowercase_intrinsics = settings.get("lowercase_intrinsics", False)
        self.use_signature_help = settings.get("use_signature_help", False)
        self.variable_hover = settings.get("variable_hover", False)
        self.sort_keywords = settings.get("sort_keywords", True)
        self.enable_code_actions = settings.get("enable_code_actions", False)
        # Set object settings
        set_keyword_ordering(self.sort_keywords)

    def post_message(self, message, type=1):
        self.conn.send_notification("window/showMessage", {
            "type": type,
            "message": message
        })

    def run(self):
        # Run server
        while self.running:
            try:
                request = self.conn.read_message()
                self.handle(request)
            except EOFError:
                break
            except Exception as e:
                log.error("Unexpected error: %s", e, exc_info=True)
                break
            else:
                for message in self.post_messages:
                    self.post_message(message[1], message[0])
                self.post_messages = []

    def handle(self, request):
        def noop(request):
            return None
        # Request handler
        log.debug("REQUEST %s %s", request.get("id"), request.get("method"))
        handler = {
            "initialize": self.serve_initialize,
            "textDocument/documentSymbol": self.serve_document_symbols,
            "textDocument/completion": self.serve_autocomplete,
            "textDocument/signatureHelp": self.serve_signature,
            "textDocument/definition": self.serve_definition,
            "textDocument/references": self.serve_references,
            "textDocument/hover": self.serve_hover,
            "textDocument/implementation": self.serve_implementation,
            "textDocument/rename": self.serve_rename,
            "textDocument/didOpen": self.serve_onSave,
            "textDocument/didSave": self.serve_onSave,
            "textDocument/didClose": self.serve_onClose,
            "textDocument/didChange": self.serve_onChange,
            "textDocument/codeAction": self.serve_codeActions,
            "initialized": noop,
            "workspace/didChangeWatchedFiles": noop,
            "workspace/symbol": self.serve_workspace_symbol,
            "$/cancelRequest": noop,
            "shutdown": noop,
            "exit": self.serve_exit,
        }.get(request["method"], self.serve_default)
        # handler = {
        #     "workspace/symbol": self.serve_symbols,
        # }.get(request["method"], self.serve_default)
        # We handle notifications differently since we can't respond
        if "id" not in request:
            try:
                handler(request)
            except:
                log.warning(
                    "error handling notification %s", request, exc_info=True)
            return
        #
        try:
            resp = handler(request)
        except JSONRPC2Error as e:
            self.conn.write_error(
                request["id"], code=e.code, message=e.message, data=e.data)
            log.warning("RPC error handling request %s", request, exc_info=True)
        except Exception as e:
            self.conn.write_error(
                request["id"],
                code=-32603,
                message=str(e),
                data={
                    "traceback": traceback.format_exc(),
                })
            log.warning("error handling request %s", request, exc_info=True)
        else:
            self.conn.write_response(request["id"], resp)

    def serve_initialize(self, request):
        # Setup language server
        params = request["params"]
        self.root_path = path_from_uri(
            params.get("rootUri") or params.get("rootPath") or "")
        self.source_dirs.append(self.root_path)
        # Check for config file
        config_path = os.path.join(self.root_path, ".fortls")
        config_exists = os.path.isfile(config_path)
        if config_exists:
            try:
                import json
                with open(config_path, 'r') as fhandle:
                    config_dict = json.load(fhandle)
                    for excl_path in config_dict.get("excl_paths", []):
                        self.excl_paths.append(os.path.join(self.root_path, excl_path))
                    source_dirs = config_dict.get("source_dirs", [])
                    # Legacy definition
                    if len(source_dirs) == 0:
                        source_dirs = config_dict.get("mod_dirs", [])
                    for source_dir in source_dirs:
                        dir_path = os.path.join(self.root_path, source_dir)
                        if os.path.isdir(dir_path):
                            self.source_dirs.append(dir_path)
                    self.excl_suffixes = config_dict.get("excl_suffixes", [])
                    self.lowercase_intrinsics = config_dict.get("lowercase_intrinsics", self.lowercase_intrinsics)
                    self.debug_log = config_dict.get("debug_log", self.debug_log)
                    self.pp_defs = config_dict.get("pp_defs", {})
                    if isinstance(self.pp_defs, list):
                        self.pp_defs = {key: "" for key in self.pp_defs}
            except:
                self.post_messages.append([1, "Error while parsing '.fortls' settings file"])
        # Setup logging
        if self.debug_log and (self.root_path != ""):
            logging.basicConfig(filename=os.path.join(self.root_path, "fortls_debug.log"),
                                level=logging.DEBUG, filemode='w')
            log.debug("REQUEST %s %s", request.get("id"), request.get("method"))
            self.post_messages.append([3, "FORTLS debugging enabled"])
        # Load intrinsics
        set_keyword_ordering(True)  # Always sort intrinsics
        if self.lowercase_intrinsics:
            set_lowercase_intrinsics()
        self.statements, self.keywords, self.intrinsic_funs, self.intrinsic_mods = load_intrinsics()
        for module in self.intrinsic_mods:
            self.obj_tree[module.FQSN] = [module, None]
        # Set object settings
        set_keyword_ordering(self.sort_keywords)
        # Recursively add sub-directories
        if len(self.source_dirs) == 1:
            self.source_dirs = []
            for dirName, subdirList, fileList in os.walk(self.root_path):
                if self.excl_paths.count(dirName) > 0:
                    while(len(subdirList) > 0):
                        del subdirList[0]
                    continue
                contains_source = False
                for filename in fileList:
                    _, ext = os.path.splitext(os.path.basename(filename))
                    if FORTRAN_EXT_REGEX.match(ext):
                        contains_source = True
                        break
                if contains_source:
                    self.source_dirs.append(dirName)
        # Initialize workspace
        self.workspace_init()
        #
        server_capabilities = {
            "completionProvider": {
                "resolveProvider": False,
                "triggerCharacters": ["%"]
            },
            "definitionProvider": True,
            "documentSymbolProvider": True,
            "referencesProvider": True,
            "hoverProvider": True,
            "implementationProvider": True,
            "renameProvider": True,
            "workspaceSymbolProvider": True,
            "textDocumentSync": self.sync_type
        }
        if self.use_signature_help:
            server_capabilities["signatureHelpProvider"] = {
                "triggerCharacters": ["(", ","]
            }
        if self.enable_code_actions:
            server_capabilities["codeActionProvider"] = True
        if self.notify_init:
            self.post_messages.append([3, "FORTLS initialization complete"])
        return {"capabilities": server_capabilities}
        #     "workspaceSymbolProvider": True,
        #     "streaming": False,
        # }

    def serve_workspace_symbol(self, request):
        def map_types(type):
            if type == 1:
                return 2
            elif type == 2:
                return 6
            elif type == 3:
                return 12
            elif type == 4:
                return 5
            elif type == 5:
                return 11
            elif type == 6:
                return 13
            elif type == 7:
                return 6
            else:
                return 1
        matching_symbols = []
        query = request["params"]["query"].lower()
        for candidate in find_in_workspace(self.obj_tree, query):
            tmp_out = {
                "name": candidate.name,
                "kind": map_types(candidate.get_type()),
                "location": {
                    "uri": path_to_uri(candidate.file_ast.path),
                    "range": {
                        "start": {"line": candidate.sline-1, "character": 0},
                        "end": {"line": candidate.eline-1, "character": 0}
                    }
                }
            }
            # Set containing scope
            if candidate.FQSN.find('::') > 0:
                tmp_list = candidate.FQSN.split("::")
                tmp_out["containerName"] = tmp_list[0]
            matching_symbols.append(tmp_out)
        return sorted(matching_symbols, key=lambda k: k['name'])

    def serve_document_symbols(self, request):
        def map_types(type):
            if type == 1:
                return 2
            elif type == 2:
                return 6
            elif type == 3:
                return 12
            elif type == 4:
                return 5
            elif type == 5:
                return 11
            elif type == 6:
                return 13
            elif type == 7:
                return 6
            else:
                return 1
        # Get parameters from request
        params = request["params"]
        uri = params["textDocument"]["uri"]
        path = path_from_uri(uri)
        file_obj = self.workspace.get(path)
        if file_obj is None:
            return []
        # Add scopes to outline view
        test_output = []
        for scope in file_obj.ast.get_scopes():
            if (scope.name[0] == "#") or (scope.get_type() == SELECT_TYPE_ID):
                continue
            scope_tree = scope.FQSN.split("::")
            if len(scope_tree) > 2:
                if scope_tree[1].startswith("#gen_int"):
                    scope_type = 11
                else:
                    continue
            else:
                scope_type = map_types(scope.get_type())
            tmp_out = {}
            tmp_out["name"] = scope.name
            tmp_out["kind"] = scope_type
            sline = scope.sline-1
            eline = scope.eline-1
            tmp_out["location"] = {
                "uri": uri,
                "range": {
                    "start": {"line": sline, "character": 0},
                    "end": {"line": eline, "character": 0}
                }
            }
            # Set containing scope
            if scope.FQSN.find('::') > 0:
                tmp_list = scope.FQSN.split("::")
                tmp_out["containerName"] = tmp_list[0]
            test_output.append(tmp_out)
            # If class add members
            if (scope.get_type() == CLASS_TYPE_ID) and self.symbol_include_mem:
                for child in scope.children:
                    tmp_out = {}
                    tmp_out["name"] = child.name
                    tmp_out["kind"] = map_types(child.get_type())
                    tmp_out["location"] = {
                        "uri": uri,
                        "range": {
                            "start": {"line": child.sline-1, "character": 0},
                            "end": {"line": child.sline-1, "character": 0}
                        }
                    }
                    tmp_out["containerName"] = scope.name
                    test_output.append(tmp_out)
        return test_output

    def serve_autocomplete(self, request):
        #
        def map_types(type):
            if type == 1:
                return 9
            elif type == 4:
                return 7
            elif type == 6:
                return 6
            else:
                return type

        def set_type_mask(def_value):
            return [def_value if i < 8 else True for i in range(15)]

        def get_candidates(scope_list, var_prefix, inc_globals=True, public_only=False, abstract_only=False):
            #
            def child_candidates(scope, only_list=[], filter_public=True, req_abstract=False):
                tmp_list = []
                # Filter children
                nonly = len(only_list)
                for child in scope.get_children(filter_public):
                    if req_abstract:
                        if child.is_abstract():
                            tmp_list += child_candidates(child, only_list, filter_public)
                    else:
                        if child.is_external_int():
                            tmp_list += child_candidates(child, only_list, filter_public)
                        else:
                            if (nonly > 0) and (child.name.lower() not in only_list):
                                continue
                            tmp_list.append(child)
                return tmp_list
            var_list = []
            use_dict = {}
            for scope in scope_list:
                var_list += child_candidates(scope, filter_public=public_only, req_abstract=abstract_only)
                # Traverse USE tree and add to list
                use_dict = get_use_tree(scope, use_dict, self.obj_tree)
            # Look in found use modules
            for use_mod, only_list in use_dict.items():
                scope = self.obj_tree[use_mod][0]
                var_list += child_candidates(scope, only_list, req_abstract=abstract_only)
            # Add globals
            if inc_globals:
                for _, obj in self.obj_tree.items():
                    var_list.append(obj[0])
                var_list += self.intrinsic_funs
            # Filter by prefix if necessary
            if var_prefix == '':
                return var_list
            else:
                tmp_list = []
                for var in var_list:
                    if var.name.lower().startswith(var_prefix):
                        tmp_list.append(var)
                return tmp_list

        def build_comp(candidate, name_only=False, name_replace=None, is_interface=False):
            comp_obj = {}
            call_sig = None
            if name_only:
                comp_obj["label"] = candidate.name
            else:
                comp_obj["label"] = candidate.name
                if name_replace is not None:
                    comp_obj["label"] = name_replace
                call_sig, snippet = candidate.get_snippet(name_replace)
                if snippet is not None:
                    if self.use_signature_help and (not is_interface):
                        arg_open = snippet.find('(')
                        if arg_open > 0:
                            snippet = snippet[:arg_open]
                    comp_obj["insertText"] = snippet
                    comp_obj["insertTextFormat"] = 2
            comp_obj["kind"] = map_types(candidate.get_type())
            comp_obj["detail"] = candidate.get_desc()
            if call_sig is not None:
                comp_obj["detail"] += ' ' + call_sig
            doc_str, _ = candidate.get_hover()
            if doc_str is not None:
                comp_obj["documentation"] = doc_str
            return comp_obj
        # Get parameters from request
        req_dict = {"isIncomplete": False, "items": []}
        params = request["params"]
        uri = params["textDocument"]["uri"]
        path = path_from_uri(uri)
        file_obj = self.workspace.get(path)
        if file_obj is None:
            return req_dict
        # Check line
        ac_line = params["position"]["line"]
        ac_char = params["position"]["character"]
        # Get full line (and possible continuations) from file
        pre_lines, curr_line, _ = file_obj.get_code_line(ac_line, forward=False, strip_comment=True)
        line_prefix = get_line_prefix(pre_lines, curr_line, ac_char)
        if line_prefix is None:
            return req_dict
        is_member = False
        try:
            var_stack = get_var_stack(line_prefix)
            is_member = (len(var_stack) > 1)
            var_prefix = var_stack[-1].strip()
        except:
            return req_dict
        # print(var_stack)
        item_list = []
        scope_list = file_obj.ast.get_scopes(ac_line+1)
        # Get context
        name_only = False
        public_only = False
        include_globals = True
        line_context, context_info = get_line_context(line_prefix)
        if (line_context == 'skip') or (var_prefix == '' and (not is_member)):
            return req_dict
        if self.autocomplete_no_prefix:
            var_prefix = ''
        # Suggestions for user-defined type members
        if is_member:
            curr_scope = file_obj.ast.get_inner_scope(ac_line+1)
            type_scope = climb_type_tree(var_stack, curr_scope, self.obj_tree)
            # Set enclosing type as scope
            if type_scope is None:
                return {"isIncomplete": False, "items": []}
            else:
                include_globals = False
                scope_list = [type_scope]
        # Setup based on context
        req_callable = False
        abstract_only = False
        type_mask = set_type_mask(False)
        type_mask[1] = True
        type_mask[4] = True
        if line_context == 'mod_only':
            # Module names only (USE statement)
            for key in self.obj_tree:
                candidate = self.obj_tree[key][0]
                if (candidate.get_type() == MODULE_TYPE_ID) and \
                   candidate.name.lower().startswith(var_prefix):
                    item_list.append(build_comp(candidate, name_only=True))
            req_dict["items"] = item_list
            return req_dict
        elif line_context == 'mod_mems':
            # Public module members only (USE ONLY statement)
            name_only = True
            mod_name = context_info.lower()
            if mod_name in self.obj_tree:
                scope_list = [self.obj_tree[mod_name][0]]
                public_only = True
                include_globals = False
                type_mask[4] = False
            else:
                return {"isIncomplete": False, "items": []}
        elif line_context == 'call':
            # Callable objects only ("CALL" statements)
            req_callable = True
        elif line_context == 'type_only':
            # User-defined types only (variable definitions, select clauses)
            type_mask = set_type_mask(True)
            type_mask[4] = False
        elif line_context == 'import':
            # Import statement (variables and user-defined types only)
            name_only = True
            type_mask = set_type_mask(True)
            type_mask[4] = False
            type_mask[6] = False
        elif line_context == 'int_only':
            # Interfaces only (procedure definitions)
            abstract_only = True
            include_globals = False
            name_only = True
            type_mask = set_type_mask(True)
            type_mask[2] = False
            type_mask[3] = False
        elif line_context == 'var_only':
            # Variables only (variable definitions)
            name_only = True
            type_mask[2] = True
            type_mask[3] = True
        elif line_context == 'var_key':
            # Variable definition keywords only (variable definition)
            key_context = 0
            enc_scope_type = scope_list[-1].get_type()
            if enc_scope_type == MODULE_TYPE_ID:
                key_context = 1
            elif (enc_scope_type == SUBROUTINE_TYPE_ID) or (enc_scope_type == FUNCTION_TYPE_ID):
                key_context = 2
            elif enc_scope_type == CLASS_TYPE_ID:
                key_context = 3
            for candidate in get_intrinsic_keywords(self.statements, self.keywords, key_context):
                if candidate.name.lower().startswith(var_prefix):
                    item_list.append(build_comp(candidate))
            req_dict["items"] = item_list
            return req_dict
        elif line_context == 'first':
            # First word -> default context plus Fortran statements
            for candidate in get_intrinsic_keywords(self.statements, self.keywords, 0):
                if candidate.name.lower().startswith(var_prefix):
                    item_list.append(build_comp(candidate))
        # Build completion list
        for candidate in get_candidates(scope_list, var_prefix, include_globals, public_only, abstract_only):
            # Skip module names (only valid in USE)
            candidate_type = candidate.get_type()
            if type_mask[candidate_type]:
                continue
            if req_callable and (not candidate.is_callable()):
                continue
            #
            if candidate_type == INTERFACE_TYPE_ID:
                tmp_list = []
                for member in candidate.mems:
                    tmp_text, _ = member.get_snippet(candidate.name)
                    if tmp_list.count(tmp_text) > 0:
                        continue
                    tmp_list.append(tmp_text)
                    item_list.append(build_comp(member, name_replace=candidate.name, is_interface=True))
                continue
            #
            item_list.append(build_comp(candidate, name_only=name_only))
        req_dict["items"] = item_list
        return req_dict

    def get_definition(self, def_file, def_line, def_char):
        # Get full line (and possible continuations) from file
        pre_lines, curr_line, _ = def_file.get_code_line(def_line, forward=False, strip_comment=True)
        line_prefix = get_line_prefix(pre_lines, curr_line, def_char)
        if line_prefix is None:
            return None
        is_member = False
        try:
            var_stack = get_var_stack(line_prefix)
            is_member = (len(var_stack) > 1)
            def_name = expand_name(curr_line, def_char)
        except:
            return None
        # print(var_stack, def_name)
        if def_name == '':
            return None
        curr_scope = def_file.ast.get_inner_scope(def_line+1)
        # Traverse type tree if necessary
        if is_member:
            type_scope = climb_type_tree(var_stack, curr_scope, self.obj_tree)
            # Set enclosing type as scope
            if type_scope is None:
                curr_scope = None
            else:
                curr_scope = type_scope
        # Find in available scopes
        var_obj = None
        if curr_scope is not None:
            if (curr_scope.get_type() == CLASS_TYPE_ID) and (not is_member) and \
               line_prefix.lstrip().lower().startswith('procedure') and (line_prefix.count("=>") > 0):
                curr_scope = curr_scope.parent
            var_obj = find_in_scope(curr_scope, def_name, self.obj_tree)
        # Search in global scope
        if var_obj is None:
            key = def_name.lower()
            if key in self.obj_tree:
                return self.obj_tree[key][0]
            for obj in self.intrinsic_funs:
                if obj.name.lower() == key:
                    return obj
        else:
            return var_obj
        return None

    def serve_signature(self, request):
        def get_sub_name(line):
            _, sections = get_paren_level(line)
            if sections[0][0] <= 1:
                return None, None, None
            arg_string = line[sections[0][0]:sections[-1][1]]
            sub_string, sections = get_paren_level(line[:sections[0][0]-1])
            return sub_string.strip(), arg_string.split(','), sections[-1][0]

        def check_optional(arg, params):
            opt_split = arg.split("=")
            if len(opt_split) > 1:
                opt_arg = opt_split[0].strip().lower()
                for i, param in enumerate(params):
                    param_split = param["label"].split("=")[0]
                    if param_split.lower() == opt_arg:
                        return i
            return None
        # Get parameters from request
        req_dict = {"signatures": []}
        params = request["params"]
        uri = params["textDocument"]["uri"]
        path = path_from_uri(uri)
        file_obj = self.workspace.get(path)
        if file_obj is None:
            return req_dict
        # Check line
        sig_line = params["position"]["line"]
        sig_char = params["position"]["character"]
        # Get full line (and possible continuations) from file
        pre_lines, curr_line, _ = file_obj.get_code_line(sig_line, forward=False, strip_comment=True)
        line_prefix = get_line_prefix(pre_lines, curr_line, sig_char)
        if line_prefix is None:
            return req_dict
        # Test if scope declaration or end statement
        if SCOPE_DEF_REGEX.match(curr_line) or END_REGEX.match(curr_line):
            return req_dict
        is_member = False
        try:
            sub_name, arg_strings, sub_end = get_sub_name(line_prefix)
            var_stack = get_var_stack(sub_name)
            is_member = (len(var_stack) > 1)
        except:
            return req_dict
        #
        curr_scope = file_obj.ast.get_inner_scope(sig_line+1)
        # Traverse type tree if necessary
        if is_member:
            type_scope = climb_type_tree(var_stack, curr_scope, self.obj_tree)
            # Set enclosing type as scope
            if type_scope is None:
                curr_scope = None
            else:
                curr_scope = type_scope
        sub_name = var_stack[-1]
        # Find in available scopes
        var_obj = None
        if curr_scope is not None:
            var_obj = find_in_scope(curr_scope, sub_name, self.obj_tree)
        # Search in global scope
        if var_obj is None:
            key = sub_name.lower()
            if key in self.obj_tree:
                var_obj = self.obj_tree[key][0]
            else:
                for obj in self.intrinsic_funs:
                    if obj.name.lower() == key:
                        var_obj = obj
                        break
        # Check keywords
        if (var_obj is None) and (INT_STMNT_REGEX.match(line_prefix[:sub_end]) is not None):
            key = sub_name.lower()
            for candidate in get_intrinsic_keywords(self.statements, self.keywords, 0):
                if candidate.name.lower() == key:
                    var_obj = candidate
                    break
        if var_obj is None:
            return req_dict
        # Build signature
        label, doc_str, params = var_obj.get_signature()
        if label is None:
            return req_dict
        # Find current parameter by index or by
        # looking at last arg with optional name
        param_num = len(arg_strings)-1
        opt_num = check_optional(arg_strings[-1], params)
        if opt_num is None:
            if len(arg_strings) > 1:
                opt_num = check_optional(arg_strings[-2], params)
                if opt_num is not None:
                    param_num = opt_num + 1
        else:
            param_num = opt_num
        signature = {
            "label": label,
            "parameters": params
        }
        if doc_str is not None:
            signature["documentation"] = doc_str
        req_dict = {
            "signatures": [signature],
            "activeParameter": param_num
        }
        return req_dict

    def serve_references(self, request):
        # Get parameters from request
        params = request["params"]
        uri = params["textDocument"]["uri"]
        def_line = params["position"]["line"]
        def_char = params["position"]["character"]
        path = path_from_uri(uri)
        file_obj = self.workspace.get(path)
        if file_obj is None:
            return []
        # Find object
        def_obj = self.get_definition(file_obj, def_line, def_char)
        if def_obj is None:
            return []
        #
        restrict_file = None
        type_mem = False
        if def_obj.FQSN.count(":") > 2:
            if def_obj.parent.get_type() == CLASS_TYPE_ID:
                type_mem = True
            else:
                restrict_file = def_obj.file_ast.file
                if restrict_file is None:
                    return []
        # Search through all files
        def_name = def_obj.name.lower()
        def_fqsn = def_obj.FQSN
        NAME_REGEX = re.compile(r'(?:\W|^)({0})(?:\W|$)'.format(def_name), re.I)
        if restrict_file is None:
            file_set = self.workspace.items()
        else:
            file_set = ((restrict_file.path, restrict_file), )
        override_cache = []
        refs = []
        for filename, file_obj in sorted(file_set):
            # Search through file line by line
            for (i, line) in enumerate(file_obj.contents_split):
                if len(line) == 0:
                    continue
                # Skip comment lines
                line = file_obj.strip_comment(line)
                if (line == '') or (line[0] == '#'):
                    continue
                for match in NAME_REGEX.finditer(line):
                    var_def = self.get_definition(file_obj, i, match.start(1)+1)
                    if var_def is not None:
                        ref_match = False
                        if (def_fqsn == var_def.FQSN) or (var_def.FQSN in override_cache):
                            ref_match = True
                        elif var_def.parent.get_type() == CLASS_TYPE_ID:
                            if type_mem:
                                for inherit_def in var_def.parent.get_overriden(def_name):
                                    if def_fqsn == inherit_def.FQSN:
                                        ref_match = True
                                        override_cache.append(var_def.FQSN)
                                        break
                            if (var_def.sline-1 == i) and (var_def.file_ast.path == filename) \
                               and (line.count("=>") == 0):
                                try:
                                    if var_def.link_obj is def_obj:
                                        ref_match = True
                                except:
                                    pass
                        if ref_match:
                            refs.append({
                                "uri": path_to_uri(filename),
                                "range": {
                                    "start": {"line": i, "character": match.start(1)},
                                    "end": {"line": i, "character": match.end(1)}
                                }
                            })
        return refs

    def serve_definition(self, request):
        # Get parameters from request
        params = request["params"]
        uri = params["textDocument"]["uri"]
        def_line = params["position"]["line"]
        def_char = params["position"]["character"]
        path = path_from_uri(uri)
        file_obj = self.workspace.get(path)
        if file_obj is None:
            return None
        # Find object
        var_obj = self.get_definition(file_obj, def_line, def_char)
        if var_obj is None:
            return None
        # Construct link reference
        if var_obj.file_ast.file is not None:
            var_file = var_obj.file_ast.file
            sline, schar, echar = \
                var_file.find_word_in_code_line(var_obj.sline-1, var_obj.name)
            if schar < 0:
                schar = echar = 0
            return {
                "uri": path_to_uri(var_file.path),
                "range": {
                    "start": {"line": sline, "character": schar},
                    "end": {"line": sline, "character": echar}
                }
            }
        return None

    def serve_hover(self, request):
        def create_hover(string, highlight):
            if highlight:
                return {
                    "language": "fortran90",
                    "value": string
                }
            else:
                return string
        # Get parameters from request
        params = request["params"]
        uri = params["textDocument"]["uri"]
        def_line = params["position"]["line"]
        def_char = params["position"]["character"]
        path = path_from_uri(uri)
        file_obj = self.workspace.get(path)
        if file_obj is None:
            return None
        # Find object
        var_obj = self.get_definition(file_obj, def_line, def_char)
        if var_obj is None:
            return None
        # Construct hover information
        var_type = var_obj.get_type()
        hover_str = None
        if (var_type == SUBROUTINE_TYPE_ID) or (var_type == FUNCTION_TYPE_ID):
            hover_str, highlight = var_obj.get_hover(long=True)
        elif var_type == INTERFACE_TYPE_ID:
            hover_array = []
            for member in var_obj.mems:
                hover_str, highlight = member.get_hover(long=True)
                if hover_str is not None:
                    hover_array.append(create_hover(hover_str, highlight))
            return {"contents": hover_array}
        elif self.variable_hover and (var_type == 6):
            hover_str, highlight = var_obj.get_hover()
        #
        if hover_str is not None:
            return {"contents": create_hover(hover_str, highlight)}
        return None

    def serve_implementation(self, request):
        # Get parameters from request
        params = request["params"]
        uri = params["textDocument"]["uri"]
        def_line = params["position"]["line"]
        def_char = params["position"]["character"]
        path = path_from_uri(uri)
        file_obj = self.workspace.get(path)
        if file_obj is None:
            return None
        # Find object
        var_obj = self.get_definition(file_obj, def_line, def_char)
        if var_obj is None:
            return None
        # Construct implementation reference
        if var_obj.parent.get_type() == CLASS_TYPE_ID:
            impl_obj = var_obj.link_obj
            if (impl_obj is not None) and (impl_obj.file_ast.file is not None):
                impl_file = impl_obj.file_ast.file
                sline, schar, echar = \
                    impl_file.find_word_in_code_line(impl_obj.sline-1, impl_obj.name)
                if schar < 0:
                    schar = echar = 0
                return {
                    "uri": path_to_uri(impl_file.path),
                    "range": {
                        "start": {"line": sline, "character": schar},
                        "end": {"line": sline, "character": echar}
                    }
                }
        return None

    def serve_rename(self, request):
        all_refs = self.serve_references(request)
        if all_refs is None:
            self.post_message('Rename failed: No usages found to rename', type=2)
            return None
        params = request["params"]
        new_name = params["newName"]
        changes = {}
        for ref in all_refs:
            if ref["uri"] not in changes:
                changes[ref["uri"]] = []
            changes[ref["uri"]].append({
                "range": ref["range"],
                "newText": new_name
            })
        return {"changes": changes}

    def serve_codeActions(self, request):
        params = request["params"]
        uri = params["textDocument"]["uri"]
        sline = params["range"]["start"]["line"]
        eline = params["range"]["end"]["line"]
        path = path_from_uri(uri)
        file_obj = self.workspace.get(path)
        # Find object
        if file_obj is None:
            return None
        curr_scope = file_obj.ast.get_inner_scope(sline)
        if curr_scope is None:
            return None
        action_list = curr_scope.get_actions(sline, eline)
        if action_list is None:
            return None
        # Convert diagnostics
        for action in action_list:
            diagnostics = action.get("diagnostics")
            if diagnostics is not None:
                new_diags = []
                for diagnostic in diagnostics:
                    new_diags.append(diagnostic.build(file_obj))
                action["diagnostics"] = new_diags
        return action_list

    def send_diagnostics(self, uri):
        diag_results, diag_exp = self.get_diagnostics(uri)
        if diag_results is not None:
            self.conn.send_notification("textDocument/publishDiagnostics", {
                "uri": uri,
                "diagnostics": diag_results
            })
        elif diag_exp is not None:
            self.conn.write_error(
                -1,
                code=-32603,
                message=str(diag_exp),
                data={
                    "traceback": traceback.format_exc(),
                })

    def get_diagnostics(self, uri):
        filepath = path_from_uri(uri)
        file_obj = self.workspace.get(filepath)
        if file_obj is not None:
            try:
                diags = file_obj.ast.check_file(self.obj_tree)
            except Exception as e:
                return None, e
            else:
                return diags, None
        return None, None

    def serve_onChange(self, request):
        # Update workspace from file sent by editor
        params = request["params"]
        uri = params["textDocument"]["uri"]
        path = path_from_uri(uri)
        file_obj = self.workspace.get(path)
        if file_obj is None:
            self.post_message('Change request failed for unknown file "{0}"'.format(path))
            log.error('Change request failed for unknown file "%s"', path)
            return
        else:
            # Update file contents with changes
            if self.sync_type == 1:
                file_obj.apply_change(params["contentChanges"][0])
            else:
                try:
                    for change in params["contentChanges"]:
                        _ = file_obj.apply_change(change)
                except:
                    self.post_message('Change request failed for file "{0}": Could not apply change'.format(path))
                    log.error('Change request failed for file "%s": Could not apply change', path, exc_info=True)
                    return
        # Parse newly updated file
        err_str = self.update_workspace_file(path, update_links=True)
        if err_str is not None:
            self.post_message('Change request failed for file "{0}": {1}'.format(path, err_str))
            return
        # Update include statements linking to this file
        for _, tmp_file in self.workspace.items():
            tmp_file.ast.resolve_includes(self.workspace, path=path)
        file_obj.ast.resolve_includes(self.workspace)
        # Update inheritance (currently only on open/save)
        # for key in self.obj_tree:
        #     self.obj_tree[key][0].resolve_inherit(self.obj_tree)
        #     self.obj_tree[key][0].resolve_link(self.obj_tree)

    def serve_onClose(self, request):
        self.serve_onSave(request, test_exist=True)

    def serve_onSave(self, request, test_exist=False):
        # Update workspace from file on disk
        params = request["params"]
        uri = params["textDocument"]["uri"]
        filepath = path_from_uri(uri)
        if test_exist and (not os.path.isfile(filepath)):
            return
        err_str = self.add_file(filepath)
        if err_str is not None:
            self.post_message('Save request failed for file "{0}": {1}'.format(filepath, err_str))
            return
        # Update include statements linking to this file
        for _, file_obj in self.workspace.items():
            file_obj.ast.resolve_includes(self.workspace, path=filepath)
        file_obj = self.workspace.get(filepath)
        file_obj.ast.resolve_includes(self.workspace)
        # Update inheritance
        for key in self.obj_tree:
            self.obj_tree[key][0].resolve_inherit(self.obj_tree)
            self.obj_tree[key][0].resolve_link(self.obj_tree)
        self.send_diagnostics(uri)

    def add_file(self, filepath):
        return self.update_workspace_file(filepath, read_file=True)

    def update_workspace_file(self, filepath, read_file=False, update_links=False):
        # Update workspace from file contents and path
        try:
            file_obj = self.workspace.get(filepath)
            if read_file:
                if file_obj is None:
                    file_obj = fortran_file(filepath)
                file_obj.load_from_disk()
            _, file_ext = os.path.splitext(os.path.basename(filepath))
            if file_ext == file_ext.upper():
                ast_new = process_file(file_obj, True, pp_defs=self.pp_defs)
            else:
                ast_new = process_file(file_obj, True)
        except:
            log.error("Error while parsing file %s", filepath, exc_info=True)
            return 'Error during parsing'  # Error during parsing
        # Remove old objects from tree
        ast_old = file_obj.ast
        if ast_old is not None:
            for key in ast_old.global_dict:
                self.obj_tree.pop(key, None)
        # Add new file to workspace
        file_obj.ast = ast_new
        if filepath not in self.workspace:
            self.workspace[filepath] = file_obj
        # Add top-level objects to object tree
        for key, obj in ast_new.global_dict.items():
            self.obj_tree[key] = [obj, filepath]
        # Update local links/inheritance if necessary
        if update_links:
            for key, obj in ast_new.global_dict.items():
                obj.resolve_inherit(self.obj_tree)
                obj.resolve_link(self.obj_tree)
        return None

    def workspace_init(self):
        # Get filenames
        file_list = []
        for source_dir in self.source_dirs:
            for filename in os.listdir(source_dir):
                _, ext = os.path.splitext(os.path.basename(filename))
                if FORTRAN_EXT_REGEX.match(ext):
                    filepath = os.path.normpath(os.path.join(source_dir, filename))
                    if self.excl_paths.count(filepath) > 0:
                        continue
                    inc_file = True
                    for excl_suffix in self.excl_suffixes:
                        if filepath.endswith(excl_suffix):
                            inc_file = False
                            break
                    if inc_file:
                        file_list.append(filepath)
        # Process files
        from multiprocessing import Pool
        pool = Pool(processes=self.nthreads)
        results = {}
        for filepath in file_list:
            results[filepath] = pool.apply_async(init_file, args=(filepath, self.pp_defs))
        pool.close()
        pool.join()
        for path, result in results.items():
            result_obj = result.get()
            if result_obj[0] is None:
                self.post_messages.append([1, 'Initialization failed for file "{0}": {1}'.format(path, result_obj[1])])
                continue
            self.workspace[path] = result_obj[0]
            # Add top-level objects to object tree
            ast_new = self.workspace[path].ast
            for key in ast_new.global_dict:
                self.obj_tree[key] = [ast_new.global_dict[key], path]
        # Update include statements
        for _, file_obj in self.workspace.items():
            file_obj.ast.resolve_includes(self.workspace)
        # Update inheritance
        for key in self.obj_tree:
            self.obj_tree[key][0].resolve_inherit(self.obj_tree)
            self.obj_tree[key][0].resolve_link(self.obj_tree)

    def serve_exit(self, request):
        # Exit server
        self.workspace = {}
        self.obj_tree = {}
        self.running = False

    def serve_default(self, request):
        # Default handler (errors!)
        raise JSONRPC2Error(
            code=-32601,
            message="method {} not found".format(request["method"]))


class JSONRPC2Error(Exception):
    def __init__(self, code, message, data=None):
        self.code = code
        self.message = message
        self.data = data
