import logging
import os
import traceback
import re
# Local modules
from fortls.jsonrpc import path_to_uri, path_from_uri
from fortls.parse_fortran import fortran_file, process_file, get_paren_level, \
    expand_name, get_line_context
from fortls.objects import find_in_scope, find_in_workspace, get_use_tree, \
    get_var_stack, climb_type_tree, set_keyword_ordering, MODULE_TYPE_ID, \
    SUBROUTINE_TYPE_ID, FUNCTION_TYPE_ID, CLASS_TYPE_ID, INTERFACE_TYPE_ID, \
    SELECT_TYPE_ID, VAR_TYPE_ID, METH_TYPE_ID
from fortls.intrinsics import get_intrinsic_keywords, load_intrinsics, \
    set_lowercase_intrinsics

log = logging.getLogger(__name__)
# Global regexes
FORTRAN_EXT_REGEX = re.compile(r'^\.F(77|90|95|03|08|OR|PP)?$', re.I)
INT_STMNT_REGEX = re.compile(r'^[ ]*[a-z]*$', re.I)
TYPE_DEF_REGEX = re.compile(r'[ ]*(TYPE|CLASS)[ ]*\([a-z0-9_ ]*$', re.I)
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
        file_ast = process_file(file_obj, True, pp_defs=pp_defs)
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
        self.link_version = 0
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
        self.autocomplete_no_snippets = settings.get("autocomplete_no_snippets", False)
        self.autocomplete_name_only = settings.get("autocomplete_name_only", False)
        self.lowercase_intrinsics = settings.get("lowercase_intrinsics", False)
        self.use_signature_help = settings.get("use_signature_help", False)
        self.variable_hover = settings.get("variable_hover", False)
        self.hover_signature = settings.get("hover_signature", False)
        self.sort_keywords = settings.get("sort_keywords", True)
        self.enable_code_actions = settings.get("enable_code_actions", False)
        self.max_line_length = settings.get("max_line_length", -1)
        self.max_comment_line_length = settings.get("max_comment_line_length", -1)
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
                    ext_source_dirs = config_dict.get("ext_source_dirs", [])
                    # Legacy definition
                    if len(source_dirs) == 0:
                        source_dirs = config_dict.get("mod_dirs", [])
                    for source_dir in source_dirs:
                        dir_path = os.path.join(self.root_path, source_dir)
                        if os.path.isdir(dir_path):
                            self.source_dirs.append(dir_path)
                        else:
                            self.post_messages.append(
                                [2, r'Source directory "{0}" specified in '
                                 r'".fortls" settings file does not exist'.format(dir_path)]
                            )
                    for ext_source_dir in ext_source_dirs:
                        if os.path.isdir(ext_source_dir):
                            self.source_dirs.append(ext_source_dir)
                        else:
                            self.post_messages.append(
                                [2, r'External source directory "{0}" specified in '
                                 r'".fortls" settings file does not exist'.format(ext_source_dir)]
                            )
                    self.excl_suffixes = config_dict.get("excl_suffixes", [])
                    self.lowercase_intrinsics = config_dict.get("lowercase_intrinsics", self.lowercase_intrinsics)
                    self.debug_log = config_dict.get("debug_log", self.debug_log)
                    self.pp_defs = config_dict.get("pp_defs", {})
                    self.max_line_length = config_dict.get("max_line_length", self.max_line_length)
                    self.max_comment_line_length = config_dict.get("max_comment_line_length",
                                                                   self.max_comment_line_length)
                    if isinstance(self.pp_defs, list):
                        self.pp_defs = {key: "" for key in self.pp_defs}
            except:
                self.post_messages.append([1, 'Error while parsing ".fortls" settings file'])
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

        def get_candidates(scope_list, var_prefix, inc_globals=True,
                           public_only=False, abstract_only=False, no_use=False):
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
                if not no_use:
                    use_dict = get_use_tree(scope, use_dict, self.obj_tree)
            # Look in found use modules
            rename_list = [None for _ in var_list]
            for use_mod, only_info in use_dict.items():
                scope = self.obj_tree[use_mod][0]
                only_list = only_info[0]
                rename_map = only_info[1]
                if len(rename_map) > 0:
                    only_list = [rename_map.get(only_name, only_name) for only_name in only_list]
                tmp_list = child_candidates(scope, only_list, req_abstract=abstract_only)
                # Setup renaming
                if len(rename_map) > 0:
                    rename_reversed = {value: key for (key, value) in rename_map.items()}
                    for tmp_obj in tmp_list:
                        var_list.append(tmp_obj)
                        rename_list.append(rename_reversed.get(tmp_obj.name.lower(), None))
                else:
                    var_list += tmp_list
                    rename_list += [None for _ in tmp_list]
            # Add globals
            if inc_globals:
                tmp_list = [obj[0] for (_, obj) in self.obj_tree.items()]
                var_list += tmp_list + self.intrinsic_funs
                rename_list += [None for _ in tmp_list + self.intrinsic_funs]
            # Filter by prefix if necessary
            if var_prefix == '':
                return var_list, rename_list
            else:
                tmp_list = []
                tmp_rename = []
                for (i, var) in enumerate(var_list):
                    var_name = rename_list[i]
                    if var_name is None:
                        var_name = var.name
                    if var_name.lower().startswith(var_prefix):
                        tmp_list.append(var)
                        tmp_rename.append(rename_list[i])
                return tmp_list, tmp_rename

        def build_comp(candidate, name_only=self.autocomplete_name_only,
                       name_replace=None, is_interface=False):
            comp_obj = {}
            call_sig = None
            if name_only:
                comp_obj["label"] = candidate.name
            else:
                comp_obj["label"] = candidate.name
                if name_replace is not None:
                    comp_obj["label"] = name_replace
                call_sig, snippet = candidate.get_snippet(name_replace)
                if self.autocomplete_no_snippets:
                    snippet = call_sig
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
        params = request["params"]
        uri = params["textDocument"]["uri"]
        path = path_from_uri(uri)
        file_obj = self.workspace.get(path)
        if file_obj is None:
            return None
        # Check line
        ac_line = params["position"]["line"]
        ac_char = params["position"]["character"]
        # Get full line (and possible continuations) from file
        pre_lines, curr_line, _ = file_obj.get_code_line(ac_line, forward=False, strip_comment=True)
        line_prefix = get_line_prefix(pre_lines, curr_line, ac_char)
        if line_prefix is None:
            return None
        is_member = False
        try:
            var_stack = get_var_stack(line_prefix)
            is_member = (len(var_stack) > 1)
            var_prefix = var_stack[-1].strip()
        except:
            return None
        # print(var_stack)
        item_list = []
        scope_list = file_obj.ast.get_scopes(ac_line+1)
        # Get context
        name_only = self.autocomplete_name_only
        public_only = False
        include_globals = True
        line_context, context_info = get_line_context(line_prefix)
        if (line_context == 'skip') or (var_prefix == '' and (not is_member)):
            return None
        if self.autocomplete_no_prefix:
            var_prefix = ''
        # Suggestions for user-defined type members
        if is_member:
            curr_scope = file_obj.ast.get_inner_scope(ac_line+1)
            type_scope = climb_type_tree(var_stack, curr_scope, self.obj_tree)
            # Set enclosing type as scope
            if type_scope is None:
                return None
            else:
                include_globals = False
                scope_list = [type_scope]
        # Setup based on context
        req_callable = False
        abstract_only = False
        no_use = False
        type_mask = set_type_mask(False)
        type_mask[MODULE_TYPE_ID] = True
        type_mask[CLASS_TYPE_ID] = True
        if line_context == 'mod_only':
            # Module names only (USE statement)
            for key in self.obj_tree:
                candidate = self.obj_tree[key][0]
                if (candidate.get_type() == MODULE_TYPE_ID) and \
                   candidate.name.lower().startswith(var_prefix):
                    item_list.append(build_comp(candidate, name_only=True))
            return item_list
        elif line_context == 'mod_mems':
            # Public module members only (USE ONLY statement)
            name_only = True
            mod_name = context_info.lower()
            if mod_name in self.obj_tree:
                scope_list = [self.obj_tree[mod_name][0]]
                public_only = True
                include_globals = False
                type_mask[CLASS_TYPE_ID] = False
            else:
                return None
        elif line_context == 'pro_link':
            # Link to local subroutine/functions
            type_mask = set_type_mask(True)
            type_mask[SUBROUTINE_TYPE_ID] = False
            type_mask[FUNCTION_TYPE_ID] = False
            name_only = True
            include_globals = False
            no_use = True
        elif line_context == 'call':
            # Callable objects only ("CALL" statements)
            req_callable = True
        elif line_context == 'type_only':
            # User-defined types only (variable definitions, select clauses)
            type_mask = set_type_mask(True)
            type_mask[CLASS_TYPE_ID] = False
        elif line_context == 'import':
            # Import statement (variables and user-defined types only)
            name_only = True
            type_mask = set_type_mask(True)
            type_mask[CLASS_TYPE_ID] = False
            type_mask[VAR_TYPE_ID] = False
        elif line_context == 'int_only':
            # Interfaces only (procedure definitions)
            abstract_only = True
            include_globals = False
            name_only = True
            type_mask = set_type_mask(True)
            type_mask[SUBROUTINE_TYPE_ID] = False
            type_mask[FUNCTION_TYPE_ID] = False
        elif line_context == 'var_only':
            # Variables only (variable definitions)
            name_only = True
            type_mask[SUBROUTINE_TYPE_ID] = True
            type_mask[FUNCTION_TYPE_ID] = True
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
            return item_list
        elif line_context == 'first':
            # First word -> default context plus Fortran statements
            for candidate in get_intrinsic_keywords(self.statements, self.keywords, 0):
                if candidate.name.lower().startswith(var_prefix):
                    item_list.append(build_comp(candidate))
        # Build completion list
        candidate_list, rename_list = get_candidates(
            scope_list, var_prefix, include_globals, public_only, abstract_only, no_use
        )
        for (i, candidate) in enumerate(candidate_list):
            # Skip module names (only valid in USE)
            candidate_type = candidate.get_type()
            if type_mask[candidate_type]:
                continue
            if req_callable and (not candidate.is_callable()):
                continue
            #
            name_replace = rename_list[i]
            if candidate_type == INTERFACE_TYPE_ID:
                tmp_list = []
                if name_replace is None:
                    name_replace = candidate.name
                for member in candidate.mems:
                    tmp_text, _ = member.get_snippet(name_replace)
                    if tmp_list.count(tmp_text) > 0:
                        continue
                    tmp_list.append(tmp_text)
                    item_list.append(build_comp(member, name_replace=name_replace, is_interface=True))
                continue
            #
            item_list.append(build_comp(candidate, name_only=name_only, name_replace=name_replace))
        return item_list

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
                return None
            else:
                curr_scope = type_scope
        # Find in available scopes
        var_obj = None
        if curr_scope is not None:
            if (curr_scope.get_type() == CLASS_TYPE_ID) and (not is_member) and \
               ((line_prefix.lstrip().lower().startswith('procedure') and (line_prefix.count("=>") > 0))
               or TYPE_DEF_REGEX.match(line_prefix)):
                curr_scope = curr_scope.parent
            var_obj = find_in_scope(curr_scope, def_name, self.obj_tree)
        # Search in global scope
        if var_obj is None:
            if is_member:
                return None
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
        params = request["params"]
        uri = params["textDocument"]["uri"]
        path = path_from_uri(uri)
        file_obj = self.workspace.get(path)
        if file_obj is None:
            return None
        # Check line
        sig_line = params["position"]["line"]
        sig_char = params["position"]["character"]
        # Get full line (and possible continuations) from file
        pre_lines, curr_line, _ = file_obj.get_code_line(sig_line, forward=False, strip_comment=True)
        line_prefix = get_line_prefix(pre_lines, curr_line, sig_char)
        if line_prefix is None:
            return None
        # Test if scope declaration or end statement
        if SCOPE_DEF_REGEX.match(curr_line) or END_REGEX.match(curr_line):
            return None
        is_member = False
        try:
            sub_name, arg_strings, sub_end = get_sub_name(line_prefix)
            var_stack = get_var_stack(sub_name)
            is_member = (len(var_stack) > 1)
        except:
            return None
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
            return None
        # Build signature
        label, doc_str, params = var_obj.get_signature()
        if label is None:
            return None
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

    def get_all_references(self, def_obj, type_mem, file_obj=None):
        # Search through all files
        def_name = def_obj.name.lower()
        def_fqsn = def_obj.FQSN
        NAME_REGEX = re.compile(r'(?:\W|^)({0})(?:\W|$)'.format(def_name), re.I)
        if file_obj is None:
            file_set = self.workspace.items()
        else:
            file_set = ((file_obj.path, file_obj), )
        override_cache = []
        refs = {}
        ref_objs = []
        for filename, file_obj in file_set:
            file_refs = []
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
                                        ref_objs.append(var_def)
                                        ref_match = True
                                except:
                                    pass
                        if ref_match:
                            file_refs.append([i, match.start(1), match.end(1)])
            if len(file_refs) > 0:
                refs[filename] = file_refs
        return refs, ref_objs

    def serve_references(self, request):
        # Get parameters from request
        params = request["params"]
        uri = params["textDocument"]["uri"]
        def_line = params["position"]["line"]
        def_char = params["position"]["character"]
        path = path_from_uri(uri)
        # Find object
        file_obj = self.workspace.get(path)
        if file_obj is None:
            return None
        def_obj = self.get_definition(file_obj, def_line, def_char)
        if def_obj is None:
            return None
        # Determine global accesibility and type membership
        restrict_file = None
        type_mem = False
        if def_obj.FQSN.count(":") > 2:
            if def_obj.parent.get_type() == CLASS_TYPE_ID:
                type_mem = True
            else:
                restrict_file = def_obj.file_ast.file
                if restrict_file is None:
                    return None
        all_refs, _ = self.get_all_references(def_obj, type_mem, file_obj=restrict_file)
        refs = []
        for (filename, file_refs) in all_refs.items():
            for ref in file_refs:
                refs.append({
                    "uri": path_to_uri(filename),
                    "range": {
                        "start": {"line": ref[0], "character": ref[1]},
                        "end": {"line": ref[0], "character": ref[2]}
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
        # Find object
        file_obj = self.workspace.get(path)
        if file_obj is None:
            return None
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
        hover_array = []
        if (var_type == SUBROUTINE_TYPE_ID) or (var_type == FUNCTION_TYPE_ID):
            hover_str, highlight = var_obj.get_hover(long=True)
            hover_array.append(create_hover(hover_str, highlight))
        elif var_type == INTERFACE_TYPE_ID:
            for member in var_obj.mems:
                hover_str, highlight = member.get_hover(long=True)
                if hover_str is not None:
                    hover_array.append(create_hover(hover_str, highlight))
            return {"contents": hover_array}
        elif self.variable_hover and (var_type == 6):
            hover_str, highlight = var_obj.get_hover()
            hover_array.append(create_hover(hover_str, highlight))
            if self.hover_signature:
                sig_request = request.copy()
                sig_result = self.serve_signature(sig_request)
                try:
                    arg_id = sig_result.get("activeParameter")
                    if arg_id is not None:
                        arg_info = sig_result["signatures"][0]["parameters"][arg_id]
                        arg_doc = arg_info["documentation"]
                        doc_split = arg_doc.find("\n !!")
                        if doc_split < 0:
                            arg_string = "{0} :: {1}".format(arg_doc, arg_info["label"])
                        else:
                            arg_string = "{0} :: {1}{2}".format(arg_doc[:doc_split],
                                                                arg_info["label"], arg_doc[doc_split:])
                        hover_array.append(create_hover(arg_string, True))
                except:
                    pass
        #
        if len(hover_array) > 0:
            return {"contents": hover_array}
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
        # Get parameters from request
        params = request["params"]
        uri = params["textDocument"]["uri"]
        def_line = params["position"]["line"]
        def_char = params["position"]["character"]
        path = path_from_uri(uri)
        # Find object
        file_obj = self.workspace.get(path)
        if file_obj is None:
            return None
        def_obj = self.get_definition(file_obj, def_line, def_char)
        if def_obj is None:
            return None
        # Determine global accesibility and type membership
        restrict_file = None
        type_mem = False
        if def_obj.FQSN.count(":") > 2:
            if def_obj.parent.get_type() == CLASS_TYPE_ID:
                type_mem = True
            else:
                restrict_file = def_obj.file_ast.file
                if restrict_file is None:
                    return None
        all_refs, ref_objs = self.get_all_references(def_obj, type_mem, file_obj=restrict_file)
        if len(all_refs) == 0:
            self.post_message('Rename failed: No usages found to rename', type=2)
            return None
        # Create rename changes
        new_name = params["newName"]
        changes = {}
        for (filename, file_refs) in all_refs.items():
            file_uri = path_to_uri(filename)
            changes[file_uri] = []
            for ref in file_refs:
                changes[file_uri].append({
                    "range": {
                        "start": {"line": ref[0], "character": ref[1]},
                        "end": {"line": ref[0], "character": ref[2]}
                    },
                    "newText": new_name
                })
        # Check for implicit procedure implementation naming
        bind_obj = None
        if def_obj.get_type(no_link=True) == METH_TYPE_ID:
            _, curr_line, post_lines = def_obj.file_ast.file.get_code_line(
                def_obj.sline-1, backward=False, strip_comment=True
            )
            if curr_line is not None:
                full_line = curr_line + ''.join(post_lines)
                if full_line.find('=>') < 0:
                    bind_obj = def_obj
                    bind_change = "{0} => {1}".format(new_name, def_obj.name)
        elif (len(ref_objs) > 0) and (ref_objs[0].get_type(no_link=True) == METH_TYPE_ID):
            bind_obj = ref_objs[0]
            bind_change = "{0} => {1}".format(ref_objs[0].name, new_name)
        # Replace definition statement with explicit implementation naming
        if bind_obj is not None:
            def_uri = path_to_uri(bind_obj.file_ast.file.path)
            for change in changes[def_uri]:
                if change['range']['start']['line'] == bind_obj.sline-1:
                    change["newText"] = bind_change
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
                diags = file_obj.check_file(self.obj_tree, max_line_length=self.max_line_length,
                                            max_comment_line_length=self.max_comment_line_length)
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
            reparse_req = True
            if self.sync_type == 1:
                file_obj.apply_change(params["contentChanges"][0])
            else:
                try:
                    reparse_req = False
                    for change in params["contentChanges"]:
                        reparse_flag = file_obj.apply_change(change)
                        reparse_req = (reparse_req or reparse_flag)
                except:
                    self.post_message('Change request failed for file "{0}": Could not apply change'.format(path))
                    log.error('Change request failed for file "%s": Could not apply change', path, exc_info=True)
                    return
        # Parse newly updated file
        if reparse_req:
            _, err_str = self.update_workspace_file(path, update_links=True)
            if err_str is not None:
                self.post_message('Change request failed for file "{0}": {1}'.format(path, err_str))
                return
            # Update include statements linking to this file
            for _, tmp_file in self.workspace.items():
                tmp_file.ast.resolve_includes(self.workspace, path=path)
            file_obj.ast.resolve_includes(self.workspace)
            # Update inheritance (currently file only)
            # tmp_file.ast.resolve_links(self.obj_tree, self.link_version)
        elif file_obj.preproc:
            file_obj.preprocess(pp_defs=self.pp_defs)

    def serve_onClose(self, request):
        self.serve_onSave(request, did_close=True)

    def serve_onSave(self, request, did_close=False):
        # Update workspace from file on disk
        params = request["params"]
        uri = params["textDocument"]["uri"]
        filepath = path_from_uri(uri)
        # Skip update and remove objects if file is deleted
        if did_close and (not os.path.isfile(filepath)):
            # Remove old objects from tree
            file_obj = self.workspace.get(filepath)
            if file_obj is not None:
                ast_old = file_obj.ast
                if ast_old is not None:
                    for key in ast_old.global_dict:
                        self.obj_tree.pop(key, None)
            return
        did_change, err_str = self.add_file(filepath)
        if err_str is not None:
            self.post_message('Save request failed for file "{0}": {1}'.format(filepath, err_str))
            return
        if did_change:
            # Update include statements linking to this file
            for _, file_obj in self.workspace.items():
                file_obj.ast.resolve_includes(self.workspace, path=filepath)
            file_obj = self.workspace.get(filepath)
            file_obj.ast.resolve_includes(self.workspace)
            # Update inheritance/links
            self.link_version = (self.link_version + 1) % 1000
            for _, file_obj in self.workspace.items():
                file_obj.ast.resolve_links(self.obj_tree, self.link_version)
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
                hash_old = file_obj.hash
                file_obj.load_from_disk()
                if hash_old == file_obj.hash:
                    return False, None
            ast_new = process_file(file_obj, True, pp_defs=self.pp_defs)
        except:
            log.error("Error while parsing file %s", filepath, exc_info=True)
            return False, 'Error during parsing'  # Error during parsing
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
            self.link_version = (self.link_version + 1) % 1000
            ast_new.resolve_links(self.obj_tree, self.link_version)
        return True, None

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
        # Update inheritance/links
        self.link_version = (self.link_version + 1) % 1000
        for _, file_obj in self.workspace.items():
            file_obj.ast.resolve_links(self.obj_tree, self.link_version)

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
