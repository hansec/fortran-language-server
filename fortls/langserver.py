import logging
import sys
import os
import traceback
import re
try:
    from urllib.parse import unquote, quote
except ImportError:
    from urllib2 import quote
    from urlparse import unquote
# Local modules
from fortls.parse_fortran import process_file, read_use_stmt, read_var_def, \
    detect_fixed_format, detect_comment_start
from fortls.objects import find_in_scope, get_use_tree
from fortls.intrinsics import get_keywords, load_intrinsics, set_lowercase_intrinsics

log = logging.getLogger(__name__)
PY3K = sys.version_info >= (3, 0)
# Global regexes
FORTRAN_EXT_REGEX = re.compile(r'^\.F(77|90|95|03|08|OR|PP)?$', re.I)
OBJBREAK_REGEX = re.compile(r'[\/\-(.,+*<>=$: ]', re.I)
INT_STMNT_REGEX = re.compile(r'^[ ]*[a-z]*$', re.I)
WORD_REGEX = re.compile(r'[a-z_][a-z0-9_]*', re.I)
CALL_REGEX = re.compile(r'[ ]*CALL[ ]+[a-z0-9_%]*$', re.I)
TYPE_STMNT_REGEX = re.compile(r'[ ]*(TYPE|CLASS)[ ]*(IS)?[ ]*$', re.I)
TYPE_DEF_REGEX = re.compile(r'[ ]*TYPE[, ]+', re.I)
EXTENDS_REGEX = re.compile(r'EXTENDS[ ]*$', re.I)
PROCEDURE_STMNT_REGEX = re.compile(r'[ ]*(PROCEDURE)[ ]*$', re.I)
SCOPE_DEF_REGEX = re.compile(r'[ ]*(MODULE|PROGRAM|SUBROUTINE|FUNCTION)[ ]+', re.I)
END_REGEX = re.compile(r'[ ]*(END)( |MODULE|PROGRAM|SUBROUTINE|FUNCTION|TYPE|DO|IF|SELECT)?', re.I)
IMPORT_REGEX = re.compile(r'[ ]*IMPORT[ ]+', re.I)
FIXED_CONT_REGEX = re.compile(r'(     [\S])')
FREE_OPT_CONT_REGEX = re.compile(r'([ ]*&)')


def path_from_uri(uri):
    # Convert file uri to path (strip html like head part)
    if not uri.startswith("file://"):
        return uri
    if os.name == "nt":
        _, path = uri.split("file:///", 1)
    else:
        _, path = uri.split("file://", 1)
    return unquote(path)


def path_to_uri(path):
    # Convert path to file uri (add html like head part)
    if os.name == "nt":
        return "file:///" + quote(path)
    else:
        return "file://" + quote(path)


def read_file_split(filepath):
    # Read and add file from disk
    try:
        if PY3K:
            with open(filepath, 'r', encoding="utf-8") as fhandle:
                contents = re.sub(r'\t', r'  ', fhandle.read())
                contents_split = contents.splitlines()
        else:
            with open(filepath, 'r') as fhandle:
                contents = re.sub(r'\t', r'  ', fhandle.read())
                contents_split = contents.splitlines()
    except:
        log.error("Could not read/decode file %s", filepath, exc_info=True)
        return None, 'Could not read/decode file'
    else:
        return contents_split, None


def init_file(filepath):
    #
    contents_split, err_str = read_file_split(filepath)
    if contents_split is None:
        return None, err_str
    #
    try:
        fixed_flag = detect_fixed_format(contents_split)
        ast_new = process_file(contents_split, True, filepath, fixed_flag)
    except:
        log.error("Error while parsing file %s", filepath, exc_info=True)
        return None, 'Error during parsing'
    # Construct new file object and add to workspace
    tmp_obj = {
        "contents": contents_split,
        "ast": ast_new,
        "fixed": fixed_flag
    }
    return tmp_obj, None


def tokenize_line(line):
    paren_list = [[[-1, len(line)]], []]
    level = 1
    in_string = False
    string_char = ""
    for i, char in enumerate(line):
        if in_string:
            if char == string_char:
                in_string = False
            continue
        if char == '(':
            paren_list[level].append([i, len(line)])
            level += 1
            if len(paren_list) < level+1:
                paren_list.append([])
        elif char == ')':
            paren_list[level-1][-1][1] = i
            level -= 1
        elif char == "'" or char == '"':
            in_string = True
            string_char = char
    return paren_split(line, paren_list[:-1])


def paren_split(line, paren_list):
    sections = []
    for ilev, level in enumerate(paren_list):
        sections.append([])
        for igroup, group in enumerate(level):
            i1 = group[0]
            i2 = group[1]
            tmp_str = ""
            i3 = i1 + 1
            ranges = []
            if len(paren_list) > ilev+1:
                for lower_group in paren_list[ilev+1]:
                    if lower_group[0] > i1 and lower_group[1] <= i2:
                        tmp_str += line[i3:lower_group[0]]
                        ranges.append([i3, lower_group[0]])
                        i3 = lower_group[1] + 1
            if i3 < i2:
                tmp_str += line[i3:i2]
                ranges.append([i3, i2])
            if i3 == len(line):
                tmp_str += line[i3:i2]
                ranges.append([i3, i2])
            sections[ilev].append([ranges, tmp_str])
    return sections


def get_var_stack(line):
    if len(line) == 0:
        return None
    var_list = tokenize_line(line)
    deepest_var = None
    final_var = None
    final_paren = None
    deepest_paren = None
    n = len(line)
    for var_group in var_list:
        for var_tmp in var_group:
            for parens in var_tmp[0]:
                if n >= parens[0]:
                    if n <= parens[1]:
                        final_var = var_tmp[1]
                        final_paren = parens
                        break
                    elif parens[1] == -1:
                        deepest_var = var_tmp[1]
                        deepest_paren = parens
    if final_var is None:
        if deepest_var is not None:
            final_var = deepest_var
            final_paren = deepest_paren
        else:
            return None
    if final_var.find('%') < 0:
        ntail = final_paren[1] - final_paren[0]
        #
        if ntail == 0:
            final_var = ''
        elif ntail > 0:
            final_var = final_var[len(final_var)-ntail:]
    #
    if final_var is not None:
        final_op_split = OBJBREAK_REGEX.split(final_var)
        return final_op_split[-1].split('%')
    else:
        return None


def expand_name(line, char_poss):
    for word_match in WORD_REGEX.finditer(line):
        if word_match.start(0) <= char_poss and word_match.end(0) >= char_poss:
            return word_match.group(0)
    return ''


def climb_type_tree(var_stack, curr_scope, obj_tree):
    def get_type_name(var_obj):
        type_desc = var_obj.get_desc()
        i1 = type_desc.find('(')
        i2 = type_desc.find(')')
        if i1 >= 0 and i2 >= 0:
            return type_desc[i1+1:i2].strip().lower()
        else:
            return None
    # Find base variable in current scope
    type_name = None
    type_scope = None
    iVar = 0
    var_name = var_stack[iVar].strip().lower()
    var_obj, curr_scope = find_in_scope(curr_scope, var_name, obj_tree)
    if var_obj is not None:
        type_name = get_type_name(var_obj)
    # Search for type, then next variable in stack and so on
    for depth in range(30):
        # Find variable type in available scopes
        if type_name is None:
            break
        type_scope, curr_scope = find_in_scope(curr_scope, type_name, obj_tree)
        # Exit if not found
        if type_scope is None:
            break
        # Go to next variable in stack and exit if done
        iVar += 1
        if iVar == len(var_stack)-1:
            break
        # Find next variable by name in scope
        var_name = var_stack[iVar].strip().lower()
        var_obj, new_scope = find_in_scope(type_scope, var_name, obj_tree)
        if var_obj is not None:
            type_name = get_type_name(var_obj)
        else:
            break
    return type_scope


def get_line(line, character, file_obj):
    try:
        curr_line = file_obj["contents"][line]
    except:
        return None, character
    # Handle continuation lines
    if file_obj["fixed"]:  # Fixed format file
        tmp_line = file_obj["contents"][line]
        char_out = character
        prev_line = line-1
        while(prev_line > 0):
            if FIXED_CONT_REGEX.match(tmp_line):
                tmp_line = file_obj["contents"][prev_line]
                curr_line = tmp_line + curr_line[6:]
                char_out += len(tmp_line) - 6
            else:
                break
            prev_line = prev_line - 1
        return curr_line, char_out
    else:  # Free format file
        char_out = character
        prev_line = line-1
        opt_cont_match = FREE_OPT_CONT_REGEX.match(curr_line)
        if opt_cont_match is not None:
            curr_line = curr_line[opt_cont_match.end(0):]
            char_out -= opt_cont_match.end(0)
        while(prev_line > 0):
            tmp_line = file_obj["contents"][prev_line]
            tmp_no_comm = tmp_line.split('!')[0]
            cont_ind = tmp_no_comm.rfind('&')
            opt_cont_match = FREE_OPT_CONT_REGEX.match(tmp_no_comm)
            if opt_cont_match is not None:
                if cont_ind == opt_cont_match.end(0)-1:
                    break
                tmp_no_comm = tmp_no_comm[opt_cont_match.end(0):]
                cont_ind -= opt_cont_match.end(0)
            if cont_ind >= 0:
                curr_line = tmp_no_comm[:cont_ind] + curr_line
                char_out += cont_ind
            else:
                break
            prev_line = prev_line - 1
        return curr_line, char_out


def apply_change(contents_split, change):
    """Apply a change to the document."""
    text = change.get('text', "")
    change_range = change.get('range')
    if not PY3K:
        text = text.encode('utf-8')
    if len(text) == 0:
        text_split = [""]
    else:
        text_split = text.splitlines()
        # Check for ending newline
        if (text[-1] == "\n") or (text[-1] == "\r"):
            text_split.append("")

    if change_range is None:
        # The whole file has changed
        return text_split, -1

    start_line = change_range['start']['line']
    start_col = change_range['start']['character']
    end_line = change_range['end']['line']
    end_col = change_range['end']['character']

    # Check for an edit occuring at the very end of the file
    if start_line == len(contents_split):
        return contents_split + text_split, -1

    # Check for single line edit
    if (start_line == end_line) and (len(text_split) == 1):
        prev_line = contents_split[start_line]
        contents_split[start_line] = prev_line[:start_col] + text + prev_line[end_col:]
        return contents_split, start_line

    # Apply standard change to document
    new_contents = []
    for i, line in enumerate(contents_split):
        if (i < start_line) or (i > end_line):
            new_contents.append(line)
            continue

        if i == start_line:
            for j, change_line in enumerate(text_split):
                if j == 0:
                    new_contents.append(line[:start_col] + change_line)
                else:
                    new_contents.append(change_line)

        if i == end_line:
            new_contents[-1] += line[end_col:]
    return new_contents, -1


class LangServer:
    def __init__(self, conn, debug_log=False, settings={}):
        self.conn = conn
        self.running = True
        self.root_path = None
        self.fs = None
        self.all_symbols = None
        self.workspace = {}
        self.obj_tree = {}
        self.mod_dirs = []
        self.excl_paths = []
        self.post_messages = []
        self.streaming = True
        self.debug_log = debug_log
        # Get launch settings
        self.symbol_include_mem = settings.get("symbol_include_mem", True)
        self.sync_type = settings.get("sync_type", 1)
        self.autocomplete_no_prefix = settings.get("autocomplete_no_prefix", False)
        self.lowercase_intrinsics = settings.get("lowercase_intrinsics", False)
        self.use_signature_help = settings.get("use_signature_help", False)
        self.variable_hover = settings.get("variable_hover", False)

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
            "textDocument/didOpen": self.serve_onSave,
            "textDocument/didSave": self.serve_onSave,
            "textDocument/didClose": self.serve_onClose,
            "textDocument/didChange": self.serve_onChange,
            "initialized": noop,
            "workspace/didChangeWatchedFiles": noop,
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
        self.mod_dirs.append(self.root_path)
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
                    for mod_dir in config_dict.get("mod_dirs", []):
                        self.mod_dirs.append(os.path.join(self.root_path, mod_dir))
                    self.lowercase_intrinsics = config_dict.get("lowercase_intrinsics", self.lowercase_intrinsics)
                    self.debug_log = config_dict.get("debug_log", self.debug_log)
            except:
                self.post_messages.append([1, "Error while parsing '.fortls' settings file"])
        # Setup logging
        if self.debug_log and (self.root_path != ""):
            logging.basicConfig(filename=os.path.join(self.root_path, "fortls_debug.log"),
                                level=logging.DEBUG, filemode='w')
            log.debug("REQUEST %s %s", request.get("id"), request.get("method"))
            self.post_messages.append([3, "FORTLS debugging enabled"])
        # Load intrinsics
        if self.lowercase_intrinsics:
            set_lowercase_intrinsics()
        self.statements, self.keywords, self.intrinsic_funs, self.intrinsic_mods = load_intrinsics()
        for module in self.intrinsic_mods:
            self.obj_tree[module.FQSN] = [module, None]
        # Recursively add sub-directories
        if len(self.mod_dirs) == 1:
            self.mod_dirs = []
            for dirName, subdirList, fileList in os.walk(self.root_path):
                if self.excl_paths.count(dirName) > 0:
                    while(len(subdirList) > 0):
                        del subdirList[0]
                    continue
                contains_source = False
                for filename in fileList:
                    if filename.endswith('__genmod.f90'):
                        # Automatically generated by Intel Fortran.
                        continue
                    _, ext = os.path.splitext(os.path.basename(filename))
                    if FORTRAN_EXT_REGEX.match(ext):
                        contains_source = True
                        break
                if contains_source:
                    self.mod_dirs.append(os.path.join(self.root_path, dirName))
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
            "textDocumentSync": self.sync_type
        }
        if self.use_signature_help:
            server_capabilities["signatureHelpProvider"] = {
                "triggerCharacters": ["(", ","]
            }
        return {"capabilities": server_capabilities}
        #     "workspaceSymbolProvider": True,
        #     "streaming": False,
        # }

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
        # Get file AST
        if path not in self.workspace:
            return []
        file_obj = self.workspace[path]["ast"]
        # Add scopes to outline view
        test_output = []
        for scope in file_obj.get_scopes():
            if (scope.name[0] == "#") or (scope.get_type() == 8):
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
            eline = sline
            if scope.eline is not None:
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
            if scope.get_type() == 4 and self.symbol_include_mem:
                for child in scope.children:
                    tmp_out = {}
                    tmp_out["name"] = child.name
                    tmp_out["kind"] = map_types(child.get_type())
                    tmp_out["location"] = {
                        "uri": uri,
                        "range": {
                            "start": {"line": child.sline-1, "character": 0},
                            "end": {"line": child.sline-1, "character": 1}
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
            return [def_value for i in range(10)]

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
                for key, obj in self.obj_tree.items():
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
            doc_str, _ = candidate.get_documentation()
            if doc_str is not None:
                comp_obj["documentation"] = doc_str
            return comp_obj

        def get_context(line, var_prefix):
            line_grouped = tokenize_line(line)
            lev1_end = line_grouped[0][0][0][-1][1]
            if lev1_end < 0:
                lev1_end = len(line)
            # Test if variable definition statement
            test_match = read_var_def(line)
            if test_match is not None:
                if test_match[0] == 'var':
                    if (test_match[1][2] is None) and (lev1_end == len(line)):
                        return 8, var_prefix, None
                    return 7, var_prefix, None
            # Test if in USE statement
            test_match = read_use_stmt(line)
            if test_match is not None:
                if len(test_match[1][1]) > 0:
                    return 2, var_prefix, test_match[1][0]
                else:
                    return 1, var_prefix, None
            # Test if scope declaration or end statement
            if SCOPE_DEF_REGEX.match(line) or END_REGEX.match(line):
                return -1, None, None
            # Test if import statement
            if IMPORT_REGEX.match(line):
                return 5, var_prefix, None
            # In type-def
            type_def = False
            if TYPE_DEF_REGEX.match(line) is not None:
                type_def = True
            # Test if in call statement
            if lev1_end == len(line):
                if CALL_REGEX.match(line_grouped[0][0][1]) is not None:
                    return 3, var_prefix, None
            # Test if variable definition using type/class or procedure
            if len(line_grouped) >= 2:
                lev2_end = line_grouped[1][0][0][-1][1]
                if lev2_end < 0:
                    lev2_end = len(line)
                if (lev2_end == len(line)
                        and line_grouped[1][0][0][-1][0] == lev1_end + 1):
                    test_str = line_grouped[0][0][1]
                    if ((TYPE_STMNT_REGEX.match(test_str) is not None)
                            or (type_def and EXTENDS_REGEX.search(test_str) is not None)):
                        return 4, var_prefix, None
                    if PROCEDURE_STMNT_REGEX.match(test_str) is not None:
                        return 6, var_prefix, None
            # Only thing on line?
            if INT_STMNT_REGEX.match(line) is not None:
                return 9, var_prefix, None
            # Default context
            if type_def:
                return -1, var_prefix, None
            else:
                return 0, var_prefix, None
        # Get parameters from request
        req_dict = {"isIncomplete": False, "items": []}
        params = request["params"]
        uri = params["textDocument"]["uri"]
        path = path_from_uri(uri)
        if path not in self.workspace:
            return req_dict
        # Check line
        ac_line = params["position"]["line"]
        ac_char = params["position"]["character"]
        # Get full line (and possible continuations) from file
        curr_line, ac_char = get_line(ac_line, ac_char, self.workspace[path])
        if curr_line is None:
            return req_dict
        is_member = False
        try:
            line_prefix = curr_line[:ac_char].lower()
            # Ignore for comment lines
            comm_start = detect_comment_start(line_prefix, self.workspace[path]["fixed"])
            if (comm_start >= 0) or (line_prefix[0] == '#'):
                return req_dict
            # Ignore string literals
            if (line_prefix.count("'") % 2 == 1) or \
               (line_prefix.count('"') % 2 == 1):
                return None
            var_stack = get_var_stack(line_prefix)
            is_member = (len(var_stack) > 1)
            var_prefix = var_stack[-1].strip()
        except:
            return req_dict
        # print(var_stack)
        file_obj = self.workspace[path]["ast"]
        item_list = []
        scope_list = file_obj.get_scopes(ac_line+1)
        # Get context
        name_only = False
        public_only = False
        include_globals = True
        line_context, var_prefix, context_info = \
            get_context(line_prefix, var_prefix)
        if (line_context < 0) or (var_prefix == '' and not (is_member or line_context == 2)):
            return req_dict
        if self.autocomplete_no_prefix:
            var_prefix = ''
        # Suggestions for user-defined type members
        if is_member:
            curr_scope = file_obj.get_inner_scope(ac_line+1)
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
        type_mask[8] = True
        type_mask[9] = True
        if line_context == 1:
            # Use statement module part (modules only)
            for key in self.obj_tree:
                candidate = self.obj_tree[key][0]
                if (candidate.get_type() == 1) and \
                   candidate.name.lower().startswith(var_prefix):
                    item_list.append(build_comp(candidate, name_only=True))
            req_dict["items"] = item_list
            return req_dict
        elif line_context == 2:
            # Use statement only part (module public members only)
            name_only = True
            mod_name = context_info.lower()
            if mod_name in self.obj_tree:
                scope_list = [self.obj_tree[mod_name][0]]
                public_only = True
                include_globals = False
                type_mask[4] = False
            else:
                return {"isIncomplete": False, "items": []}
        elif line_context == 3:
            # Filter callables for call statements
            req_callable = True
        elif line_context == 4:
            # Variable definition statement for user-defined type
            # (user-defined types only)
            type_mask = set_type_mask(True)
            type_mask[4] = False
        elif line_context == 5:
            # Include statement (variables and user-defined types only)
            name_only = True
            type_mask = set_type_mask(True)
            type_mask[4] = False
            type_mask[6] = False
        elif line_context == 6:
            # Variable definition statement for procedure with interface
            # (interfaces only)
            abstract_only = True
            include_globals = False
            name_only = True
            type_mask = set_type_mask(True)
            type_mask[2] = False
            type_mask[3] = False
        elif line_context == 7:
            # Variable definition statement (variables only)
            name_only = True
            type_mask[2] = True
            type_mask[3] = True
        elif line_context == 8:
            # Variable definition keywords (variables only)
            key_context = 0
            enc_scope_type = scope_list[-1].get_type()
            if enc_scope_type == 1:
                key_context = 1
            elif (enc_scope_type == 2) or (enc_scope_type == 3):
                key_context = 2
            elif enc_scope_type == 4:
                key_context = 3
            for candidate in get_keywords(self.statements, self.keywords, key_context):
                if candidate.name.lower().startswith(var_prefix):
                    item_list.append(build_comp(candidate))
            req_dict["items"] = item_list
            return req_dict
        elif line_context == 9:
            # First word -> default context plus Fortran statements
            for candidate in get_keywords(self.statements, self.keywords, 0):
                if candidate.name.lower().startswith(var_prefix):
                    item_list.append(build_comp(candidate))
        for candidate in get_candidates(scope_list, var_prefix, include_globals, public_only, abstract_only):
            # Skip module names (only valid in USE)
            candidate_type = candidate.get_type()
            if type_mask[candidate_type]:
                continue
            if req_callable and (not candidate.is_callable()):
                continue
            #
            if candidate_type == 5:
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
        curr_line, def_char = get_line(def_line, def_char, def_file)
        if curr_line is None:
            return None
        #
        is_member = False
        try:
            line_prefix = curr_line[:def_char].lower()
            # Ignore for comment lines
            comm_start = detect_comment_start(line_prefix, def_file["fixed"])
            if (comm_start >= 0) or (line_prefix[0] == '#'):
                return None
            # Ignore string literals
            if (line_prefix.count("'") % 2 == 1) or \
               (line_prefix.count('"') % 2 == 1):
                return None
            var_stack = get_var_stack(line_prefix)
            is_member = (len(var_stack) > 1)
            def_name = expand_name(curr_line, def_char)
        except:
            return None
        # print(var_stack, def_name)
        if def_name == '':
            return None
        file_obj = def_file["ast"]
        curr_scope = file_obj.get_inner_scope(def_line+1)
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
            var_obj, enc_scope = find_in_scope(curr_scope, def_name, self.obj_tree)
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
            nLine = len(line)
            line_grouped = tokenize_line(line)
            if len(line_grouped) < 2:
                return None, None, None
            lowest_level = -1
            for i, level in enumerate(line_grouped):
                if level[-1][0][-1][-1] == nLine:
                    lowest_level = i
            if lowest_level > 0:
                arg_string = ''
                for char_group in line_grouped[lowest_level]:
                    arg_string += char_group[-1]
                return line_grouped[lowest_level-1][0][1].strip(), arg_string.split(','), \
                    line_grouped[lowest_level-1][0][0][-1][1]
            else:
                return None, None, None

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
        if path not in self.workspace:
            return req_dict
        # Check line
        sig_line = params["position"]["line"]
        sig_char = params["position"]["character"]
        # Get full line (and possible continuations) from file
        curr_line, sig_char = get_line(sig_line, sig_char, self.workspace[path])
        if curr_line is None:
            return req_dict
        # Test if scope declaration or end statement
        if SCOPE_DEF_REGEX.match(curr_line) or END_REGEX.match(curr_line):
            return req_dict
        is_member = False
        try:
            line_prefix = curr_line[:sig_char].lower()
            # Ignore for comment lines
            comm_start = detect_comment_start(line_prefix, self.workspace[path]["fixed"])
            if (comm_start >= 0) or (line_prefix[0] == '#'):
                return req_dict
            # Ignore string literals
            if (line_prefix.count("'") % 2 == 1) or \
               (line_prefix.count('"') % 2 == 1):
                return req_dict
            sub_name, arg_strings, sub_end = get_sub_name(line_prefix)
            var_stack = get_var_stack(sub_name)
            is_member = (len(var_stack) > 1)
        except:
            return req_dict
        #
        file_obj = self.workspace[path]["ast"]
        curr_scope = file_obj.get_inner_scope(sig_line+1)
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
            var_obj, enc_scope = find_in_scope(curr_scope, sub_name, self.obj_tree)
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
            for candidate in get_keywords(self.statements, self.keywords, 0):
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
        refs = []
        # Find object
        if path in self.workspace:
            def_obj = self.get_definition(self.workspace[path], def_line, def_char)
        else:
            return []
        # Currently only support global and top level module objects
        if def_obj.FQSN.count(":") > 2:
            return refs
        # Search through all files
        if def_obj is not None:
            def_name = def_obj.name.lower()
            def_fqsn = def_obj.FQSN
            NAME_REGEX = re.compile(r'(?:\W|^)({0})(?:\W|$)'.format(def_name), re.I)
            for filename, file_obj in sorted(self.workspace.items()):
                # Search through file line by line
                for (i, line) in enumerate(file_obj["contents"]):
                    if len(line) == 0:
                        continue
                    # Skip comment lines
                    comm_start = detect_comment_start(line, file_obj["fixed"])
                    if (comm_start == 0) or (line[0] == '#'):
                        continue
                    elif comm_start > 0:
                        line = line[:comm_start]
                    for match in NAME_REGEX.finditer(line):
                        var_def = self.get_definition(file_obj, i, match.start(1))
                        if var_def is not None:
                            if def_fqsn == var_def.FQSN:
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
        # Find object
        if path in self.workspace:
            var_obj = self.get_definition(self.workspace[path], def_line, def_char)
        else:
            return None
        # Construct link reference
        if var_obj is not None:
            if var_obj.file.path is not None:
                sline = var_obj.sline-1
                return {
                    "uri": path_to_uri(var_obj.file.path),
                    "range": {
                        "start": {"line": sline, "character": 0},
                        "end": {"line": sline, "character": 1}
                    }
                }
        else:
            return None

    def serve_hover(self, request):
        def create_hover(string, highlight):
            if highlight:
                return {
                    "language": "fortran",
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
        # Find object
        if path in self.workspace:
            var_obj = self.get_definition(self.workspace[path], def_line, def_char)
        else:
            return None
        # Construct hover information
        if var_obj is not None:
            var_type = var_obj.get_type()
            hover_str = None
            if var_type == 2 or var_type == 3:
                hover_str, highlight = var_obj.get_documentation(long=True)
            elif var_type == 5:
                hover_array = []
                for member in var_obj.mems:
                    hover_str, highlight = member.get_documentation(long=True)
                    if hover_str is not None:
                        hover_array.append(create_hover(hover_str, highlight))
                return {"contents": hover_array}
            elif var_type == 6 and self.variable_hover:
                hover_str, highlight = var_obj.get_documentation()
            #
            if hover_str is not None:
                return {"contents": create_hover(hover_str, highlight)}
        else:
            return None

    def get_diagnostics(self, uri):
        filepath = path_from_uri(uri)
        if filepath in self.workspace:
            file_obj = self.workspace[filepath]["ast"]
            file_contents = self.workspace[filepath]["contents"]
            try:
                diags = file_obj.check_file(self.obj_tree, file_contents)
            except Exception as e:
                self.conn.write_error(
                    -1,
                    code=-32603,
                    message=str(e),
                    data={
                        "traceback": traceback.format_exc(),
                    })
            else:
                self.conn.send_notification("textDocument/publishDiagnostics", {
                    "uri": uri,
                    "diagnostics": diags
                })

    def serve_onChange(self, request):
        # Update workspace from file sent by editor
        params = request["params"]
        uri = params["textDocument"]["uri"]
        path = path_from_uri(uri)
        # Update file contents with changes
        if self.sync_type == 1:
            file_text = params["contentChanges"][0]["text"]
            if not PY3K:
                file_text = file_text.encode('utf-8')
            new_contents = file_text.splitlines()
        else:
            if path in self.workspace:
                new_contents = self.workspace[path]["contents"]
                try:
                    for change in params["contentChanges"]:
                        old_contents = new_contents
                        new_contents, line_tmp = apply_change(old_contents, change)
                except:
                    self.post_message('Change request failed for file "{0}": Could not apply change'.format(path))
                    log.error('Change request failed for file "%s": Could not apply change', path, exc_info=True)
                    return
            else:
                self.post_message('Change request failed for unknown file "{0}"'.format(path))
                log.error('Change request failed for unknown file "%s"', path)
                return
        # Parse newly updated file
        err_str = self.update_workspace_file(new_contents, path, update_links=True)
        if err_str is not None:
            self.post_message('Change request failed for file "{0}": {1}'.format(path, err_str))
            return
        # Update include statements linking to this file
        for tmp_path, file_obj in self.workspace.items():
            file_obj["ast"].resolve_includes(self.workspace, path=path)
        self.workspace[path]["ast"].resolve_includes(self.workspace)
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
        for tmp_path, file_obj in self.workspace.items():
            file_obj["ast"].resolve_includes(self.workspace, path=filepath)
        self.workspace[filepath]["ast"].resolve_includes(self.workspace)
        # Update inheritance
        for key in self.obj_tree:
            self.obj_tree[key][0].resolve_inherit(self.obj_tree)
            self.obj_tree[key][0].resolve_link(self.obj_tree)
        self.get_diagnostics(uri)

    def add_file(self, filepath):
        # Read and add file from disk
        contents_split, err_str = read_file_split(filepath)
        if contents_split is None:
            return err_str
        return self.update_workspace_file(contents_split, filepath)

    def update_workspace_file(self, contents_split, filepath, update_links=False):
        # Update workspace from file contents and path
        try:
            fixed_flag = detect_fixed_format(contents_split)
            ast_new = process_file(contents_split, True, filepath, fixed_flag)
        except:
            log.error("Error while parsing file %s", filepath, exc_info=True)
            return 'Error during parsing'  # Error during parsing
        # Remove old objects from tree
        if filepath in self.workspace:
            ast_old = self.workspace[filepath]["ast"]
            for key in ast_old.global_dict:
                self.obj_tree.pop(key, None)
        # Construct new file object and add to workspace
        tmp_obj = {
            "contents": contents_split,
            "ast": ast_new,
            "fixed": fixed_flag
        }
        self.workspace[filepath] = tmp_obj
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
        for mod_dir in self.mod_dirs:
            for filename in os.listdir(mod_dir):
                basename, ext = os.path.splitext(os.path.basename(filename))
                if FORTRAN_EXT_REGEX.match(ext):
                    filepath = os.path.join(mod_dir, filename)
                    if self.excl_paths.count(filepath) > 0:
                        continue
                    file_list.append(filepath)
        # Process files
        from multiprocessing import Pool
        pool = Pool(processes=4)
        results = {}
        for filepath in file_list:
            results[filepath] = pool.apply_async(init_file, args=(filepath,))
        pool.close()
        pool.join()
        for path, result in results.items():
            result_obj = result.get()
            if result_obj[0] is None:
                self.post_messages.append([1, 'Initialization failed for file "{0}": {1}'.format(path, result_obj[1])])
                continue
            self.workspace[path] = result_obj[0]
            # Add top-level objects to object tree
            ast_new = self.workspace[path]["ast"]
            for key in ast_new.global_dict:
                self.obj_tree[key] = [ast_new.global_dict[key], path]
        # Update include statements
        for path, file_obj in self.workspace.items():
            file_obj["ast"].resolve_includes(self.workspace)
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
