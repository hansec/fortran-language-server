import logging
import sys
import os
import traceback
import re
# Local modules
from fortls.parse_fortran import process_file, read_use_stmt, detect_fixed_format, \
    detect_comment_start
from fortls.objects import find_in_scope, fortran_meth, get_use_tree

log = logging.getLogger(__name__)
PY3K = sys.version_info >= (3, 0)
# Global regexes
FORTRAN_EXT_REGEX = re.compile(r'^\.F(77|90|95|03|08|OR|PP)?$', re.I)
OBJBREAK_REGEX = re.compile(r'[\/\-(.,+*<>=$: ]', re.I)
WORD_REGEX = re.compile(r'[a-z_][a-z0-9_]*', re.I)
CALL_REGEX = re.compile(r'[ \t]*CALL[ \t]+[a-z0-9_%]*$', re.I)
TYPE_STMNT_REGEX = re.compile(r'[ \t]*(TYPE|CLASS)[ \t]*(IS)?[ \t]*[a-z0-9_]*$', re.I)
FIXED_CONT_REGEX = re.compile(r'(     [\S])')
FREE_OPT_CONT_REGEX = re.compile(r'([ \t]*&)')


def path_from_uri(uri):
    # Convert file uri to path (strip html like head part)
    if not uri.startswith("file://"):
        return uri
    if os.name == "nt":
        _, path = uri.split("file:///", 1)
    else:
        _, path = uri.split("file://", 1)
    return path


def path_to_uri(path):
    # Convert path to file uri (add html like head part)
    if os.name == "nt":
        return "file:///"+path
    else:
        return "file://"+path


def init_file(filepath):
    #
    if PY3K:
        with open(filepath, 'r', encoding="utf-8") as fhandle:
            contents_split = fhandle.readlines()
    else:
        with open(filepath, 'r') as fhandle:
            contents_split = fhandle.readlines()
    #
    try:
        fixed_flag = detect_fixed_format(contents_split)
        ast_new = process_file(contents_split, True, filepath, fixed_flag)
    except:
        return None
    else:
        # Construct new file object and add to workspace
        tmp_obj = {
            "contents": contents_split,
            "ast": ast_new,
            "fixed": fixed_flag
        }
        return tmp_obj


def tokenize_line(line):
    var_list = []
    paren_stack = [[]]
    paren_count = [0]
    iCurr = -1
    for (i, character) in enumerate(line):
        if character == "(":
            iCurr += 1
            if len(paren_stack) < iCurr+1:
                paren_stack.append([])
                paren_count.append(0)
            paren_stack[iCurr].append([i, -1])
        elif character == ")":
            paren_stack[iCurr][-1][1] = i
            iCurr -= 1
    #
    if len(paren_stack[0]) == 0:
        var_list.append([[[[0, -1]], line]])
    else:
        var_groups = [[] for i in range(len(paren_stack)+1)]
        var_list, out_len = paren_split(line, paren_stack, 0, var_groups)
    return var_list


def paren_split(line, paren_groups, level, var_groups, i1=0, i2=-1):
    tmp_list = []
    tmp_str = ''
    nline = len(line)
    if i2 < 0:
        i2 = nline
    i0 = i1
    for paren_group in paren_groups[level]:
        pg0 = paren_group[0]
        pg1 = paren_group[1]
        if pg0 < i1 or pg1 > i2:
            continue
        tmp_str += line[i0:pg0]
        tmp_list.append([i0, pg0])
        if pg1 < 0:
            pg1 = nline
        if len(paren_groups) > level+1:
            var_groups, out_len = \
                paren_split(line, paren_groups, level+1,
                            var_groups, pg0+1, pg1)
            if out_len == 0:
                var_groups[level+1].append([[[pg0+1, pg1]], line[pg0+1:pg1]])
        else:
            var_groups[level+1].append([[[pg0+1, pg1]], line[pg0+1:pg1]])
        if pg1 == nline:
            continue
        i0 = pg1+1
    #
    if i0 > tmp_list[-1][1]:
        tmp_str += line[i0:i2]
        tmp_list.append([i0, i2])
    out_list = [[tmp_list, tmp_str]]
    var_groups[level].extend(out_list)
    return var_groups, len(out_list)


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
    text = change['text']
    change_range = change.get('range')
    if len(text) == 0:
        text_split = [""]
    else:
        text_split = text.splitlines()
        # Check for ending newline
        if text[-1] == "\n" or text[-1] == "\r":
            text_split.append("")

    if not change_range:
        # The whole file has changed
        return text_split, -1

    start_line = change_range['start']['line']
    start_col = change_range['start']['character']
    end_line = change_range['end']['line']
    end_col = change_range['end']['character']

    # Check for an edit occuring at the very end of the file
    if start_line == len(contents_split):
        return contents_split.extend(text_split), -1

    # Check for single line edit
    if start_line == end_line and len(text_split) == 1:
        prev_line = contents_split[start_line]
        contents_split[start_line] = prev_line[:start_col] + text + prev_line[end_col:]
        return contents_split, start_line

    # Apply standard change to document
    new_contents = []
    for i, line in enumerate(contents_split):
        if i < start_line or i > end_line:
            new_contents.append(line)
            continue

        if i == start_line:
            for j, change_line in enumerate(text_split):
                if j == 0:
                    new_contents.append(line[:start_col]+change_line)
                else:
                    new_contents.append(change_line)

        if i == end_line:
            new_contents[-1] += line[end_col:]
    return new_contents, -1


class LangServer:
    def __init__(self, conn, logLevel=0, settings=None):
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
        self.symbol_include_mem = True
        self.autocomplete_no_prefix = False
        self.sync_type = 1
        if logLevel == 0:
            logging.basicConfig(level=logging.ERROR)
        elif logLevel == 1:
            logging.basicConfig(level=logging.DEBUG)
        if settings is not None:
            if "symbol_include_mem" in settings:
                self.symbol_include_mem = settings["symbol_include_mem"]
            if "sync_type" in settings:
                self.sync_type = settings["sync_type"]
            if "autocomplete_no_prefix" in settings:
                self.autocomplete_no_prefix = settings["autocomplete_no_prefix"]

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
                    self.conn.send_notification("window/showMessage", {
                        "type": message[0],
                        "message": message[1]
                    })
                self.post_messages = []

    def handle(self, request):
        def noop(request):
            return None
        # Request handler
        log.info("REQUEST %s %s", request.get("id"), request.get("method"))
        handler = {
            "initialize": self.serve_initialize,
            "textDocument/documentSymbol": self.serve_document_symbols,
            "textDocument/completion": self.serve_autocomplete,
            "textDocument/definition": self.serve_definition,
            "textDocument/references": self.serve_references,
            "textDocument/hover": self.serve_hover,
            "textDocument/didOpen": self.serve_onSave,
            "textDocument/didSave": self.serve_onSave,
            "textDocument/didClose": self.serve_onSave,
            "textDocument/didChange": self.serve_onChange,
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
        except Exception as e:
            log.warning("handler for %s failed", request, exc_info=True)
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
            import json
            with open(config_path, 'r') as fhandle:
                config_dict = json.load(fhandle)
                if "excl_paths" in config_dict:
                    for excl_path in config_dict["excl_paths"]:
                        self.excl_paths.append(os.path.join(self.root_path, excl_path))
                if "mod_dirs" in config_dict:
                    for mod_dir in config_dict["mod_dirs"]:
                        self.mod_dirs.append(os.path.join(self.root_path, mod_dir))
        if len(self.mod_dirs) == 1:  # Recursively add sub-directories
            self.mod_dirs = []
            for dirName, subdirList, fileList in os.walk(self.root_path):
                if self.excl_paths.count(dirName) > 0:
                    while(len(subdirList) > 0):
                        del subdirList[0]
                    continue
                contains_source = False
                for filename in fileList:
                    basename, ext = os.path.splitext(os.path.basename(filename))
                    if FORTRAN_EXT_REGEX.match(ext):
                        contains_source = True
                        break
                if contains_source:
                    self.mod_dirs.append(os.path.join(self.root_path, dirName))
        # Initialize workspace
        self.workspace_init()
        #
        return {
            "capabilities": {
                "documentSymbolProvider": True,
                "completionProvider": {
                    "resolveProvider": False,
                    "triggerCharacters": ["%"]
                },
                "definitionProvider": True,
                "referencesProvider": True,
                "hoverProvider": True,
                "textDocumentSync": self.sync_type
            }
        }
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
            scope_tree = scope.FQSN.split("::")
            if len(scope_tree) > 2:
                if scope_tree[1].startswith("gen_int"):
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
                return 5
            elif type == 6:
                return 6
            else:
                return type

        def get_candidates(scope_list, var_prefix, inc_globals=True, public_only=False):
            var_list = []
            use_dict = {}
            for scope in scope_list:
                # Filter children
                def_vis = scope.def_vis
                for child in scope.get_children():
                    if public_only:
                        if child.vis < 0:
                            continue
                        if def_vis < 0 and child.vis <= 0:
                            continue
                    if child.name.lower().startswith(var_prefix):
                        var_list.append(child)
                # Add to use list
                use_dict = get_use_tree(scope, use_dict, self.obj_tree)
            # Look in found use modules
            for use_mod, only_list in use_dict.items():
                scope = self.obj_tree[use_mod][0]
                # Filter children
                tmp_vars = []
                def_vis = scope.def_vis
                for child in scope.get_children():
                    if child.vis < 0:
                        continue
                    if def_vis < 0 and child.vis <= 0:
                        continue
                    if child.name.lower().startswith(var_prefix):
                        tmp_vars.append(child)
                # Filter by ONLY statement
                if len(only_list) > 0:
                    for poss_var in tmp_vars:
                        if poss_var.name in only_list:
                            var_list.append(poss_var)
                else:
                    var_list.extend(tmp_vars)
            # Add globals
            if inc_globals:
                for key in self.obj_tree:
                    global_obj = self.obj_tree[key][0]
                    if global_obj.name.lower().startswith(var_prefix):
                        var_list.append(self.obj_tree[key][0])
            return var_list

        def build_comp(candidate, name_only=False, name_replace=None):
            comp_obj = {}
            if name_only:
                comp_obj["label"] = candidate.name
            else:
                comp_obj["label"], snippet = candidate.get_snippet(name_replace)
                if snippet is not None:
                    comp_obj["insertText"] = snippet
                    comp_obj["insertTextFormat"] = 2
            comp_obj["kind"] = map_types(candidate.get_type())
            comp_obj["detail"] = candidate.get_desc()
            doc_str = candidate.get_documentation()
            if doc_str is not None:
                comp_obj["documentation"] = doc_str
            return comp_obj

        def get_context(line, var_prefix):
            # tmp_prefix = var_prefix
            line_grouped = tokenize_line(line)
            # Test if in call statement
            lev1_end = line_grouped[0][0][0][-1][1]
            if lev1_end < 0:
                lev1_end = len(line)
            if lev1_end == len(line):
                test_match = CALL_REGEX.match(line_grouped[0][0][1])
                if test_match is not None:
                    return 3, var_prefix, None
            # Test if variable definition using type/class
            if len(line_grouped) >= 2:
                lev2_end = line_grouped[1][0][0][-1][1]
                if lev2_end < 0:
                    lev2_end = len(line)
                if lev2_end == len(line) and \
                   line_grouped[1][0][0][-1][0] == lev1_end + 1:
                    test_match = TYPE_STMNT_REGEX.match(line_grouped[0][0][1])
                    if test_match is not None:
                        return 4, var_prefix, None
            # Test if in USE statement
            test_match = read_use_stmt(line)
            if test_match is not None:
                if len(test_match[1][1]) > 0:
                    return 2, var_prefix, test_match[1][0]
                else:
                    return 1, var_prefix, None
            # Default context
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
            if detect_comment_start(line_prefix, self.workspace[path]["fixed"]) >= 0:
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
        line_context, var_prefix, context_info = \
            get_context(line_prefix, var_prefix)
        if var_prefix == '' and not (is_member or line_context == 2):
            return req_dict
        if self.autocomplete_no_prefix:
            var_prefix = ''
        # USE stmnt
        if line_context == 1:  # module part
            for key in self.obj_tree:
                candidate = self.obj_tree[key][0]
                candidate_type = candidate.get_type()
                if candidate_type != 1:
                    continue
                if candidate.name.lower().startswith(var_prefix):
                    item_list.append(build_comp(candidate, name_only=True))
            req_dict["items"] = item_list
            return req_dict
        elif line_context == 2:  # only part
            name_only = True
            mod_name = context_info.lower()
            if mod_name in self.obj_tree:
                scope_list = [self.obj_tree[mod_name][0]]
                public_only = True
            else:
                return {"isIncomplete": False, "items": []}
        #
        include_globals = True
        if is_member:
            curr_scope = file_obj.get_inner_scope(ac_line+1)
            type_scope = climb_type_tree(var_stack, curr_scope, self.obj_tree)
            # Set enclosing type as scope
            if type_scope is None:
                return {"isIncomplete": False, "items": []}
            else:
                include_globals = False
                scope_list = [type_scope]
        #
        for candidate in get_candidates(scope_list, var_prefix, include_globals, public_only):
            # Skip module names (only valid in USE)
            candidate_type = candidate.get_type()
            if candidate_type == 1:
                continue
            # Only include types during variable definitions
            # or select statements
            if line_context == 4:
                if candidate_type != 4:
                    continue
            elif line_context != 2:
                if candidate_type == 4:
                    continue
            # Filter callables for call statements
            if line_context == 3 and (not candidate.is_callable()):
                continue
            #
            if candidate_type == 5:
                tmp_list = []
                for member in candidate.mems:
                    tmp_text, _ = member.get_snippet(candidate.name)
                    if tmp_list.count(tmp_text) > 0:
                        continue
                    tmp_list.append(tmp_text)
                    item_list.append(build_comp(member, name_replace=candidate.name))
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
            if detect_comment_start(line_prefix, def_file["fixed"]) >= 0:
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
        # print(var_stack)
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
        else:
            return var_obj
        return None

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
                    # Skip comment lines
                    comm_start = detect_comment_start(line, file_obj["fixed"])
                    if comm_start == 0:
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
            # Currently only show for subroutines
            if var_type == 2 or var_type == 3:
                skip_arg = False
                hover_str, _ = var_obj.get_snippet()
                hover_str += "\n"
                if isinstance(var_obj, fortran_meth):
                    if var_obj.modifiers.count(6) == 0:
                        skip_arg = True
                    var_obj = var_obj.link_obj
                for arg_obj in var_obj.arg_objs:
                    if skip_arg:
                        skip_arg = False
                        continue
                    tmp_str = "  " + arg_obj.get_documentation()
                    tmp_str += " :: {0}".format(arg_obj.name)
                    hover_str = hover_str + tmp_str + "\n"
                return {
                    "contents": {
                        "language": "fortran",
                        "value": hover_str
                    }
                }
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
        # Update file with changes
        if self.sync_type == 1:
            new_contents = params["contentChanges"][0]["text"].splitlines()
            self.update_workspace_file(new_contents, path, update_links=True)
        else:
            if path in self.workspace:
                new_contents = self.workspace[path]["contents"]
                try:
                    for change in params["contentChanges"]:
                        old_contents = new_contents
                        new_contents, line_tmp = apply_change(old_contents, change)
                except:
                    self.conn.send_notification("window/showMessage", {
                        "type": 1,
                        "message": 'Change request failed for unknown file "{0}"'.format(path)
                    })
                else:
                    self.update_workspace_file(new_contents, path, update_links=True)
            else:
                self.conn.send_notification("window/showMessage", {
                    "type": 1,
                    "message": 'Change request failed for unknown file "{0}"'.format(path)
                })
        # Update inheritance (currently only on open/save)
        # for key in self.obj_tree:
        #     self.obj_tree[key][0].resolve_inherit(self.obj_tree)
        #     self.obj_tree[key][0].resolve_link(self.obj_tree)

    def serve_onSave(self, request):
        # Update workspace from file on disk
        params = request["params"]
        uri = params["textDocument"]["uri"]
        self.add_file(path_from_uri(uri))
        # Update inheritance
        for key in self.obj_tree:
            self.obj_tree[key][0].resolve_inherit(self.obj_tree)
            self.obj_tree[key][0].resolve_link(self.obj_tree)
        self.get_diagnostics(uri)

    def add_file(self, filepath):
        # Read and add file from disk
        if PY3K:
            with open(filepath, 'r', encoding="utf-8") as fhandle:
                self.update_workspace_file(fhandle.readlines(), filepath)
        else:
            with open(filepath, 'r') as fhandle:
                self.update_workspace_file(fhandle.readlines(), filepath)

    def update_workspace_file(self, contents_split, filepath, update_links=False):
        # Update workspace from file contents and path
        try:
            fixed_flag = detect_fixed_format(contents_split)
            ast_new = process_file(contents_split, True, filepath, fixed_flag)
        except:
            self.conn.send_notification("window/showMessage", {
                "type": 1,
                "message": 'Parsing failed for file "{0}"'.format(filepath)
            })
            return  # Error during parsing
        else:
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
            if result_obj is None:
                self.post_messages.append([1, 'Parsing failed for file "{0}"'.format(path)])
                continue
            self.workspace[path] = result_obj
            # Add top-level objects to object tree
            ast_new = self.workspace[path]["ast"]
            for key in ast_new.global_dict:
                self.obj_tree[key] = [ast_new.global_dict[key], path]
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
