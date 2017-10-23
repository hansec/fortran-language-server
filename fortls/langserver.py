import logging
import os
import traceback
import hashlib
import re
# Local modules
from fortls.parse_fortran import process_file, read_use_stmt
from fortls.objects import find_in_scope, fortran_meth

log = logging.getLogger(__name__)
# Global regexes
FORTRAN_EXT_REGEX = re.compile(r'^\.F(77|90|95|03|08|OR|PP)?$', re.I)
FIXED_EXT_REGEX = re.compile(r'^\.F(77|OR|PP)?$', re.I)
objBreak_REGEX = re.compile(r'[\/\-(.,+*<>=$: ]', re.I)
word_REGEX = re.compile(r'[a-z][a-z0-9_]*', re.I)
CALL_REGEX = re.compile(r'[ \t]*CALL[ \t]*([a-z0-9_]*)$', re.I)
TYPE_STMNT_REGEX = re.compile(r'[ \t]*(TYPE|CLASS)[ \t]*(IS)?[ \t]*\([ \t]*([a-z0-9_]*)$', re.I)


def path_from_uri(uri):
    # Convert file uri to path (strip html like head part)
    if not uri.startswith("file://"):
        return uri
    _, path = uri.split("file://", 1)
    return path


def init_file(filepath):
    #
    with open(filepath, 'r') as fhandle:
        contents = fhandle.read()
    #
    try:
        filename, ext = os.path.splitext(os.path.basename(filepath))
        fixed_flag = False
        if FIXED_EXT_REGEX.match(ext):
            fixed_flag = True
        hash_tmp = hashlib.md5(contents.encode()).hexdigest()
        contents_split = contents.splitlines()
        ast_new = process_file(contents_split, True, fixed_flag)
    except:
        return None
    else:
        # Construct new file object and add to workspace
        tmp_obj = {
            "contents": contents_split,
            "ast": ast_new,
            "hash": hash_tmp
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
    open_vars = []
    tmp_list = []
    tmp_str = ''
    if i2 < 0:
        i2 = len(line)
    i0 = i1
    for paren_group in paren_groups[level]:
        pg0 = paren_group[0]
        pg1 = paren_group[1]
        if pg0 < i1 or pg1 > i2:
            continue
        tmp_str += line[i0:pg0]
        tmp_list.append([i0, pg0])
        if pg1 < 0:
            open_vars.append([[[pg0+1, -1]], line[pg0+1:]])
            continue
        i0 = pg1+1
        if len(paren_groups) > level+1:
            var_groups, out_len = \
                paren_split(line, paren_groups, level+1,
                            var_groups, pg0+1, pg1)
            if out_len == 0:
                var_groups[level+1].append([[pg0+1, pg1], line[pg0+1:pg1]])
        else:
            var_groups[level+1].append([[pg0+1, pg1], line[pg0+1:pg1]])
    #
    tmp_str += line[i0:i2]
    tmp_list.append([i0, i2])
    out_list = [[tmp_list, tmp_str]]
    if len(open_vars) > 0:
        out_list.extend(open_vars)
    var_groups[level].extend(out_list)
    return var_groups, len(out_list)


def get_var_stack(line):
    var_list = tokenize_line(line)
    final_var = var_list[0][-1][1]
    paren_groups = var_list[0][-1][0]
    if final_var.find('%') < 0:
        paren_groups = var_list[0][-1][0]
        ntail = paren_groups[-1][1] - paren_groups[-1][0]
        #
        if ntail == 0:
            final_var = ''
        elif ntail > 0:
            final_var = final_var[len(final_var)-ntail:]
    #
    if final_var is not None:
        final_op_split = objBreak_REGEX.split(final_var)
        return final_op_split[-1].split('%')
    else:
        return None


def expand_name(line, char_poss):
    for word_match in word_REGEX.finditer(line):
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


class LangServer:
    def __init__(self, conn, logLevel=0):
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
        if logLevel == 0:
            logging.basicConfig(level=logging.ERROR)
        elif logLevel == 1:
            logging.basicConfig(level=logging.DEBUG)

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
        #     "textDocument/references": self.serve_references,
        #     "workspace/symbol": self.serve_symbols,
        # }.get(request["method"], self.serve_default)
        # We handle notifications differently since we can't respond
        if "id" not in request:
            try:
                handler(request)
            except Exception as e:
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
                if "mod_dirs" in config_dict:
                    for mod_dir in config_dict["mod_dirs"]:
                        self.mod_dirs.append(os.path.join(self.root_path, mod_dir))
                if "excl_paths" in config_dict:
                    for excl_path in config_dict["excl_paths"]:
                        self.excl_paths.append(os.path.join(self.root_path, excl_path))
        # Initialize workspace
        self.workspace_init()
        #
        return {
            "capabilities": {
                "documentSymbolProvider": True,
                "completionProvider": {
                    "resolveProvider": False,
                    "triggerCharacters": ["."]
                },
                "definitionProvider": True,
                "hoverProvider": True,
                "textDocumentSync": 1
            }
        }
        #     "referencesProvider": True,
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
            if scope.FQSN.count(":") > 2:
                continue
            tmp_out = {}
            tmp_out["name"] = scope.name
            tmp_out["kind"] = map_types(scope.get_type())
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
                tmp_out["containerName"] = tmp_list[-2]
            test_output.append(tmp_out)
            # If class add members
            if scope.get_type() == 4:
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

        def intersect_lists(l1, l2):
            tmp_list = []
            for val1 in l1:
                if l2.count(val1) > 0:
                    tmp_list.append(val1)
            return tmp_list

        def get_use_tree(scope, use_dict, only_list=[]):
            # Add recursively
            for use_stmnt in scope.use:
                use_mod = use_stmnt[0]
                if len(only_list) == 0:
                    merged_use_list = use_stmnt[1]
                elif len(use_stmnt[1]) == 0:
                    merged_use_list = only_list
                else:
                    merged_use_list = intersect_lists(only_list, use_stmnt[1])
                    if len(merged_use_list) == 0:
                        continue
                if use_mod in self.obj_tree:
                    if use_mod in use_dict:
                        if len(use_dict[use_mod]) > 0:
                            for only_name in use_stmnt[1]:
                                if use_dict[use_mod].count(only_name) == 0:
                                    use_dict[use_mod].append(only_name)
                    else:
                        use_dict[use_mod] = merged_use_list
                    use_dict = get_use_tree(self.obj_tree[use_mod][0], use_dict, merged_use_list)
            return use_dict

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
                use_dict = get_use_tree(scope, use_dict)
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
                comp_obj["label"] = candidate.get_snippet(name_replace)
            comp_obj["kind"] = map_types(candidate.get_type())
            comp_obj["detail"] = candidate.get_desc()
            doc_str = candidate.get_documentation()
            if doc_str is not None:
                comp_obj["documentation"] = doc_str
            return comp_obj

        def get_context(line, var_prefix):
            # tmp_prefix = var_prefix
            # Test if in call statement
            # test_match = CALL_REGEX.match(line)
            # if test_match is not None:
            #     return 3, var_prefix, None
            # Test if variable definition using type/class
            test_match = TYPE_STMNT_REGEX.match(line)
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
        params = request["params"]
        uri = params["textDocument"]["uri"]
        path = path_from_uri(uri)
        # Check line
        ac_line = params["position"]["line"]
        ac_char = params["position"]["character"]
        try:
            curr_line = self.workspace[path]["contents"][ac_line]
        except:
            return {"isIncomplete": False, "items": []}
        is_member = False
        try:
            line_prefix = curr_line[:ac_char].lower()
            var_stack = get_var_stack(line_prefix)
            is_member = (len(var_stack) > 1)
            var_prefix = var_stack[-1].strip()
        except:
            return {"isIncomplete": False, "items": []}
        # print var_stack
        file_obj = self.workspace[path]["ast"]
        test_output = {"isIncomplete": False}
        item_list = []
        scope_list = file_obj.get_scopes(ac_line+1)
        # Get context
        name_only = False
        public_only = False
        line_context, var_prefix, context_info = \
            get_context(line_prefix, var_prefix)
        if var_prefix == '' and not (is_member or line_context == 2):
            return {"isIncomplete": False, "items": []}
        # USE stmnt
        if line_context == 1:  # module part
            for key in self.obj_tree:
                candidate = self.obj_tree[key][0]
                candidate_type = candidate.get_type()
                if candidate_type != 1:
                    continue
                if candidate.name.lower().startswith(var_prefix):
                    item_list.append(build_comp(candidate, name_only=True))
            test_output["items"] = item_list
            return test_output
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
            #
            if candidate_type == 5:
                for member in candidate.mems:
                    item_list.append(build_comp(member, name_replace=candidate.name))
                continue
            #
            item_list.append(build_comp(candidate, name_only=name_only))
        test_output["items"] = item_list
        return test_output

    def get_definition(self, filepath, def_line, def_char):
        if filepath not in self.workspace:
            return None
        #
        try:
            curr_line = self.workspace[filepath]["contents"][def_line]
        except:
            return None
        #
        is_member = False
        try:
            line_prefix = curr_line[:def_char].lower()
            var_stack = get_var_stack(line_prefix)
            is_member = (len(var_stack) > 1)
            def_name = expand_name(curr_line, def_char)
        except:
            return None
        if def_name == '':
            return None
        file_obj = self.workspace[filepath]["ast"]
        # item_list = []
        scope_list = file_obj.get_scopes(def_line+1)
        # Traverse type tree if necessary
        if is_member:
            curr_scope = file_obj.get_inner_scope(def_line+1)
            type_scope = climb_type_tree(var_stack, curr_scope, self.obj_tree)
            # Set enclosing type as scope
            if type_scope is None:
                scope_list = []
            else:
                scope_list = [type_scope]
        # Find in available scopes
        var_obj = None
        for scope in scope_list:
            var_obj, enc_scope = find_in_scope(scope, def_name, self.obj_tree)
            if var_obj is not None:
                return var_obj
        # Search in global scope
        if var_obj is None:
            key = def_name.lower()
            if key in self.obj_tree:
                return self.obj_tree[key][0]
        return None

    def serve_definition(self, request):
        # Get parameters from request
        params = request["params"]
        uri = params["textDocument"]["uri"]
        def_line = params["position"]["line"]
        def_char = params["position"]["character"]
        path = path_from_uri(uri)
        # Find object
        var_obj = self.get_definition(path, def_line, def_char)
        # Construct link reference
        if var_obj is not None:
            var_top_scope = var_obj.FQSN.split("::")[0]
            if var_top_scope in self.obj_tree:
                var_path = self.obj_tree[var_top_scope][1]
                sline = var_obj.sline-1
                return {
                    "uri": "file://"+var_path,
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
        var_obj = self.get_definition(path, def_line, def_char)
        # Construct hover information
        if var_obj is not None:
            var_type = var_obj.get_type()
            # Currently only show for subroutines
            if var_type == 2 or var_type == 3:
                skip_arg = False
                hover_str = var_obj.get_snippet() + "\n"
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

    def serve_onChange(self, request):
        # Update workspace from file sent by editor
        params = request["params"]
        uri = params["textDocument"]["uri"]
        path = path_from_uri(uri)
        # Updating full file
        self.update_workspace_file(params["contentChanges"][0]["text"], path)
        # Update inheritance (currently only on open/save)
        # for key in self.obj_tree:
        #     self.obj_tree[key][0].resolve_inherit(self.obj_tree)
        #     self.obj_tree[key][0].resolve_link()

    def serve_onSave(self, request):
        # Update workspace from file on disk
        params = request["params"]
        uri = params["textDocument"]["uri"]
        self.add_file(path_from_uri(uri))
        # Update inheritance
        for key in self.obj_tree:
            self.obj_tree[key][0].resolve_inherit(self.obj_tree)
            self.obj_tree[key][0].resolve_link()

    def add_file(self, filepath):
        # Read and add file from disk
        with open(filepath, 'r') as fhandle:
            self.update_workspace_file(fhandle.read(), filepath)

    def update_workspace_file(self, contents, filepath):
        # Update workspace from file contents and path
        hash_tmp = hashlib.md5(contents.encode()).hexdigest()
        if filepath in self.workspace:
            if hash_tmp == self.workspace[filepath]["hash"]:
                return  # Same hash no updates
        try:
            filename, ext = os.path.splitext(os.path.basename(filepath))
            fixed_flag = False
            if FIXED_EXT_REGEX.match(ext):
                fixed_flag = True
            contents_split = contents.splitlines()
            ast_new = process_file(contents_split, True, fixed_flag)
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
                "hash": hash_tmp
            }
            self.workspace[filepath] = tmp_obj
            # Add top-level objects to object tree
            for key in ast_new.global_dict:
                self.obj_tree[key] = [ast_new.global_dict[key], filepath]

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
            self.obj_tree[key][0].resolve_link()

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
