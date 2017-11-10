import copy
import re
WORD_REGEX = re.compile(r'[a-z][a-z0-9_]*', re.I)


def parse_keywords(keywords):
    modifiers = []
    for key in keywords:
        key_lower = key.lower()
        if key_lower == 'pointer':
            modifiers.append(1)
        elif key_lower == 'allocatable':
            modifiers.append(2)
        elif key_lower == 'optional':
            modifiers.append(3)
        elif key_lower == 'public':
            modifiers.append(4)
        elif key_lower == 'private':
            modifiers.append(5)
        elif key_lower == 'nopass':
            modifiers.append(6)
        elif key_lower == 'intent(in)':
            modifiers.append(7)
        elif key_lower == 'intent(out)':
            modifiers.append(8)
        elif key_lower == 'intent(inout)':
            modifiers.append(9)
        elif key_lower.startswith('dimension'):
            ndims = key_lower.count(':')
            modifiers.append(20+ndims)
    modifiers.sort()
    return modifiers


def get_keywords(modifiers):
    mod_strings = []
    for modifier in modifiers:
        if modifier == 1:
            mod_strings.append('POINTER')
        elif modifier == 2:
            mod_strings.append('ALLOCATABLE')
        elif modifier == 3:
            mod_strings.append('OPTIONAL')
        elif modifier == 4:
            mod_strings.append('PUBLIC')
        elif modifier == 5:
            mod_strings.append('PRIVATE')
        elif modifier == 6:
            mod_strings.append('NOPASS')
        elif modifier == 7:
            mod_strings.append('INTENT(IN)')
        elif modifier == 8:
            mod_strings.append('INTENT(OUT)')
        elif modifier == 9:
            mod_strings.append('INTENT(INOUT)')
        elif modifier > 20:
            dim_str = ":"
            for i in range(modifier-21):
                dim_str += ",:"
            mod_strings.append('DIMENSION({0})'.format(dim_str))
    return mod_strings


def intersect_lists(l1, l2):
    tmp_list = []
    for val1 in l1:
        if l2.count(val1) > 0:
            tmp_list.append(val1)
    return tmp_list


def get_use_tree(scope, use_dict, obj_tree, only_list=[]):
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
        if use_mod in obj_tree:
            if use_mod in use_dict:
                if len(use_dict[use_mod]) > 0:
                    for only_name in use_stmnt[1]:
                        if use_dict[use_mod].count(only_name) == 0:
                            use_dict[use_mod].append(only_name)
            else:
                use_dict[use_mod] = merged_use_list
            use_dict = get_use_tree(obj_tree[use_mod][0], use_dict, obj_tree, merged_use_list)
    return use_dict


def find_in_scope(scope, var_name, obj_tree):
    var_name_lower = var_name.lower()
    # Check local scope
    for child in scope.get_children():
        if child.name.lower() == var_name_lower:
            return child, scope
    # Setup USE search
    use_dict = get_use_tree(scope, {}, obj_tree)
    # Look in found use modules
    for use_mod, only_list in use_dict.items():
        use_scope = obj_tree[use_mod][0]
        # Module name is request
        if use_mod.lower() == var_name_lower:
            return use_scope, None
        # Filter children by only_list
        if len(only_list) > 0:
            if var_name_lower not in only_list:
                continue
        # Check for variable in public children
        def_vis = use_scope.def_vis
        for child in use_scope.get_children():
            if child.vis < 0:
                continue
            if def_vis < 0 and child.vis <= 0:
                continue
            if child.name.lower() == var_name_lower:
                return child, use_scope
    # Check parent scopes
    if scope.parent is not None:
        curr_scope = scope.parent
        tmp_var, tmp_scope = find_in_scope(curr_scope, var_name, obj_tree)
        if tmp_var is not None:
            return tmp_var, tmp_scope
    return None, None


def find_word_in_line(line, word):
    i0 = 0
    for poss_name in WORD_REGEX.finditer(line):
        if poss_name.group() == word:
            i0 = poss_name.start()
            break
    return i0, i0 + len(word)


class fortran_scope:
    def __init__(self, line_number, name, enc_scope=None):
        self.base_setup(line_number, name, enc_scope)

    def base_setup(self, sline, name, enc_scope=None):
        self.sline = sline
        self.eline = None
        self.name = name
        self.children = []
        self.members = []
        self.use = []
        self.inherit = None
        self.parent = None
        self.vis = 0
        self.def_vis = 0
        if enc_scope is not None:
            self.FQSN = enc_scope.lower() + "::" + self.name.lower()
        else:
            self.FQSN = self.name.lower()

    def set_default_vis(self, new_vis):
        self.def_vis = new_vis

    def set_visibility(self, new_vis):
        self.vis = new_vis

    def add_use(self, use_mod, line_number, only_list=[]):
        lower_only = []
        for only in only_list:
            lower_only.append(only.lower())
        self.use.append([use_mod.lower(), lower_only, line_number])

    def set_inherit(self, inherit_type):
        self.inherit = inherit_type

    def resolve_inherit(self, obj_tree):
        for child in self.children:
            child.resolve_inherit(obj_tree)

    def resolve_link(self, obj_tree):
        for child in self.children:
            child.resolve_link(obj_tree)

    def add_parent(self, parent_obj):
        self.parent = parent_obj

    def add_child(self, child):
        self.children.append(child)

    def add_member(self, member):
        self.members.append(member)

    def get_type(self):
        return -1

    def get_desc(self):
        return 'unknown'

    def get_snippet(self, name_replace=None, drop_arg=None):
        if name_replace is not None:
            return name_replace
        return self.name

    def get_documentation(self):
        return None

    def get_children(self):
        return self.children

    def is_optional(self):
        return False

    def end(self, line_number):
        self.eline = line_number

    def check_double_def(self, file_contents, obj_tree):
        """Check for double definition errors in scope"""
        if self.parent is not None:
            if self.parent.get_type() == 5:
                return []
        FSQN_list = []
        errors = []
        for child in self.children:
            # Check other variables in current scope
            if FSQN_list.count(child.FQSN) > 0:
                line_number = child.sline - 1
                i0, i1 = find_word_in_line(file_contents[line_number].lower(), child.name.lower())
                errors.append([0, line_number, i0, i1, child.name])
            else:
                FSQN_list.append(child.FQSN)
            # Check for masking from parent scope in subroutines and functions
            if self.parent is not None and (self.get_type() == 2 or self.get_type() == 3):
                parent_var, parent_scope = \
                    find_in_scope(self.parent, child.name, obj_tree)
                if parent_var is not None:
                    line_number = child.sline - 1
                    i0, i1 = find_word_in_line(file_contents[line_number].lower(), child.name.lower())
                    errors.append([1, line_number, i0, i1, child.name])
        return errors

    def check_use(self, obj_tree, file_contents):
        errors = []
        for use_line in self.use:
            use_mod = use_line[0]
            if use_mod not in obj_tree:
                line_number = use_line[2] - 1
                line = file_contents[line_number]
                i0 = line.lower().find(use_mod)
                if i0 == -1:
                    i0 = 0
                errors.append([line_number, i0, i0+len(use_mod), use_mod])
        return errors


class fortran_module(fortran_scope):
    def get_type(self):
        return 1

    def get_desc(self):
        return 'MODULE'


class fortran_program(fortran_module):
    def get_desc(self):
        return 'PROGRAM'


class fortran_subroutine(fortran_scope):
    def __init__(self, line_number, name, enc_scope=None, args=None):
        self.base_setup(line_number, name, enc_scope)
        self.args = args
        self.arg_objs = []

    def resolve_link(self, obj_tree):
        self.arg_objs = []
        arg_list = self.args.replace(' ', '').lower().split(',')
        for child in self.children:
            ind = -1
            for i, arg in enumerate(arg_list):
                if arg == child.name.lower():
                    ind = i
                    break
            if ind >= 0:
                self.arg_objs.append(child)
                if child.is_optional():
                    arg_list[ind] = "{0}={0}".format(arg_list[ind])
            child.resolve_link(obj_tree)
        self.args = ",".join(arg_list)

    def get_type(self):
        return 2

    def get_snippet(self, name_replace=None, drop_arg=None):
        arg_str = "({0})".format(self.args)
        if drop_arg is not None:
            first_comma = self.args.find(",")
            if first_comma > 0:
                arg_str = "({0})".format(self.args[first_comma+1:])
            else:
                arg_str = "()"
        if name_replace is not None:
            return name_replace + arg_str
        return self.name + arg_str

    def get_desc(self):
        return 'SUBROUTINE'


class fortran_function(fortran_subroutine):
    def __init__(self, line_number, name, enc_scope=None, args=None,
                 return_type=None, result_var=None):
        self.base_setup(line_number, name, enc_scope)
        self.args = args
        self.arg_objs = []
        self.result_var = result_var
        if return_type is not None:
            self.return_type = return_type[0]
            self.modifiers = parse_keywords(return_type[1])
        else:
            self.return_type = None
            self.modifiers = []

    def get_type(self):
        return 3

    def get_desc(self):
        # desc = None
        if self.result_var is not None:
            result_var_lower = self.result_var.lower()
            for child in self.children:
                if child.name == result_var_lower:
                    return child.get_desc()
        if self.return_type is not None:
            return self.return_type
        return 'FUNCTION'


class fortran_type(fortran_scope):
    def __init__(self, line_number, name, modifiers, enc_scope=None):
        self.base_setup(line_number, name, enc_scope)
        #
        self.in_children = []
        self.modifiers = modifiers
        self.inherit = None
        for modifier in self.modifiers:
            if modifier == 4:
                self.vis = 1
            elif modifier == 5:
                self.vis = -1

    def get_type(self):
        return 4

    def get_desc(self):
        return 'TYPE'

    def get_children(self):
        tmp_list = copy.copy(self.children)
        tmp_list.extend(self.in_children)
        return tmp_list

    def resolve_inherit(self, obj_tree):
        if self.inherit is None:
            return
        #
        inherit_var, inherit_scope = \
            find_in_scope(self.parent, self.inherit, obj_tree)
        if inherit_var is not None:
            inherit_var.resolve_inherit(obj_tree)
            # Get current fields
            child_names = []
            for child in self.children:
                child_names.append(child.name.lower())
                child.resolve_inherit(obj_tree)
            # Import for parent objects
            self.in_children = []
            for child in inherit_var.children:
                if child.name.lower() not in child_names:
                    self.in_children.append(child)


class fortran_int(fortran_scope):
    def __init__(self, line_number, name, enc_scope=None):
        self.base_setup(line_number, name, enc_scope)
        self.mems = []

    def get_type(self):
        return 5

    def get_desc(self):
        return 'INTERFACE'

    def resolve_link(self, obj_tree):
        if self.parent is None:
            return
        self.mems = []
        for member in self.members:
            mem_obj, _ = find_in_scope(self.parent, member, obj_tree)
            if mem_obj is not None:
                self.mems.append(mem_obj)


class fortran_obj:
    def __init__(self, line_number, name, var_desc, modifiers,
                 enc_scope=None, link_obj=None):
        self.sline = line_number
        self.name = name
        self.desc = var_desc
        self.modifiers = modifiers
        self.children = []
        self.vis = 0
        self.parent = None
        self.link_obj = None
        if link_obj is not None:
            self.link_name = link_obj.lower()
        else:
            self.link_name = None
        if enc_scope is not None:
            self.FQSN = enc_scope.lower() + "::" + self.name.lower()
        else:
            self.FQSN = self.name.lower()
        for modifier in self.modifiers:
            if modifier == 4:
                self.vis = 1
            elif modifier == 5:
                self.vis = -1

    def add_parent(self, parent_obj):
        self.parent = parent_obj

    def resolve_link(self, obj_tree):
        if self.link_name is None:
            return
        if self.parent is not None:
            parent = self.parent
            while(parent is not None):
                for child in parent.children:
                    if child.name.lower() == self.link_name:
                        self.link_obj = child
                        return
                parent = parent.parent

    def set_visibility(self, new_vis):
        self.vis = new_vis

    def get_type(self):
        if self.link_obj is not None:
            return self.link_obj.get_type()
        # Normal variable
        return 6

    def get_desc(self):
        if self.link_obj is not None:
            return self.link_obj.get_desc()
        # Normal variable
        return self.desc

    def set_dim(self, ndim):
        for (i, modifier) in enumerate(self.modifiers):
            if modifier > 20:
                self.modifiers[i] = ndim+20
                return
        self.modifiers.append(ndim+20)

    def get_snippet(self, name_replace=None, drop_arg=None):
        name = self.name
        if name_replace is not None:
            name = name_replace
        if self.link_obj is not None:
            return self.link_obj.get_snippet(name, drop_arg)
        # Normal variable
        return name

    def get_documentation(self):
        if self.link_obj is not None:
            return self.link_obj.get_documentation()
        #
        doc_str = self.desc
        if len(self.modifiers) > 0:
            doc_str += ", "
            doc_str += ", ".join(get_keywords(self.modifiers))
        return doc_str

    def get_children(self):
        return []

    def resolve_inherit(self, obj_tree):
        return

    def is_optional(self):
        if self.modifiers.count(3) > 0:
            return True
        else:
            return False


class fortran_meth(fortran_obj):
    def get_snippet(self, name_replace=None, drop_arg=None):
        if self.modifiers.count(6) > 0:
            nopass = True
        else:
            nopass = False
        #
        name = self.name
        if name_replace is not None:
            name = name_replace
        if self.link_obj is not None:
            return self.link_obj.get_snippet(name, nopass)

    def get_type(self):
        if self.link_obj is not None:
            return self.link_obj.get_type()
        # Generic
        return 7


class fortran_file:
    def __init__(self):
        self.global_dict = {}
        self.scope_list = []
        self.variable_list = []
        self.public_list = []
        self.private_list = []
        self.scope_stack = []
        self.end_stack = []
        self.current_scope = None
        self.END_REGEX = None
        self.enc_scope_name = None

    def get_enc_scope_name(self):
        if self.current_scope is None:
            return None
        name_str = self.current_scope.name
        if len(self.scope_stack) > 0:
            for scope in reversed(self.scope_stack):
                name_str = scope.name + '::' + name_str
        return name_str

    def add_scope(self, new_scope, END_SCOPE_REGEX, hidden=False):
        if hidden:
            self.variable_list.append(new_scope)
        else:
            self.scope_list.append(new_scope)
        if self.current_scope is None:
            self.global_dict[new_scope.FQSN] = new_scope
        else:
            self.current_scope.add_child(new_scope)
            new_scope.add_parent(self.current_scope)
            self.scope_stack.append(self.current_scope)
        if self.END_REGEX is not None:
            self.end_stack.append(self.END_REGEX)
        self.current_scope = new_scope
        self.END_REGEX = END_SCOPE_REGEX
        self.enc_scope_name = self.get_enc_scope_name()

    def end_scope(self, line_number):
        self.current_scope.end(line_number)
        if len(self.scope_stack) > 0:
            self.current_scope = self.scope_stack.pop()
        else:
            self.current_scope = None
        if len(self.end_stack) > 0:
            self.END_REGEX = self.end_stack.pop()
        else:
            self.END_REGEX = None
        self.enc_scope_name = self.get_enc_scope_name()

    def add_variable(self, new_var):
        self.current_scope.add_child(new_var)
        new_var.add_parent(self.current_scope)
        self.variable_list.append(new_var)

    def add_int_member(self, key):
        self.current_scope.add_member(key)

    def add_private(self, name):
        self.private_list.append(self.enc_scope_name+'::'+name)

    def add_public(self, name):
        self.public_list.append(self.enc_scope_name+'::'+name)

    def add_use(self, mod_words, line_number):
        if len(mod_words) > 0:
            n = len(mod_words)
            if n > 2:
                use_list = mod_words[2:]
                self.current_scope.add_use(mod_words[0], line_number, use_list)
            else:
                self.current_scope.add_use(mod_words[0], line_number)

    def get_scopes(self, line_number=None):
        if line_number is None:
            return self.scope_list
        scope_list = []
        for scope in self.scope_list:
            if line_number >= scope.sline and line_number <= scope.eline:
                scope_list.append(scope)
        return scope_list

    def get_inner_scope(self, line_number):
        scope_sline = -1
        curr_scope = None
        for scope in self.scope_list:
            if scope.sline > scope_sline:
                if line_number >= scope.sline and line_number <= scope.eline:
                    curr_scope = scope
                    scope_sline = scope.sline
        return curr_scope

    def get_object(self, FQSN):
        FQSN_split = FQSN.split("::")
        curr_obj = self.global_dict[FQSN_split[0]]
        if len(FQSN_split) > 1:
            for name in FQSN_split[1:]:
                next_obj = None
                for child in curr_obj.children:
                    if child.name == name:
                        next_obj = child
                        break
                if next_obj is None:
                    return None
                curr_obj = next_obj
        return curr_obj

    def close_file(self):
        # Tasks to be done when file parsing is finished
        for private_name in self.private_list:
            obj = self.get_object(private_name)
            if obj is not None:
                obj.set_visibility(-1)
        for public_name in self.public_list:
            obj = self.get_object(public_name)
            if obj is not None:
                obj.set_visibility(1)

    def check_file(self, obj_tree, file_contents):
        errors = []
        for scope in self.scope_list:
            for error in scope.check_double_def(file_contents, obj_tree):
                if error[0] == 0:
                    errors.append({
                        "range": {
                            "start": {"line": error[1], "character": error[2]},
                            "end": {"line": error[1], "character": error[3]}
                        },
                        "message": 'Variable "{0}" declared twice in scope'.format(error[4]),
                        "severity": 1
                    })
                elif error[0] == 1:
                    errors.append({
                        "range": {
                            "start": {"line": error[1], "character": error[2]},
                            "end": {"line": error[1], "character": error[3]}
                        },
                        "message": 'Variable "{0}" masks variable in parent scope'.format(error[4]),
                        "severity": 2
                    })
            for error in scope.check_use(obj_tree, file_contents):
                errors.append({
                    "range": {
                        "start": {"line": error[0], "character": error[1]},
                        "end": {"line": error[0], "character": error[2]}
                    },
                    "message": 'Module "{0}" not found in project'.format(error[3]),
                    "severity": 2
                })
        return errors
