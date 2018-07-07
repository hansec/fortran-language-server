import copy
import re
import os
WORD_REGEX = re.compile(r'[a-z_][a-z0-9_]*', re.I)
CLASS_VAR_REGEX = re.compile(r'(TYPE|CLASS)[ ]*\(', re.I)


def map_keywords(keywords):
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
                old_len = len(use_dict[use_mod])
                if (old_len > 0) and (len(merged_use_list) > 0):
                    for only_name in use_stmnt[1]:
                        if use_dict[use_mod].count(only_name) == 0:
                            use_dict[use_mod].append(only_name)
                else:
                    use_dict[use_mod] = []
                # Skip if we have already visited module with the same only list
                if old_len == len(use_dict[use_mod]):
                    continue
            else:
                use_dict[use_mod] = merged_use_list
            use_dict = get_use_tree(obj_tree[use_mod][0], use_dict, obj_tree, merged_use_list)
    return use_dict


def find_in_scope(scope, var_name, obj_tree):
    def check_scope(local_scope, var_name_lower, filter_public=False):
        for child in local_scope.get_children():
            if child.name.startswith("#GEN_INT"):
                tmp_var, tmp_scope = check_scope(child, var_name_lower, filter_public)
                if tmp_var is not None:
                    return tmp_var, tmp_scope
            if filter_public:
                if child.vis < 0:
                    continue
                if local_scope.def_vis < 0 and child.vis <= 0:
                    continue
            if child.name.lower() == var_name_lower:
                return child, local_scope
        return None, None
    #
    var_name_lower = var_name.lower()
    # Check local scope
    tmp_var, tmp_scope = check_scope(scope, var_name_lower)
    if tmp_var is not None:
        return tmp_var, tmp_scope
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
        tmp_var, tmp_scope = check_scope(use_scope, var_name_lower, filter_public=True)
        if tmp_var is not None:
            return tmp_var, tmp_scope
    # Check parent scopes
    if scope.parent is not None:
        curr_scope = scope.parent
        tmp_var, tmp_scope = find_in_scope(curr_scope, var_name, obj_tree)
        if tmp_var is not None:
            return tmp_var, tmp_scope
    # Check ancestor scopes
    for ancestor in scope.get_ancestors():
        tmp_var, tmp_scope = find_in_scope(ancestor, var_name, obj_tree)
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
    def __init__(self, file_obj, line_number, name, enc_scope=None):
        self.base_setup(file_obj, line_number, name, enc_scope)

    def base_setup(self, file_obj, sline, name, enc_scope=None):
        self.file = file_obj
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

    def set_parent(self, parent_obj):
        self.parent = parent_obj

    def add_child(self, child):
        self.children.append(child)
        child.set_parent(self)

    def update_fqsn(self, enc_scope=None):
        if enc_scope is not None:
            self.FQSN = enc_scope.lower() + "::" + self.name.lower()
        else:
            self.FQSN = self.name.lower()
        for child in self.children:
            child.update_fqsn(self.FQSN)

    def add_member(self, member):
        self.members.append(member)

    def get_type(self):
        return -1

    def get_desc(self):
        return 'unknown'

    def get_snippet(self, name_replace=None, drop_arg=None):
        if name_replace is not None:
            return name_replace, None
        return self.name, None

    def get_documentation(self, long=False):
        return None, False

    def get_signature(self):
        return None, None, None

    def get_children(self, public_only=False):
        if public_only:
            pub_children = []
            for child in self.children:
                if child.vis < 0:
                    continue
                if (self.def_vis < 0) and (child.vis <= 0):
                    continue
                if child.name.startswith("#GEN_INT"):
                    pub_children.append(child)
                    continue
                pub_children.append(child)
            return pub_children
        else:
            return self.children

    def get_ancestors(self):
        return []

    def is_optional(self):
        return False

    def is_mod_scope(self):
        return False

    def is_callable(self):
        return False

    def is_external_int(self):
        return False

    def is_abstract(self):
        return False

    def end(self, line_number):
        self.eline = line_number

    def check_double_def(self, file_contents, obj_tree):
        """Check for double definition errors in scope"""
        if self.parent is not None:
            if self.parent.get_type() == 5:
                return []
        FQSN_list = []
        errors = []
        for child in self.children:
            # Check other variables in current scope
            if FQSN_list.count(child.FQSN) > 0:
                line_number = child.sline - 1
                i0, i1 = find_word_in_line(file_contents[line_number].lower(), child.name.lower())
                errors.append([0, line_number, i0, i1, child.name])
            else:
                FQSN_list.append(child.FQSN)
            # Check for masking from parent scope in subroutines and functions
            if (self.parent is not None) and (self.get_type() in (2, 3, 8)):
                parent_var, parent_scope = \
                    find_in_scope(self.parent, child.name, obj_tree)
                if parent_var is not None:
                    # Ignore if function return variable
                    if (self.get_type() == 3) and (parent_var.FQSN == self.FQSN):
                        continue
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


class fortran_submodule(fortran_module):
    def __init__(self, file_obj, line_number, name, enc_scope=None, ancestor_name=None):
        self.base_setup(file_obj, line_number, name, enc_scope)
        self.ancestor_name = ancestor_name
        self.ancestor_obj = None

    def get_desc(self):
        return 'SUBMODULE'

    def get_ancestors(self):
        if self.ancestor_obj is not None:
            great_ancestors = self.ancestor_obj.get_ancestors()
            if great_ancestors is not None:
                return [self.ancestor_obj] + great_ancestors
            return [self.ancestor_obj]
        return []

    def resolve_inherit(self, obj_tree):
        if self.ancestor_name is None:
            return
        if self.ancestor_name in obj_tree:
            self.ancestor_obj = obj_tree[self.ancestor_name][0]

    def resolve_link(self, obj_tree):
        # Link subroutine/function implementations to prototypes
        if self.ancestor_obj is None:
            return
        # Grab ancestor interface definitions (function/subroutine only)
        ancestor_interfaces = []
        for child in self.ancestor_obj.children:
            if child.get_type() == 5:
                for prototype in child.children:
                    prototype_type = prototype.get_type()
                    if (prototype_type == 2 or prototype_type == 3) and prototype.is_mod_scope():
                        ancestor_interfaces.append(prototype)
        # Match interface definitions to implementations
        for prototype in ancestor_interfaces:
            for child in self.children:
                if (child.name.lower() == prototype.name.lower()) and (child.get_type() == prototype.get_type()):
                    prototype.resolve_link(obj_tree)
                    child.copy_interface(prototype)
                    break


class fortran_subroutine(fortran_scope):
    def __init__(self, file_obj, line_number, name, enc_scope=None, args="", mod_sub=False):
        self.base_setup(file_obj, line_number, name, enc_scope)
        self.args = args.replace(' ', '').lower()
        self.args_snip = self.args
        self.arg_objs = []
        self.in_children = []
        self.mod_scope = mod_sub

    def is_mod_scope(self):
        return self.mod_scope

    def is_callable(self):
        return True

    def copy_interface(self, copy_source):
        # Copy arguments
        self.args = copy_source.args
        self.args_snip = copy_source.args_snip
        self.arg_objs = copy_source.arg_objs
        # Get current fields
        child_names = []
        for child in self.children:
            child_names.append(child.name.lower())
        # Import arg_objs from copy object
        self.in_children = []
        for child in copy_source.arg_objs:
            if child is None:
                continue
            if child.name.lower() not in child_names:
                self.in_children.append(child)

    def get_children(self, public_only=False):
        tmp_list = copy.copy(self.children)
        tmp_list.extend(self.in_children)
        return tmp_list

    def resolve_arg_link(self, obj_tree):
        arg_list = self.args.replace(' ', '').split(',')
        self.arg_objs = [None for arg in arg_list]
        for child in self.children:
            ind = -1
            for i, arg in enumerate(arg_list):
                if arg == child.name.lower():
                    ind = i
                    break
            if ind >= 0:
                self.arg_objs[ind] = child
                if child.is_optional():
                    arg_list[ind] = "{0}={0}".format(arg_list[ind])
            child.resolve_link(obj_tree)
        self.args_snip = ",".join(arg_list)

    def resolve_link(self, obj_tree):
        self.resolve_arg_link(obj_tree)

    def get_type(self):
        return 2

    def get_snippet(self, name_replace=None, drop_arg=None):
        arg_list = self.args_snip.split(",")
        if drop_arg is not None:
            if len(arg_list) > 1:
                arg_list = arg_list[1:]
            else:
                arg_list = []
        arg_snip = None
        if len(arg_list) > 0:
            place_holders = []
            for i, arg in enumerate(arg_list):
                opt_split = arg.split("=")
                if len(opt_split) > 1:
                    place_holders.append("{1}=${{{0}:{2}}}".format(i+1, opt_split[0], opt_split[1]))
                else:
                    place_holders.append("${{{0}:{1}}}".format(i+1, arg))
            arg_str = "({0})".format(", ".join(arg_list))
            arg_snip = "({0})".format(", ".join(place_holders))
        else:
            arg_str = "()"
        name = self.name
        if name_replace is not None:
            name = name_replace
        snippet = None
        if arg_snip is not None:
            snippet = name + arg_snip
        return name + arg_str, snippet

    def get_desc(self):
        return 'SUBROUTINE'

    def get_documentation(self, long=False):
        if long:
            hover_str, _ = self.get_snippet()
            hover_str += "\n"
            for arg_obj in self.arg_objs:
                if arg_obj is None:
                    continue
                arg_doc, _ = arg_obj.get_documentation()
                hover_str += " {0} :: {1}\n".format(arg_doc, arg_obj.name)
            return hover_str, True
        else:
            return None, False

    def get_signature(self):
        arg_sigs = []
        arg_list = self.args.split(",")
        for i, arg_obj in enumerate(self.arg_objs):
            if arg_obj is None:
                arg_sigs.append({"label": arg_list[i]})
            else:
                if arg_obj.is_optional():
                    label = "{0}={0}".format(arg_obj.name.lower())
                else:
                    label = arg_obj.name.lower()
                arg_sigs.append({
                    "label": label,
                    "documentation": arg_obj.get_documentation()[0]
                })
        call_sig, _ = self.get_snippet()
        return call_sig, None, arg_sigs


class fortran_function(fortran_subroutine):
    def __init__(self, file_obj, line_number, name, enc_scope=None, args="",
                 mod_fun=False, return_type=None, result_var=None):
        self.base_setup(file_obj, line_number, name, enc_scope)
        self.args = args.replace(' ', '').lower()
        self.args_snip = self.args
        self.arg_objs = []
        self.in_children = []
        self.mod_scope = mod_fun
        self.result_var = result_var
        self.result_obj = None
        if return_type is not None:
            self.return_type = return_type[0]
            self.modifiers = map_keywords(return_type[1])
        else:
            self.return_type = None
            self.modifiers = []

    def copy_interface(self, copy_source):
        # Copy arguments and returns
        self.args = copy_source.args
        self.args_snip = copy_source.args_snip
        self.arg_objs = copy_source.arg_objs
        self.result_var = copy_source.result_var
        self.result_obj = copy_source.result_obj
        # Get current fields
        child_names = []
        for child in self.children:
            child_names.append(child.name.lower())
        # Import arg_objs from copy object
        self.in_children = []
        for child in copy_source.arg_objs:
            if child is None:
                continue
            if child.name.lower() not in child_names:
                self.in_children.append(child)
        if copy_source.result_obj is not None:
            if copy_source.result_obj.name.lower() not in child_names:
                self.in_children.append(copy_source.result_obj)

    def resolve_link(self, obj_tree):
        self.resolve_arg_link(obj_tree)
        if self.result_var is not None:
            result_var_lower = self.result_var.lower()
            for child in self.children:
                if child.name.lower() == result_var_lower:
                    self.result_obj = child

    def get_type(self):
        return 3

    def get_desc(self):
        # desc = None
        if self.result_obj is not None:
            return self.result_obj.get_desc()
        if self.return_type is not None:
            return self.return_type
        return 'FUNCTION'

    def is_callable(self):
        return False


class fortran_type(fortran_scope):
    def __init__(self, file_obj, line_number, name, modifiers, enc_scope=None):
        self.base_setup(file_obj, line_number, name, enc_scope)
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

    def get_children(self, public_only=False):
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


class fortran_block(fortran_scope):
    def __init__(self, file_obj, line_number, name, enc_scope=None):
        self.base_setup(file_obj, line_number, name, enc_scope)

    def get_type(self):
        return 8

    def get_desc(self):
        return 'BLOCK'

    def get_children(self, public_only=False):
        return self.children


class fortran_int(fortran_scope):
    def __init__(self, file_obj, line_number, name, enc_scope=None, abstract=False):
        self.base_setup(file_obj, line_number, name, enc_scope)
        self.mems = []
        self.abstract = abstract
        self.external = name.startswith('#GEN_INT') and (not abstract)

    def get_type(self):
        return 5

    def get_desc(self):
        return 'INTERFACE'

    def is_callable(self):
        return True

    def is_external_int(self):
        return self.external

    def is_abstract(self):
        return self.abstract

    def resolve_link(self, obj_tree):
        if self.parent is None:
            return
        self.mems = []
        for member in self.members:
            mem_obj, _ = find_in_scope(self.parent, member, obj_tree)
            if mem_obj is not None:
                self.mems.append(mem_obj)
        for child in self.children:
            child.resolve_link(obj_tree)


class fortran_obj:
    def __init__(self, file_obj, line_number, name, var_desc, modifiers,
                 enc_scope=None, link_obj=None):
        self.base_setup(file_obj, line_number, name, var_desc, modifiers,
                        enc_scope, link_obj)

    def base_setup(self, file_obj, line_number, name, var_desc, modifiers,
                   enc_scope, link_obj):
        self.file = file_obj
        self.sline = line_number
        self.name = name
        self.desc = var_desc
        self.modifiers = modifiers
        self.callable = (CLASS_VAR_REGEX.match(var_desc) is not None)
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

    def set_parent(self, parent_obj):
        self.parent = parent_obj

    def update_fqsn(self, enc_scope=None):
        if enc_scope is not None:
            self.FQSN = enc_scope.lower() + "::" + self.name.lower()
        else:
            self.FQSN = self.name.lower()
        for child in self.children:
            child.update_fqsn(self.FQSN)

    def resolve_link(self, obj_tree):
        if self.link_name is None:
            return
        if self.parent is not None:
            link_obj, _ = find_in_scope(self.parent, self.link_name, obj_tree)
            if link_obj is not None:
                self.link_obj = link_obj

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
        return name, None

    def get_documentation(self, long=False):
        if self.link_obj is not None:
            return self.link_obj.get_documentation()
        #
        doc_str = self.desc
        if len(self.modifiers) > 0:
            doc_str += ", "
            doc_str += ", ".join(get_keywords(self.modifiers))
        return doc_str, True

    def get_signature(self):
        return None, None, None

    def get_children(self, public_only=False):
        return []

    def resolve_inherit(self, obj_tree):
        return

    def is_optional(self):
        if self.modifiers.count(3) > 0:
            return True
        else:
            return False

    def is_callable(self):
        return self.callable

    def is_external_int(self):
        return False

    def is_abstract(self):
        return False


class fortran_meth(fortran_obj):
    def __init__(self, file_obj, line_number, name, var_desc, modifiers,
                 enc_scope=None, link_obj=None):
        self.base_setup(file_obj, line_number, name, var_desc, modifiers,
                        enc_scope, link_obj)
        if link_obj is None:
            open_paren = var_desc.find('(')
            close_paren = var_desc.find(')')
            if (open_paren > 0) and (close_paren > open_paren):
                self.link_name = var_desc[open_paren+1:close_paren].lower()

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
        return name, None

    def get_type(self):
        if self.link_obj is not None:
            return self.link_obj.get_type()
        # Generic
        return 7

    def get_documentation(self, long=False):
        if long:
            skip_arg = False
            hover_str, _ = self.get_snippet()
            hover_str += "\n"
            if self.modifiers.count(6) == 0:
                skip_arg = True
            var_obj = self.link_obj
            for arg_obj in var_obj.arg_objs:
                if skip_arg:
                    skip_arg = False
                    continue
                if arg_obj is None:
                    continue
                arg_doc, _ = arg_obj.get_documentation()
                hover_str += " {0} :: {1}\n".format(arg_doc, arg_obj.name)
            return hover_str, True
        else:
            if self.link_obj is not None:
                return self.link_obj.get_documentation()
            #
            doc_str = self.desc
            if len(self.modifiers) > 0:
                doc_str += ", "
                doc_str += ", ".join(get_keywords(self.modifiers))
            return doc_str, True

    def get_signature(self):
        arg_sigs = []
        skip_arg = False
        if self.modifiers.count(6) == 0:
            skip_arg = True
        var_obj = self.link_obj
        arg_list = var_obj.args.split(",")
        for i, arg_obj in enumerate(var_obj.arg_objs):
            if skip_arg:
                skip_arg = False
                continue
            if arg_obj is None:
                arg_sigs.append({"label": arg_list[i]})
            else:
                if arg_obj.is_optional():
                    label = "{0}={0}".format(arg_obj.name.lower())
                else:
                    label = arg_obj.name.lower()
                arg_sigs.append({
                    "label": label,
                    "documentation": arg_obj.get_documentation()[0]
                })
        call_sig, _ = self.get_snippet()
        return call_sig, None, arg_sigs

    def is_callable(self):
        return True


class fortran_file:
    def __init__(self, path=None):
        self.path = path
        self.global_dict = {}
        self.scope_list = []
        self.variable_list = []
        self.public_list = []
        self.private_list = []
        self.scope_stack = []
        self.end_stack = []
        self.pp_if = []
        self.include_stmnts = []
        self.none_scope = None
        self.inc_scope = None
        self.current_scope = None
        self.END_REGEX = None
        self.enc_scope_name = None

    def create_none_scope(self):
        if self.none_scope is not None:
            raise ValueError
        self.none_scope = fortran_program(self, 1, "main")
        self.add_scope(self.none_scope, re.compile(r'[ ]*END[ ]*PROGRAM', re.I), exportable=False)

    def get_enc_scope_name(self):
        if self.current_scope is None:
            return None
        return self.current_scope.FQSN

    def add_scope(self, new_scope, END_SCOPE_REGEX, exportable=True, req_container=False):
        self.scope_list.append(new_scope)
        if self.current_scope is None:
            if req_container:
                self.create_none_scope()
                new_scope.FQSN = self.none_scope.FQSN + "::" + new_scope.name.lower()
                self.current_scope.add_child(new_scope)
                self.scope_stack.append(self.current_scope)
            else:
                if exportable:
                    self.global_dict[new_scope.FQSN] = new_scope
        else:
            self.current_scope.add_child(new_scope)
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
        if self.current_scope is None:
            self.create_none_scope()
            new_var.FQSN = self.none_scope.FQSN + "::" + new_var.name.lower()
        self.current_scope.add_child(new_var)
        self.variable_list.append(new_var)

    def add_int_member(self, key):
        self.current_scope.add_member(key)

    def add_private(self, name):
        self.private_list.append(self.enc_scope_name+'::'+name)

    def add_public(self, name):
        self.public_list.append(self.enc_scope_name+'::'+name)

    def add_use(self, mod_word, line_number, only_list):
        if self.current_scope is None:
            self.create_none_scope()
        self.current_scope.add_use(mod_word, line_number, only_list)

    def add_include(self, path, line_number):
        if os.name == "nt":
            path = path.replace("/", "\\")
        self.include_stmnts.append([line_number, path, []])

    def start_ppif(self, line_number):
        self.pp_if.append([line_number-1, -1])

    def end_ppif(self, line_number):
        if len(self.pp_if) > 0:
            self.pp_if[-1][1] = line_number-1

    def check_ppif(self, line_number):
        for pp_if in self.pp_if:
            if line_number >= pp_if[0] and line_number <= pp_if[1]:
                return True
        return False

    def get_scopes(self, line_number=None):
        if line_number is None:
            return self.scope_list
        scope_list = []
        for scope in self.scope_list:
            if line_number >= scope.sline and line_number <= scope.eline:
                scope_list.append(scope)
                for ancestor in scope.get_ancestors():
                    scope_list.append(ancestor)
        if (len(scope_list) == 0) and (self.none_scope is not None):
            return [self.none_scope]
        return scope_list

    def get_inner_scope(self, line_number):
        scope_sline = -1
        curr_scope = None
        for scope in self.scope_list:
            if scope.sline > scope_sline:
                if (line_number >= scope.sline) and (line_number <= scope.eline):
                    curr_scope = scope
                    scope_sline = scope.sline
        if (curr_scope is None) and (self.none_scope is not None):
            return self.none_scope
        return curr_scope

    def get_object(self, FQSN):
        FQSN_split = FQSN.split("::")
        curr_obj = self.global_dict.get(FQSN_split[0])
        if curr_obj is None:
            # Look for non-exportable scopes
            for scope in self.scope_list:
                if FQSN_split[0] == scope.FQSN:
                    curr_obj = scope
                    break
        if curr_obj is None:
            return None
        if len(FQSN_split) > 1:
            for name in FQSN_split[1:]:
                next_obj = None
                for child in curr_obj.children:
                    if child.name.startswith("#GEN_INT"):
                        for int_child in child.get_children():
                            if int_child.name == name:
                                next_obj = int_child
                                break
                        if next_obj is not None:
                            break
                    if child.name == name:
                        next_obj = child
                        break
                if next_obj is None:
                    return None
                curr_obj = next_obj
        return curr_obj

    def resolve_includes(self, workspace, path=None):
        file_dir = os.path.dirname(self.path)
        for include_path in self.include_stmnts:
            file_path = os.path.join(file_dir, include_path[1])
            if path is not None:
                if not (path == file_path):
                    continue
            parent_scope = self.get_inner_scope(include_path[0])
            added_entities = include_path[2]
            if file_path in workspace:
                include_obj = workspace[file_path]["ast"]
                if include_obj.none_scope is not None:
                    if include_obj.inc_scope is None:
                        include_obj.inc_scope = include_obj.none_scope
                    # Remove old objects
                    for obj in added_entities:
                        parent_scope.children.remove(obj)
                    added_entities = []
                    for child in include_obj.inc_scope.children:
                        added_entities.append(child)
                        parent_scope.add_child(child)
                        child.update_fqsn(parent_scope.FQSN)
                    include_obj.none_scope = parent_scope
                    include_path[2] = added_entities

    def close_file(self, line_number):
        # Tasks to be done when file parsing is finished
        for private_name in self.private_list:
            obj = self.get_object(private_name)
            if obj is not None:
                obj.set_visibility(-1)
        for public_name in self.public_list:
            obj = self.get_object(public_name)
            if obj is not None:
                obj.set_visibility(1)
        if self.none_scope is not None:
            self.none_scope.end(line_number)
            self.scope_list.remove(self.none_scope)

    def check_file(self, obj_tree, file_contents):
        errors = []
        tmp_list = self.scope_list
        if self.none_scope is not None:
            tmp_list += [self.none_scope]
        for scope in tmp_list:
            for error in scope.check_double_def(file_contents, obj_tree):
                # Check preproc if
                if self.check_ppif(error[1]):
                    continue
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
                # Check preproc if
                if self.check_ppif(error[0]):
                    continue
                errors.append({
                    "range": {
                        "start": {"line": error[0], "character": error[1]},
                        "end": {"line": error[0], "character": error[2]}
                    },
                    "message": 'Module "{0}" not found in project'.format(error[3]),
                    "severity": 2
                })
        return errors
