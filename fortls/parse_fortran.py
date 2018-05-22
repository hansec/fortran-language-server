from __future__ import print_function
import re
from fortls.objects import parse_keywords, fortran_module, fortran_program, \
    fortran_submodule, fortran_subroutine, fortran_function, fortran_type, \
    fortran_int, fortran_obj, fortran_meth, fortran_file
#
USE_REGEX = re.compile(r'[ \t]*USE([, \t]*INTRINSIC)?[ \t:]*([a-z0-9_]*)', re.I)
SUB_REGEX = re.compile(r'[ \t]*(PURE|ELEMENTAL|RECURSIVE)*[ \t]*(SUBROUTINE)', re.I)
END_SUB_REGEX = re.compile(r'[ \t]*END[ \t]*SUBROUTINE', re.I)
FUN_REGEX = re.compile(r'[ \t]*(PURE|ELEMENTAL|RECURSIVE)*[ \t]*(FUNCTION)', re.I)
RESULT_REGEX = re.compile(r'RESULT[ ]*\(([a-z0-9_]*)\)', re.I)
END_FUN_REGEX = re.compile(r'[ \t]*END[ \t]*FUNCTION', re.I)
MOD_REGEX = re.compile(r'[ \t]*MODULE[ \t]*([a-z0-9_]*)', re.I)
END_MOD_REGEX = re.compile(r'[ \t]*END[ \t]*MODULE', re.I)
SUBMOD_REGEX = re.compile(r'[ \t]*SUBMODULE[ \t]*\(', re.I)
END_SMOD_REGEX = re.compile(r'[ \t]*END[ \t]*SUBMODULE', re.I)
PROG_REGEX = re.compile(r'[ \t]*PROGRAM[ \t]*([a-z0-9_]*)', re.I)
END_PROG_REGEX = re.compile(r'[ \t]*END[ \t]*PROGRAM', re.I)
INT_REGEX = re.compile(r'[ \t]*(?:ABSTRACT)?[ \t]*INTERFACE[ \t]*([a-z0-9_]*)', re.I)
END_INT_REGEX = re.compile(r'[ \t]*END[ \t]*INTERFACE', re.I)
END_GEN_REGEX = re.compile(r'[ \t]*END[ \t]*$', re.I)
TYPE_DEF_REGEX = re.compile(r'[ \t]*TYPE', re.I)
EXTENDS_REGEX = re.compile(r'EXTENDS[ ]*\(([a-z0-9_]*)\)', re.I)
END_TYPED_REGEX = re.compile(r'[ \t]*END[ \t]*TYPE', re.I)
NAT_VAR_REGEX = re.compile(r'[ \t]*(INTEGER|REAL|DOUBLE PRECISION|COMPLEX'
                           r'|CHARACTER|LOGICAL|PROCEDURE|CLASS|TYPE)', re.I)
KIND_SPEC_REGEX = re.compile(r'([ \t]*\([a-z0-9_ =*]*\)|\*[0-9]*)', re.I)
KEYWORD_LIST_REGEX = re.compile(r'[ \t]*,[ \t]*(PUBLIC|PRIVATE|ALLOCATABLE|'
                                r'POINTER|TARGET|DIMENSION\([a-z0-9_:, ]*\)|'
                                r'OPTIONAL|INTENT\([inout]*\)|DEFERRED|NOPASS|'
                                r'SAVE|PARAMETER)', re.I)
TATTR_LIST_REGEX = re.compile(r'[ \t]*,[ \t]*(PUBLIC|PRIVATE|ABSTRACT|EXTENDS\([a-z0-9_]*\))', re.I)
VIS_REGEX = re.compile(r'(PUBLIC|PRIVATE)', re.I)
WORD_REGEX = re.compile(r'[a-z_][a-z0-9_]*', re.I)
SUB_PAREN_MATCH = re.compile(r'\([a-z0-9_, ]*\)', re.I)
KIND_SPEC_MATCH = re.compile(r'\([a-z0-9_, =*]*\)', re.I)
SQ_STRING_REGEX = re.compile(r'\'[^\']*\'', re.I)
DQ_STRING_REGEX = re.compile(r'\"[^\"]*\"', re.I)
#
FIXED_COMMENT_LINE_MATCH = re.compile(r'(!|c|d|\*)', re.I)
FIXED_CONT_REGEX = re.compile(r'(     [\S])')
#
FREE_COMMENT_LINE_MATCH = re.compile(r'([ \t]*!)')
FREE_CONT_REGEX = re.compile(r'([ \t]*&)')
FREE_FORMAT_TEST = re.compile(r'[ ]{1,4}[a-z]', re.I)
OPENMP_LINE_MATCH = re.compile(r'[ \t]*[!|c|\*]\$OMP', re.I)
#
PPIF_START_TEST = re.compile(r'#if')
PPIF_END_TEST = re.compile(r'#endif')


def detect_fixed_format(file_lines):
    """Detect fixed/free format by looking for characters in label columns
    and variable declarations before column 6. Treat intersection format
    files as free format."""
    for line in file_lines:
        if FREE_FORMAT_TEST.match(line):
            return False
        tmp_match = NAT_VAR_REGEX.match(line)
        if tmp_match and tmp_match.start(1) < 6:
            return False
        # Trailing ampersand indicates free or intersection format
        line_end = line.split('!')[0].strip()
        if len(line_end) > 0 and line_end[-1] == '&':
            return False
    return True


def detect_comment_start(line, fixed_format=False):
    if OPENMP_LINE_MATCH.match(line) is not None:
        return -1
    if fixed_format:
        if FIXED_COMMENT_LINE_MATCH.match(line) is not None:
            return 0
    else:
        return strip_strings(line).find('!')
    return -1


def strip_strings(in_str):
    out_str = SQ_STRING_REGEX.sub('', in_str)
    out_str = DQ_STRING_REGEX.sub('', out_str)
    return out_str


def separate_def_list(test_str):
    stripped_str = strip_strings(test_str)
    paren_count = 0
    def_list = []
    curr_str = ''
    for char in stripped_str:
        if char == '(':
            paren_count += 1
        elif char == ')':
            paren_count -= 1
        elif char == ',' and paren_count == 0:
            if curr_str != '':
                def_list.append(curr_str)
                curr_str = ''
            continue
        curr_str += char
    if curr_str != '':
        def_list.append(curr_str)
    return def_list


def get_var_dims(test_str):
    paren_count = 0
    curr_dim = 0
    for char in test_str:
        if char == '(':
            paren_count += 1
            if paren_count == 1:
                curr_dim = 1
        elif char == ')':
            paren_count -= 1
        elif char == ',' and paren_count == 1:
            curr_dim += 1
    return curr_dim


def read_var_def(line, type_word=None):
    if type_word is None:
        type_match = NAT_VAR_REGEX.match(line)
        if type_match is None:
            return None
        else:
            type_word = type_match.group(0).strip()
            trailing_line = line[type_match.end(0):]
    else:
        trailing_line = line[len(type_word):]
    type_word = type_word.upper()
    trailing_line = trailing_line.split('!')[0]
    if len(trailing_line) == 0:
        return None
    #
    kind_match = KIND_SPEC_REGEX.match(trailing_line)
    if kind_match is not None:
        type_word += kind_match.group(0).strip().lower()
        trailing_line = trailing_line[kind_match.end(0):]
    else:
        # Class and Type statements need a kind spec
        if type_word.lower() == 'class' or type_word.lower() == 'type':
            return None
        # Make sure next character is space or comma
        if trailing_line[0] != ' ' and trailing_line[0] != ',':
            return None
    #
    keyword_match = KEYWORD_LIST_REGEX.match(trailing_line)
    keywords = []
    while (keyword_match is not None):
        keywords.append(keyword_match.group(0).replace(',', ' ').strip().upper())
        trailing_line = trailing_line[keyword_match.end(0):]
        keyword_match = KEYWORD_LIST_REGEX.match(trailing_line)
    # Check if function
    fun_def = read_fun_def(trailing_line, [type_word, keywords])
    if fun_def is not None:
        return fun_def
    #
    line_split = trailing_line.split('::')
    if len(line_split) == 1:
        if len(keywords) > 0:
            return None
        else:
            trailing_line = line_split[0]
    else:
        trailing_line = line_split[1]
    #
    var_words = separate_def_list(trailing_line.strip())
    #
    return 'var', [type_word, keywords, var_words]


def read_fun_def(line, return_type=None, mod_fun=False):
    fun_match = FUN_REGEX.match(line)
    if fun_match is None:
        return None
    #
    trailing_line = line[fun_match.end(0):].strip()
    trailing_line = trailing_line.split('!')[0]
    name_match = WORD_REGEX.match(trailing_line)
    if name_match is not None:
        name = name_match.group(0)
        trailing_line = trailing_line[name_match.end(0):].strip()
    else:
        return None
    #
    paren_match = SUB_PAREN_MATCH.match(trailing_line)
    args = ''
    if paren_match is not None:
        word_match = WORD_REGEX.findall(paren_match.group(0))
        if word_match is not None:
            word_match = [word for word in word_match]
            args = ','.join(word_match)
        trailing_line = trailing_line[paren_match.end(0):]
    #
    return_var = None
    if return_type is None:
        trailing_line = trailing_line.strip()
        results_match = RESULT_REGEX.match(trailing_line)
        if results_match is not None:
            return_var = results_match.group(1).strip().lower()
    return 'fun', [name, args, [return_type, return_var], mod_fun]


def read_sub_def(line, mod_sub=False):
    sub_match = SUB_REGEX.match(line)
    if sub_match is None:
        return None
    #
    trailing_line = line[sub_match.end(0):].strip()
    trailing_line = trailing_line.split('!')[0]
    name_match = WORD_REGEX.match(trailing_line)
    if name_match is not None:
        name = name_match.group(0)
        trailing_line = trailing_line[name_match.end(0):].strip()
    else:
        return None
    #
    paren_match = SUB_PAREN_MATCH.match(trailing_line)
    args = ''
    if paren_match is not None:
        word_match = WORD_REGEX.findall(paren_match.group(0))
        if word_match is not None:
            word_match = [word for word in word_match]
            args = ','.join(word_match)
        trailing_line = trailing_line[paren_match.end(0):]
    return 'sub', [name, args, mod_sub]


def read_type_def(line):
    type_match = TYPE_DEF_REGEX.match(line)
    if type_match is None:
        return None
    trailing_line = line[type_match.end(0):]
    trailing_line = trailing_line.split('!')[0]
    # Parse keywords
    keyword_match = TATTR_LIST_REGEX.match(trailing_line)
    keywords = []
    parent = None
    while (keyword_match is not None):
        keyword_strip = keyword_match.group(0).replace(',', ' ').strip().upper()
        extend_match = EXTENDS_REGEX.match(keyword_strip)
        if extend_match is not None:
            parent = extend_match.group(1).lower()
        else:
            keywords.append(keyword_strip)
        #
        trailing_line = trailing_line[keyword_match.end(0):]
        keyword_match = TATTR_LIST_REGEX.match(trailing_line)
    # Get name
    line_split = trailing_line.split('::')
    if len(line_split) == 1:
        if len(keywords) > 0 and parent is None:
            return None
        else:
            if trailing_line.split('(')[0].strip().lower() == 'is':
                return None
            trailing_line = line_split[0]
    else:
        trailing_line = line_split[1]
    #
    word_match = WORD_REGEX.match(trailing_line.strip())
    if word_match is not None:
        name = word_match.group(0)
    else:
        return None
    #
    return 'typ', [name, parent, keywords]


def read_mod_def(line):
    mod_match = MOD_REGEX.match(line)
    if mod_match is None:
        return None
    else:
        name = mod_match.group(1)
        if name.lower() == 'procedure':
            trailing_line = line[mod_match.end(1):]
            pro_names = []
            line_split = trailing_line.split(',')
            for name in line_split:
                pro_names.append(name.strip().lower())
            return 'int_pro', pro_names
        # Check for submodule definition
        trailing_line = line[mod_match.start(1):]
        sub_res = read_sub_def(trailing_line, mod_sub=True)
        if sub_res is not None:
            return sub_res
        fun_res = read_fun_def(trailing_line, mod_fun=True)
        if fun_res is not None:
            return fun_res
        return 'mod', name


def read_submod_def(line):
    submod_match = SUBMOD_REGEX.match(line)
    if submod_match is None:
        return None
    else:
        parent_name = None
        name = None
        trailing_line = line[submod_match.end(0):].strip()
        trailing_line = trailing_line.split('!')[0]
        parent_match = WORD_REGEX.match(trailing_line)
        if parent_match is not None:
            parent_name = parent_match.group(0).lower()
            if len(trailing_line) > parent_match.end(0)+1:
                trailing_line = trailing_line[parent_match.end(0)+1:].strip()
            else:
                trailing_line = ''
        #
        name_match = WORD_REGEX.match(trailing_line)
        if name_match is not None:
            name = name_match.group(0).lower()
        return 'smod', [name, parent_name]


def read_prog_def(line):
    prog_match = PROG_REGEX.match(line)
    if prog_match is None:
        return None
    else:
        return 'prog', prog_match.group(1)


def read_int_def(line):
    int_match = INT_REGEX.match(line)
    if int_match is None:
        return None
    else:
        int_name = int_match.group(1).lower()
        if int_name == '':
            return 'int', None
        if int_name == 'assignment' or int_name == 'operator':
            return None
        return 'int', int_match.group(1)


def read_use_stmt(line):
    use_match = USE_REGEX.match(line)
    if use_match is None:
        return None
    else:
        trailing_line = line[use_match.end(0):].lower()
        use_mod = use_match.group(2)
        only_ind = trailing_line.find('only:')
        only_list = []
        if only_ind > -1:
            only_split = trailing_line[only_ind+5:].split(',')
            for only_stmt in only_split:
                only_list.append(only_stmt.split('=>')[0].strip())
        return 'use', [use_mod, only_list]


def_tests = [read_var_def, read_sub_def, read_fun_def, read_type_def,
             read_use_stmt, read_int_def, read_mod_def, read_prog_def, read_submod_def]


def process_file(file_str, close_open_scopes, path=None, fixed_format=False, debug=False):
    #
    if fixed_format:
        COMMENT_LINE_MATCH = FIXED_COMMENT_LINE_MATCH
        CONT_REGEX = FIXED_CONT_REGEX
    else:
        COMMENT_LINE_MATCH = FREE_COMMENT_LINE_MATCH
        CONT_REGEX = FREE_CONT_REGEX
    #
    file_obj = fortran_file(path)
    line_number = 0
    next_line_num = 1
    int_counter = 0
    # at_eof = False
    next_line = None
    line_ind = 0
    while(line_ind < len(file_str)):
        # Get next line
        if next_line is None:
            line = file_str[line_ind]
            line_ind += 1
        else:
            line = next_line
            next_line = None
        line_number = next_line_num
        next_line_num = line_number + 1
        if line == '':
            continue  # Skip empty lines
        # Skip comment lines
        match = COMMENT_LINE_MATCH.match(line)
        if (match is not None):
            continue
        # Start preproccesor if stmt
        match = PPIF_START_TEST.match(line)
        if (match is not None):
            file_obj.start_ppif(line_number)
            continue
        # End preproccesor if stmt
        match = PPIF_END_TEST.match(line)
        if (match is not None):
            file_obj.end_ppif(line_number)
            continue
        # Merge lines with continuations
        if fixed_format:
            if line_ind < len(file_str):
                next_line = file_str[line_ind]
                line_ind += 1
                cont_match = CONT_REGEX.match(next_line)
                while(cont_match is not None and (line_ind < len(file_str))):
                    line = line.rstrip() + next_line[6:].strip()
                    next_line_num += 1
                    next_line = file_str[line_ind]
                    line_ind += 1
                    cont_match = CONT_REGEX.match(next_line)
        else:
            iAmper = line.find('&')
            iComm = line.find('!')
            if iComm < 0:
                iComm = iAmper + 1
            while (iAmper >= 0 and iAmper < iComm):
                split_line = line.split('&')
                next_line = file_str[line_ind]
                line_ind += 1
                if next_line == '':
                    break  # Next line is empty
                    # at_eof = True
                    # break # Reached end of file
                # Skip comment lines
                match = COMMENT_LINE_MATCH.match(next_line)
                if (match is not None):
                    continue
                cont_match = CONT_REGEX.match(next_line)
                if cont_match is not None:
                    next_line = next_line[cont_match.end(0):]
                next_line_num += 1
                line = split_line[0].rstrip() + ' ' + next_line.strip()
                iAmper = line.find('&')
                iComm = line.find('!')
                if iComm < 0:
                    iComm = iAmper + 1
            next_line = None
        line = line.rstrip()
        # Test for scope end
        if file_obj.END_REGEX is not None:
            match = file_obj.END_REGEX.match(line)
            if (match is not None):
                file_obj.end_scope(line_number)
                if(debug):
                    print('{1} !!! END scope({0})'.format(line_number, line.strip()))
                continue
            line_no_comment = line.split('!')[0]
            match = END_GEN_REGEX.match(line_no_comment)
            if (match is not None):
                file_obj.end_scope(line_number)
                if(debug):
                    print('{1} !!! END scope({0})'.format(line_number, line.strip()))
                continue
        # Loop through tests
        obj_read = None
        for test in def_tests:
            obj_read = test(line)
            if obj_read is not None:
                break
        #
        if obj_read is not None:
            obj_type = obj_read[0]
            obj = obj_read[1]
            if obj_type == 'var':
                link_name = None
                if obj[0][:3] == 'PRO':
                    if isinstance(file_obj.current_scope, fortran_int):
                        for var_name in obj[2]:
                            file_obj.add_int_member(var_name)
                        if(debug):
                            print('{1} !!! INTERFACE-PRO statement({0})'.format(line_number, line.strip()))
                        continue
                    i1 = obj[0].find('(')
                    i2 = obj[0].find(')')
                    if i1 > -1 and i2 > -1:
                        link_name = obj[0][i1+1:i2]
                for var_name in obj[2]:
                    if var_name.find('=>') > -1:
                        name_split = var_name.split('=>')
                        name_stripped = name_split[0]
                        link_name = name_split[1].split('(')[0].strip()
                        if link_name.lower() == 'null':
                            link_name = None
                    else:
                        name_stripped = var_name.split('=')[0]
                    var_dim = 0
                    if name_stripped.find('(') > -1:
                        var_dim = get_var_dims(name_stripped)
                    name_stripped = name_stripped.split('(')[0].strip()
                    modifiers = parse_keywords(obj[1])
                    if obj[0][:3] == 'PRO':
                        new_var = fortran_meth(file_obj, line_number, name_stripped, obj[0],
                                               modifiers, file_obj.enc_scope_name, link_name)
                    else:
                        new_var = fortran_obj(file_obj, line_number, name_stripped, obj[0],
                                              modifiers, file_obj.enc_scope_name, link_name)
                    if var_dim > 0:
                        new_var.set_dim(var_dim)
                    file_obj.add_variable(new_var)
                if(debug):
                    print('{1} !!! VARIABLE statement({0})'.format(line_number, line.strip()))
            elif obj_type == 'mod':
                new_mod = fortran_module(file_obj, line_number, obj, file_obj.enc_scope_name)
                file_obj.add_scope(new_mod, END_MOD_REGEX)
                if(debug):
                    print('{1} !!! MODULE statement({0})'.format(line_number, line.strip()))
            elif obj_type == 'smod':
                new_smod = fortran_submodule(file_obj, line_number, obj[0], file_obj.enc_scope_name, obj[1])
                file_obj.add_scope(new_smod, END_SMOD_REGEX)
                if(debug):
                    print('{1} !!! SUBMODULE statement({0})'.format(line_number, line.strip()))
            elif obj_type == 'prog':
                new_prog = fortran_program(file_obj, line_number, obj, file_obj.enc_scope_name)
                file_obj.add_scope(new_prog, END_PROG_REGEX)
                if(debug):
                    print('{1} !!! PROGRAM statement({0})'.format(line_number, line.strip()))
            elif obj_type == 'sub':
                new_sub = fortran_subroutine(file_obj, line_number, obj[0], file_obj.enc_scope_name, obj[1], obj[2])
                file_obj.add_scope(new_sub, END_SUB_REGEX)
                if(debug):
                    print('{1} !!! SUBROUTINE statement({0})'.format(line_number, line.strip()))
            elif obj_type == 'fun':
                new_fun = fortran_function(file_obj, line_number, obj[0], file_obj.enc_scope_name,
                                           obj[1], obj[3], return_type=obj[2][0], result_var=obj[2][1])
                file_obj.add_scope(new_fun, END_FUN_REGEX)
                if obj[2][0] is not None:
                    new_obj = fortran_obj(file_obj, line_number, obj[0], obj[2][0][0], obj[2][0][1],
                                          file_obj.enc_scope_name, None)
                    file_obj.add_variable(new_obj)
                if(debug):
                    print('{1} !!! FUNCTION statement({0})'.format(line_number, line.strip()))
            elif obj_type == 'typ':
                modifiers = parse_keywords(obj[2])
                new_type = fortran_type(file_obj, line_number, obj[0], modifiers, file_obj.enc_scope_name)
                if obj[1] is not None:
                    new_type.set_inherit(obj[1])
                file_obj.add_scope(new_type, END_TYPED_REGEX, req_container=True)
                if(debug):
                    print('{1} !!! TYPE statement({0})'.format(line_number, line.strip()))
            elif obj_type == 'int':
                hidden = False
                if obj is None:
                    int_counter += 1
                    obj = 'GEN_INT{0}'.format(int_counter)
                    hidden = True
                new_int = fortran_int(file_obj, line_number, obj, file_obj.enc_scope_name)
                file_obj.add_scope(new_int, END_INT_REGEX, hidden=hidden, req_container=True)
                if(debug):
                    print('{1} !!! INTERFACE statement({0})'.format(line_number, line.strip()))
            elif obj_type == 'int_pro':
                if file_obj.current_scope is None:
                    continue
                if not isinstance(file_obj.current_scope, fortran_int):
                    continue
                for name in obj:
                    file_obj.add_int_member(name)
                if(debug):
                    print('{1} !!! INTERFACE-PRO statement({0})'.format(line_number, line.strip()))
            elif obj_type == 'use':
                file_obj.add_use(obj[0], line_number, obj[1])
                if(debug):
                    print('{1} !!! USE statement({0})'.format(line_number, line.strip()))
        # Look for visiblity statement
        match = VIS_REGEX.match(line)
        if (match is not None):
            match_lower = match.group(0).lower()
            trailing_line = line[match.end(0):]
            mod_words = WORD_REGEX.findall(trailing_line)
            if len(mod_words) == 0:
                if match_lower == 'private':
                    file_obj.current_scope.set_default_vis(-1)
            else:
                if match_lower == 'private':
                    for word in mod_words:
                        file_obj.add_private(word)
                else:
                    for word in mod_words:
                        file_obj.add_public(word)
            if(debug):
                print('{1} !!! Visiblity statement({0})'.format(line_number, line.strip()))
            continue
    file_obj.close_file()
    return file_obj
