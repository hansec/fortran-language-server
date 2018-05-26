import os
import json
from fortls.objects import fortran_file, fortran_module, fortran_subroutine, \
    fortran_function, fortran_type, fortran_obj, parse_keywords
none_file = fortran_file()


class fortran_intrinsic_obj:
    def __init__(self, name, type, doc_str, args="", parent=None):
        self.name = name
        self.type = type
        self.doc_str = doc_str
        self.args = args
        self.parent = parent
        self.file = none_file

    def get_type(self):
        return self.type

    def get_desc(self):
        if self.type == 2:
            return 'SUBROUTINE'
        else:
            return 'INTRINSIC'

    def get_snippet(self, name_replace=None, drop_arg=None):
        arg_list = self.args.split(",")
        arg_snip = None
        if len(arg_list) > 0:
            place_holders = []
            for i, arg in enumerate(arg_list):
                opt_split = arg.split("=")
                if len(opt_split) > 1:
                    place_holders.append("{1}=${{{0}:{2}}}".format(i+1, opt_split[0], opt_split[1]))
                else:
                    place_holders.append("${{{0}:{1}}}".format(i+1, arg))
            arg_str = "({0})".format(",".join(arg_list))
            arg_snip = "({0})".format(",".join(place_holders))
        else:
            arg_str = "()"
        name = self.name
        if name_replace is not None:
            name = name_replace
        snippet = None
        if arg_snip is not None:
            snippet = name + arg_snip
        return name + arg_str, snippet

    def get_documentation(self, long=False):
        return self.doc_str, False

    def get_children(self):
        return []

    def resolve_inherit(self, obj_tree):
        return

    def is_callable(self):
        if self.type == 2:
            return True
        else:
            return False


def get_intrinsic_modules():
    def create_object(json_obj, enc_obj=None):
        enc_scope_name = None
        args = ""
        modifiers = []
        if enc_obj is not None:
            enc_scope_name = enc_obj.FQSN
        if "args" in json_obj:
            args = json_obj["args"]
        if "mods" in json_obj:
            modifiers = parse_keywords(json_obj["mods"])
        if json_obj["type"] == 0:
            mod_tmp = fortran_module(none_file, 0, json_obj["name"], enc_scope=enc_scope_name)
            if "use" in json_obj:
                mod_tmp.add_use(json_obj["use"], 0)
            return mod_tmp
        elif json_obj["type"] == 1:
            return fortran_subroutine(none_file, 0, json_obj["name"], enc_scope=enc_scope_name,
                                      args=args)
        elif json_obj["type"] == 2:
            return fortran_function(none_file, 0, json_obj["name"], enc_scope=enc_scope_name,
                                    args=args, return_type=[json_obj["return"], modifiers])
        elif json_obj["type"] == 3:
            return fortran_obj(none_file, 0, json_obj["name"], json_obj["desc"], modifiers, enc_scope_name)
        elif json_obj["type"] == 4:
            return fortran_type(none_file, 0, json_obj["name"], modifiers, enc_scope_name)
        else:
            raise ValueError

    def add_children(json_obj, fort_obj):
        if "children" not in json_obj:
            return
        for child in json_obj["children"]:
            child_obj = create_object(child, enc_obj=fort_obj)
            fort_obj.add_child(child_obj)
            add_children(child, child_obj)

    json_file = os.path.join(os.path.dirname(os.path.abspath(__file__)), "intrinsic_mods.json")
    obj_list = []
    with open(json_file, 'r') as fid:
        intrin_file = json.load(fid)
        for key, json_obj in intrin_file.items():
            fort_obj = create_object(json_obj)
            add_children(json_obj, fort_obj)
            obj_list.append(fort_obj)
    return obj_list


def get_intrinsics():
    # Definitions taken from gfortran documentation
    # (https://gcc.gnu.org/onlinedocs/gfortran/Intrinsic-Procedures.html#Intrinsic-Procedures)
    return [
        fortran_intrinsic_obj("ABORT", 2, "ABORT causes immediate termination of the program.", ""),
        fortran_intrinsic_obj("ABS", 3, "ABS(A) computes the absolute value of A.", "A"),
        fortran_intrinsic_obj("ACCESS", 3, "ACCESS(NAME,MODE) checks whether the file NAME exists, "
                              "is readable, writable or executable.", "NAME,MODE"),
        fortran_intrinsic_obj("ACHAR", 3, "ACHAR(I,KIND=kind) returns the character located at position "
                              "I in the ASCII collating sequence.", "I,KIND=kind"),
        fortran_intrinsic_obj("ACOS", 3, "ACOS(X) computes the arccosine of X (inverse of COS(X)).", "X"),
        fortran_intrinsic_obj("ACOSH", 3, "ACOSH(X) computes the inverse hyperbolic cosine of X.", "X"),
        fortran_intrinsic_obj("ADJUSTL", 3, "ADJUSTL(STRING) will left adjust a string by removing "
                              "leading spaces.", "STRING"),
        fortran_intrinsic_obj("ADJUSTR", 3, "ADJUSTR(STRING) will right adjust a string by removing "
                              "trailing spaces.", "STRING"),
        fortran_intrinsic_obj("AIMAG", 3, "AIMAG(Z) yields the imaginary part of complex argument Z.", "Z"),
        fortran_intrinsic_obj("AINT", 3, "AINT(A,KIND=kind) truncates its argument to a whole number.",
                              "A,KIND=kind"),
        fortran_intrinsic_obj("ALL", 3, "ALL(MASK,DIM=dim) determines if all the values are true in "
                              "MASK in the array along dimension DIM.", "MASK,DIM=dim"),
        fortran_intrinsic_obj("ALLOCATED", 3, "ALLOCATED(A) check the allocation status of A.", "A"),
        fortran_intrinsic_obj("ANINT", 3, "ANINT(A,KIND=kind) rounds its argument to the nearest whole "
                              "number.", "A,KIND=kind"),
        fortran_intrinsic_obj("ANY", 3, "ANY(MASK,DIM=dim) determines if any of the values are true in "
                              "MASK in the array along dimension DIM.", "MASK,DIM=dim"),
        fortran_intrinsic_obj("ASIN", 3, "ASIN(X) computes the arcsine of X (inverse of SIN(X)).", "X"),
        fortran_intrinsic_obj("ASINH", 3, "ASINH(X) computes the inverse hyperbolic sine of X.", "X"),
        fortran_intrinsic_obj("ASSOCIATED", 3, "ASSOCIATED(POINTER,TARGET=target) determines "
                              "the status of the pointer POINTER or if POINTER is associated "
                              "with the target TARGET.", "POINTER,TARGET=target"),
        fortran_intrinsic_obj("ATAN", 3, "ATAN(X) computes the arctangent of X (inverse of TAN(X)).", "X"),
        fortran_intrinsic_obj("ATAN2", 3, "ATAN2(Y,X) computes the principal value of the argument "
                              "function of the complex number X + i Y.", "Y,X"),
        fortran_intrinsic_obj("ATANH", 3, "ATANH(X) computes the inverse hyperbolic tangent of X.", "X"),
        fortran_intrinsic_obj("BESSEL_J0", 3, "BESSEL_J0(X) computes the Bessel function of the first "
                              "kind of order 0 of X.", "X"),
        fortran_intrinsic_obj("BESSEL_J1", 3, "BESSEL_J1(X) computes the Bessel function of the first "
                              "kind of order 1 of X.", "X"),
        fortran_intrinsic_obj("BESSEL_JN", 3, "BESSEL_JN(N,X) computes the Bessel function of the first "
                              "kind of order N of X.", "N,X"),
        fortran_intrinsic_obj("BESSEL_Y0", 3, "BESSEL_Y0(X) computes the Bessel function of the second "
                              "kind of order 0 of X.", "X"),
        fortran_intrinsic_obj("BESSEL_Y1", 3, "BESSEL_Y1(X) computes the Bessel function of the second "
                              "kind of order 1 of X.", "X"),
        fortran_intrinsic_obj("BESSEL_YN", 3, "BESSEL_YN(N,X) computes the Bessel function of the second "
                              "kind of order N of X.", "N,X"),
        fortran_intrinsic_obj("BGE", 3, "BGE(I,J) determines whether an integral is a bitwise greater "
                              "than or equal to another.", "I,J"),
        fortran_intrinsic_obj("BGT", 3, "BGT(I,J) determines whether an integral is a bitwise greater "
                              "than another.", "I,J"),
        fortran_intrinsic_obj("BIT_SIZE", 3, "BIT_SIZE(I) returns the number of bits represented by "
                              "the type of I", "I"),
        fortran_intrinsic_obj("BLE", 3, "BLE(I,J) determines whether an integral is a bitwise less "
                              "than or equal to another.", "I,J"),
        fortran_intrinsic_obj("BLT", 3, "BLT(I,J) determines whether an integral is a bitwise less "
                              "than another.", "I,J"),
        fortran_intrinsic_obj("BTEST", 3, "BTEST(I,POS) returns logical .TRUE. if the bit at POS in "
                              "I is set.", "I,J"),
        fortran_intrinsic_obj("CEILING", 3, "CEILING(A,KIND=kind) returns the least integer greater "
                              "than or equal to A.", "A,KIND=kind"),
        fortran_intrinsic_obj("CHAR", 3, "CHAR(I,KIND=kind) returns the character represented by the "
                              "integer I.", "I,KIND=kind"),
        fortran_intrinsic_obj("CMPLX", 3, "CMPLX(X,Y=y,KIND=kind) returns a complex number where X "
                              "is converted to the real component.", "X,Y=y,KIND=kind"),
        fortran_intrinsic_obj("COMMAND_ARGUMENT_COUNT", 3, "COMMAND_ARGUMENT_COUNT() returns the "
                              "number of arguments passed on the command line when the containing "
                              "program was invoked.", "X"),
        fortran_intrinsic_obj("CONJG", 3, "CONJG(Z) returns the conjugate of Z.", "Z"),
        fortran_intrinsic_obj("COS", 3, "COS(X) computes the cosine of X.", "X"),
        fortran_intrinsic_obj("COSH", 3, "COSH(X) computes the hyperbolic cosine of X.", "X"),
        fortran_intrinsic_obj("COTAN", 3, "COTAN(X) computes the cotangent of X.", "X"),
        fortran_intrinsic_obj("COUNT", 3, "COUNT(ARRAY,DIM=dim,MASK=mask) counts the number "
                              "of .TRUE. elements of ARRAY along  dimension DIM if the "
                              "corresponding element in MASK is TRUE.", "ARRAY,DIM=dim,MASK=mask"),
        fortran_intrinsic_obj("CPU_TIME", 2, "CPU_TIME(TIME) returns a REAL value representing "
                              "the elapsed CPU time in seconds.", "TIME"),
        fortran_intrinsic_obj("CSHIFT", 3, "CSHIFT(ARRAY,SHIFT,DIM=dim) performs a circular "
                              "shift on elements of ARRAY along the dimension of DIM.",
                              "ARRAY,SHIFT,DIM=dim"),
        fortran_intrinsic_obj("DATE_AND_TIME", 2, "DATE_AND_TIME(DATE,TIME,ZONE,VALUES) gets "
                              "the corresponding date and time information from the real-time "
                              "system clock.", "DATE,TIME,ZONE,VALUES"),
        fortran_intrinsic_obj("DBLE", 3, "DBLE(A) converts A to double precision real type.", "A"),
        fortran_intrinsic_obj("DIGITS", 3, "DIGITS(X) returns the number of significant binary "
                              "digits of the internal model representation of X.", "X"),
        fortran_intrinsic_obj("DIM", 3, "DIM(X,Y) returns the difference X-Y if the result "
                              "is positive; otherwise returns zero.", "X,Y"),
        fortran_intrinsic_obj("DOT_PRODUCT", 3, "DOT_PRODUCT(A,B) computes the dot product "
                              "multiplication of two vectors A and B.", "A,B"),
        fortran_intrinsic_obj("DPROD", 3, "DPROD(X,Y) returns the product X*Y.", "X,Y"),
        fortran_intrinsic_obj("DSHIFTL", 3, "DSHIFTL(I,J,SHIFT) combines bits of I and J.",
                              "I,J,SHIFT"),
        fortran_intrinsic_obj("DSHIFTR", 3, "DSHIFTR(I,J,SHIFT) combines bits of I and J.",
                              "I,J,SHIFT"),
        fortran_intrinsic_obj("EOSHIFT", 3, "EOSHIFT(ARRAY,SHIFT,BOUNDARY=boundary,DIM=dim) "
                              "performs a end-off shift on elements of ARRAY along the "
                              "dimension of DIM.", "ARRAY,SHIFT,DIM=dim"),
        fortran_intrinsic_obj("EPSILON", 3, "EPSILON(X) returns the smallest number E of the "
                              "same kind as X such that 1 + E > 1.", "X"),
        fortran_intrinsic_obj("ERF", 3, "ERF(X) computes the error function of X.", "X"),
        fortran_intrinsic_obj("ERFC", 3, "ERFC(X) computes the complementary error function of X.", "X"),
        fortran_intrinsic_obj("ERFC_SCALED", 3, "ERFC_SCALED(X) computes the exponentially-scaled "
                              "complementary error function of X.", "X"),
        fortran_intrinsic_obj("EXECUTE_COMMAND_LINE", 2,
                              "EXECUTE_COMMAND_LINE(COMMAND,WAIT=wait,EXITSTAT=exitstat,CMDSTAT=cmdstat,CMDMSG=cmdmsg) "
                              "runs a shell command, synchronously or asynchronously.",
                              "COMMAND,WAIT=wait,EXITSTAT=exitstat,CMDSTAT=cmdstat,CMDMSG=cmdmsg"),
        fortran_intrinsic_obj("EXP", 3, "EXP(X) computes the base e exponential of X.", "X"),
        fortran_intrinsic_obj("EXPONENT", 3, "EXPONENT(X) returns the value of the exponent part of X.", "X"),
        fortran_intrinsic_obj("EXTENDS_TYPE_OF", 3, "EXTENDS_TYPE_OF(A,MOLD) queries dynamic type for extension.",
                              "A,MOLD"),
        fortran_intrinsic_obj("FLOOR", 3, "FLOOR(A,KIND=kind) returns the greatest integer less "
                              "than or equal to A.", "A,KIND=kind"),
        fortran_intrinsic_obj("FRACTION", 3, "FRACTION(X) returns the fractional part of the model "
                              "representation of X.", "X"),
        fortran_intrinsic_obj("GAMMA", 3, "GAMMA(X) computes the gamma function of X.", "X"),
        fortran_intrinsic_obj("GET_COMMAND", 2,
                              "GET_COMMAND(COMMAND=command,LENGTH=length,STATUS=status) "
                              "retrieve the entire command line that was used to invoke the program.",
                              "COMMAND=command,LENGTH=length,STATUS=status"),
        fortran_intrinsic_obj("GET_COMMAND_ARGUMENT", 2,
                              "GET_COMMAND_ARGUMENT(NUMBER=number,VALUE=value,LENGTH=length,STATUS=status) "
                              "retrieve the NUMBER-th argument that was passed on the command line when "
                              "the containing program was invoked.",
                              "NUMBER=number,VALUE=value,LENGTH=length,STATUS=status"),
        fortran_intrinsic_obj("GET_ENVIRONMENT_VARIABLE", 2,
                              "GET_ENVIRONMENT_VARIABLE(NAME=name,VALUE=value,LENGTH=length,STATUS=status,"
                              "TRIM_NAME=trim_name) gets the VALUE of the environmental variable NAME.",
                              "NAME=name,VALUE=value,LENGTH=length,STATUS=status,TRIM_NAME=trim_name"),
        fortran_intrinsic_obj("HUGE", 3, "HUGE(X) returns the largest number that "
                              "is not an infinity in the model of the type of X.", "X"),
        fortran_intrinsic_obj("HYPOT", 3, "HYPOT(X,Y)  is the Euclidean distance function "
                              "without undue underflow or overflow.", "X,Y"),
        fortran_intrinsic_obj("IACHAR", 3, "IACHAR(C,KIND=kind) returns the code for the ASCII character "
                              "in the first character position of C.", "I,KIND=kind"),
        fortran_intrinsic_obj("IALL", 3, "IALL(MASK,DIM=dim) reduces with bitwise AND the elements "
                              "of ARRAY along dimension DIM.", "MASK,DIM=dim"),
        fortran_intrinsic_obj("IAND", 3, "IAND(I,J) Bitwise logical AND.", "I,J"),
        fortran_intrinsic_obj("IANY", 3, "IANY(MASK,DIM=dim) reduces with bitwise OR the elements "
                              "of ARRAY along dimension DIM.", "MASK,DIM=dim"),
        fortran_intrinsic_obj("IBCLR", 3, "IBCLR(I,POS) returns the value of I with the bit at position "
                              "POS set to zero.", "I,POS"),
        fortran_intrinsic_obj("IBITS", 3, "IBITS(I,POS,LEN) extracts a field of length LEN from I, "
                              "starting from bit position POS and extending left for LEN bits.", "I,POS,LEN"),
        fortran_intrinsic_obj("IBSET", 3, "IBSET(I,POS) returns the value of I with the bit at position "
                              "POS set to one.", "I,POS"),
        fortran_intrinsic_obj("ICHAR", 3, "ICHAR(C,KIND=kind) returns the code for the character "
                              "in the first character position of C in the system's native character set.",
                              "I,KIND=kind"),
        fortran_intrinsic_obj("IEOR", 3, "IEOR(I,J) Bitwise logical exclusive OR.", "I,J"),
        fortran_intrinsic_obj("IMAGE_INDEX", 3, "IMAGE_INDEX(COARRAY,SUB) returns the image index "
                              "belonging to a cosubscript.", "COARRAY,SUB"),
        fortran_intrinsic_obj("INDEX", 3, "INDEX(STRING,SUBSTRING,BACK=back,KIND=kind) returns the "
                              "position of the start of the first occurrence of string SUBSTRING as "
                              "a substring in STRING, counting from one.", "STRING,SUBSTRING,BACK=back,KIND=kind"),
        fortran_intrinsic_obj("IOR", 3, "IOR(I,J) Bitwise logical inclusive OR.", "I,J"),
        fortran_intrinsic_obj("IPARITY", 3, "IPARITY(ARRAY,DIM=dim,MASK=mask) reduces with bitwise XOR "
                              "(exclusive or) the elements of ARRAY along dimension DIM if the "
                              "corresponding element in MASK is TRUE.", "ARRAY,DIM=dim,MASK=mask"),
        fortran_intrinsic_obj("IS_IOSTAT_END", 3, "IS_IOSTAT_END(I) tests whether the variable I has the "
                              "value of the I/O status 'end of file'", "I"),
        fortran_intrinsic_obj("IS_IOSTAT_EOR", 3, "IS_IOSTAT_EOR(I) tests whether the variable I has the "
                              "value of the I/O status 'end of record'", "I"),
        fortran_intrinsic_obj("ISHIFT", 3, "ISHIFT(I,SHIFT) returns a value corresponding to I with "
                              "all of the bits shifted SHIFT places", "I,SHIFT"),
        fortran_intrinsic_obj("ISHIFTC", 3, "ISHIFTC(I,SHIFT,SIZE=size) returns a value corresponding "
                              "to I with the rightmost SIZE bits shifted circularly SHIFT places; that "
                              "is, bits shifted out one end are shifted into the opposite end.", "I,SHIFT,SIZE=size"),
        fortran_intrinsic_obj("KIND", 3, "KIND(X) returns the kind value of the entity X.", "X"),
        fortran_intrinsic_obj("LBOUND", 3, "LBOUND(ARRAY,DIM=dim,KIND=kind) returns the lower bounds "
                              "of an array, or a single lower bound along the DIM dimension.",
                              "ARRAY,DIM=dim,KIND=kind"),
        fortran_intrinsic_obj("LEN", 3, "LEN(STRING,KIND=kind) returns the length of a character "
                              "string.", "STRING,KIND=kind"),
        fortran_intrinsic_obj("LEN_TRIM", 3, "LEN_TRIM(STRING,KIND=kind) returns the length of a character "
                              "string, ignoring any trailing blanks.", "STRING,KIND=kind"),
        fortran_intrinsic_obj("LGE", 3, "LGE(STRING_A,STRING_B) determines whether one string is "
                              "lexically greater than or equal to another string.", "STRING_A,STRING_B"),
        fortran_intrinsic_obj("LGT", 3, "LGT(STRING_A,STRING_B) determines whether one string is "
                              "lexically greater than another string.", "STRING_A,STRING_B"),
        fortran_intrinsic_obj("LLE", 3, "LLE(STRING_A,STRING_B) determines whether one string is "
                              "lexically less than or equal to another string.", "STRING_A,STRING_B"),
        fortran_intrinsic_obj("LLT", 3, "LLT(STRING_A,STRING_B) determines whether one string is "
                              "lexically less than another string.", "STRING_A,STRING_B"),
        fortran_intrinsic_obj("LOG", 3, "LOG(X) computes the natural logarithm of X, i.e. the "
                              "logarithm to the base e.", "X"),
        fortran_intrinsic_obj("LOG10", 3, "LOG10(X) computes the base 10 logarithm of X.", "X"),
        fortran_intrinsic_obj("MATMUL", 3, "MATMUL(MATRIX_A,MATRIX_B) performs a matrix "
                              "multiplication on numeric or logical arguments.", "MATRIX_A,MATRIX_B"),
        fortran_intrinsic_obj("MAX", 3, "MAX(A1,A2,...) returns the argument with the smallest "
                              "(most negative) value.", "A1,A2"),
        fortran_intrinsic_obj("MAXEXPONENT", 3, "MAXEXPONENT(X) returns the maximum exponent in "
                              "the model of the type of X.", "X"),
        fortran_intrinsic_obj("MAXLOC", 3, "MAXLOC(ARRAY,DIM=dim,MASK=mask,KIND=kind,BACK=back) "
                              "determines the location of the element in the array with the maximum "
                              "value.",
                              "ARRAY,DIM=dim,MASK=mask,KIND=kind,BACK=back"),
        fortran_intrinsic_obj("MAXVAL", 3, "MAXVAL(ARRAY,DIM=dim,MASK=mask) "
                              "determines the maximum value of the elements in an array.",
                              "ARRAY,DIM=dim,MASK=mask,KIND=kind,BACK=back"),
        fortran_intrinsic_obj("MERGE", 3, "MERGE(TSOURCE,FSOURCE,MASK) select values from two "
                              "arrays according to a logical mask.", "TSOURCE,FSOURCE,MASK"),
        fortran_intrinsic_obj("MIN", 3, "MIN(A1,A2,...) returns the argument with the largest (most "
                              "positive) value.", "A1,A2"),
        fortran_intrinsic_obj("MINEXPONENT", 3, "MINEXPONENT(X) returns the minimum exponent in "
                              "the model of the type of X.", "X"),
        fortran_intrinsic_obj("MINLOC", 3, "MINLOC(ARRAY,DIM=dim,MASK=mask,KIND=kind,BACK=back) "
                              "determines the location of the element in the array with the minimum "
                              "value.",
                              "ARRAY,DIM=dim,MASK=mask,KIND=kind,BACK=back"),
        fortran_intrinsic_obj("MINVAL", 3, "MINVAL(ARRAY,DIM=dim,MASK=mask) "
                              "determines the minimum value of the elements in an array.",
                              "ARRAY,DIM=dim,MASK=mask,KIND=kind,BACK=back"),
        fortran_intrinsic_obj("MOD", 3, "MOD(A,P) computes the remainder of the division of A by P.", "A,P"),
        fortran_intrinsic_obj("MODULO", 3, "MODULO(A,P) computes the A modulo P.", "A,P"),
        fortran_intrinsic_obj("MOVE_ALLOC", 3, "MOVE_ALLOC(FROM,TO) moves the allocation from FROM to TO.",
                              "FROM,TO"),
        fortran_intrinsic_obj("MVBITS", 3, "MVBITS(FROM,FROMPOS,LEN,TO,TOPOS) moves LEN bits from "
                              "positions FROMPOS through FROMPOS+LEN-1 of FROM to positions TOPOS "
                              "through TOPOS+LEN-1 of TO.", "FROM,TO"),
        fortran_intrinsic_obj("NEAREST", 3, "NEAREST(X,S) returns the processor-representable "
                              "number nearest to X in the direction indicated by the sign of S.",
                              "X,S"),
        fortran_intrinsic_obj("NEW_LINE", 3, "NEW_LINE(C) returns the new-line character.", "C"),
        fortran_intrinsic_obj("NINT", 3, "NINT(A,KIND=kind) rounds its argument to the nearest whole number.",
                              "A,KIND=kind"),
        fortran_intrinsic_obj("NOT", 3, "NOT(I) returns the bitwise Boolean inverse of I.", "I"),
        fortran_intrinsic_obj("NULL", 3, "NULL() returns a disassociated pointer.", ""),
        fortran_intrinsic_obj("PACK", 3, "PACK(ARRAY,MASK,VECTOR=vector) stores the elements "
                              "of ARRAY in an array of rank one.", "A,KIND=kind"),
        fortran_intrinsic_obj("PRECISION", 3, "PRECISION(X) returns the decimal precision in the "
                              "model of the type of X.", "X"),
        fortran_intrinsic_obj("PRESENT", 3, "PRESENT(A) determines whether an optional dummy argument is present.",
                              "A"),
        fortran_intrinsic_obj("PRODUCT", 3, "PRODUCT(ARRAY,DIM=dim,MASK=mask) multiplies the elements "
                              "of ARRAY along dimension DIM if the corresponding element in MASK is TRUE.",
                              "ARRAY,DIM=dim,MASK=mask"),
        fortran_intrinsic_obj("RADIX", 3, "RADIX(X) returns the base of the model representing the entity X.",
                              "X"),
        fortran_intrinsic_obj("RANDOM_NUMBER", 2, "RANDOM_NUMBER(HARVEST) returns a single "
                              "pseudorandom number or an array of pseudorandom numbers.", "HARVEST"),
        fortran_intrinsic_obj("RANDOM_SEED", 2, "RANDOM_SEED(SIZE=size,PUT=put,GET=get) restarts or "
                              "queries the state of the pseudorandom number generator used by RANDOM_NUMBER.",
                              "SIZE=size,PUT=put,GET=get"),
        fortran_intrinsic_obj("RANGE", 3, "RANGE(X) returns the decimal exponent range in the model "
                              "of the type of X.", "X"),
        fortran_intrinsic_obj("REPEAT", 3, "REPEAT(STRING,NCOPIES) concatenates NCOPIES copies of a string.",
                              "STRING,NCOPIES"),
        fortran_intrinsic_obj("RESHAPE", 3, "RESHAPE(SOURCE,SHAPE,PAD=pad,ORDER=order) reshapes SOURCE "
                              "to correspond to SHAPE.", "SOURCE,SHAPE,PAD=pad,ORDER=order"),
        fortran_intrinsic_obj("RRSPACING", 3, "RRSPACING(X) returns the reciprocal of the relative spacing "
                              "of model numbers near X.", "X"),
        fortran_intrinsic_obj("SAME_TYPE_AS", 3, "SAME_TYPE_AS(A,B) query dynamic types for equality.",
                              "A,B"),
        fortran_intrinsic_obj("SCALE", 3, "SCALE(X,I) returns X * RADIX(X)**I.",
                              "X,I"),
        fortran_intrinsic_obj("SCAN", 3, "SCAN(STRING,SET,BACK=back,KIND=kind) scans a STRING for "
                              "any of the characters in a SET of characters.", "SOURCE,KIND=kind"),
        fortran_intrinsic_obj("SELECTED_CHAR_KIND", 3, "SELECTED_CHAR_KIND(NAME) returns the kind "
                              "value for the character set named NAME, if a character set with "
                              "such a name is supported, or -1 otherwise.", "NAME"),
        fortran_intrinsic_obj("SELECTED_INT_KIND", 3, "SELECTED_INT_KIND(R) return the kind "
                              "value of the smallest integer type that can represent all values "
                              "ranging from -10^R (exclusive) to 10^R (exclusive).", "R"),
        fortran_intrinsic_obj("SELECTED_REAL_KIND", 3, "SELECTED_REAL_KIND(P,R) returns the kind "
                              "value of a real data type with decimal precision of at least P "
                              "digits, exponent range of at least R, and with a radix of RADIX.", "P,R"),
        fortran_intrinsic_obj("SET_EXPONENT", 3, "SET_EXPONENT(X,I) returns the real number "
                              "whose fractional part is that that of X and whose exponent part is I.", "X,I"),
        fortran_intrinsic_obj("SHAPE", 3, "SHAPE(SOURCE,KIND=kind) determines the shape of an array.",
                              "SOURCE,KIND=kind"),
        fortran_intrinsic_obj("SIGN", 3, "SIGN(A,B) returns the value of A with the sign of B.", "A,B"),
        fortran_intrinsic_obj("SIN", 3, "SIN(X) computes the sine of X.", "X"),
        fortran_intrinsic_obj("SINH", 3, "SINH(X) computes the hyperbolic sine of X.", "X"),
        fortran_intrinsic_obj("SIZE", 3, "SIZE(ARRAY,DIM=dim,KIND=kind) determines the extent of ARRAY "
                              "along a specified dimension DIM, or the total number of elements in ARRAY "
                              "if DIM is absent.", "ARRAY,DIM=dim,KIND=kind"),
        fortran_intrinsic_obj("SPACING", 3, "SPACING(X) determines the distance between the argument X "
                              "and the nearest adjacent number of the same type.", "X"),
        fortran_intrinsic_obj("SPREAD", 3, "SPREAD(SOURCE,DIM,NCOPIES) replicates a SOURCE array "
                              "NCOPIES times along a specified dimension DIM.", "SOURCE,DIM,NCOPIES"),
        fortran_intrinsic_obj("SQRT", 3, "SQRT(X) computes the square root of X.", "X"),
        fortran_intrinsic_obj("SUM", 3, "SUM(ARRAY,DIM=dim,MASK=mask) adds the elements of ARRAY along "
                              "dimension DIM if the corresponding element in MASK is TRUE.",
                              "ARRAY,DIM=dim,MASK=mask"),
        fortran_intrinsic_obj("SYSTEM_CLOCK", 3, "SYSTEM_CLOCK(COUNT=count,COUNT_RATE=count_rate,COUNT_MAX=count_max) "
                              "determines the COUNT of a processor clock since an unspecified time in the past modulo "
                              "COUNT_MAX, COUNT_RATE determines the number of clock ticks per second.",
                              "COUNT=count,COUNT_RATE=count_rate,COUNT_MAX=count_max"),
        fortran_intrinsic_obj("TAN", 3, "TAN(X) computes the tangent of X.", "X"),
        fortran_intrinsic_obj("TANH", 3, "TANH(X) computes the hyperbolic tangent of X.", "X"),
        fortran_intrinsic_obj("TINY", 3, "TINY(X) returns the smallest positive (non zero) number "
                              "in the model of the type of X.", "X"),
        fortran_intrinsic_obj("TRANSFER", 3, "TRANSFER(SOURCE,MOLD,SIZE=size) interprets the bitwise "
                              "representation of SOURCE in memory as if it is the representation of a "
                              "variable or array of the same type and type parameters as MOLD.",
                              "SOURCE,MOLD,SIZE=size"),
        fortran_intrinsic_obj("TRANSPOSE", 3, "TRANSPOSE(MATRIX) transpose an array of rank two.",
                              "MATRIX"),
        fortran_intrinsic_obj("TRIM", 3, "TRIM(STRING) removes trailing blank characters of a string.",
                              "STRING"),
        fortran_intrinsic_obj("UBOUND", 3, "UBOUND(ARRAY,DIM=dim,KIND=kind) returns the upper bounds "
                              "of an array, or a single upper bound along the DIM dimension.",
                              "ARRAY,DIM=dim,KIND=kind"),
        fortran_intrinsic_obj("UPACK", 3, "UPACK(VECTOR,MASK,FIELD) stores the elements "
                              "of VECTOR in an array of higher rank.", "VECTOR,MASK,FIELD"),
        fortran_intrinsic_obj("VERIFY", 3, "VERIFY(STRING,SET,BACK=back,KIND=kind) verifies that "
                              "all the characters in STRING belong to the set of characters in SET.",
                              "STRING,SET,BACK=back,KIND=kind"),
    ]
