## 1.10.3

### Fixes
* Fix parsing bug with spaces in "old-style" kind specifications, fixes [#142](https://github.com/hansec/fortran-language-server/issues/142)
* Fix issue with erroneous sub-word matching in preprocessor macro substitutions, fixes [#141](https://github.com/hansec/fortran-language-server/issues/141)

## 1.10.2

### Improvements
* Add support for "old-style" character length specification, fixes [#130](https://github.com/hansec/fortran-language-server/issues/130) and [#134](https://github.com/hansec/fortran-language-server/issues/134)

### Fixes
* Fix "can't set attribute" error in USE traversal, fixes [#132](https://github.com/hansec/fortran-language-server/issues/132)
* Fix bugs related to optional leading ampersands on continuation lines, fixes [#131](https://github.com/hansec/fortran-language-server/issues/131)
* Fix bug in block parsing with string literals, fixes [#128](https://github.com/hansec/fortran-language-server/issues/128)

## 1.10.1

### Fixes
* Fix bug in semicolon parsing, fixes [#127](https://github.com/hansec/fortran-language-server/issues/127)

## 1.10.0

### Improvements
* Initial implementation of preprocessor include file handling, ref [#115](https://github.com/hansec/fortran-language-server/issues/115)
* Add support for specifying file suffixes for preprocessing, ref [#115](https://github.com/hansec/fortran-language-server/issues/115)
* Add support for completion in visibility statements, fixes [#120](https://github.com/hansec/fortran-language-server/issues/120)
* Support "onOpen" requests before a file is written to disk, fixes [#123](https://github.com/hansec/fortran-language-server/issues/123)
* Add support for IMPURE keyword (contributed by @mcocdawc)
* Improve readability by replacing various result arrays with namedtuples

### Fixes
* Fix bug in open string literal detection, fixes [#124](https://github.com/hansec/fortran-language-server/issues/124)
* Fix bug with multiline docstrings that start with a trailing comment, fixes [#118](https://github.com/hansec/fortran-language-server/issues/118)
* Fix symbols types for subroutines and methods in "documentSymbol" and "completion" requests, fixes [#117](https://github.com/hansec/fortran-language-server/issues/117)
* Fix bug where ONLY renaming was not fully tracked in some circumstances
* Fix bug with inline dimension specifications for variables
* Fix accidental message swap in "object not found" and "object not imported" diagnostics
* Fix bug where errors were reported with "module subroutine" and "module function" definitions (no import required)

## 1.9.1

### Fixes
* Fix bug in USE ONLY accounting used for graph pruning, fixes [#122](https://github.com/hansec/fortran-language-server/issues/122)

## 1.9.0

### Improvements
* Add support for USE statement renaming requests, ref [#109](https://github.com/hansec/fortran-language-server/issues/109)
* Add support for argument information in variable hover requests, fixes [#107](https://github.com/hansec/fortran-language-server/issues/107)
* Add support for disabling snippets in autocomplete results, fixes [#112](https://github.com/hansec/fortran-language-server/issues/112)
* Prevent file AST updates on Open/Close requests when contents have not changed, ref [#105](https://github.com/hansec/fortran-language-server/issues/105)
* Reduce unnecessary parsing with single line file changes
* Debugging: Add support for printing full result object

### Fixes
* Remove required space between "DOUBLE PRECISION" and "DOUBLE COMPLEX" definitions, fixes [#110](https://github.com/hansec/fortran-language-server/issues/110)
* Fix requests when a user-defined type variable has the same name as a defined type used in that scope

## 1.8.2

### Fixes
* Fix parsing single line WHERE statements with trailing parentheses, fixes [#106](https://github.com/hansec/fortran-language-server/issues/106)
* Fix erroneous object duplication diagnostics for DO, INTERFACE, etc. blocks
* Remove erroneous "unimplemented procedure" diagnostics from abstract type definitions
* Fix parsing bugs with semicolons in trailing comments

## 1.8.1

### Fixes
* Fix bug with requests in lines with tab characters, fixes [#93](https://github.com/hansec/fortran-language-server/issues/93)
* Fix bug with requests following "WRITE(\*,\*)" statements

## 1.8.0

### Improvements
* Add full support for ASSOCIATE statements, fixes [#101](https://github.com/hansec/fortran-language-server/issues/101)
* Filter completion suggestions after "MODULE PROCEDURE" statements, fixes [#103](https://github.com/hansec/fortran-language-server/issues/103)
* Filter completion suggestions in type-bound procedure links
* Add support for including external source file directories
* Diagnostics: Line length exceeds maximum length errors
* Speedup language server initialization
* Speedup "textDocument/references" requests

## 1.7.3

### Fixes
* Fix case preservation in hover requests, fixes [#102](https://github.com/hansec/fortran-language-server/issues/102)
* Fix rename requests for type-bound procedures without an explicit link statement (ie. "=>"), fixes [#104](https://github.com/hansec/fortran-language-server/issues/104)
* Fix incorrect "CONTAINS" diagnostic errors with procedure pointers and external interfaces
* Fix bug in diagnostic construction/reporting (introduced in v1.7)
* Fix bugs caused by accidental modification of child object lists

## 1.7.2

### Fixes
* Fix bug with definition/hover requests involving intrinsic functions/modules/variables (introduced in v1.7)

## 1.7.1

### Fixes
* Fix bug with completion and signatureHelp requests on continuation lines (introduced in v1.7)
* Fix out-of-range error with various requests on zero-length lines (introduced in v1.7)

## 1.7.0

### Improvements
* Add initial support for "textDocument/codeAction" requests, generate unimplemented deferred procedures
* Show subroutine/function keywords ("PURE", "ELEMENTAL", etc.)
* Add position of object in line to "textDocument/definition" and "textDocument/implementation" results
* Diagnostics: CONTAINS statement placement errors
* Diagnostics: Visibility statement placement errors
* Command line options: Notify when workspace initialization is complete
* Command line options: Set number of threads used during initialization
* Significant refactoring of core code

### Fixes
* Fix "RecursionError" exception with circular user-defined type references, fixes [#100](https://github.com/hansec/fortran-language-server/issues/100)
* Fix bug detecting TYPE definitions with an immediately following colon, ref [#100](https://github.com/hansec/fortran-language-server/issues/100)
* Fix incorrect diagnostics for interface statements with USE instead of IMPORT statements

## 1.6.0

### Improvements
* Add support for EXTERNAL subroutines
* Diagnostics: Missing subroutine/function arguments and argument declarations
* Diagnostics: Unimplemented deferred type-bound procedures
* Diagnostics: Unknown TYPE/KIND objects (only if candidate is visible in workspace)
* Diagnostics: IMPORT statements (missing objects and placement)
* Diagnostics: Basic handling for IMPLICIT statements

## 1.5.1

### Improvements
* Add support for semicolon separators and multiline preprocessor macros, fixes [#98](https://github.com/hansec/fortran-language-server/issues/98)
* Add various "parsing errors" to debug_parser output

### Fixes
* Use consistent file access method across debug_parser run and language server requests

## 1.5.0

### Improvements
* Add support for "textDocument/rename" requests
* Add initial support for Doxygen and FORD style comment blocks, ref [#44](https://github.com/hansec/fortran-language-server/issues/44)

### Fixes
* Fix language server crash with unknown user-defined type fields

### Other changes
* Deprecate "mod_dirs" option in favor of more accurate "source_dirs". Support for "mod_dirs" will be removed in a future release.

## 1.4.0

### Improvements
* Add support for "textDocument/implementation" requests, ref [#94](https://github.com/hansec/fortran-language-server/issues/94)
* Add option to preserve keyword ordering, ref [#97](https://github.com/hansec/fortran-language-server/issues/97)

### Fixes
* Fix parsing bug with single line WHERE statements, fixes [#92](https://github.com/hansec/fortran-language-server/issues/92)
* Fix bug with keyword parsing with nested parenthesis, fixes [#97](https://github.com/hansec/fortran-language-server/issues/97)
* Differentiate between type-bound procedures and implementations in "textDocument/references" requests, fixes [#94](https://github.com/hansec/fortran-language-server/issues/94)
* Fix typos in MAX and MIN intrinsic functions, ref [#96](https://github.com/hansec/fortran-language-server/pull/96)

## 1.3.0

### Improvements
* Add support for user-defined type members in "textDocument/references" requests, fixes [#88](https://github.com/hansec/fortran-language-server/issues/88)
* Link type-bound procedures with no explicit link to matching named scope in module, fixes [#89](https://github.com/hansec/fortran-language-server/issues/89)
* Report diagnostics related to misplaced "CONTAINS" statements
* Restructure README for improved clarity on capabilities/limitations

### Fixes
* Fix bug with blank/empty lines in free-format continuations, fixes [#91](https://github.com/hansec/fortran-language-server/issues/91)
* Fix exception in "textDocument/references" requests when no object is found, fixes [#86](https://github.com/hansec/fortran-language-server/issues/86)
* Fix bug when relative path is used for --debug_rootpath, fixes [#87](https://github.com/hansec/fortran-language-server/issues/87)

## 1.2.1

### Fixes
* Fix bug in nested user-defined type inheritance, fixes [#85](https://github.com/hansec/fortran-language-server/issues/85)
* Fix bug in completion requests with empty parenthesis in request line

## 1.2.0

### Improvements
* Add support for local variables/objects in "textDocument/references" requests, ref [#84](https://github.com/hansec/fortran-language-server/issues/78)
* Improve preprocessing to handle more types of conditional statements and macro substitution, ref [#78](https://github.com/hansec/fortran-language-server/issues/78)
* Report diagnostics for excess "END" statements instead of causing parser failure, ref [#78](https://github.com/hansec/fortran-language-server/issues/78)

### Fixes
* Fix missing "textDocument/references" results when line starts with target object, fixes [#84](https://github.com/hansec/fortran-language-server/issues/84)

## 1.1.1

### Fixes
* Fix bug with backslash URI separators on Windows, fixes [#83](https://github.com/hansec/fortran-language-server/issues/83)

## 1.1.0

### Improvements
* Add initial implementation of simple preprocessor, ref [#78](https://github.com/hansec/fortran-language-server/issues/78)

### Fixes
* Updated Fixed/Free detection logic using ampersands to check for comment line, fixes [#81](https://github.com/hansec/fortran-language-server/issues/81)
* Support use of "END" as a variable, fixes [#82](https://github.com/hansec/fortran-language-server/issues/82)

## 1.0.5

### Fixes
* Add support for named "SELECT" statements, fixes [#80](https://github.com/hansec/fortran-language-server/issues/80)
* Track scopes for "ASSIGNMENT" and "OPERATOR" interface statements, fixes [#79](https://github.com/hansec/fortran-language-server/issues/79)
* Fix bug in parsing "SELECT" statements with no space, fixes [#77](https://github.com/hansec/fortran-language-server/issues/77)
* Further improve discrimination between end statements and other items, ref [#73](https://github.com/hansec/fortran-language-server/issues/73)

## 1.0.4

### Fixes
* Normalize file paths when storing/accessing file index, fixes [#75](https://github.com/hansec/fortran-language-server/issues/75)
* Fix intrinsic statement "COUNT" ([#76](https://github.com/hansec/fortran-language-server/pull/76))

## 1.0.3

### Fixes
* Further improve discrimination between end statements and variables/block labels, ref [#73](https://github.com/hansec/fortran-language-server/issues/73)
* Fix autocomplete errors when ASSOCIATE and ENUM statements are present
* Fix severity reporting with "debug_diagnostics" command line option

## 1.0.2

### Fixes
* Fix discrimination between end statements and variables with underscores, fixes [#73](https://github.com/hansec/fortran-language-server/issues/73)
* Detect enum definitions, fixes [#74](https://github.com/hansec/fortran-language-server/issues/74)

## 1.0.1

### Fixes
* Detect and support associate statements, fixes [#72](https://github.com/hansec/fortran-language-server/issues/72)

## 1.0.0

### Improvements
* Add parsing of DO/IF/WHERE blocks and report scope end errors
* Detect and report errors with invalid parent for scope definitions
* Improve highlighting for hover requests in VSCode
* Downgrade missing use warnings to information level
* Add intrinsic declaration statement "double complex" ([#70](https://github.com/hansec/fortran-language-server/pull/70))

### Fixes
* Fix bug with leading whitespace on visibility statements, fixes [#69](https://github.com/hansec/fortran-language-server/issues/69)
* Fix parsing errors when "&" and "!" characters are present inside string literals
* Fix parsing bug with multiple leading function/subroutine modifiers (PURE, ELEMENTAL, etc.)

## 0.9.3

### Fixes
* Fix detection of function definitions with leading module and variable statements, fixes [#66](https://github.com/hansec/fortran-language-server/issues/66)
* Properly close remaining open scopes at end of file
* Initialize scope "eline" property, [PR #67](https://github.com/hansec/fortran-language-server/pull/67)

## 0.9.2

### Improvements
* Improve handling of different file encodings, [PR #57](https://github.com/hansec/fortran-language-server/pull/57)

### Fixes
* Fix autocomplete results for inherited members of user-defined types when the member type definition is only available in parent type's scope

## 0.9.1

### Improvements
* Add support for generic interfaces in type-bound procedures, [#64](https://github.com/hansec/fortran-language-server/issues/64)
* Add parent scope information to masked variable errors, [#48](https://github.com/hansec/fortran-language-server/issues/48)

### Fixes
* Fix parsing deferred length character definitions, [#61](https://github.com/hansec/fortran-language-server/issues/61)
* Fix parsing function definitions with modifiers before type, [#63](https://github.com/hansec/fortran-language-server/issues/63)
* Fix parsing with array construction in subroutine/function calls, [#60](https://github.com/hansec/fortran-language-server/issues/60)

## 0.9.0

### Improvements
* Add basic support for workspace/symbol requests
* Add support for excluding source files based on a common suffix

### Fixes
* Prevent detection of variables starting with "use" as USE statements, [#59](https://github.com/hansec/fortran-language-server/issues/59)
* Improve parsing of USE ONLY statements, [#53](https://github.com/hansec/fortran-language-server/issues/53)
* Make sure explicitly specified module directories exist, fixes [#52](https://github.com/hansec/fortran-language-server/issues/52)
* Fix visibility statements with trailing comments, [#49](https://github.com/hansec/fortran-language-server/issues/49)

## 0.8.4

### Fixes
* Check for existence of file during "textDocument/didClose" requests, [#46](https://github.com/hansec/fortran-language-server/issues/46)
* Encode text as UTF-8 in change requests, fixes [#41](https://github.com/hansec/fortran-language-server/issues/41)

## 0.8.3

### Improvements
* Add support for generating debug logs
* Add Fortran statements to autocomplete suggestions
* Add support for explicit dimension specifications, fixes [#37](https://github.com/hansec/fortran-language-server/issues/37)

## 0.8.2

### Improvements
* Add support for F03 style bracket array initialization, fixes [#35](https://github.com/hansec/fortran-language-server/issues/35)

## 0.8.1

### Fixes
* Fix crash in completion requests with intrinsic modules

## 0.8.0

### Improvements
* Reformat completion information and snippets to match common language server conventions
* Provide hover information for overloaded interfaces
* Add support for autocompletion in select type statements
* Add support for type bound procedures with explicit pass statements
* Add support for arguments defined as interfaces in hover and signatureHelp requests
* Unbetafy signatureHelp support

### Fixes
* Fix linking type bound procedures with same name as subroutine/function definition

## 0.7.3

### Fixes
* Improve detection of block statements, fixes [#32](https://github.com/hansec/fortran-language-server/issues/32)
* Fix autocompletion with mixed case object definitions

## 0.7.2

### Fixes
* Fix variable definition detection without spaces, fixes [#30](https://github.com/hansec/fortran-language-server/issues/30)

## 0.7.1

### Improvements
* Add option for displaying hover information for variables
* Add subroutine/function keywords to hover information
* Add more keywords to variable information
* Support spaces between subroutine name and parentheses in signatureHelp

### Fixes
* Fix bug with file paths that include spaces, fixes [#29](https://github.com/hansec/fortran-language-server/issues/29)
* Fix bug where arguments were erroneously dropped for procedure variables
* Fix bug where arguments of procedure type did not have definition information in subroutine/function hover results
* Correct spelling of incremental_sync argument, fixes [#28](https://github.com/hansec/fortran-language-server/issues/28)

## 0.7.0

### Improvements
* Add support for signatureHelp requests with non-overloaded subroutines/functions
* Provide autocomplete and hover information for procedures with explicit interface definitions
* Add support for Fortran 2008 block constructs, fixes [#23](https://github.com/hansec/fortran-language-server/issues/23)
* Add support for "DOUBLE COMPLEX" datatype

### Fixes
* Fix bug where external interfaces were erroneously public in default private modules
* Fix bug producing repeated objects with include statements

## 0.6.2

### Improvements
* Catch and report more types of errors related to file processing, fixes [#21](https://github.com/hansec/fortran-language-server/issues/21)

## 0.6.1

### Fixes
* Fix bug with incremental sync using VSCode on windows, fixes [#20](https://github.com/hansec/fortran-language-server/issues/20)

## 0.6.0

### Improvements
* Add keywords to autocomplete results in variable definition statements
* Filter autocompletion results in extend, import, and procedure statements
* Ignore completion requests on scope definition and ending lines to reduce autocomplete noise
* Filter autocompletion results in variable definition statements to reduce autocomplete noise (variables only)
* Ignore autocomplete and definition requests on preprocessor lines
* Add option to test completion and definition requests in debug mode

### Fixes
* Improve export of abstract and external interfaces for completion and definition requests
* Fix scope name detection to prevent confusing variables that start with Fortran statement names
* Fix handling of external and abstract interface specifications
* Fix bug preventing unrestricted USE statements from overriding USE only statements
* Fix bug where file parsing ended prematurely in some cases with line continuations

## 0.5.0

### Improvements
* Add intrinsic functions and modules to autocomplete suggestions
* Add support for include statements

### Fixes
* Remove erroneously included global objects from autocomplete results in USE ONLY statements
* Fix displayed type for derived type objects in autocomplete requests

## 0.4.0

### Improvements
* Add support for find_references, global and top-level module objects only
* Filter autocomplete suggestions for callable objects in call statements
* Speedup initialization and updates on large projects by accelerating construction of USE tree

### Fixes
* Fix parser error with definitions requiring enclosing scopes in #include files and unnamed programs, fixes [#17](https://github.com/hansec/fortran-language-server/issues/17)
* Fix parser failure with visibility statements in included fortran files, fixes [#16](https://github.com/hansec/fortran-language-server/issues/16)
* Fix detection of lines with trailing comments

## 0.3.7

### Improvements
* Automatically trigger autocomplete on `%` character
* Show named interfaces and prototypes in document outline
* Add support for autocomplete without prefix filtering

### Fixes
* Fix occasional language server error in autocompletion with class methods

## 0.3.6

### Improvements
* Add support for fortran submodules, fixes [#14](https://github.com/hansec/fortran-language-server/issues/14) and [#15](https://github.com/hansec/fortran-language-server/issues/15)
* Improve line tokenization and parsing

### Fixes
* Fix parsing errors with incomplete function definitions
* Fix bugs in symbol and parser debugging

## 0.3.5

### Fixes
* Improve unicode file handling with Python 3.x
* Add support for unnamed programs, fixes [#13](https://github.com/hansec/fortran-language-server/issues/13)

## 0.3.4

### Fixes
* Fix parser error with uppercase characters in scope names, fixes [#11](https://github.com/hansec/fortran-language-server/issues/11)
* Add support for object names with a leading underscore, fixes [#9](https://github.com/hansec/fortran-language-server/issues/9)
* Do not report diagnostics inside preprocessor if statements, fixes [#7](https://github.com/hansec/fortran-language-server/issues/7)

## 0.3.3

### Improvements
* Improved Windows support and added AppVeyor CI testing
* Add support for snippets in autocompletion
* Ignore requests in comment sections

### Fixes
* Fix bug with string/byte handling in Python 3
* Fix bug with multiprocess support on Windows
* Fix bug with URI formatting and paths on Windows, fixes [#8](https://github.com/hansec/fortran-language-server/issues/8)

## 0.3.2

### Fixes
* Fix parsing variable definitions containing separators inside strings, fixes [#4](https://github.com/hansec/fortran-language-server/issues/4)
* Fix incorrect variable masking error in functions, fixes [#5](https://github.com/hansec/fortran-language-server/issues/5)
* Do not report intrinsic modules as unknown, fixes [#2](https://github.com/hansec/fortran-language-server/issues/2) and [#3](https://github.com/hansec/fortran-language-server/issues/3)

## 0.3.1

### Improvements
* Do not show warnings for variable masking in interface definitions
* Respect visibility statements when searching for object in scope

### Fixes
* Fix bug in incremental document sync with ending newline

## 0.3.0

### Improvements
* Add basic file diagnostics (double declaration, variable masking, unknown USE)
* Indicate optional arguments in autocomplete suggestions
* Detect source code format from file contents instead of extension
* Add support for incremental document synchronization

### Fixes
* Fix parsing error when variable definition line is incomplete
* Fix incorrect line handling with open parentheses
* Fix bug when file parsing/hashing fails in workspace initialization

## 0.2.0

### Improvements
* Add support for recursive directory inclusion from "root_path"
* Provide option to skip type members in documentSymbol requests
* Apply visibility statements to objects for autocomplete suggestions
* Filter interface suggestions to only show unique signatures
* Link imported procedures in interface definitions

### Fixes
* Fix line continuation handling for free form files with trailing and leading ampersands
* Improve parentheses matching in line parsing

## 0.1.4

### Improvements
* Handle line continuations in language server requests
* Add server version number to help output

### Fixes
* Fix bug when parsing files with unicode characters

## 0.1.3

### Improvements
* Include interfaces in autocomplete suggestions
* Restrict autocomplete suggestions by object visibility
* Improve USE statement traversal
* Add notifications for parser failures

### Fixes
* Fix bug where parsing errors during workspace initialization could crash the language server

## 0.1.2
* Synchronize version numbers

## 0.1.1
* fix download link in setup.py

## 0.1.0 - First Release
* Initial release
