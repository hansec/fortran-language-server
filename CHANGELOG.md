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
