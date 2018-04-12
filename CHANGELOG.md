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
