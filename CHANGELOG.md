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
