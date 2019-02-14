Fortran Language Server
=======================

.. image:: https://travis-ci.org/hansec/fortran-language-server.svg?branch=master
     :target: https://travis-ci.org/hansec/fortran-language-server

.. image:: https://ci.appveyor.com/api/projects/status/github/hansec/fortran-language-server?branch=master&svg=true
     :target: https://ci.appveyor.com/project/hansec/fortran-language-server

.. image:: https://img.shields.io/github/license/hansec/fortran-language-server.svg
     :target: https://github.com/hansec/fortran-language-server/blob/master/LICENSE

A Fortran implementation of the `Language Server Protocol <https://github.com/Microsoft/language-server-protocol>`_ using Python (2.7+ or 3.0+).

Editor extensions using this language server to provide autocomplete and other IDE-like functionality are
available for `Atom <https://atom.io/packages/ide-fortran>`_,
`Visual Studio Code <https://marketplace.visualstudio.com/items?itemName=hansec.fortran-ls>`_,
`Visual Studio <https://github.com/michaelkonecny/vs-fortran-ls-client>`_,
`(Neo)vim <https://github.com/hansec/fortran-language-server/wiki/Using-forts-with-vim>`_,
and `Emacs <https://github.com/emacs-lsp/lsp-mode>`_.

Language Server Features
------------------------

Document Symbols:

.. image:: https://raw.githubusercontent.com/hansec/fortran-language-server/master/images/fortls_outline.png

Auto Completion:

.. image:: https://raw.githubusercontent.com/hansec/fortran-language-server/master/images/fortls_autocomplete.gif

Signature Help:

*Note:* Not available for overloaded subroutines/functions.

.. image:: https://raw.githubusercontent.com/hansec/fortran-language-server/master/images/fortls_sigHelp.gif

Go to definition:

.. image:: https://raw.githubusercontent.com/hansec/fortran-language-server/master/images/fortls_gotodef.gif

Hover:

.. image:: https://raw.githubusercontent.com/hansec/fortran-language-server/master/images/fortls_hover.gif

Find references:

*Note:* Currently not supported for member variables/procedures of user-defined types.

.. image:: https://raw.githubusercontent.com/hansec/fortran-language-server/master/images/fortls_refs.png

Diagnostics:

- Multiple use of the same variable name
- Unknown module in USE statement
- Variable masking definition from parent scope
- Unclosed blocks/scopes
- Invalid scope nesting

*Note:* Diagnostics are only updated when files are saved or opened/closed

.. image:: https://raw.githubusercontent.com/hansec/fortran-language-server/master/images/fortls_diag.png

Installation
------------

``pip install fortran-language-server``

Language server settings
------------------------

The following global settings can be used when launching the language server.

* ``--symbol_skip_mem`` Do not include type members in document symbol results
* ``--incremental_sync`` Use incremental document synchronization
* ``--autocomplete_no_prefix`` Do not filter autocomplete results by variable prefix
* ``--lowercase_intrinsics`` Use lowercase for intrinsics and keywords in autocomplete requests
* ``--use_signature_help`` Use signature help instead of snippets for subroutines/functions
* ``--variable_hover`` Show hover information for variables (default: subroutines/functions only)
* ``--debug_log`` Write debug information to ``root_dir/fortls_debug.log`` (requires a specified ``root_dir`` during initialization)

**Debug settings:**

The following settings can be used to perform `standalone debug tests <https://github.com/hansec/fortran-language-server/wiki>`_ on the language server.

* ``--debug_filepath=DEBUG_FILEPATH`` File path for language server tests
* ``--debug_rootpath=DEBUG_ROOTPATH`` Root path for language server tests
* ``--debug_line=DEBUG_FILEPATH`` Line position for language server tests (1-indexed)
* ``--debug_char=DEBUG_ROOTPATH`` Character position for language server tests (1-indexed)
* ``--debug_parser`` Test source code parser on specified file
* ``--debug_diagnostics`` Test diagnostic notifications for specified file
* ``--debug_symbols`` Test symbol request for specified file
* ``--debug_workspace_symbols=QUERY_STRING`` Test workspace/symbol request for project-wide search
* ``--debug_completion`` Test completion request for specified file and position
* ``--debug_signature`` Test signatureHelp request for specified file and position
* ``--debug_definition`` Test definition request for specified file and position
* ``--debug_hover`` Test hover request for specified file and position
* ``--debug_references`` Test references request for specified file and position

Configuration
-------------

Project specific settings can be specified by placing a JSON file named ``.fortls`` (example below)
in the ``root_dir`` directory.

* ``lowercase_intrinsics`` Use lowercase for intrinsics and keywords in autocomplete requests (default: false)
* ``debug_log`` Write debug information to ``root_dir/fortls_debug.log`` (default: false)

**Setup module search paths:**

By default all files with the suffix ``F,F77,F90,F95,F03,F08,FOR,FPP`` (case-insensitive) in the
``root_dir`` directory, specified during initialization, and all its sub-directories are parsed and included in
the project.

Directories and files can be excluded from the project by specifying their paths (relative to ``root_dir``) in
the ``excl_paths`` variable in the ``.fortls`` file. Excluded directories also exclude all sub-directories. Source
files with a common suffix may also be excluded using the ``excl_suffixes`` variable.

Module directories can also be specified manually by specifying their paths (relative to ``root_dir``) in
the ``mod_dirs`` variable in the ``.fortls`` file. When ``mod_dirs`` is specified directories are not added
recursively, so any nested sub directories must be explicitly listed. However, ``root_dir`` does not need to
be specified manually as it is always included.

**Preprocessor definitions:**

Preprocessor definitions can be set for each project, to improve support for Fortran files using conditional
compilation, using the ``pp_defs`` variable in the ``.fortls`` file. Preprocessing is performed _only_ for files
where the file extension is all caps (ie. ".F90", ".F", etc.). Currently, support for preprocessing is limited
to variables declared in the project's ``.fortls`` file or in the source file of interest as ``#include`` files
and inheritance through ``USE`` statements are yet not supported. Variable substitution is also performed
within files, but is currently limited to non-recursive cases. For example, ``#define PP_VAR1 PP_VAR2`` will
cause ``PP_VAR1`` to be replaced with the text ``PP_VAR2`` throughout the file, not that value of ``PP_VAR2``.

*Note:* The language server will only analyze code within preprocessor conditional regions if the conditional
test can be evaluated by the server or if the region is the *default* path (ie. a bare ``#else`` region).


::

    {
      "mod_dirs": ["subdir1", "subdir2"],
      "excl_paths": ["subdir3", "subdir1/file_to_skip.F90"],
      "excl_suffixes": ["_skip.f90"],
      "pp_defs": {"HAVE_PACKAGE": ""},
      "lowercase_intrinsics": false,
      "debug_log": false
    }

Bug reports
-----------
When `filing bugs <https://github.com/hansec/fortran-language-server/issues/new>`_ please provide example code to reproduce the observed issue.

License
-------

This project is made available under the MIT License.

Support
-------

If you *really* like `this package <https://github.com/hansec/fortran-language-server>`_ you can `buy me a coffee <https://paypal.me/hansec>`_ to say thanks.
