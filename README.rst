Fortran Language Server (beta)
==============================

.. image:: https://travis-ci.org/hansec/fortran-language-server.svg?branch=master
     :target: https://travis-ci.org/hansec/fortran-language-server

.. image:: https://ci.appveyor.com/api/projects/status/github/hansec/fortran-language-server?branch=master&svg=true
     :target: https://ci.appveyor.com/project/hansec/fortran-language-server

.. image:: https://img.shields.io/github/license/hansec/fortran-language-server.svg
     :target: https://github.com/hansec/fortran-language-server/blob/master/LICENSE

A Fortran implementation of the `Language Server Protocol <https://github.com/Microsoft/language-server-protocol>`_ using Python (2.7+).

Editor extensions using this language server to provide autocomplete and other IDE-like functionality are available for `Atom <https://atom.io/packages/ide-fortran>`_ and `Visual Studio Code <https://marketplace.visualstudio.com/items?itemName=hansec.fortran-ls>`_.

**Note: This language server is currently in the early stages of development.
Not all features are supported or planned.**

Language Server Features
------------------------

Document Symbols:

.. image:: https://raw.githubusercontent.com/hansec/fortran-language-server/master/images/fortls_outline.png

Auto Completion:

.. image:: https://raw.githubusercontent.com/hansec/fortran-language-server/master/images/fortls_autocomplete.gif

Go to definition:

.. image:: https://raw.githubusercontent.com/hansec/fortran-language-server/master/images/fortls_gotodef.gif

Hover:

.. image:: https://raw.githubusercontent.com/hansec/fortran-language-server/master/images/fortls_hover.gif

Find references:

*Note:* Currently for global and top level module objects only.

.. image:: https://raw.githubusercontent.com/hansec/fortran-language-server/master/images/fortls_refs.png

Diagnostics:

- Multiple use of the same variable name
- Unknown module in USE statement
- Variable masking definition from parent scope

.. image:: https://raw.githubusercontent.com/hansec/fortran-language-server/master/images/fortls_diag.png

Installation
------------

``pip install fortran-language-server``

Language server settings
------------------------

The following global settings can be used when launching the language server.

* ``--symbol_skip_mem`` Do not include type members in document symbol results
* ``--incrmental_sync`` Use incremental document synchronization (beta)
* ``--autocomplete_no_prefix`` Do not filter autocomplete results by variable prefix
* ``--lowercase_intrinsics`` Use lowercase for intrinsics and keywords in autocomplete requests

**Debug settings:**

The following settings can be used to perform `standalone debug tests <https://github.com/hansec/fortran-language-server/wiki>`_ on the language server.

* ``--debug_filepath=DEBUG_FILEPATH`` File path for language server tests
* ``--debug_rootpath=DEBUG_ROOTPATH`` Root path for language server tests
* ``--debug_line=DEBUG_FILEPATH`` Line position for language server tests (1-indexed)
* ``--debug_char=DEBUG_ROOTPATH`` Character position for language server tests (1-indexed)
* ``--debug_parser`` Test source code parser on specified file
* ``--debug_symbols`` Test symbol request for specified file
* ``--debug_completion`` Test completion request for specified file and position
* ``--debug_definition`` Test definition request for specified file and position

Configuration
-------------

Project specific settings can be specified by placing a JSON file named ``.fortls`` (example below)
in the ``root_dir`` directory.

**Setup module search paths:**

By default all files with the suffix ``F,F77,F90,F95,F03,F08,FOR,FPP`` (case-insensitive) in the
``root_dir`` directory, specified during initialization, and all its sub-directories are parsed and included in
the project.

Directories and files can be excluded from the project by specifying their paths (relative to ``root_dir``) in
the ``excl_paths`` variable in the ``.fortls`` file. Excluded directories also exclude all sub-directories.

Module directories can also be specified manually by specifying their paths (relative to ``root_dir``) in
the ``mod_dirs`` variable in the ``.fortls`` file. When ``mod_dirs`` is specified directories are not added
recursively, so any nested sub directories must be explicitly listed. However, ``root_dir`` does not need to
be specified manually as it is always included.


::

    {
      "mod_dirs": ["subdir1", "subdir2"],
      "excl_paths": ["subdir3", "subdir1/file_to_skip.F90"]
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
