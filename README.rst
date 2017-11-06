Fortran Language Server (beta)
==============================

.. image:: https://travis-ci.org/hansec/fortran-language-server.svg?branch=master
     :target: https://travis-ci.org/hansec/fortran-language-server

.. image:: https://img.shields.io/github/license/hansec/fortran-language-server.svg
     :target: https://github.com/hansec/fortran-language-server/blob/master/LICENSE

A FORTRAN implementation of the `Language Server Protocol`_ using Python (2.7+).

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

Diagnostics:

- Multiple use of the same variable name
- Unknown module in USE statement
- Variable masking definition from parent scope

.. image:: https://raw.githubusercontent.com/hansec/fortran-language-server/master/images/fortls_diag.png

Installation
------------

``pip install fortran-language-server``

Configuration
-------------

Project specific settings can be specified by placing a JSON file named ``.fortls`` (example below)
in the ``root_dir`` directory.

**Setup module search paths:**

By default all files with the suffix ``f,F,f77,F77,for,FOR,fpp,FPP`` or ``f90,F90,f95,F95,f03,F03,f08,F08`` in the
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
When `filing bugs`_ please provide example code to reproduce the observed issue.

License
-------

This project is made available under the MIT License.

.. _Language Server Protocol: https://github.com/Microsoft/language-server-protocol
.. _filing bugs: https://github.com/atom/fortran-language-server/issues/new
