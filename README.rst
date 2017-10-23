Fortran Language Server (beta)
======================

.. image:: https://travis-ci.org/hansec/fortran-language-server.svg?branch=master
     :target: https://travis-ci.org/hansec/fortran-language-server

.. image:: https://img.shields.io/github/license/hansec/fortran-language-server.svg
     :target: https://github.com/hansec/fortran-language-server/blob/master/LICENSE

A FORTRAN implementation of the `Language Server Protocol`_ using Python.

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

Installation
------------

``pip install fortran-language-server``

Configuration
-------------

**Setup module search paths:**

By default all files with the suffix ``f,F,f77,F77,for,FOR,fpp,FPP`` or ``f90,F90,f95,F95,f03,F03,f08,F08`` in the
``root_dir`` directory specified during initialization are parsed and included in the project. Specific folders
containing FORTRAN source files can be set for a given project by placing a JSON file (example below) named
``.fortls`` in the ``root_dir`` directory. Folders to search are listed in the variable ```mod_dirs`` (relative
to ``root_dir``) and excluded files can be specified using the variable ``excl_paths``. Directories are
not added recursively, so any nested sub directories must be explicitly listed.

::

    {
      "mod_dirs": ["subdir1", "subdir2"],
      "excl_paths": ["subdir1/file_to_skip.F90"]
    }

Bug reports
-----------
When [filing bugs](https://github.com/atom/fortran-language-server/issues/new) please provide example code
to reproduce the observed issue.

License
-------

This project is made available under the MIT License.

.. _Language Server Protocol: https://github.com/Microsoft/language-server-protocol
