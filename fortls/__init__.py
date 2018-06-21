from __future__ import print_function
import sys
import os
import argparse
from multiprocessing import freeze_support
from .langserver import LangServer
from .jsonrpc import JSONRPC2Connection, ReadWriter
from .parse_fortran import process_file, detect_fixed_format
__version__ = '0.6.0'


def error_exit(error_str):
    print("ERROR: {0}".format(error_str))
    sys.exit(-1)


def main():
    #
    freeze_support()
    parser = argparse.ArgumentParser()
    parser.description = "FORTRAN Language Server ({0})".format(__version__)
    parser.add_argument(
        '--version', action="store_true",
        help="Print server version number and exit"
    )
    parser.add_argument(
        '--symbol_skip_mem', action="store_true",
        help="Do not include type members in document symbol results"
    )
    parser.add_argument(
        '--incrmental_sync', action="store_true",
        help="Use incremental document synchronization (beta)"
    )
    parser.add_argument(
        '--autocomplete_no_prefix', action="store_true",
        help="Do not filter autocomplete results by variable prefix"
    )
    parser.add_argument(
        '--lowercase_intrinsics', action="store_true",
        help="Use lowercase for intrinsics and keywords in autocomplete requests"
    )
    group = parser.add_argument_group("DEBUG", "Options for debugging language server")
    group.add_argument(
        '--debug_parser', action="store_true",
        help="Test source code parser on specified file"
    )
    group.add_argument(
        '--debug_symbols', action="store_true",
        help="Test symbol request for specified file"
    )
    group.add_argument(
        '--debug_completion', action="store_true",
        help="Test completion request for specified file and position"
    )
    group.add_argument(
        '--debug_definition', action="store_true",
        help="Test definition request for specified file and position"
    )
    group.add_argument(
        '--debug_filepath', type=str,
        help="File path for language server tests"
    )
    group.add_argument(
        '--debug_rootpath', type=str,
        help="Root path for language server tests"
    )
    group.add_argument(
        '--debug_line', type=int,
        help="Line position for language server tests (1-indexed)"
    )
    group.add_argument(
        '--debug_char', type=int,
        help="Character position for language server tests (1-indexed)"
    )
    args = parser.parse_args()
    if args.version:
        print("{0}".format(__version__))
        sys.exit(0)
    debug_server = (args.debug_symbols or args.debug_completion
                    or (args.debug_rootpath is not None))
    #
    settings = {
        "symbol_include_mem": (not args.symbol_skip_mem),
        "sync_type": 2 if args.incrmental_sync else 1,
        "autocomplete_no_prefix": args.autocomplete_no_prefix,
        "lowercase_intrinsics": args.lowercase_intrinsics
    }
    #
    if args.debug_parser:
        if args.debug_filepath is None:
            error_exit("'debug_filepath' not specified for parsing test")
        file_exists = os.path.isfile(args.debug_filepath)
        if file_exists is False:
            error_exit("Specified 'debug_filepath' does not exist")
        filename, ext = os.path.splitext(os.path.basename(args.debug_filepath))
        #
        print('\nTesting parser')
        print('  File = "{0}"'.format(args.debug_filepath))
        with open(args.debug_filepath, 'r') as fhandle:
            contents_split = fhandle.readlines()
            fixed_flag = detect_fixed_format(contents_split)
            print('  Detected format: {0}'.format("fixed" if fixed_flag else "free"))
            print("\n=========\nParser Output\n=========\n")
            ast_new = process_file(contents_split, True, fixed_format=fixed_flag, debug=True)
            print("\n=========\nObject Tree\n=========\n")
            for obj in ast_new.get_scopes():
                print("{0}: {1}".format(obj.get_type(), obj.FQSN))
                print_children(obj)
            print("\n=========\nExportable Objects\n=========\n")
            for key, obj in ast_new.global_dict.items():
                print("{0}: {1}".format(obj.get_type(), obj.FQSN))
    #
    elif debug_server:
        prb, pwb = os.pipe()
        tmpin = os.fdopen(prb, 'rb')
        tmpout = os.fdopen(pwb, 'wb')
        s = LangServer(conn=JSONRPC2Connection(ReadWriter(tmpin, tmpout)),
                       logLevel=0, settings=settings)
        #
        if args.debug_rootpath:
            dir_exists = os.path.isdir(args.debug_rootpath)
            if dir_exists is False:
                error_exit("Specified 'debug_rootpath' does not exist or is not a directory")
            print('\nTesting "initialize" request:')
            print('  Root = "{0}"\n'.format(args.debug_rootpath))
            s.serve_initialize({
                "params": {
                    "rootPath": args.debug_rootpath
                }
            })
            if len(s.post_messages) == 0:
                print("  Succesful")
            else:
                print("  Succesful with errors:")
                for message in s.post_messages:
                    print("    {0}".format(message[1]))
            # Print module directories
            print("  Found module directories:")
            for mod_dir in s.mod_dirs:
                print("    {0}".format(mod_dir))
        #
        if args.debug_symbols:
            print('\nTesting "textDocument/documentSymbol" request:')
            if args.debug_filepath is None:
                error_exit("'debug_filepath' not specified for document symbol test")
            file_exists = os.path.isfile(args.debug_filepath)
            if file_exists is False:
                error_exit("Specified 'debug_filepath' does not exist")
            print('  File = "{0}"\n'.format(args.debug_filepath))
            s.serve_onSave({
                "params": {
                    "textDocument": {"uri": args.debug_filepath}
                }
            })
            symbol_results = s.serve_document_symbols({
                "params": {
                    "textDocument": {"uri": args.debug_filepath}
                }
            })
            for symbol in symbol_results:
                sline = symbol["location"]["range"]["start"]["line"]
                if "containerName" in symbol:
                    parent = symbol["containerName"]
                else:
                    parent = "null"
                print('  line {2:5d}  symbol -> {1:3d}:{0:30} parent = {3}'.format(symbol["name"],
                      symbol["kind"], sline, parent))
        #
        if args.debug_completion:
            print('\nTesting "textDocument/completion" request:')
            if args.debug_filepath is None:
                error_exit("'debug_filepath' not specified for completion test")
            file_exists = os.path.isfile(args.debug_filepath)
            if file_exists is False:
                error_exit("Specified 'debug_filepath' does not exist")
            print('  File = "{0}"'.format(args.debug_filepath))
            if args.debug_line is None:
                error_exit("'debug_line' not specified for completion test")
            print('  Line = {0}'.format(args.debug_line))
            if args.debug_char is None:
                error_exit("'debug_char' not specified for completion test")
            print('  Char = {0}\n'.format(args.debug_char))
            s.serve_onSave({
                "params": {
                    "textDocument": {"uri": args.debug_filepath}
                }
            })
            completion_results = s.serve_autocomplete({
                "params": {
                    "textDocument": {"uri": args.debug_filepath},
                    "position": {"line": args.debug_line-1, "character": args.debug_char-1}
                }
            })
            print('  Results:')
            for obj in completion_results['items']:
                print('    {0}: {1}'.format(obj['kind'], obj['label']))
        #
        if args.debug_definition:
            print('\nTesting "textDocument/definition" request:')
            if args.debug_filepath is None:
                error_exit("'debug_filepath' not specified for definition test")
            file_exists = os.path.isfile(args.debug_filepath)
            if file_exists is False:
                error_exit("Specified 'debug_filepath' does not exist")
            print('  File = "{0}"'.format(args.debug_filepath))
            if args.debug_line is None:
                error_exit("'debug_line' not specified for definition test")
            print('  Line = {0}'.format(args.debug_line))
            if args.debug_char is None:
                error_exit("'debug_char' not specified for definition test")
            print('  Char = {0}\n'.format(args.debug_char))
            s.serve_onSave({
                "params": {
                    "textDocument": {"uri": args.debug_filepath}
                }
            })
            completion_results = s.serve_definition({
                "params": {
                    "textDocument": {"uri": args.debug_filepath},
                    "position": {"line": args.debug_line-1, "character": args.debug_char-1}
                }
            })
            print('  Result:')
            if completion_results is None:
                print('    No result found!')
            else:
                print('    URI  = "{0}"'.format(completion_results['uri']))
                print('    Line = {0}'.format(completion_results['range']['start']['line']+1))
                print('    Char = {0}'.format(completion_results['range']['start']['character']+1))
        tmpout.close()
        tmpin.close()
    #
    else:
        stdin, stdout = _binary_stdio()
        s = LangServer(conn=JSONRPC2Connection(ReadWriter(stdin, stdout)),
                       logLevel=0, settings=settings)
        s.run()


def print_children(obj, indent=""):
    for child in obj.get_children():
        print("  {0}{1}: {2}".format(indent, child.get_type(), child.FQSN))
        print_children(child, indent+"  ")


def _binary_stdio():
    """Construct binary stdio streams (not text mode).
    This seems to be different for Window/Unix Python2/3, so going by:
        https://stackoverflow.com/questions/2850893/reading-binary-data-from-stdin
    """
    PY3K = sys.version_info >= (3, 0)

    if PY3K:
        stdin, stdout = sys.stdin.buffer, sys.stdout.buffer
    else:
        # Python 2 on Windows opens sys.stdin in text mode, and
        # binary data that read from it becomes corrupted on \r\n
        if sys.platform == "win32":
            # set sys.stdin to binary mode
            import os
            import msvcrt
            msvcrt.setmode(sys.stdin.fileno(), os.O_BINARY)
            msvcrt.setmode(sys.stdout.fileno(), os.O_BINARY)
        stdin, stdout = sys.stdin, sys.stdout

    return stdin, stdout
