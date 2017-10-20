from __future__ import print_function
import sys
import os
import argparse
from .langserver import LangServer, FIXED_EXT_REGEX
from .jsonrpc import JSONRPC2Connection, ReadWriter
from .parse_fortran import process_file
__version__ = '0.1.2'


def main():
    #
    parser = argparse.ArgumentParser()
    parser.description = "FORTRAN Language Server (beta)"
    parser.add_argument(
        '--debug_parser', action="store_true",
        help="Test source parser on specified file instead of running language server"
    )
    parser.add_argument(
        '--debug_filepath',
        help="Path to file for parser test"
    )
    args = parser.parse_args()
    #
    if args.debug_parser:
        if args.debug_filepath is None:
            print("ERROR: 'debug_filepath' not specified")
            sys.exit(-1)
        config_exists = os.path.isfile(args.debug_filepath)
        if config_exists is False:
            print("ERROR: Specified 'debug_filepath' does not exist")
            sys.exit(-1)
        filename, ext = os.path.splitext(os.path.basename(args.debug_filepath))
        fixed_flag = False
        detected_format = "free"
        if FIXED_EXT_REGEX.match(ext):
            fixed_flag = True
            detected_format = "fixed"
        #
        print('\nTesting parser')
        print('  File = "{0}"'.format(args.debug_filepath))
        print('  Detected format: {0}'.format(detected_format))
        with open(args.debug_filepath, 'r') as fhandle:
            print("\n=========\nParser Output\n=========\n")
            ast_new = process_file(fhandle.readlines(), True, fixed_flag, True)
            print("\n=========\nObject Tree\n=========\n")
            for key, obj in ast_new.global_dict.items():
                print("{0}: {1}".format(obj.get_type(), obj.FQSN))
                print_children(obj)
    #
    else:
        stdin, stdout = _binary_stdio()
        s = LangServer(conn=JSONRPC2Connection(ReadWriter(sys.stdin, sys.stdout)),
                       logLevel=0)
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
