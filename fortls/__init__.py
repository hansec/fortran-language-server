from __future__ import print_function
import sys
import os
import argparse
from multiprocessing import freeze_support
from .langserver import LangServer
from .jsonrpc import JSONRPC2Connection, ReadWriter, path_from_uri
from .parse_fortran import process_file, detect_fixed_format
__version__ = '1.2.1'


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
        '--incremental_sync', '--incrmental_sync', action="store_true",
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
    parser.add_argument(
        '--use_signature_help', action="store_true",
        help="Use signature help instead of subroutine/function snippets"
    )
    parser.add_argument(
        '--variable_hover', action="store_true",
        help="Show hover information for variables (default: subroutines/functions only)"
    )
    parser.add_argument(
        '--debug_log', action="store_true",
        help="Generate debug log in project root folder"
    )
    group = parser.add_argument_group("DEBUG", "Options for debugging language server")
    group.add_argument(
        '--debug_parser', action="store_true",
        help="Test source code parser on specified file"
    )
    group.add_argument(
        '--debug_diagnostics', action="store_true",
        help="Test diagnostic notifications for specified file"
    )
    group.add_argument(
        '--debug_symbols', action="store_true",
        help="Test symbol request for specified file"
    )
    group.add_argument(
        '--debug_workspace_symbols', type=str,
        help="Test workspace/symbol request"
    )
    group.add_argument(
        '--debug_completion', action="store_true",
        help="Test completion request for specified file and position"
    )
    group.add_argument(
        '--debug_signature', action="store_true",
        help="Test signatureHelp request for specified file and position"
    )
    group.add_argument(
        '--debug_definition', action="store_true",
        help="Test definition request for specified file and position"
    )
    group.add_argument(
        '--debug_hover', action="store_true",
        help="Test hover request for specified file and position"
    )
    group.add_argument(
        '--debug_references', action="store_true",
        help="Test references request for specified file and position"
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
    debug_server = (args.debug_diagnostics or args.debug_symbols
                    or args.debug_completion or args.debug_signature
                    or args.debug_definition or args.debug_hover
                    or args.debug_references
                    or (args.debug_rootpath is not None)
                    or (args.debug_workspace_symbols is not None))
    #
    settings = {
        "symbol_include_mem": (not args.symbol_skip_mem),
        "sync_type": 2 if args.incremental_sync else 1,
        "autocomplete_no_prefix": args.autocomplete_no_prefix,
        "lowercase_intrinsics": args.lowercase_intrinsics,
        "use_signature_help": args.use_signature_help,
        "variable_hover": args.variable_hover
    }
    #
    if args.debug_parser:
        if args.debug_filepath is None:
            error_exit("'debug_filepath' not specified for parsing test")
        file_exists = os.path.isfile(args.debug_filepath)
        if file_exists is False:
            error_exit("Specified 'debug_filepath' does not exist")
        # Get preprocessor definitions from config file
        pp_defs = {}
        if args.debug_rootpath:
            config_path = os.path.join(args.debug_rootpath, ".fortls")
            config_exists = os.path.isfile(config_path)
            if config_exists:
                try:
                    import json
                    with open(config_path, 'r') as fhandle:
                        config_dict = json.load(fhandle)
                        pp_defs = config_dict.get("pp_defs", {})
                        if isinstance(pp_defs, list):
                            pp_defs = {key: "" for key in pp_defs}
                except:
                    print("Error while parsing '.fortls' settings file")
        #
        print('\nTesting parser')
        print('  File = "{0}"'.format(args.debug_filepath))
        with open(args.debug_filepath, 'r') as fhandle:
            contents_split = fhandle.readlines()
            fixed_flag = detect_fixed_format(contents_split)
            print('  Detected format: {0}'.format("fixed" if fixed_flag else "free"))
            print("\n=========\nParser Output\n=========\n")
            _, file_ext = os.path.splitext(os.path.basename(args.debug_filepath))
            if file_ext == file_ext.upper():
                ast_new = process_file(contents_split, True, fixed_format=fixed_flag, debug=True, pp_defs=pp_defs)
            else:
                ast_new = process_file(contents_split, True, fixed_format=fixed_flag, debug=True)
            print("\n=========\nObject Tree\n=========\n")
            for obj in ast_new.get_scopes():
                print("{0}: {1}".format(obj.get_type(), obj.FQSN))
                print_children(obj)
            print("\n=========\nExportable Objects\n=========\n")
            for _, obj in ast_new.global_dict.items():
                print("{0}: {1}".format(obj.get_type(), obj.FQSN))
    #
    elif debug_server:
        prb, pwb = os.pipe()
        tmpin = os.fdopen(prb, 'rb')
        tmpout = os.fdopen(pwb, 'wb')
        s = LangServer(conn=JSONRPC2Connection(ReadWriter(tmpin, tmpout)),
                       debug_log=args.debug_log, settings=settings)
        #
        if args.debug_rootpath:
            dir_exists = os.path.isdir(args.debug_rootpath)
            if dir_exists is False:
                error_exit("Specified 'debug_rootpath' does not exist or is not a directory")
            print('\nTesting "initialize" request:')
            print('  Root = "{0}"\n'.format(args.debug_rootpath))
            s.serve_initialize({
                "params": {"rootPath": args.debug_rootpath}
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
        if args.debug_diagnostics:
            print('\nTesting "textDocument/publishDiagnostics" notification:')
            check_request_params(args, loc_needed=False)
            s.serve_onSave({
                "params": {
                    "textDocument": {"uri": args.debug_filepath}
                }
            })
            diag_results, _ = s.get_diagnostics(args.debug_filepath)
            if diag_results is not None:
                sev_map = ["ERROR", "WARNING", "INFO"]
                if len(diag_results) == 0:
                    print("\nNo errors or warnings")
                else:
                    print("\nReported errors or warnings:")
                for diag in diag_results:
                    sline = diag["range"]["start"]["line"]
                    message = diag["message"]
                    sev = sev_map[diag["severity"]-1]
                    print('  {0:5d}:{1}  "{2}"'.format(sline,
                          sev, message))
        #
        if args.debug_symbols:
            print('\nTesting "textDocument/documentSymbol" request:')
            check_request_params(args, loc_needed=False)
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
        if args.debug_workspace_symbols is not None:
            print('\nTesting "workspace/symbol" request:')
            if args.debug_rootpath is None:
                error_exit("'debug_rootpath' not specified for debug request")
            symbol_results = s.serve_workspace_symbol({
                "params": {
                    "query": args.debug_workspace_symbols
                }
            })
            for symbol in symbol_results:
                path = path_from_uri(symbol["location"]["uri"])
                sline = symbol["location"]["range"]["start"]["line"]
                if "containerName" in symbol:
                    parent = symbol["containerName"]
                else:
                    parent = "null"
                print('  {2}::{3:d}  symbol -> {1:3d}:{0:30} parent = {4}'.format(symbol["name"],
                      symbol["kind"], os.path.relpath(path, args.debug_rootpath), sline, parent))
        #
        if args.debug_completion:
            print('\nTesting "textDocument/completion" request:')
            check_request_params(args)
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
                print('    {0}: {1} -> {2}'.format(obj['kind'], obj['label'], obj['detail']))
        #
        if args.debug_signature:
            print('\nTesting "textDocument/signatureHelp" request:')
            check_request_params(args)
            s.serve_onSave({
                "params": {
                    "textDocument": {"uri": args.debug_filepath}
                }
            })
            signature_results = s.serve_signature({
                "params": {
                    "textDocument": {"uri": args.debug_filepath},
                    "position": {"line": args.debug_line-1, "character": args.debug_char-1}
                }
            })
            if len(signature_results['signatures']) == 0:
                print('  No Results')
            else:
                print('  Results:')
                active_param = signature_results.get('activeParameter', 0)
                print('    Active param = {0}'.format(active_param))
                active_signature = signature_results.get('activeSignature', 0)
                print('    Active sig   = {0}'.format(active_signature))
                for i, signature in enumerate(signature_results['signatures']):
                    print('    {0}'.format(signature['label']))
                    for j, obj in enumerate(signature['parameters']):
                        if (i == active_signature) and (j == active_param):
                            active_mark = '*'
                        else:
                            active_mark = ' '
                        arg_desc = obj.get('documentation')
                        if arg_desc is not None:
                            print('{2}     {0} :: {1}'.format(arg_desc, obj['label'], active_mark))
                        else:
                            print('{1}     {0}'.format(obj['label'], active_mark))
        #
        if args.debug_definition:
            print('\nTesting "textDocument/definition" request:')
            check_request_params(args)
            s.serve_onSave({
                "params": {
                    "textDocument": {"uri": args.debug_filepath}
                }
            })
            definition_results = s.serve_definition({
                "params": {
                    "textDocument": {"uri": args.debug_filepath},
                    "position": {"line": args.debug_line-1, "character": args.debug_char-1}
                }
            })
            print('  Result:')
            if definition_results is None:
                print('    No result found!')
            else:
                print('    URI  = "{0}"'.format(definition_results['uri']))
                print('    Line = {0}'.format(definition_results['range']['start']['line']+1))
                print('    Char = {0}'.format(definition_results['range']['start']['character']+1))
        #
        if args.debug_hover:
            print('\nTesting "textDocument/hover" request:')
            check_request_params(args)
            s.serve_onSave({
                "params": {
                    "textDocument": {"uri": args.debug_filepath}
                }
            })
            hover_results = s.serve_hover({
                "params": {
                    "textDocument": {"uri": args.debug_filepath},
                    "position": {"line": args.debug_line-1, "character": args.debug_char-1}
                }
            })
            print('  Result:')
            if hover_results is None:
                print('    No result found!')
            else:
                contents = hover_results['contents']
                print('=======')
                if isinstance(contents, dict):
                    print(contents['value'])
                else:
                    print(contents)
                print('=======')
        #
        if args.debug_references:
            print('\nTesting "textDocument/references" request:')
            check_request_params(args)
            s.serve_onSave({
                "params": {
                    "textDocument": {"uri": args.debug_filepath}
                }
            })
            ref_results = s.serve_references({
                "params": {
                    "textDocument": {"uri": args.debug_filepath},
                    "position": {"line": args.debug_line-1, "character": args.debug_char-1}
                }
            })
            print('  Result:')
            if ref_results is None:
                print('    No result found!')
            else:
                print('=======')
                for result in ref_results:
                    print('  {0}  ({1}, {2})'.format(result['uri'],
                          result['range']['start']['line']+1, result['range']['start']['character']+1))
                print('=======')
        tmpout.close()
        tmpin.close()
    #
    else:
        stdin, stdout = _binary_stdio()
        s = LangServer(conn=JSONRPC2Connection(ReadWriter(stdin, stdout)),
                       debug_log=args.debug_log, settings=settings)
        s.run()


def check_request_params(args, loc_needed=True):
    if args.debug_filepath is None:
        error_exit("'debug_filepath' not specified for debug request")
    file_exists = os.path.isfile(args.debug_filepath)
    if file_exists is False:
        error_exit("Specified 'debug_filepath' does not exist")
    print('  File = "{0}"'.format(args.debug_filepath))
    if loc_needed:
        if args.debug_line is None:
            error_exit("'debug_line' not specified for debug request")
        print('  Line = {0}'.format(args.debug_line))
        if args.debug_char is None:
            error_exit("'debug_char' not specified for debug request")
        print('  Char = {0}\n'.format(args.debug_char))


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
