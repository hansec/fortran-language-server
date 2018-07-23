import subprocess
import sys
import os
try:
    import StringIO.StringIO as StringIO
except:
    from io import StringIO
root_dir = os.path.join(os.path.dirname(os.path.abspath(__file__)), "..")
sys.path.insert(0, root_dir)
from fortls.jsonrpc import write_rpc_request, write_rpc_notification, read_rpc_messages
from fortls.langserver import path_to_uri

run_command = os.path.join(root_dir, "fortls.py --incrmental_sync --use_signature_help")
test_dir = os.path.join(root_dir, "test", "test_source")


def run_request(request):
    pid = subprocess.Popen(run_command, shell=True, stdin=subprocess.PIPE,
                           stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    results = pid.communicate(input=request.encode())
    tmp_file = StringIO(results[0].decode())
    results = read_rpc_messages(tmp_file)
    parsed_results = []
    for result in results:
        if "method" in result:
            continue
        parsed_results.append(result['result'])
    errcode = pid.poll()
    return errcode, parsed_results


def test_init():
    def check_return(result_dict):
        # Expected capabilities
        # {
        #     "completionProvider": {
        #         "resolveProvider": false,
        #         "triggerCharacters": ["%"]
        #     },
        #     "definitionProvider": true,
        #     "documentSymbolProvider": true,
        #     "referencesProvider": True,
        #     "hoverProvider": true,
        #     "textDocumentSync": 2
        # }
        #
        assert "capabilities" in result_dict
        assert result_dict["capabilities"]["textDocumentSync"] == 2
        assert result_dict["capabilities"]["definitionProvider"] is True
        assert result_dict["capabilities"]["documentSymbolProvider"] is True
        assert result_dict["capabilities"]["hoverProvider"] is True
        assert result_dict["capabilities"]["referencesProvider"] is True
        assert result_dict["capabilities"]["completionProvider"]["resolveProvider"] is False
        assert result_dict["capabilities"]["completionProvider"]["triggerCharacters"][0] == "%"
    #
    string = write_rpc_request(1, "initialize", {"rootPath": test_dir})
    errcode, results = run_request(string)
    #
    assert errcode == 0
    check_return(results[0])


def test_open():
    string = write_rpc_request(1, "initialize", {"rootPath": test_dir})
    file_path = os.path.join(test_dir, "subdir", "test_free.f90")
    string += write_rpc_notification("textDocument/didOpen", {
        "textDocument": {"uri": file_path}
    })
    errcode, results = run_request(string)
    #
    assert errcode == 0
    assert len(results) == 1


def test_change():
    string = write_rpc_request(1, "initialize", {"rootPath": test_dir})
    file_path = os.path.join(test_dir, "subdir", "test_free.f90")
    string += write_rpc_notification("textDocument/didChange", {
        "textDocument": {"uri": file_path},
        "contentChanges": [{
            "text": "",
            "range": {
                "start": {"line": 6, "character": 0},
                "end": {"line": 31, "character": 0}
            }
        },
        {
            "text": "",
            "range": {
                "start": {"line": 7, "character": 0},
                "end": {"line": 39, "character": 0}
            }
        }]
    })
    string += write_rpc_request(2, "textDocument/documentSymbol", {
        "textDocument": {"uri": file_path}
    })
    errcode, results = run_request(string)
    #
    assert errcode == 0
    assert len(results) == 2
    assert len(results[1]) == 5


def test_symbols():
    def check_return(result_array):
        # Expected objects
        objs = (
            ["test_free", 2, 0, 77],
            ["scale_type", 5, 4, 6],
            ["val", 13, 5, 5],
            ["vector", 5, 8, 16],
            ["n", 13, 9, 9],
            ["v", 13, 10, 10],
            ["bound_nopass", 6, 11, 11],
            ["create", 6, 13, 13],
            ["norm", 12, 14, 14],
            ["bound_pass", 6, 15, 15],
            ["scaled_vector", 5, 18, 23],
            ["scale", 13, 19, 19],
            ["set_scale", 6, 21, 21],
            ["norm", 12, 22, 22],
            ["fort_wrap", 11, 26, 29],
            ["vector_create", 6, 35, 41],
            ["vector_norm", 12, 43, 47],
            ["scaled_vector_set", 6, 49, 53],
            ["scaled_vector_norm", 12, 55, 59],
            ["unscaled_norm", 12, 61, 65],
            ["test_sig_Sub", 6, 67, 70],
            ["bound_pass", 6, 72, 76]
        )
        assert len(result_array) == len(objs)
        for i, obj in enumerate(objs):
            assert result_array[i]["name"] == obj[0]
            assert result_array[i]["kind"] == obj[1]
            assert result_array[i]["location"]["range"]["start"]["line"] == obj[2]
            assert result_array[i]["location"]["range"]["end"]["line"] == obj[3]
    #
    string = write_rpc_request(1, "initialize", {"rootPath": test_dir})
    file_path = os.path.join(test_dir, "subdir", "test_free.f90")
    string += write_rpc_request(2, "textDocument/documentSymbol", {
        "textDocument": {"uri": file_path}
    })
    errcode, results = run_request(string)
    #
    assert errcode == 0
    check_return(results[1])


def test_comp():
    def check_return(result_array, checks):
        assert len(result_array["items"]) == checks[0]
        if checks[0] > 0:
            assert result_array["items"][0]["label"] == checks[1]

    def comp_request(file_path, line, char):
        return write_rpc_request(1, "textDocument/completion", {
            "textDocument": {"uri": file_path},
            "position": {"line": line, "character": char}
        })
    #
    string = write_rpc_request(1, "initialize", {"rootPath": test_dir})
    file_path = os.path.join(test_dir, "test_prog.f08")
    string += comp_request(file_path, 12, 6)
    string += comp_request(file_path, 13, 6)
    string += comp_request(file_path, 17, 24)
    string += comp_request(file_path, 18, 23)
    string += comp_request(file_path, 20, 7)
    string += comp_request(file_path, 21, 20)
    string += comp_request(file_path, 21, 42)
    string += comp_request(file_path, 23, 26)
    file_path = os.path.join(test_dir, "subdir", "test_submod.F90")
    string += comp_request(file_path, 16, 12)
    string += comp_request(file_path, 17, 8)
    string += comp_request(file_path, 17, 23)
    file_path = os.path.join(test_dir, "test_inc.f90")
    string += comp_request(file_path, 10, 2)
    file_path = os.path.join(test_dir, "subdir", "test_inc2.f90")
    string += comp_request(file_path, 3, 2)
    file_path = os.path.join(test_dir, "subdir", "test_abstract.f90")
    string += comp_request(file_path, 7, 12)
    file_path = os.path.join(test_dir, "subdir", "test_free.f90")
    string += comp_request(file_path, 10, 22)
    string += comp_request(file_path, 28, 14)
    file_path = os.path.join(test_dir, "test_block.f08")
    string += comp_request(file_path, 2, 2)
    string += comp_request(file_path, 5, 4)
    string += comp_request(file_path, 8, 6)
    errcode, results = run_request(string)
    assert errcode == 0
    #
    exp_results = (
        [1, "myfun(n, xval)"],
        [4, "glob_sub(n, xval, yval)"],
        [1, "bound_nopass(a, b)"],
        [1, "bound_pass(arg1)"],
        [1, "stretch_vector"],
        [6, "scale"],
        [2, "n"],
        [1, "val"],
        [1, "point"],
        [1, "distance"],
        [2, "x"],
        [2, "val1"],
        [2, "val1"],
        [1, "abs_interface"],
        [1, "DIMENSION(:)"],
        [3, "INTENT(IN)"],
        [3, "res0"],
        [4, "res0"],
        [5, "res0"]
    )
    assert len(exp_results)+1 == len(results)
    for i in range(len(exp_results)):
        check_return(results[i+1], exp_results[i])


def test_sig():
    def check_return(results, checks):
        assert results.get('activeParameter', -1) == checks[0]
        signatures = results.get('signatures')
        assert signatures[0].get('label') == checks[2]
        assert len(signatures[0].get('parameters')) == checks[1]

    def sig_request(file_path, line, char):
        return write_rpc_request(1, "textDocument/signatureHelp", {
            "textDocument": {"uri": file_path},
            "position": {"line": line, "character": char}
        })
    #
    string = write_rpc_request(1, "initialize", {"rootPath": test_dir})
    file_path = os.path.join(test_dir, "test_prog.f08")
    string += sig_request(file_path, 25, 18)
    string += sig_request(file_path, 25, 20)
    string += sig_request(file_path, 25, 22)
    string += sig_request(file_path, 25, 27)
    string += sig_request(file_path, 25, 29)
    errcode, results = run_request(string)
    assert errcode == 0
    #
    sub_sig = "test_sig_Sub(arg1, arg2, opt1=opt1, opt2=opt2, opt3=opt3)"
    exp_results = (
        [0, 5, sub_sig],
        [1, 5, sub_sig],
        [2, 5, sub_sig],
        [3, 5, sub_sig],
        [4, 5, sub_sig]
    )
    assert len(exp_results)+1 == len(results)
    for i in range(len(exp_results)):
        check_return(results[i+1], exp_results[i])


def test_def():
    def check_return(result_array, checks):
        assert result_array["uri"] == path_to_uri(checks[2])
        assert result_array["range"]["start"]["line"] == checks[0]
        assert result_array["range"]["start"]["line"] == checks[1]

    def def_request(file_path, line, char):
        return write_rpc_request(1, "textDocument/definition", {
            "textDocument": {"uri": file_path},
            "position": {"line": line, "character": char}
        })
    #
    string = write_rpc_request(1, "initialize", {"rootPath": test_dir})
    file_path = os.path.join(test_dir, "test_prog.f08")
    string += def_request(file_path, 12, 6)
    string += def_request(file_path, 13, 6)
    string += def_request(file_path, 20, 7)
    string += def_request(file_path, 21, 20)
    string += def_request(file_path, 21, 42)
    string += def_request(file_path, 23, 26)
    file_path = os.path.join(test_dir, "subdir", "test_submod.F90")
    string += def_request(file_path, 16, 12)
    file_path = os.path.join(test_dir, "test_inc.f90")
    string += def_request(file_path, 10, 2)
    file_path = os.path.join(test_dir, "subdir", "test_inc2.f90")
    string += def_request(file_path, 3, 2)
    errcode, results = run_request(string)
    assert errcode == 0
    #
    fixed_path = os.path.join(test_dir, "subdir", "test_fixed.f")
    free_path = os.path.join(test_dir, "subdir", "test_free.f90")
    exp_results = (
        [0, 0, fixed_path],
        [20, 20, fixed_path],
        [10, 10, os.path.join(test_dir, "test_prog.f08")],
        [21, 21, free_path],
        [14, 14, free_path],
        [5, 5, free_path],
        [1, 1, os.path.join(test_dir, "subdir", "test_submod.F90")],
        [0, 0, os.path.join(test_dir, "subdir", "test_inc2.f90")],
        [4, 4, os.path.join(test_dir, "test_inc.f90")]
    )
    assert len(exp_results)+1 == len(results)
    for i in range(len(exp_results)):
        check_return(results[i+1], exp_results[i])


def test_refs():
    def check_return(result_array, checks):
        assert len(result_array) == len(checks)
        for (i, check) in enumerate(checks):
            assert result_array[i]["uri"] == path_to_uri(check[2])
            assert result_array[i]["range"]["start"]["character"] == check[0]
            assert result_array[i]["range"]["end"]["character"] == check[1]
    #
    string = write_rpc_request(1, "initialize", {"rootPath": test_dir})
    file_path = os.path.join(test_dir, "test_prog.f08")
    string += write_rpc_request(2, "textDocument/references", {
        "textDocument": {"uri": file_path},
        "position": {"line": 9, "character": 8}
    })
    errcode, results = run_request(string)
    assert errcode == 0
    #
    free_path = os.path.join(test_dir, "subdir", "test_free.f90")
    check_return(results[1], (
        [8, 14, free_path],
        [9, 15, free_path],
        [14, 20, free_path],
        [6, 12, free_path],
        [6, 12, free_path],
        [6, 12, free_path],
        [6, 12, free_path],
        [21, 27, os.path.join(test_dir, "test_prog.f08")],
        [5, 11, os.path.join(test_dir, "test_prog.f08")]
    ))
