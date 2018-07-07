import subprocess
try:
    import StringIO.StringIO as StringIO
except:
    from io import StringIO
import sys
import os
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
                "end": {"line": 29, "character": 0}
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
    assert len(results[1]) == 4


def test_symbols():
    def check_return(result_array):
        # Expected objects
        objs = [
            ["test_free", 2, 0, 69],
            ["scale_type", 5, 4, 6],
            ["val", 13, 5, 5],
            ["vector", 5, 8, 14],
            ["n", 13, 9, 9],
            ["v", 13, 10, 10],
            ["create", 6, 12, 12],
            ["norm", 12, 13, 13],
            ["scaled_vector", 5, 16, 21],
            ["scale", 13, 17, 17],
            ["set_scale", 6, 19, 19],
            ["norm", 12, 20, 20],
            ["fort_wrap", 11, 24, 27],
            ["vector_create", 6, 33, 39],
            ["vector_norm", 12, 41, 45],
            ["scaled_vector_set", 6, 47, 51],
            ["scaled_vector_norm", 12, 53, 57],
            ["unscaled_norm", 12, 59, 63],
            ["test_sig_sub", 6, 65, 68]
        ]
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
    #
    string = write_rpc_request(1, "initialize", {"rootPath": test_dir})
    file_path = os.path.join(test_dir, "test_prog.f08")
    string += write_rpc_request(2, "textDocument/completion", {
        "textDocument": {"uri": file_path},
        "position": {"line": 12, "character": 6}
    })
    string += write_rpc_request(3, "textDocument/completion", {
        "textDocument": {"uri": file_path},
        "position": {"line": 13, "character": 6}
    })
    string += write_rpc_request(4, "textDocument/completion", {
        "textDocument": {"uri": file_path},
        "position": {"line": 18, "character": 7}
    })
    string += write_rpc_request(5, "textDocument/completion", {
        "textDocument": {"uri": file_path},
        "position": {"line": 19, "character": 20}
    })
    string += write_rpc_request(6, "textDocument/completion", {
        "textDocument": {"uri": file_path},
        "position": {"line": 19, "character": 42}
    })
    string += write_rpc_request(7, "textDocument/completion", {
        "textDocument": {"uri": file_path},
        "position": {"line": 21, "character": 26}
    })
    file_path = os.path.join(test_dir, "subdir", "test_submod.F90")
    string += write_rpc_request(8, "textDocument/completion", {
        "textDocument": {"uri": file_path},
        "position": {"line": 16, "character": 12}
    })
    string += write_rpc_request(9, "textDocument/completion", {
        "textDocument": {"uri": file_path},
        "position": {"line": 17, "character": 8}
    })
    string += write_rpc_request(10, "textDocument/completion", {
        "textDocument": {"uri": file_path},
        "position": {"line": 17, "character": 23}
    })
    file_path = os.path.join(test_dir, "test_inc.f90")
    string += write_rpc_request(11, "textDocument/completion", {
        "textDocument": {"uri": file_path},
        "position": {"line": 10, "character": 2}
    })
    file_path = os.path.join(test_dir, "subdir", "test_inc2.f90")
    string += write_rpc_request(12, "textDocument/completion", {
        "textDocument": {"uri": file_path},
        "position": {"line": 3, "character": 2}
    })
    file_path = os.path.join(test_dir, "subdir", "test_abstract.f90")
    string += write_rpc_request(13, "textDocument/completion", {
        "textDocument": {"uri": file_path},
        "position": {"line": 7, "character": 12}
    })
    file_path = os.path.join(test_dir, "subdir", "test_free.f90")
    string += write_rpc_request(14, "textDocument/completion", {
        "textDocument": {"uri": file_path},
        "position": {"line": 10, "character": 22}
    })
    string += write_rpc_request(15, "textDocument/completion", {
        "textDocument": {"uri": file_path},
        "position": {"line": 26, "character": 14}
    })
    file_path = os.path.join(test_dir, "test_block.f08")
    string += write_rpc_request(16, "textDocument/completion", {
        "textDocument": {"uri": file_path},
        "position": {"line": 2, "character": 2}
    })
    string += write_rpc_request(17, "textDocument/completion", {
        "textDocument": {"uri": file_path},
        "position": {"line": 5, "character": 4}
    })
    string += write_rpc_request(18, "textDocument/completion", {
        "textDocument": {"uri": file_path},
        "position": {"line": 8, "character": 6}
    })
    errcode, results = run_request(string)
    #
    assert errcode == 0
    check_return(results[1], [1, "myfun(n, xval)"])
    check_return(results[2], [4, "glob_sub(n, xval, yval)"])
    check_return(results[3], [1, "stretch_vector"])
    check_return(results[4], [4, "scale"])
    check_return(results[5], [2, "n"])
    check_return(results[6], [1, "val"])
    check_return(results[7], [1, "point"])
    check_return(results[8], [1, "distance"])
    check_return(results[9], [2, "x"])
    check_return(results[10], [2, "val1"])
    check_return(results[11], [2, "val1"])
    check_return(results[12], [1, "abs_interface"])
    check_return(results[13], [1, "DIMENSION(:)"])
    check_return(results[14], [3, "INTENT(IN)"])
    check_return(results[15], [3, "res0"])
    check_return(results[16], [4, "res0"])
    check_return(results[17], [5, "res0"])

def test_sig():
    def check_return(results, checks):
        assert results.get('activeParameter', -1) == checks[0]
        signatures = results.get('signatures')
        assert signatures[0].get('label') == checks[2]
        assert len(signatures[0].get('parameters')) == checks[1]
    #
    string = write_rpc_request(1, "initialize", {"rootPath": test_dir})
    file_path = os.path.join(test_dir, "test_prog.f08")
    string += write_rpc_request(2, "textDocument/signatureHelp", {
        "textDocument": {"uri": file_path},
        "position": {"line": 23, "character": 18}
    })
    string += write_rpc_request(2, "textDocument/signatureHelp", {
        "textDocument": {"uri": file_path},
        "position": {"line": 23, "character": 20}
    })
    string += write_rpc_request(2, "textDocument/signatureHelp", {
        "textDocument": {"uri": file_path},
        "position": {"line": 23, "character": 22}
    })
    string += write_rpc_request(2, "textDocument/signatureHelp", {
        "textDocument": {"uri": file_path},
        "position": {"line": 23, "character": 27}
    })
    string += write_rpc_request(2, "textDocument/signatureHelp", {
        "textDocument": {"uri": file_path},
        "position": {"line": 23, "character": 29}
    })
    errcode, results = run_request(string)
    #
    assert errcode == 0
    sub_sig = "test_sig_sub(arg1, arg2, opt1=opt1, opt2=opt2, opt3=opt3)"
    check_return(results[1], [0, 5, sub_sig])
    check_return(results[2], [1, 5, sub_sig])
    check_return(results[3], [2, 5, sub_sig])
    check_return(results[4], [3, 5, sub_sig])
    check_return(results[5], [4, 5, sub_sig])


def test_def():
    def check_return(result_array, checks):
        assert result_array["uri"] == checks[2]
        assert result_array["range"]["start"]["line"] == checks[0]
        assert result_array["range"]["start"]["line"] == checks[1]
    #
    string = write_rpc_request(1, "initialize", {"rootPath": test_dir})
    file_path = os.path.join(test_dir, "test_prog.f08")
    string += write_rpc_request(2, "textDocument/definition", {
        "textDocument": {"uri": file_path},
        "position": {"line": 12, "character": 6}
    })
    string += write_rpc_request(3, "textDocument/definition", {
        "textDocument": {"uri": file_path},
        "position": {"line": 13, "character": 6}
    })
    string += write_rpc_request(4, "textDocument/definition", {
        "textDocument": {"uri": file_path},
        "position": {"line": 18, "character": 7}
    })
    string += write_rpc_request(5, "textDocument/definition", {
        "textDocument": {"uri": file_path},
        "position": {"line": 19, "character": 20}
    })
    string += write_rpc_request(6, "textDocument/definition", {
        "textDocument": {"uri": file_path},
        "position": {"line": 19, "character": 42}
    })
    string += write_rpc_request(7, "textDocument/definition", {
        "textDocument": {"uri": file_path},
        "position": {"line": 21, "character": 26}
    })
    file_path = os.path.join(test_dir, "subdir", "test_submod.F90")
    string += write_rpc_request(8, "textDocument/definition", {
        "textDocument": {"uri": file_path},
        "position": {"line": 16, "character": 12}
    })
    file_path = os.path.join(test_dir, "test_inc.f90")
    string += write_rpc_request(9, "textDocument/definition", {
        "textDocument": {"uri": file_path},
        "position": {"line": 10, "character": 2}
    })
    file_path = os.path.join(test_dir, "subdir", "test_inc2.f90")
    string += write_rpc_request(10, "textDocument/definition", {
        "textDocument": {"uri": file_path},
        "position": {"line": 3, "character": 2}
    })
    errcode, results = run_request(string)
    #
    assert errcode == 0
    check_return(results[1], [0, 0, path_to_uri(os.path.join(test_dir, "subdir", "test_fixed.f"))])
    check_return(results[2], [20, 20, path_to_uri(os.path.join(test_dir, "subdir", "test_fixed.f"))])
    check_return(results[3], [10, 10, path_to_uri(os.path.join(test_dir, "test_prog.f08"))])
    check_return(results[4], [19, 19, path_to_uri(os.path.join(test_dir, "subdir", "test_free.f90"))])
    check_return(results[5], [13, 13, path_to_uri(os.path.join(test_dir, "subdir", "test_free.f90"))])
    check_return(results[6], [5, 5, path_to_uri(os.path.join(test_dir, "subdir", "test_free.f90"))])
    check_return(results[7], [1, 1, path_to_uri(os.path.join(test_dir, "subdir", "test_submod.F90"))])
    check_return(results[8], [0, 0, path_to_uri(os.path.join(test_dir, "subdir", "test_inc2.f90"))])
    check_return(results[9], [4, 4, path_to_uri(os.path.join(test_dir, "test_inc.f90"))])


def test_refs():
    def check_return(result_array, checks):
        assert len(result_array) == len(checks)
        for (i,check) in enumerate(checks):
            assert result_array[i]["uri"] == check[2]
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
    #
    assert errcode == 0
    check_return(results[1], [
        [8,  14, path_to_uri(os.path.join(test_dir, "subdir", "test_free.f90"))],
        [9,  15, path_to_uri(os.path.join(test_dir, "subdir", "test_free.f90"))],
        [14, 20, path_to_uri(os.path.join(test_dir, "subdir", "test_free.f90"))],
        [6,  12, path_to_uri(os.path.join(test_dir, "subdir", "test_free.f90"))],
        [6,  12, path_to_uri(os.path.join(test_dir, "subdir", "test_free.f90"))],
        [6,  12, path_to_uri(os.path.join(test_dir, "subdir", "test_free.f90"))],
        [21, 27, path_to_uri(os.path.join(test_dir, "test_prog.f08"))],
        [5,  11, path_to_uri(os.path.join(test_dir, "test_prog.f08"))]
    ])
