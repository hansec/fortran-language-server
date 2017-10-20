__version__ = 0.1


def main():
    import sys
    from .langserver import LangServer
    from .jsonrpc import JSONRPC2Connection, ReadWriter
    #
    s = LangServer(conn=JSONRPC2Connection(ReadWriter(sys.stdin, sys.stdout)),
                   logLevel=0)
    s.run()
