#!/usr/bin/env python
if __name__ == "__main__":
    import sys
    import os
    root_dir = os.path.join(os.path.dirname(os.path.abspath(__file__)), "..")
    sys.path.insert(0, root_dir)
    import fortls
    fortls.main()
