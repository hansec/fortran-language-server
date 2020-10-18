from distlib.scripts import ScriptMaker
maker = ScriptMaker('.', '.')
# maker.executable = 'C:\\miniconda\\miniconda3\\python.exe'
with open('fortls.py') as f:
    maker.script_template = f.read()
maker.make('fortls = fortls')
