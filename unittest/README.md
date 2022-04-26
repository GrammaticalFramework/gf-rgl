# Python script for unit testing RGL grammars

## Usage

```
python path/to/unittest.py [-h] [-v] [--no-pmcfg] path/to/testfile.gftest (...)
```

The script must be located in a sibling directory
to the RGL `src` directory to work properly.

**Note:** On Windows use WSL (Windows Subsystem for Linux) to run `unittest.py` script, also replace the commented lines for Windows inside the script.

## Test format

The test file should look something like this:

```
LangSwe: jag sover i huset
LangEng: I sleep in the house

LangSwe: huset verkar stort
Lang: PhrUtt NoPConj (UttS (UseCl (TTAnt TPres ASimul) PPos (PredVP ...
```

This contains two tests: Every test should be separated by empty lines.
Every line starts with a language, followed by ":" and the sentence
(or the abstract syntax tree if the abstract grammar is specified).

You can also see an example in the file [`unittest-example.gftest`](unittest-example.gftest).

## No PMCFG

If your grammar is complex and takes long time to compile, you can try 
the option `--no-pmcfg`, which tells GF to not build the parsing grammar.

Note however that in this case, every test case needs to contain a parse tree.
