# Python script for unit testing RGL grammars

## Usage

```
python path-to-script.py path/to/testfile.gftest (...)
```

The script must be located in a sibling directory
to the RGL `src` directory to work properly.

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
