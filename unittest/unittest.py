"""
Python 2+3 script for unit testing RGL grammars.

Usage: python path-to-script.py path/to/testfile.gftest (...)
The script must be located in a sibling directory
to the RGL 'src' directory to work properly.

The test file should look something like this:

    LangSwe: jag sover i huset
    LangEng: I sleep in the house

    LangSwe: huset verkar stort
    Lang: PhrUtt NoPConj (UttS (UseCl (TTAnt TPres ASimul) PPos (PredVP ...

This contains two tests: Every test should be separated by empty lines.
Every line starts with a language, followed by ":" and the sentence
(or the abstract syntax tree if the abstract grammar is specified).
"""

from __future__ import print_function

import sys
import io
import os.path
from subprocess import Popen, PIPE
from glob import glob

GRAMMARDIR = '../src'
ENCODING = 'utf-8'


def usage():
    print("Usage: python %s path/to/testfile.gftest (...)" % (sys.argv[0],))
    print()
    print("(Note: to work properly this script must be located in")
    print("the RGL 'unittest' directory, and gf must be in the system path)")
    print()


def error(linenr, *args):
    print("[Error at line %s]" % (linenr,), *args)


def gferror(reply):
    return (reply.startswith('The parser failed')
            or reply.startswith('The sentence is not complete')
            or reply.startswith('Function') and reply.endswith('is not in scope'))


def importfile(linenr, lang):
    scriptdir = os.path.dirname(sys.argv[0]) or '.'
    langfiles = glob('%s/%s/*/%s.gf' % (scriptdir, GRAMMARDIR, lang))
    if not langfiles:
        error(linenr, "Cannot find language:", lang)
        exit(1)
    elif len(langfiles) > 1:
        error(linenr, "Found multiple language files for %s:" % (lang,), *langfiles)
        exit(1)
    return langfiles[0]


def stripstrings(strings):
    return [s for s0 in strings for s in [s0.strip()] if s]


def runtest(testlines):
    # first we build the input to the GF process:
    gfinput = ''
    testing = False
    for linenr, line in enumerate(testlines, 1):
        if line.startswith('#') or line.startswith('--'):
            # a comment line: do nothing
            pass
        elif ':' in line:
            if not testing:
                gfinput += 'ps "### %d" \n' % (linenr,)
                testing = True
            lang, sent = stripstrings(line.split(':', 1))
            gfinput += 'ps "+++ %d %s" \n' % (linenr, lang)
            langfile = importfile(linenr, lang)
            gfinput += 'i %s \n' % (langfile,)
            if '/abstract/' in langfile:
                gfinput += 'pt %s \n' % (sent,)
            else:
                gfinput += 'p -lang=%s "%s" \n' % (lang, sent)
        elif not line.strip():
            # an empty line: start a new test
            testing = False
        else:
            error(linenr, "Ill-formatted line in test file:", line)
            exit(1)

    # then we call GF with the script, catching stdout:
    gf = Popen('gf -run'.split(), stdin=PIPE, stdout=PIPE)
    stdout, _stderr = gf.communicate(gfinput.encode(ENCODING))
    stdout = stdout.decode(ENCODING)

    # then we analyse the result from the GF process:
    totalerrors = 0
    alltests = stripstrings(stdout.split('###'))
    for testnr, test in enumerate(alltests, 1):
        sents = stripstrings(test.split('+++'))
        startline = int(sents.pop(0))
        print("Test %d (line %d..): %d examples" % (testnr, startline, len(sents)))
        testerrors = 0
        oldresults = []
        for sresults in sents:
            alltrees = stripstrings(sresults.splitlines())
            linenr, lang = alltrees.pop(0).split()
            if len(alltrees) == 0 or len(alltrees) == 1 and gferror(alltrees[0]):
                theerror = alltrees[0] if alltrees else "No parse trees found"
                error(linenr, theerror)
                testerrors += 1
            else:
                allerrors = [(sum(tree not in oldtrees for _, _, oldtrees in oldresults), tree)
                             for tree in alltrees]
                besterrors, besttree = min(allerrors)
                if besterrors > 0:
                    for oldlinenr, oldlang, oldtrees in oldresults:
                        if besttree not in oldtrees:
                            error(linenr, "Line %s (%s) is not a translation of line %s (%s)"
                                    % (linenr, lang, oldlinenr, oldlang))
                            testerrors += 1
                oldresults.append((linenr, lang, alltrees))
        if not testerrors:
            print("OK!")
        print()
        totalerrors += testerrors

    # finally we report a summary:
    if not totalerrors:
        print("All %d tests passed!" % (len(alltests),))
    else:
        print("There were %d errors in %d tests!" % (totalerrors, len(alltests)))
    print()


if __name__ == '__main__':
    if len(sys.argv) <= 1:
        usage()
        exit(1)
    for filename in sys.argv[1:]:
        try:
            print("# Testing file:", filename)
            with io.open(filename, encoding=ENCODING) as F:
                print()
                runtest(F)
        except IOError as err:
            print(err)
            print()
            usage()
            exit(1)
