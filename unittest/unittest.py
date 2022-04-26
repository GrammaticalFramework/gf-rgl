"""
Python 2+3 script for unit testing RGL grammars.

This script must be located in a sibling directory
to the RGL 'src' directory to work properly.

For for information see README.md, or run with argument '-h'
"""

from __future__ import print_function

import sys
import io
import os.path
import argparse
from subprocess import Popen, PIPE
from glob import glob

GF_PROCESS = 'gf -run'
## For WSL on windows replace the GF_PROCESS with following line
##GF_PROCESS = 'gf.exe -run'
GRAMMARDIR = '../src'
ENCODING = 'utf-8'


def create_argparser():
    """Creates an command line argument parser"""
    parser = argparse.ArgumentParser(
                description="Unit-test one (or more) RGL language(s).",
                epilog="""
This script must be located in a sibling directory
to the RGL 'src' directory to work properly.
For for information see README.md.
""")
    parser.add_argument('testfile', nargs='+',
                        help="one (or more) .gfscript file(s) containing unittests")
    parser.add_argument('-v', '--verbose', action='store_true',
                        help="be more verbose")
    parser.add_argument('--no-pmcfg', action='store_true',
                        help="don't calculate the PMCFG (faster for complex grammars); "
                        "for this to work, every test case needs a parse tree")
    return parser


def error(linenr, *args):
    """Prints an error to the terminal"""
    print("[Error at line %s]" % (linenr,), *args)


def gferror(reply):
    """Determines if a GF reply is an error"""
    return (reply.startswith('The parser failed')
            or reply.startswith('The sentence is not complete')
            or reply.startswith('Warning:')
            or reply.startswith('Function') and reply.endswith('is not in scope'))


def importfile(linenr, lang):
    """Calculate the path to the GF file to import"""
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
    """Strip leading/trailing blanks of every string in the given list"""
    return [s for s0 in strings for s in [s0.strip()] if s]


def numbered_np(num, noun, plural=None):
    """Crude way of inflecting nouns for number"""
    return "%d %s" % (num, noun if num == 1 else (plural or noun+'s'))


def collect_testcases(testlines):
    """Parse the test file and return a list of test cases"""
    tests = []
    test = []
    for linenr, line in enumerate(testlines, 1):
        line = line.strip()
        if line.startswith('#') or line.startswith('--'):
            # a comment line: do nothing
            pass
        elif not line:
            # an empty line: start a new test
            if test:
                tests.append(test)
                test = []
        elif ':' in line:
            lang, sentence = stripstrings(line.split(':', 1))
            langfile = importfile(linenr, lang)
            is_tree = ('/abstract/' in langfile) or 'Abs' in langfile
            test.append((is_tree, linenr, lang, langfile, sentence))
        else:
            error(linenr, "Ill-formatted line in test file:", line)
            exit(1)
    if test:
        tests.append(test)
    return tests


def create_gf_input(testcases, args):
    """Create a GF test script from the collected test cases"""
    gfscript = []
    for test in testcases:
        test_linenr = test[0][1]
        # check if the test contains an abstract tree:
        abs_linenr = abs_tree = None
        test.sort(key=lambda x:x[0], reverse=True)
        if test[0][0]:
            _, abs_linenr, abs_lang, _, abs_tree = test.pop(0)
        # the test should not consist of only a tree:
        if not test:
            error(test_linenr, "Empty test case")
            exit(1)
        # there should not be more than one abstree in the test:
        if test[0][0]:
            error(test[0][1], "Multiple abstract trees in test case")
            exit(1)
        # if there is an abstree, we use it for linearisation:
        if abs_tree:
            for _, linenr, lang, langfile, sentence in test:
                gfscript += ['ps "### %d"' % (test_linenr,),
                            'ps "+++ %d %s"' % (abs_linenr, abs_lang)]
                if not args.no_pmcfg:
                    gfscript += ['i %s' % (langfile,),
                                'l -lang=%s %s' % (lang, abs_tree)]
                else:
                    gfscript += ['i -retain -no-pmcfg %s' % (langfile,),
                                'cc -unqual -one %s' % (abs_tree,)]
                gfscript += ['ps "+++ %d %s"' % (linenr, lang),
                            'ps "%s"' % (sentence,)]
        # if there is no abstree, we have to use parsing;
        # in this case, the flag 'no_pmfcg' is of no use:
        elif args.no_pmcfg:
            error(test_linenr, "The flag '--no-pmcfg' requires that all test cases contain an abstract tree")
            exit(1)
        else:
            gfscript += ['ps "### %d"' % (test_linenr,)]
            for _, linenr, lang, langfile, sentence in test:
                gfscript += ['ps "+++ %d %s"' % (linenr, lang),
                            'i %s' % (langfile,),
                            'p -lang=%s "%s"' % (lang, sentence)]
    return gfscript


def runtest(testlines, args):
    """Read the test cases, run GF and report the results"""

    # first we build the input to the GF process:
    testcases = collect_testcases(testlines)
    gfscript = create_gf_input(testcases, args)

    if args.verbose:
        print("---+ GF testing script:")
        for line in gfscript:
            print('   |', line)
        print()

    # calling GF from a subprocess:
    command = GF_PROCESS.split()
    gfinput = '\n'.join(gfscript) + '\n'
    gf = Popen(command, stdin=PIPE, stdout=PIPE)
    stdout, _stderr = gf.communicate(gfinput.encode(ENCODING))
    stdout = stdout.decode(ENCODING)

    # then we analyse the result from the GF process:
    totalerrors = 0
    alltests = stripstrings(stdout.split('###'))

    for testnr, test in enumerate(alltests, 1):
        sents = stripstrings(test.split('+++'))
        startline = int(sents.pop(0))
        print("Test %d (line %d..): %s" % (testnr, startline, numbered_np(len(sents), "example")))
        testerrors = 0
        oldresults = []
        for sresults in sents:
            alltrees = stripstrings(sresults.splitlines())
            linenr, lang = alltrees.pop(0).split()
            if args.verbose:
                print('---+ line %s (%s), result from GF:' % (linenr, lang))
                for tree in alltrees:
                    print('   |', tree)
            if len(alltrees) == 0 or gferror("\n".join(alltrees)):
                theerror = "\n".join(alltrees) if alltrees else "No parse trees found"
                error(linenr, theerror)
                testerrors += 1
            else:
                allerrors = [(sum(tree not in oldtrees for _, _, oldtrees in oldresults), tree)
                                for tree in alltrees]
                besterrors, besttree = min(allerrors)
                if besterrors > 0:
                    for oldlinenr, oldlang, oldtrees in oldresults:
                        if besttree not in oldtrees:
                            error(linenr,
                                    "The result of line %s (%s):\n    %s\n"
                                    "is not among the results of line %s (%s):\n    %s"
                                    % (linenr, lang, besttree, oldlinenr, oldlang, "\n    ".join(oldtrees)))
                            testerrors += 1
                oldresults.append((linenr, lang, alltrees))
        if not testerrors:
            print("OK!")
        print()
        totalerrors += testerrors

    # finally we report a summary:
    if not totalerrors:
        print("All %s passed!" % (numbered_np(len(alltests), "test"),))
    else:
        print("Found %s in %s!" % (numbered_np(totalerrors, "error"), numbered_np(len(alltests), "test")))
    print()


if __name__ == '__main__':
    parser = create_argparser()
    args = parser.parse_args()
    for filename in args.testfile:
        try:
            print("# Testing file:", filename)
            with io.open(filename, encoding=ENCODING) as F:
                print()
                runtest(F, args)
        except IOError as err:
            print(err)
            print()
            parser.print_usage()
            exit(1)
