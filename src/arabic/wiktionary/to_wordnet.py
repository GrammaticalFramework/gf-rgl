import sys
import csv
import json

from arabic_utilities import *

# to run: python3 to_wordnet.py >arabic-wn-morpho.jsonl
# the following are assumed

# from https://www.grammaticalframework.org/~krasimir/arabic.tsv.gz
# WN_TSV = 'arabic.tsv'  # Krasimir
WN_TSV = 'ar2en_words_gf.csv'  # Zarzoura

# built as explained in ./read_wiktionary.py
MORPHO_GF = 'MorphoDictAraAbs.gf'


# fun 'دُبُ_N' : N ; -- 10 [['bear']]
funmap = {}
with open(MORPHO_GF) as gffile:
    for line in gffile:
        line = line.split()
        if line[2:] and line[0] == 'fun':
            fun = line[1]
            key = unvocalize(fun)
            cat = line[3] 
            sense = ' '.join(line[6:])
            funmap[(key, cat)] = funmap.get((key, cat), [])
            funmap[(key, cat)].append({'fun': fun,  'sense': sense})


# abandon_1_V2    ParseAra        ترك     (1,1,1,3,322,3)
with open(WN_TSV) as wnfile:
    print('--# -path=.:../gf-wordnet')
    print('concrete WordNetAra of WordNet = CatAra ** open MorphoDictAra, MoreAra, ParadigmsAra in {') 

##    wnreader = csv.reader(wnfile, delimiter='\t')
    for row in wnfile:
##        word = row[-1].strip()   # does not show tha arabic, but the second-last word
        word = unvocalize(get_arabic(row))
        wnfun = row.split()[-1]  # 0 in Krasimir
        cat = [c for c in wnfun if c.isalpha()][-1]  # the last letter; the dict only contains N, A, V
        funs = funmap.get((word, cat), [])
        mk = 'mkV2 ' if wnfun.endswith('V2') else ''
        results = [' '.join(['lin', wnfun, '=', mk + fs['fun'], ';', '--', str(fs['sense'])])
                         for fs in funs]
        if results:
            print(results[0])
            for r in results[1:]:
                print('--', r)
        else:
            if (cat := wnfun[-2:]) in ['_A', '_N', '_V']:
                lin = 'mk' + cat[-1] + ' "' + word + '"'
            else:
                lin = 'variants {}'
            print(' '.join(['lin', wnfun, '=', lin, ';', '---', 'guess from', word]))
    print('}')    
 

