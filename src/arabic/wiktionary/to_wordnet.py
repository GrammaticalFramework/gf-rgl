import csv
import json

# to run: python3 to_wordnet.py >arabic-wn-morpho.jsonl
# the following are assumed


# from https://www.grammaticalframework.org/~krasimir/arabic.tsv.gz
WN_TSV = 'arabic.tsv'

# built as explained in ./read_wiktionary.py
MORPHO_GF = 'MorphoDictAraAbs.gf'

def is_arabic(s):
    return s and any(1574 <= ord(c) <= 1616 for c in s)

def get_arabic(s):
    return ''.join([c for c in s if is_arabic(c)])

def unvocalize(s):
    return ''.join([c for c in s if 0x621 <= ord(c) <= 0x64a])


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
##    wnreader = csv.reader(wnfile, delimiter='\t')
    for row in wnfile:
##        word = row[-1].strip()   # does not show tha arabic, but the second-last word
        word = unvocalize(get_arabic(row))
        wnfun = row.split()[0]
        cat = [c for c in wnfun if c.isalpha()][-1]  # the last letter; the dict only contains N, A, V
        funs = funmap.get((word, cat), [])
        result = {'wnfun': wnfun, 'sought': word, 'found': funs}
        print(json.dumps(result, ensure_ascii=False))
 

