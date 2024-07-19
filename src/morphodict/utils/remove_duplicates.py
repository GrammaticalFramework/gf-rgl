import sys

FUNS_FILE='funs.tmp'  # obtained by pg -funs with the module to exclude

# example:
# cat MorphoDictEng.gf | python3 remove_duplicates.py >tmp/MorphoDictEng.gf
W cat MorphoDictEngAbs.gf | python3 remove_duplicates.py >tmp/MorphoDictEngAbs.gf


with open(FUNS_FILE) as funs:
    funset = {line.split()[0] for line in funs if line.split()}

for line in sys.stdin:
    ws = line.split()
    if ws[1:] and ws[0] in ['fun', 'lin'] and ws[1] in funset:
        print('---', line.strip())
    else:
        print(line.strip())
