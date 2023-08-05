# show missing functions in the RGL languages
# basic usage: python3 show_missing.py >missing-in-rgl-Grammar.tsv

import subprocess

# this is the functions you want to find: default is all funs in Grammar
GF_LIB_PATH = '/Users/aarne/GF/dist/build/rgl/'
ALLTENSES_PATH = GF_LIB_PATH + 'alltenses/'
RGL_SOURCE_PATH = '/Users/aarne/GF/gf-rgl/src/'
GRAMMAR = 'Grammar'

# these are the languages you investigate
LANGS = ['Cze', 'Urd']

# get all functions in GRAMMAR, together with their types
def get_funs(module=GRAMMAR):
    cmd = 'echo "pg -funs" | gf -run ' + ALLTENSES_PATH + module + '.gfo'
    missing = subprocess.Popen(cmd, text=True, shell=True,
                               stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
    output = missing.communicate()[0].split(';')
    return ([(fun.split()[0].strip(), ' '.join(fun.split()[2:]))  for fun in output if fun.split()])

# get all missing functions in GRAMMARLng; this can take a long time
def get_missing_from_compiled(Lng, module=GRAMMAR):
    print('investigating', Lng)
    cmd = 'echo "pg -missing" | gf -run ' + ALLTENSES_PATH + module + Lng + '.gfo'
    missing = subprocess.Popen(cmd, text=True, shell=True,
                               stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
    output = missing.communicate()[0].split()
    return set(output[2:])

# to get all languages in the compiled RGL, use all_langs from here
def all_rgl_compiled_langs():
    modules = subprocess.run(['ls', ALLTENSES_PATH], capture_output=True, text=True)
    files = str(modules.stdout)
    return [file[-7:-4] for file in files.split() if file[:-7].endswith(GRAMMAR)]

# LANGS = all_rgl_compiled_langs()  # uncomment this line if you want all languages


# it is much faster to use source files (sending gfos to /tmp)
def str_until(c, s):
    i = s.find(c)
    if i >= 0:
        return s[:i]
    else:
        return s
    
def get_missing_from_source(lang, gf_file):
    cmd = 'gf -batch -retain -no-pmcfg -src -gfo-dir=/tmp ' + gf_file
    missing = subprocess.Popen(cmd, text=True, shell=True,
                               stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    output = missing.communicate()[1].split('\n')
    missing = (lang, {str_until('\x1b[39;49m', line.split()[-1])
                      for line in output
                      if line.strip().startswith('Warning: no linearization of')})
    return missing


def all_rgl_source_modules(module=GRAMMAR):
    cmd = 'ls ' + RGL_SOURCE_PATH + '*/' + module + '?*.gf'
    files = subprocess.Popen(cmd, text=True, shell=True,
                               stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
    output = files.communicate()[0].split()
    return {file[-6:-3]: file for file in output}

LANGS = all_rgl_source_modules()  # Lng: file dict

# make a text with tab-separated strings withcolumns fun, type, langs and rows for each fun
def make_table(funs=get_funs(), module=GRAMMAR, langs=LANGS):
    header = ['fun', 'type'] + list(langs.keys())  # if dict, otherwise langs ###
    rows = [header]
    print('\t'.join(header))
    
#    missings = {lang: get_missing_from_compiled(lang, module) for lang in langs}
    missings = {lang: get_missing_from_source(lang, file)[1] for lang,file in langs.items()}
    for fun in funs:
        row = [fun[0], fun[1]]
        for lang in langs.keys():
            if fun[0] in missings[lang]:
                row.append('-')
            else:
                row.append('+')
        rows.append(row)
        print('\t'.join(row))


# the output is send to stdout
make_table()


    

