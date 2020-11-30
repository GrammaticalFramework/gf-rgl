import sys

"""
Converting a form-lemma-description lexicon to a GF lexicon.

Example: Romance resources from http://nlp.lsi.upc.edu/freeling/index.php/node/12

  $ python3 get_dict.py Ita tmp/IrregItaAbs.gf tmp/entries

where entries is obtained by 

  $ cat adjs adv noun verb other | sort -u >entries

in FreeLing/data/it/dictionary/entries/
In this file, each line has three words, e.g.

  abbondanze abbondanza NCFP000

from which the script produces two lines,

  fun abbondanza_N : N ;
  lin abbondanza_N = mkN "abbondanza" "abbondanze" feminine ;

These can be directed to an abstract and concrete module by using grep, as shown in Makefile.

Adaptation to a new language requires extending lang_args() with a new language code and the functions for that language.

"""
# an auxiliary

def quoted(s):
    return '"' + s + '"'

######################
## language-specific code
######################

## Ita = Italian

# how each argument is obtained from the set of descriptions
def ita_noun_args(irregs,entry):
    arg1 = entry.get('NCFS000', entry.get('NCMS000',entry.get('NCFN000',entry.get('NCMN000','NONE'))))
    arg2 = entry.get('NCFP000', entry.get('NCMP000','NONE'))
    if 'NCFS000' in entry.keys():
        arg3 = 'feminine'
    else:
        arg3 = 'masculine'
    if arg2 == 'NONE':
        return ['NSg', arg1, 'mkNSg', quoted(arg1), arg3]  # nouns that occur in singular only
    elif arg1 == 'NONE':
        return ['NPl', arg2, 'mkNPl', quoted(arg2), arg3]  # nouns that occur in pular only
    else:
        return ['N', arg1, 'mkN', quoted(arg1), quoted(arg2),arg3]

    
def ita_adj_args(irregs,entry):
    arg1 = entry.get('AQ0MS00', entry.get('AQ0CS00',entry.get('AQ0CN00','NONE')))
    arg2 = entry.get('AQ0FS00', entry.get('AQ0CS00',entry.get('AQ0CN00','NONE')))
    arg3 = entry.get('AQ0MP00', entry.get('AQ0CP00',entry.get('AQ0CN00','NONE')))
    arg4 = entry.get('AQ0FP00', entry.get('AQ0CS00',entry.get('AQ0CN00','NONE_' + str(entry))))
    return ['A', arg1, 'mkA', quoted(arg1), quoted(arg2),quoted(arg3),quoted(arg4)]


def ita_verb_args(irregs,entry):
    arg1 = entry.get('VMN0000', 'NONE_' + str(entry))
    if (arg1 + '_V') in irregs:
        return ['V', arg1, '', 'IrregIta.' + arg1 + '_V']
    else:
        return ['V', arg1, 'mkV', quoted(arg1)]

    
def ita_adv_args(irregs,entry):
    arg1 = entry.get('RG', 'NONE_' + str(entry))
    return ['Adv', arg1, 'mkAdv', quoted(arg1)]


def ita_prep_args(irregs,entry):
    arg1 = entry.get('SPS00', 'NONE_' + str(entry)) ## includes di,in,... but not their contractions
    return ['Prep', arg1, 'mkPrep', quoted(arg1)]

def ignore_args(irregs,entry):
    arg1 = 'NONE_' + str(entry)
    return ['IGNORED', list(entry.values())[0], '', quoted(arg1)]

    
def ita_args(key):
    if key[0] == 'A':
        return ita_adj_args
    elif key[0] == 'N':
        return ita_noun_args
    elif key[0] == 'V':
        return ita_verb_args
    elif key[0] == 'R':
        return ita_adv_args
    elif key[0] == 'S':
        return ita_prep_args
    else:
        return ignore_args

def lang_args(lang,key):
    if lang == 'Ita':
        return ita_args(key)
    else:
        print("unknown language", lang)

##########################################################
## from this point, the code is generic for all languages
##########################################################

        
def get_irregs(ifile):
    file = open(ifile)
    irregs = [] 
    for line in file:
        ws = line.split()
        if ws and ws[0] == 'fun':
            irregs.append(ws[1])
    file.close()
    return irregs


def get_dict(filename):
    file = open(filename)
    dict = {}
    for line in file:
        words = line.split()
        if len(words) == 3:
            cat = words[2][0] # N,A,V,R,S ...
            key = cat + '_' + words[1]
            dict[key] = dict.get(key,{})
            dict[key][words[2]] = words[0]
    return dict

def clean_fun(lemma,cat):
    if lemma.isalpha():
        return lemma + '_' + cat
    else:
        fun = ''
        for c in lemma:
            if c=="'":
                fun += "\\'"
            else:
                fun += c
        return "'" + fun + '_' + cat + "'"

def print_entry(irregs,mapping,entry):
    args = mapping(irregs,entry)
    fun = clean_fun(args[1],args[0])
    comment = min(1,str(args).count('NONE')) * '-- '
    rule = [comment + "fun", fun, ":", args[0], ";"]
    print(' '.join(rule))
    rule = [comment + "lin", fun, "=", args[2]]
    for arg in args[3:]:
        rule.append(arg)
    rule.append(';')
    print(' '.join(rule))

    
def main():
    if len(sys.argv) != 4:
        print("usage: python3 get_dict <lang> <irregsfile> <dictfile>")
        return 1
    lang = sys.argv[1]
    irregsfile = sys.argv[2]
    dictfile = sys.argv[3]
    irregs = get_irregs(irregsfile)
    dict = get_dict(dictfile)
    for key,entry in dict.items():
        print_entry(irregs,lang_args(lang,key),entry)
    print("}")
    return 0

        
main()


