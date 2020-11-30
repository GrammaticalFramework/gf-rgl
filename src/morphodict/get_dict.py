import sys

source = "tmp/sentries"
irregsfile = "tmp/IrregItaAbs.gf"

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

def quoted(s):
    return '"' + s + '"'

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

# how each argument is obtained from the set of descriptions
def ita_noun_args(irregs,entry):
    arg1 = entry.get('NCFS000', entry.get('NCMS000',entry.get('NCFN000',entry.get('NCMN000','NONE'))))
    arg2 = entry.get('NCFP000', entry.get('NCMP000','NONE'))
    if 'NCFS000' in entry.keys():
        arg3 = 'feminine'
    else:
        arg3 = 'masculine'
    if arg2 == 'NONE':
        return ['NSg', clean_fun(arg1,'NSg'), 'mkNSg', quoted(arg1), arg3]        
    elif arg1 == 'NONE':
        return ['NPl', clean_fun(arg2,'NPl'), 'mkNPl', quoted(arg2), arg3]        
    else:
        return ['N', clean_fun(arg1,'N'), 'mkN', quoted(arg1), quoted(arg2),arg3]

    
def ita_adj_args(irregs,entry):
    arg1 = entry.get('AQ0MS00', entry.get('AQ0CS00',entry.get('AQ0CN00','NONE')))
    arg2 = entry.get('AQ0FS00', entry.get('AQ0CS00',entry.get('AQ0CN00','NONE')))
    arg3 = entry.get('AQ0MP00', entry.get('AQ0CP00',entry.get('AQ0CN00','NONE')))
    arg4 = entry.get('AQ0FP00', entry.get('AQ0CS00',entry.get('AQ0CN00','NONE_' + str(entry))))
    return ['A', clean_fun(arg1,'A'), 'mkA', quoted(arg1), quoted(arg2),quoted(arg3),quoted(arg4)]


def ita_verb_args(irregs,entry):
    arg1 = entry.get('VMN0000', 'NONE_' + str(entry))
    if (arg1 + '_V') in irregs:
        return ['V', clean_fun(arg1,'V'), '', 'IrregIta.' + arg1 + '_V']
    else:
        return ['V', clean_fun(arg1,'V'), 'mkV', quoted(arg1)]

    
def ita_adv_args(irregs,entry):
    arg1 = entry.get('RG', 'NONE_' + str(entry))
    return ['Adv', clean_fun(arg1,'Adv'), 'mkAdv', quoted(arg1)]


def ita_prep_args(irregs,entry):
    arg1 = entry.get('SPS00', 'NONE_' + str(entry)) ## includes di,in,... but not their contractions
    return ['Prep', clean_fun(arg1,'Prep'), 'mkPrep', quoted(arg1)]

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


def print_entry(irregs,mapping,entry):
    args = mapping(irregs,entry)
    comment = min(1,str(args).count('NONE')) * '-- '
    rule = [comment + "fun", args[1], ":", args[0], ";"]
    print(' '.join(rule))
    rule = [comment + "lin", args[1], "=", args[2]]
    for arg in args[3:]:
        rule.append(arg)
    rule.append(';')
    print(' '.join(rule))
    
def main():
    irregs = get_irregs(irregsfile)
    dict = get_dict(source)
#    for word in dict:
#        print(word,dict[word])
    for key,entry in dict.items():
        print_entry(irregs,ita_args(key),entry)
    print("}")

        
main()


