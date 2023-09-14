import gzip
import json
import sys
import unicodedata
import pgf


# data from https://kaikki.org/dictionary/rawdata.html
# thanks Tatu Ylonen: Wiktextract: Wiktionary as Machine-Readable Structured Data,
# Proceedings of the 13th Conference on Language Resources and Evaluation (LREC), pp. 1317-1325, Marseille, 20-25 June 2022.

"""
This file converts Wiktionary data to GF morphological dictionary files.
It words for Arabic but some functionalities could be modified to other languges.

The steps to take are the following:

fetch data:

  raw-wiktextract-data.json.gz from https://kaikki.org/dictionary/rawdata.html

filter Arabic entries:

  $ python3 read_wiktionary.py raw >wikt_arabic.jsonl

create GF files:

  $ python3 read_wiktionary.py gf-abs >MorphoDictAraAbs.gf
  $ python3 read_wiktionary.py gf-cnc >MorphoDictAra.gf

automatic evaluation:

  $ gf -make MorphoDictAra.gf
  $ python3 read_wiktionary.py gf-map >function_sources_arabic.jsonl
  $ python3 read_wiktionary.py eval

TODO:
- better generation of GF
- better paradigms to use Wiktionary data
- refactor the code so that it can be used for other languages

"""




MODE = ''

if __name__ == '__main__':
    if not sys.argv[1:]:
        print('usage: read_wiktionary (raw | gf-cnc | gf-abs | gf-map | eval | eval-verbose)')
        exit()
    MODE = sys.argv[1]  # 

    
# step 1: extract data from this file using the raw option
WIKTIONARY_DUMP = 'raw-wiktextract-data.json.gz'

# the following file is generated.
# in the sequel, use this file with gf-abs or gf-cnc option
FILTERED_WIKT = 'wikt_arabic.jsonl'

# map each successfully extracted GF function to its source record in Wiktionary
# created with option gf-map
FUNCTION_SOURCE_MAP = 'function_sources_arabic.jsonl'

PGF_FILE = 'MorphoDictAraAbs.pgf'
CONCRETE_MODULE = 'MorphoDictAra'


def read_function_source_map():
    with open(FUNCTION_SOURCE_MAP) as file:
        sourcemap = {}
        for line in file:
            try:
                obj = json.loads(line)
                sourcemap[obj['fun']] = obj['source']
            except:
                continue
    return sourcemap
            

def get_gzip_json(file, sample=100000, langs=[]):
    with gzip.open(file) as decompressed:
        n = 0
        for line in decompressed:
            n += 1
            if n % sample == 0:
                obj = json.loads(line)
                if obj.get('lang', None) in langs:
                    print(line.decode("utf-8"))
#        print(n)

if MODE == 'raw':
    get_gzip_json(WIKTIONARY_DUMP, 1, ['Arabic'])  

# python3 read_wiktionary.py raw >wikt_arabic.jsonl

# https://en.wikipedia.org/wiki/Buckwalter_transliteration
buckwalter_dict = {
  0x621: "'",  # ء
  0x622: '|',  # آ
  0x623: '>',  # أ
  0x624: '&',  # ؤ
  0x625: '<',  # إ
  0x626: '}',  # ئ
  0x627: 'A',  # ا
  0x628: 'b',  # ب
  0x629: 'p',  # ة
  0x62a: 't',  # ت
  0x62b: 'v',  # ث
  0x62c: 'j',  # ج
  0x62d: 'H',  # ح
  0x62e: 'x',  # خ
  0x62f: 'd',  # د
  0x630: '*',  # ذ
  0x631: 'r',  # ر
  0x632: 'z',  # ز
  0x633: 's',  # س
  0x634: '$',  # ش
  0x635: 'S',  # ص
  0x636: 'D',  # ض
  0x637: 'T',  # ط
  0x638: 'Z',  # ظ
  0x639: 'E',  # ع
  0x63a: 'g',  # غ
  0x641: 'f',  # ف
  0x642: 'q',  # ق
  0x643: 'k',  # ك
  0x644: 'l',  # ل
  0x645: 'm',  # م
  0x646: 'n',  # ن
  0x647: 'h',  # ه
  0x648: 'w',  # و
  0x649: 'Y',  # ى
  0x64a: 'y',  # ي
  0x64b: 'F',  # ً
  0x64c: 'N',  # ٌ
  0x64d: 'K',  # ٍ
  0x64e: 'a',  # َ
  0x64f: 'u',  # ُ
  0x650: 'i',  # ِ
  0x651: '~',  # ّ
  0x652: 'o',  # ْ
  0x670: '`',  # '
  0x671: '{'   # ٱ
  }

buckwalter_dict_rev = {b: chr(a) for a, b in buckwalter_dict.items()}


def to_buckwalter(s):
    return ''.join([buckwalter_dict.get(ord(c), c) for c in s])


def from_buckwalter(s):
    return ''.join([buckwalter_dict_rev.get(c, c) for c in s])


def unvocalize(s):
    return ''.join([c for c in s if 0x621 <= ord(c) <= 0x64a])


def is_arabic(s):
    return s and any(1574 <= ord(c) <= 1616 for c in s)

def normal(s):
    return unicodedata.normalize('NFD', s)


# Wikt uses vowel+shadda which is a Unicode normalization
# GF uses shadda+vowel which is linguistically correct
# see https://stackoverflow.com/questions/58559390/in-unicode-should-u0651-arabic-shadda-be-before-or-after-kasra
# unicodedata.normalize does this wrong, as noted by Ariel Gutman 
## todo: more direct implementation
def reorder_shadda(s):
    return from_buckwalter(to_buckwalter(s).replace('a~', '~a').replace('u~', '~u').replace('i~', '~i'))


# quote forms but not parameters
def quote_if(s, cond=is_arabic, change=reorder_shadda):
    if cond(s):
        return '"' + change(s) + '"'
    else:
        return s

def gf_fun(s, pos, disamb=0):
    discrim = '_' + str(disamb) if disamb else ''
    return ''.join(["'", s, discrim, "_", pos, "'"])


rgl_features = {
    # V
    'VPerf': 'perfective',
    'Act': 'active',
    'Pas': 'passive',
    'Per3': 'third-person',
    'Per2': 'second-person',
    'Per1': 'first-person',
    'Masc': 'masculine',
    'Fem': 'feminine',
    'Sing': 'singular',
    'Plur': 'plural',
    'Sg': 'singular',
    'Pl': 'plural',
    'Dl': 'dual',
    'VImpf': 'imperfective',
    'Ind': 'indicative',
    'Cnj': 'subjunctive',
    'Jus': 'jussive',
    'VImp': 'imperative',
    # N: also Sg, Pl, Dl
    'Def': 'definite',
    'Indef': 'indefinite',
    'Nom': 'nominative',
    'Acc': 'accusative',
    'Gen': 'genitive',
#    'Bare':
#    'Dat':
    'Const': 'construct',
#    'Poss':
    #A: also N features
    'APosit': 'positive',
    'AComp': 'comparative'
    }


# obsolote:
# format of GF table: MorphoDictAra: s (VPerf Act (Per3 Masc Sg)) : أَجْرََ    
# coming from 'l -treebank -table'
# now used:
#  {'s (AComp Def Bare)': 'الأَيَُونَانِ'}
# coming from tabularLinearize

def compare_tables(gf, wikt, fun):
    report = {}    
    for pair in gf.items():
        gf_form = pair[1]
        gf_tags = tuple(word for word in
                    pair[0].replace('(', ' ').replace(')', ' ').split()
                      if word in rgl_features)
        if not gf_tags:
            continue
        wikt_tags = {rgl_features[tag] for tag in gf_tags}
        wikt_form = None
        wikt_descr = None
        for form, descr in wikt:
            if all([tag in descr for tag in wikt_tags]):
                wikt_form = reorder_shadda(form)
                wikt_descr = descr
                break
        report[gf_tags] = {
            'gf_form': gf_form,
            'wikt_form': wikt_form,
            'gf_form_rom': to_buckwalter(gf_form) if gf_form else None,
            'wikt_form_rom': to_buckwalter(wikt_form) if wikt_form else None,
            'wikt_descr': wikt_descr
            }
        if wikt_form:
            report[gf_tags]['voc_match'] = int(normal(gf_form) == normal(wikt_form))
            report[gf_tags]['unvoc_match'] = int(normal(unvocalize(gf_form)) == normal(unvocalize(wikt_form)))
    ritems = tuple(report.items())  # need an unmutable structure, because otherwise ints are added to items
    report['fun'] = fun
    report['total_found'] = len([f for f, v  in ritems if v['wikt_form'] is not None ])
    report['total_voc'] = sum([v.get('voc_match', 0) for f, v in ritems])
    report['total_unvoc'] = sum([v.get('unvoc_match', 0) for f, v in ritems])
    return report



def wikt_forms_for_pos(obj):
    return {
        form['form']:
          form.get('tags', []) for
            form in obj.get('forms', []) if
               'romanization' not in form.get('tags', []) and
                   is_arabic(form['form'])
        }.items()


def forms_for_pos(obj):
    forms = wikt_forms_for_pos(obj)
    if obj['pos'] == 'noun':
        lemma = [form[:-1] for form, descr in forms
                         if all([w in descr for w in ['construct', 'nominative', 'singular']])][:1]
        plural = [form[:-1] for form, descr in forms
                         if all([w in descr for w in ['construct', 'nominative', 'plural']])][:1]
        gender = (['fem'] if 'Arabic feminine nouns' in obj['categories']
                            else (['masc'] if  'Arabic masculine nouns' in obj['categories']
                                  else []))
        gf_entry = {
            'cat': 'N',
            'lemma': lemma,
            'args': {
                'sg': lemma,  
                'pl': plural,
                'g': gender
                }
            } 
    elif obj['pos'] == 'verb':
        lemma = [form for form, descr in forms
                      if all([w in descr for
                              w in ["active", "indicative", "masculine", "past",
                                        "perfective", "singular", "third-person"]])][:1]
        gf_entry = {
          'cat': 'V',
          'lemma': lemma,
          'args': {
              'perfect': lemma, 
              'imperfect': [form for form, descr in forms
                      if all([w in descr for
                              w in [
                                  "active", "indicative", "masculine", "non-past",
                                  "imperfective", "singular", "third-person"]])][:1],
              'cls': ['Form' + max([n for n in [
                  'I', 'II','III','IV','V','VI','VII','VIII','IX','X','XI','']
                            if n in ' '.join([c for c in obj['categories']
                                if c.endswith('verbs') and any([n in c for n in 'IVX'])])],
                           key=len)]  # max in RGL is XI, in Wikt XIII
              }
          }
    elif obj['pos'] == 'adj':
        lemma = [form for form, descr in forms
                    if all([w in descr for w in [
                        'indefinite', 'masculine', 'singular', 'informal']])][:1]
        gf_entry = {
            'cat': 'A',
            'lemma': lemma,
            'args': {
                'masc_sg': lemma,   
                'masc_pl': [form for form, descr in forms
                         if all([w in descr for w in ['indefinite', 'masculine', 'plural', 'informal']])][:1],
                'fem_sg': [form for form, descr in forms
                         if all([w in descr for w in ['indefinite', 'feminine', 'singular', 'informal']])][:1],  
                'fem_pl': [form for form, descr in forms
                         if all([w in descr for w in ['indefinite', 'feminine', 'plural', 'informal']])][:1],
                }
            } 

    else:
        gf_entry = {f: d for f, d in forms}
        
    if 'lemma' in gf_entry and gf_entry['lemma']:
        gf_entry['lemma'] = gf_entry['lemma'][0]
        if obj['root'] and obj['root'][0].strip():
            gf_entry['args']['root'] = obj['root']
        args = [r + ' = ' + quote_if(x[0]) for r, x in gf_entry['args'].items() if x]
        gf_entry['lin'] = 'wmk' + gf_entry['cat'] + ' {' + ' ; '.join(args) + '}' 

    return gf_entry
    
# "root": ["ش ر ح (š-r-ḥ)"]
def find_root(s):
    return ''.join([c for c in s if is_arabic(c)])
    
if MODE == 'gf-abs':
    print('abstract MorphoDictAraAbs = Cat ** {')    
if MODE == 'gf-cnc':
    print('concrete MorphoDictAra of MorphoDictAraAbs = CatAra ** open ParadigmsAra in {') 

    
if MODE.startswith('gf') or MODE=='json':
  with open(FILTERED_WIKT) as file:
    seen_gf_funs = {}
    number = 1
    for line in file:
        try:
            obj = json.loads(line)
        except:
            continue
        number += 1
        root = [find_root(t['expansion']) for
                t in obj.get('etymology_templates', []) if
                t.get('name', None) =='ar-root'][:1]
        obj['root'] = root
        if 'Arabic lemmas' in obj.get('categories', []):
            entry = {
                'pos': obj['pos'],
                'forms': forms_for_pos(obj),
                'senses': [sense['glosses'] for sense in obj.get('senses', [])
                           if 'glosses' in sense]
                }
#            entry['n_forms'] = len(entry['forms'])
#            print(entry['pos'], entry['n_forms'])
            if MODE == 'json':
                print(json.dumps(entry, ensure_ascii=False))

            if MODE.startswith('gf'):

                lemma = entry['forms'].get('lemma', None)
                if lemma:
                    cat = entry['forms']['cat']
                    lin = entry['forms']['lin']
                    discrim = seen_gf_funs.get((lemma, cat), 0)
                    fun = gf_fun(lemma, cat, discrim)
                        
                    if MODE == 'gf-abs':
                        print('fun', fun, ':', cat, ';', '--', number, entry['senses'])
                    if MODE == 'gf-cnc':
                        print('lin', fun, '=', lin, ';')
                    if MODE == 'gf-map':
                        mapitem = {'fun': fun, 'source': obj}
                        print(json.dumps(mapitem, ensure_ascii=False))
                            
                    seen_gf_funs[(lemma, cat)] = discrim + 1

                # to do: rename duplicate function names: of 13762 names, 12946 are unique

if MODE.startswith('gf'):            
    print('}')

    
def eval_all(gr, funmap, concrete=CONCRETE_MODULE):
    lang = gr.languages[CONCRETE_MODULE]
    funs = gr.functions
    reports = []
    for fun in funs:
        funn = "'" + fun + "'"
        if funn not in funmap:
            print(funn, 'not found')
            continue
        wikt = wikt_forms_for_pos(funmap[funn])
        gf = lang.tabularLinearize(pgf.Expr(fun, []))
        report = compare_tables(gf, wikt, fun)
        reports.append(report)
    return reports


def first_error(report):
    for f, v in report.items():
        if 'voc_match' in v:
            if v['voc_match'] == 0:
                return f, v


if MODE.startswith('eval'):
    gr = pgf.readPGF(PGF_FILE)
    print('using', PGF_FILE)
    funmap = read_function_source_map()
    print(len(funmap), 'functions')
    for report in eval_all(gr, funmap):    

        if MODE == 'eval-verbose':
            for line in report.items():
                print(line)
        else:
            if report['total_found'] == 0:
                verdict = 'NOT_FOUND'
            elif report['total_found'] == report['total_voc']:
                verdict = 'PERFECT'
            elif report['total_found'] == report['total_unvoc']:
                verdict = 'PERFECT_UNVOC ' + str(first_error(report))
            elif report['total_voc'] == 0:
                verdict = 'TOTALLY_WRONG ' + str(first_error(report))
            else:
                verdict = 'PARTIAL ' + str(first_error(report))
            print(report['fun'], 'forms', report['total_found'],
                  'voc', report['total_voc'], 'unvoc', report['total_unvoc'],
                  verdict
                  )

