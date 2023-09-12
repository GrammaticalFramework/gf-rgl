import gzip
import json

WIKTIONARY_DUMP = 'raw-wiktextract-data.json.gz'
FILTERED_WIKT = 'wikt_arabic.jsonl'


def get_gzip_json(file, sample=100000, langs=[]):
    with gzip.open(file) as decompressed:
        n = 0
        for line in decompressed:
            n += 1
            if n % sample == 0:
                obj = json.loads(line)
                if obj.get('lang', None) in langs:
                    print(line.decode("utf-8"))
        print(n)


# get_gzip_json(WIKTIONARY_DUMP, 1, ['Arabic'])  
# python3 read_wiktionary.py >wikt_arabic.jsonl
# 621-671

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

def to_buckwalter(s):
    return ''.join([buckwalter_dict.get(ord(c), '?') for c in s])


def unvocalize(s):
    return ''.join([c for c in s if 0x621 <= ord(c) <= 0x64a])

def is_arabic(s):
    return s and any(1574 <= ord(c) <= 1616 for c in s)


def gf_fun(s, pos):
    return ''.join(["'", s, "_", pos, "'"])


def forms_for_pos(obj):
    forms = {
        form['form']:
          form.get('tags', []) for
            form in obj.get('forms', []) if
               'romanization' not in form.get('tags', []) and
                   is_arabic(form['form'])
        }.items()
    if obj['pos'] == 'noun':
        lemma = [form[:-1] for form, descr in forms
                         if all([w in descr for w in ['construct', 'nominative', 'singular']])][:1]
        return {
            'gf_fun': gf_fun(lemma[0], 'N') if lemma else None,
            'gf_cat': 'N',
            'singular': lemma,  
            'plural': [form[:-1] for form, descr in forms
                         if all([w in descr for w in ['construct', 'nominative', 'plural']])][:1],
            'gender': 'Fem' if 'Arabic feminine nouns' in obj['categories']
                            else ('Masc' if  'Arabic masculine nouns' in obj['categories']
                                else None)
            } 
    elif obj['pos'] == 'verb':
        lemma = [form for form, descr in forms
                      if all([w in descr for
                              w in ["active", "indicative", "masculine", "past", "perfective", "singular", "third-person"]])][:1]
        return {
          'gf_fun': gf_fun(lemma[0], 'V') if lemma else None,
          'gf_cat': 'V',
          'perfect': lemma, 
          'imperfect': [form for form, descr in forms
                      if all([w in descr for
                              w in ["active", "indicative", "masculine", "non-past", "imperfective", "singular", "third-person"]])][:1],
          'verbclass': max([n for n in ['I', 'II','III','IV','V','VI','VII','VIII','IX','X','XI','XII','']
                            if n in ' '.join([c for c in obj['categories'] if c.endswith('verbs') and any([n in c for n in 'IVX'])])],
                           key=len)
          }
    elif obj['pos'] == 'adj':
        lemma = [form for form, descr in forms
                         if all([w in descr for w in ['indefinite', 'masculine', 'singular', 'informal']])][:1]
        return {
            'gf_fun': gf_fun(lemma[0], 'A') if lemma else None,
            'gf_cat': 'A',
            'masc_singular': lemma,   
            'masc_plural': [form for form, descr in forms
                         if all([w in descr for w in ['indefinite', 'masculine', 'plural', 'informal']])][:1],
            'fem_singular': [form for form, descr in forms
                         if all([w in descr for w in ['indefinite', 'feminine', 'singular', 'informal']])][:1],  
            'fem_plural': [form for form, descr in forms
                         if all([w in descr for w in ['indefinite', 'feminine', 'plural', 'informal']])][:1],
            } 

    else:
        return {f: d for f, d in forms}


# "root": ["ش ر ح (š-r-ḥ)"]
def find_root(s):
    return ''.join([c for c in s if is_arabic(c)])
    
import sys
MODE = sys.argv[1]

if MODE == 'gf':
    print('abstract MorphoDictAraAbs = Cat ** {') 

with open(FILTERED_WIKT) as file:
    seen_gf_funs = set()
    for line in file:
        obj = json.loads(line)
        if 'Arabic lemmas' in obj.get('categories', []):
            entry = {
                'pos': obj['pos'],
                'root': [find_root(t['expansion']) for t in obj.get('etymology_templates', []) if t.get('name', None) =='ar-root'][:1],
                'forms': forms_for_pos(obj),
                'senses': [sense['glosses'] for sense in obj.get('senses', [])
                           if 'glosses' in sense]
                }
#            entry['n_forms'] = len(entry['forms'])
#            print(entry['pos'], entry['n_forms'])
            if MODE == 'json':
                print(json.dumps(entry, ensure_ascii=False))

            if MODE == 'gf':

                if 'gf_fun' in entry['forms'] and entry['forms']['gf_fun']:
                    if entry['forms']['gf_fun'] not in seen_gf_funs:
                        print('fun', entry['forms']['gf_fun'], ':', entry['forms']['gf_cat'], ';', '--', entry['senses'])
                        seen_gf_funs.add(entry['forms']['gf_fun'])

                # to do: rename duplicate function names: of 13762 names, 12946 are unique

if MODE == 'gf':            
    print('}')
