import json
import sys

# https://kaikki.org/dictionary/rawdata.html
# Tatu Ylonen: Wiktextract: Wiktionary as Machine-Readable Structured Data,
# Proceedings of the 13th Conference on Language Resources and Evaluation (LREC),
# pp. 1317-1325, Marseille, 20-25 June 2022. 

FILE = 'data/raw-wiktextract-data.json'

MYLANG = 'Serbo-Croatian'
REFLANG = 'English'

GENDERS = ['masculine', 'feminine', 'neuter']

NOUN_FORMS = {
    'singular': {
        'nominative': 'snom',
        'genitive': 'sgen',
        'dative': 'sdat',
        'accusative': 'sacc',
        'vocative': 'svoc',
        'instrumental': 'sins'
        },
    'plural': {
        'nominative': 'pnom',
        'genitive': 'pgen',
        'dative': 'pdat',
        'accusative': 'pacc'
        }
    }

ADJ_FORMS = {
    'masculine': {
         'singular': {
            'nominative': 'msnom',
            'genitive': 'msgen',
            'dative': 'msdat',
            'locative': 'msloc',
            'instrumental': 'msins'
            },
        'plural': {
            'nominative': 'mpnom',
            'genitive': 'pgen'
            }
        },
    'feminine': {
         'singular': {
            'nominative': 'fsnom',
            'genitive': 'fsgen',
            'dative': 'fsdat',
            'accusative': 'fsacc'
            }
        },
    'neuter': {
         'singular': {
            'nominative': 'nsnom'
            }
        }
    }

VERB_FORMS = {
    'present': {
        'singular': {
            'first-person': 'pres_sg_1',
            'second-person': 'pres_sg_2',
            'third-person': 'pres_sg_3'
            },
        'plural': {
            'first-person': 'pres_pl_1',
            'second-person': 'pres_pl_2',
            'third-person': 'pres_pl_3'
            }
        },
    'participle': {
        'singular': {
            'masculine': 'ppart_masc_sg',
            'feminine': 'ppart_fem_sg',
            'neuter': 'ppart_neutr_sg'
            },
        'plural': {
            'masculine': 'ppart_masc_pl',
            'feminine': 'ppart_fem_pl',
            'neuter': 'ppart_neutr_pl'
            }
        }
    }



def get_forms(pos, forms):
    dict = {}
    if pos == 'noun':
        for f in forms:
            for g in GENDERS:
                if g in f.get('tags', []):
                    dict['gender'] = g
            tags = f.get('tags', [])
            for num in NOUN_FORMS:
                if num in tags:
                    for case in NOUN_FORMS[num]:
                        if case in tags:
                            dict[NOUN_FORMS[num][case]] = f['form']
    elif pos == 'adj':
        for f in forms:
            tags = f.get('tags', [])
            if 'positive' in tags and 'indefinite' in tags:
                for g in ADJ_FORMS:
                    if g in tags:
                        for n in ADJ_FORMS[g]:
                            if n in tags:
                                for c in ADJ_FORMS[g][n]:
                                    if c in tags:
                                        dict[ADJ_FORMS[g][n][c]] = f['form']
    elif pos == 'verb':
        for f in forms:
            tags = f.get('tags', [])
            for t in VERB_FORMS:
                if t in tags:
                    for n in VERB_FORMS[t]:
                        if n in tags:
                            for g in VERB_FORMS[t][n]:
                               if g in tags:
                                   dict[VERB_FORMS[t][n][g]] = f['form']

    else:
        dict['forms'] = forms[:10] ####
    return dict


def lexinfo(data):
    return {'pos': data['pos'],
            'word': data['word'],
            'forms': get_forms(data['pos'], data['forms'])
            }

def morpho(mylang, data):
    if data.get('lang', '') == mylang and (
            all([x in data for x in ['pos', 'word', 'forms']])):
        print(lexinfo(data))


def translations(mylang, reflang, data):
    if data.get('lang', '') == reflang and (
            all([x in data for x in ['pos', 'word']])):
        for t in [t for t in data.get('translations', [])
                    if t['lang'] == mylang]:
            print(data['word'], data['pos'], t.get('word'))


def main():
    if not sys.argv[1:]:
        print('usage: extract.py (morpho|trans) mylang reflang')
        return
    mode = sys.argv[1]
    mylang, reflang = MYLANG, REFLANG
    if sys.argv[3:]:
        mylang, reflang = sys.argv[2:]
    with open(FILE, "r", encoding="utf-8") as f:
        for line in f:
            data = json.loads(line)
            if mode == 'trans':
                translations(mylang, reflang, data)
            else:
                morpho(mylang, data)

if __name__ == '__main__':
    main()



# noun plata [{'form': 'pláta', 'tags': ['canonical', 'feminine']}, {'form': 'пла́та', 'tags': ['Cyrillic']}, {'form': '', 'source': 'Declension', 'tags': ['table-tags']}, {'form': 'plata', 'tags': ['nominative', 'singular'], 'source': 'Declension'}, {'form': 'plate', 'tags': ['nominative', 'plural'], 'source': 'Declension'}, {'form': 'plate', 'tags': ['genitive', 'singular'], 'source': 'Declension'}, {'form': 'plata', 'tags': ['genitive', 'plural'], 'source': 'Declension'}, {'form': 'plati', 'tags': ['dative', 'singular'], 'source': 'Declension'}, {'form': 'platama', 'tags': ['dative', 'plural'], 'source': 'Declension'}, {'form': 'platu', 'tags': ['accusative', 'singular'], 'source': 'Declension'}, {'form': 'plate', 'tags': ['accusative', 'plural'], 'source': 'Declension'}, {'form': 'plato', 'tags': ['singular', 'vocative'], 'source': 'Declension'}, {'form': 'plate', 'tags': ['plural', 'vocative'], 'source': 'Declension'}, {'form': 'plati', 'tags': ['locative', 'singular'], 'source': 'Declension'}, {'form': 'platama', 'tags': ['locative', 'plural'], 'source': 'Declension'}, {'form': 'platom', 'tags': ['instrumental', 'singular'], 'source': 'Declension'}, {'form': 'platama', 'tags': ['instrumental', 'plural'], 'source': 'Declension'}]

