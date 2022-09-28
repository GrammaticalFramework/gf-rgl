import json
import sys

# https://kaikki.org/dictionary/rawdata.html
# Tatu Ylonen: Wiktextract: Wiktionary as Machine-Readable Structured Data,
# Proceedings of the 13th Conference on Language Resources and Evaluation (LREC),
# pp. 1317-1325, Marseille, 20-25 June 2022. 

WIKTIONARY_FILE = 'data/raw-wiktextract-data.json'

MYLANG = 'Serbo-Croatian'
REFLANG = 'English'

MORPHO_OUTPUT_FILE = 'm.json'
TRANS_OUTPUT_FILE = 't.json'


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
    return data['word'], {
        'pos': data['pos'], 'forms': get_forms(data['pos'], data['forms'])}


# write morphology of mylang in m.json
def morpho(mylang, lines):
    with open(MORPHO_OUTPUT_FILE, 'w', encoding="utf-8") as file:
        for line in lines:
            data = json.loads(line)
            if data.get('lang', '') == mylang and (
                  all([x in data for x in ['pos', 'word', 'forms']])):
                word, info = lexinfo(data)
                file.write(json.dumps({word: info})+'\n')


# write translations from reflang to mylang in t.json
def translations(mylang, reflang, lines):
    with open(TRANS_OUTPUT_FILE, 'w', encoding="utf-8") as file:
        for line in lines:
            data = json.loads(line)
            if data.get('lang', '') == reflang and (
                  all([x in data for x in ['pos', 'word']])):
                for t in [t for t in data.get('translations', [])
                            if t['lang'] == mylang]:
                    file.write(json.dumps(
                        {data['word']:
                           {'pos': data['pos'],
                            'trans': t.get('word'),
                            'sense': t.get('sense')}
                        })+'\n')


def main():
    if not sys.argv[1:]:
        print('usage: extract.py (morpho|trans) mylang reflang')
        return
    mode = sys.argv[1]
    mylang, reflang = MYLANG, REFLANG
    if sys.argv[3:]:
        mylang, reflang = sys.argv[2:]
    with open(WIKTIONARY_FILE, "r", encoding="utf-8") as lines:
            if mode == 'trans':
                translations(mylang, reflang, lines)
            elif mode == 'morpho':
                morpho(mylang, lines)


if __name__ == '__main__':
    main()

