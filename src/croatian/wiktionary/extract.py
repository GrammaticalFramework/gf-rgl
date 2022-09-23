import json

# https://kaikki.org/dictionary/rawdata.html

FILE = 'raw-wiktextract-data.json'

MYLANG = 'Serbo-Croatian'

GENDERS = ['masculine', 'feminine', 'neuter']

NOUN_CASES = {
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



def get_forms(pos, forms):
    dict = {}
    if pos == 'noun':
        for f in forms:
            for g in GENDERS:
                if g in f.get('tags', []):
                    dict['gender'] = g
            tags = f.get('tags', [])
            for num in NOUN_CASES:
                if num in tags:
                    for case in NOUN_CASES[num]:
                        if case in tags:
                            dict[NOUN_CASES[num][case]] = f['form']
    return dict


def lexinfo(data):
    return {'pos': data['pos'],
            'word': data['word'],
            'forms': get_forms(data['pos'], data['forms'])
            }



if __name__ == '__main__':
    with open(FILE, "r", encoding="utf-8") as f:
        for line in f:
            data = json.loads(line)
            if data.get('lang', '') == MYLANG and (
                    all([x in data for x in ['pos', 'word', 'forms']])):
                print(lexinfo(data))


# noun plata [{'form': 'pláta', 'tags': ['canonical', 'feminine']}, {'form': 'пла́та', 'tags': ['Cyrillic']}, {'form': '', 'source': 'Declension', 'tags': ['table-tags']}, {'form': 'plata', 'tags': ['nominative', 'singular'], 'source': 'Declension'}, {'form': 'plate', 'tags': ['nominative', 'plural'], 'source': 'Declension'}, {'form': 'plate', 'tags': ['genitive', 'singular'], 'source': 'Declension'}, {'form': 'plata', 'tags': ['genitive', 'plural'], 'source': 'Declension'}, {'form': 'plati', 'tags': ['dative', 'singular'], 'source': 'Declension'}, {'form': 'platama', 'tags': ['dative', 'plural'], 'source': 'Declension'}, {'form': 'platu', 'tags': ['accusative', 'singular'], 'source': 'Declension'}, {'form': 'plate', 'tags': ['accusative', 'plural'], 'source': 'Declension'}, {'form': 'plato', 'tags': ['singular', 'vocative'], 'source': 'Declension'}, {'form': 'plate', 'tags': ['plural', 'vocative'], 'source': 'Declension'}, {'form': 'plati', 'tags': ['locative', 'singular'], 'source': 'Declension'}, {'form': 'platama', 'tags': ['locative', 'plural'], 'source': 'Declension'}, {'form': 'platom', 'tags': ['instrumental', 'singular'], 'source': 'Declension'}, {'form': 'platama', 'tags': ['instrumental', 'plural'], 'source': 'Declension'}]

