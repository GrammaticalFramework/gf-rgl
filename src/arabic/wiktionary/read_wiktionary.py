import gzip
import json

WIKTIONARY_DUMP = 'raw-wiktextract-data.json.gz'


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
    return ''.join(list(map(lambda c: buckwalter_dict.get(ord(c), '?'), s)))


def is_arabic(s):
    return s and any(1574 <= ord(c) <= 1616 for c in s)

"""
with open('wikt_arabic.jsonl') as file:
    for line in file:
        obj = json.loads(line)
        if 'Arabic lemmas' in obj.get('categories', []):
            entry = {
                'pos': obj['pos'],
                'forms': {form['form']: form.get('tags', []) for
                          form in obj.get('forms', []) if
                          'romanization' not in form.get('tags', []) and
                          is_arabic(form['form'])
                          },
                'senses': obj.get('senses', [])
                }
            entry['n_forms'] = len(entry['forms'])
            print(entry['pos'], entry['n_forms'])
#            print(json.dumps(entry, ensure_ascii=False))
"""
