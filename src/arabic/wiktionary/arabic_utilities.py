# utilities for Arabic script
# in the main mode, converts string literals in stdin 'to' or 'from' Buckwalter
# as specified by the command line argument:
#
#   % python3 arabic_utilities.py to <MorphoDictAra.gf | python3 arabic_utilities.py from >b.tmp
#   % diff MorphoDictAra.gf b.tmp 
#   %

import unicodedata

def is_arabic(s):
    return s and any(1574 <= ord(c) <= 1616 for c in s)


def get_arabic(s):
    return ''.join([c for c in s if is_arabic(c)])


def unvocalize(s):
    return ''.join([c for c in s if 0x621 <= ord(c) <= 0x64a])


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

arabic_vowels = {chr(c) for c in {0x64b, 0x64c, 0x64d, 0x64e, 0x64f, 0x650}}

sound_consonants = {chr(c) for c in range(0x628, 0x648)}  # excluding alif, waw, ya

def to_buckwalter(s):
    return ''.join([buckwalter_dict.get(ord(c), c) for c in s])


def from_buckwalter(s):
    return ''.join([buckwalter_dict_rev.get(c, c) for c in s])


def drop_final_vowel(s):
    if s[-1] in arabic_vowels:
        return s[:-1]
    else:
        return s


def normal(s):
    return unicodedata.normalize('NFD', s)

# heuristic for finding the three radicals from certain forms
# works only for sound (strong) 3-radical roots, otherwise None
def get_sound_trigram_root(s):
    sounds = [c for c in s if c in sound_consonants]
    if len(sounds) == 3:
        return ''.join(sounds)
    else:
        return None

    
# reverse engineer fcl pattern from a given form, with a sound trigram root
# one more condition: each of the root letters occurs exactly ones
# TODO: better use the given root of the lex entry
def get_sound_fcl_pattern(s):
    if root := get_sound_trigram_root(s):
        if len([c in s for c in root]) == 3:
            p = list(s)
            r = s.find(root[0])
            p[r] = chr(0x641)
            r += s[r+1:].find(root[1]) + 1
            p[r] = chr(0x639)
            r += s[r+1:].find(root[2]) + 1
            p[r] = chr(0x644)
            p = ''.join(p)
##            print('---PATT', s, root, p)
            return p
    

# Wikt uses vowel+shadda which is a Unicode normalization
# GF uses shadda+vowel which is linguistically correct
# see https://stackoverflow.com/questions/58559390/in-unicode-should-u0651-arabic-shadda-be-before-or-after-kasra
# unicodedata.normalize does this wrong, as noted by Ariel Gutman 
## todo: more direct implementation
def reorder_shadda(s):
    return from_buckwalter(to_buckwalter(s).replace('a~', '~a').replace('u~', '~u').replace('i~', '~i'))


# quote word forms but not parameters
def quote_if(s, cond=is_arabic, change=reorder_shadda):
    if cond(s):
        return '"' + change(s) + '"'
    else:
        return s


# for a string, change each string literal in "..." with a change function
# leaving other characters as they are; print the string to stdout as you go
def change_literals(s, change):
    inliteral = False
    literal = ''
    for c in s:
        if c == '"' and inliteral:
            print('"'+change(literal)+'"', end='')
            inliteral = False
            literal = ''
        elif c == '"':
            inliteral = True
        elif inliteral:
            literal += c
        else:
            print(c, end='')


# convert literals in stdin 'to' or 'from' Buckwalter
if __name__ == '__main__':
    import sys
    mode = sys.argv[1]
    for line in sys.stdin:
        if mode == 'from':
            change_literals(line, from_buckwalter)
        elif mode == 'to':
            change_literals(line, to_buckwalter)

        
