# Russian

Russian Resource Grammar v.2.

## Language information
- English name: Russian
- Autonym: русский
- ISO code: Rus

## Authors

* Roman Suzi

Borrowed heavily from older Russian Resource Grammar: ...

Lexicon information from ru.wiktionary.org

## Implementation information

See also [References](references.txt) and [some theory](theory.txt)

### Morphology

The [ZaliznyakRus](ZaliznyakRus.gf) module contains inflection tables for
Russian nouns and adjectives using a subset of algorithms by A.A.Zaliznyak (see http://gramdict.ru ).

### Known issues

See [TODO](todo.txt).

### Test

See [test_rus_grammar.gfs](test_rus_grammar.gfs).

Can be invoked: as `gf --run AllRus.gf < test_rus_grammar.gfs  > test_res.treebank`.

## Maintainer
