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

TODO: Most frequent copula in present tense requires Nom, so needs to be done via Extend / Idioms. For example:

TODO: Check: Old
```
> l CAdvAP less_CAdv (ComplA2 married_A2 (UsePron youPl_Pron)) (MassNP (ApposCN (UseN2 brother_N2) (UsePron youSg_Pron)))
менее замужем за вами менее брата тебя
```
New:

```
Lang: CAdvAP less_CAdv (ComplA2 married_A2 (UsePron youPl_Pron)) (MassNP (ApposCN (UseN2 brother_N2) (UsePron youSg_Pron)))
LangRus: менее замужем за вами , чем брат ты
```

TODO: Check:
```
Lang: UseCl (TTAnt TPres ASimul) PPos (GenericCl (UseComp (CompAdv already_Adv)))
LangRus: уже — ты
```

```
> l DetCN (DetQuant (PossPron i_Pron) NumSg)  (ConjCN and_Conj (ConsCN (UseN grammar_N) (BaseCN (UseN apple_N) (UseN flower_N))))
мой грамматика , яблоко и цветок
```

```
> l UseCl (TTAnt TCond ASimul) PPos (GenericCl (ComplVV can_VV hungry_VP))
мог быть бы ты голоден
```

```
> cc mkUtt (mkNP (mkDigits n4_Dig) (mkCN old_A apple_N))
{s = "4" ++ "старого" ++ "яблока"; lock_Utt = <>}
11 msec
> cc mkUtt (mkNP (mkDigits n4_Dig) (mkCN apple_N))
{s = "4" ++ "яблока"; lock_Utt = <>}
```

## Maintainer
