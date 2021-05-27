# morphodict: purely morphological unilingual dictionaries

Aarne Ranta 2020-03-02 -- 2021-05-27

UNDER CONSTRUCTION, INCOMPLETE AND BUGGY

## The vision

Vision 1: if you need the noun "stjärna" in Swedish, you will find it
as `MorphoDictSwe.stjärna_N`.

Vision 2: if you analyse a Swedish text that contains the word "stjärnornas", it will be returned as `MorphoDictSwe.stjärna_N`.

Vision 3: this will work for all words of Swedish and all other RGL languages. Only seldom will you need `ParadigmsSwe`.


## What is contained

The guiding principle is to provide a single source for each *lemgram* (i.e. linearization records, i.e. inflection table plus inherent features).
Functions names should be easy to guess:
- `baseform_Category`

Baseforms that have many different lemgrams are an exception.
They should be numbered as
- `lie_1_V` ("lie, lay, lain") 
- `lie_2_V` ("lie, lied lied")

Such distinctions are made in all cases where there are alternative inflections, even if there is no sense distinction:
- `learn_1_V` ("learn, learned, learned")
- `learn_1_V` ("learn, learnt, learnt")

Hence,
- no `variants` should appear in the MorphoDict
- no entries should be duplicated if their lemgrams are the same
- hence, in particular, sense distinctions do not result in different entries

The dictionary will also exclude *multiwords* consisting of several tokens.
Most of the time, even *compounds* written as single tokens should be excluded.
However, as the status of a compound is not always clear, and since they do not create spurious morphological analyses, they can be tolerated, in particular if extracted from legacy sources.


## Relevant categories 

In addition to sense distinctions, MorphoDict ignores subcategorizations.
One reason is that, just like senses (although in a lesser degree), they are open-ended and sometimes vague.
Another reason is that different subcategory variants overload morphological analysis.

The most numerous categories to be addressed are content words:
- `A`
- `Adv`
- `Interj`
- `N`
- `PN`
- `Symb`
- `V`

In addition, structural words should appear here with their native lemma names:
- `Conj`
- `Det`
- `IAdv`
- `IDet`
- `IP`
- `NP` (special NP-like "pronouns", such as "somebody")
- `Prep`
- `Pron` (in the RGL only covering personal pronouns)
- `Punct`
- `Subj`

Additional language-specific categories can be included if the reasons are clear.
They must then be defined in the `Ext` module for that language.

Following the model of Universal Tagset, we add a category `X` for unspecified words in `Ext`, with the linearization type `{s : Str}`.
Hence it can only be used for uninflected strings with unclear status.

## Naming

As stated before,
- `functionname` = `baseform_category` if there is a unique lemgram
- = `baseform_number_category` if there is a need to disambiguate

The disambiguation numbering should reflect the frequency or probability of the lemgram, but this is just a recommendation, since the frequency is not always known.

The baseform should be the native alphabet baseform in Unicode letters, which is as such a valid GF identifier.
However, if the word contains characters that are not legal in identifiers, the function name should be simply included in single quotes, rather than inventing transliterations.
If function names are formed by the API function `PGF.mkCId`, these conventions are automatically followed.


## Bootstrapping with `MkMorphoDict`

THIS WAS AN EARLY EXPERIMENT, TO BE UPDATED

Example run, English:

   gf -make ../english/DictEng.gf
   runghc MkMorphodict.hs DictEngAbs.pgf MorphoDictEng

Result: 64923 ->  56599 functions, of which 21679 could be compounds

Swedish, using a dump of SALDO (not available in these sources)
```
  cd saldo/
  runghc SaldoGF.hs
  # combine abs.tmp with Saldo.header to obtain Saldo.gf
  # combine cnc.tmp with SaldoSwe.header to obtain SaldoSwe.gf
  gf -make SaldoSwe.gf
  cd ..
  runghc MkMorphodict.hs saldo/Saldo.pgf MorphoDictSwe
```
  
