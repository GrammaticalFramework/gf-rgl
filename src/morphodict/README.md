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
They should be disambiguated by adding the differing forms, as in
- `lie_lay_V` ("lie, lay, lain") 
- `lie_lied_V` ("lie, lied lied")

Such distinctions are made in all cases where there are alternative inflections, even if there is no sense distinction:
- `learn_learned_V` ("learn, learned, learned")
- `learn_learnt_V` ("learn, learnt, learnt")

Hence,
- no `variants` should appear in the MorphoDict
- no entries should be duplicated if their lemgrams are the same
- hence, in particular, sense distinctions do not result in different entries

The dictionary will also exclude *multiwords* consisting of several tokens.
Most of the time, even *compounds* written as single tokens should be excluded.
However, as the status of a compound is not always clear, and since they do not create spurious morphological analyses, they can be tolerated, in particular if extracted from legacy sources.

Since multiwords and compounds are excluded, `Paradigms` and `MakeStructural` should for each language provide API functions for easy definitions of them, preferably of the form
```
 mkC : Str -> C -> C
```
The situation when this is not enough is when separate functions are needed for gluing and concatenation compounds.

*Open question*: what to do with compound prepositions that are common in e.g. English?
The above principles imply
```
 according_to_Prep = mkPrep "according" to_Prep
```
defined *outside* `MorphoDictEng`, so that `mkPrep` comes from `ParadigmsEng` and `to_Prep` from `MorphoDictEng`.
This may sound like against tradition, but follows the general guidelines of morphological dictionaries.


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
They must then be importable from the `Paradigms` module for that language, together with `mk` functions.
The `Extend` module may also put them in use in syntax.

Following the model of Universal Tagset, we add a category `X` for unspecified words in `Extend`, with the linearization type `{s : Str}`.
Hence it can only be used for uninflected strings with unclear status.

## Naming

As stated before,
- `functionname` = `baseform_category` if there is a unique lemgram
- = `baseform_number_category` if there is a need to disambiguate

The disambiguation numbering should reflect the frequency or probability of the lemgram, but this is just a recommendation, since the frequency is not always known.

The baseform should be the native alphabet baseform in Unicode letters, which is as such a valid GF identifier.
However, if the word contains characters that are not legal in identifiers, the function name should be simply included in single quotes, rather than inventing transliterations.
If function names are formed by the API function `PGF.mkCId`, these conventions are automatically followed.


## Coding conventions

To enable easy ocular and automatic inspection,
- write one entry per line, each prefixed by `fun` or `lin` keyword
- sort the entries alphabetically
- use paradigms with enough many arguments to make the characteristic forms explicit

To guarantee compatibility with the rest of the RGL and application grammars,
- paradigms used should be imported from `Paradigms` and `MakeStructural` rather than defined in `MorphoDict` itself
- import of *low-level modules* such as `Res` should be avoided
- `MorphoDict` should be self-contained, i.e. not inherit from other modules such as `Structural` or `Irreg`. But it is OK to `open` them in a qualified mode to use when defining linearizations.



## Bootstrapping with `MkMorphoDict`

Example run, English:
```
  gf -make ../english/DictEng.gf
  runghc MkMorphodict.hs pgf MorphoDictEng.config DictEngAbs.pgf MorphoDictEng
  ```
Or, if you have raw data from another source, of the format "N woman women", you can do
```
  runghc MkMorphodict.hs raw MorphoDictEng.config raw_words_eng.txt MorphoDictEng
  ```
The script needs a *configuration file* mapping legacy categories and forms lists to parts of GF code:
```
  N : N mkN 0 2
  A : A mkA 0 2 4 6
  V : V mkV 0 4 2
  V2 : V mkV 0 4 2
  Adv : Adv mkAdv 0
  Prep : Prep mkPrep 0
```
In addition, it needs *header files* containing lines to be prefixed to the generated files:
```
  concrete MorphoDictEng of MorphoDictEngAbs =
    CatEng [N,A,V,Adv,Prep] **
    open
      ParadigmsEng
    in
   {
```
```
  abstract MorphoDictEngAbs =
    Cat [N,A,V,Adv,Prep] **
  {
```
For more details, we refer to `MkMorphodict.hs` for the time being.

If the config and header files are sound, the script produces compilable GF files.
They also mostly comply to the guidelines given in this document.

Some things TODO:
- deal with multiwords such as "more regular" generated by Paradigms
- use references to native Irreg files instead of very long smart paradigms
- support increments in addition to overwrites



## Things to do

To support the construction of a `MorphoDict`, the following should be provided in `Paradigms`:
- explicit smart paradigms with characteristic forms and inherent features for each category
- API constants for all inherent features that are needed
- compound-constructing functions for all categories that need them
- the extra categories that one wants to include in that language


