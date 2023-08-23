# TEMPLATE

This is a starting point to clone a new RGL language. It has some pre-populated lincats and lins, mostly in the `Noun` module, but also a few minimal things for verbs and sentences.

**Note that this is not 100% just strings.** Some of the lincats have a more complex lincat, like an inflection table with `Number` as a parameter. In addition, the modules contain comments and suggestions aimed for new grammarians.

**If you want a 100% just strings template**, you can find that in [github.com/daherb/gf-rgl-template](https://github.com/daherb/gf-rgl-template). If you choose that one, you can still read this document for suggestions about which functions to start with.



# How to use this tutorial

If you haven't done so yet, clone your language from this template as instructed [here](../README.md#from-a-generic-template). The cloning doesn't include README.md, so there's only one copy of this README document.

You can open the grammar in a GF shell and see its functions as follows. (I'm using here the `TMP` concrete syntax, but you should have cloned it to some other concrete syntax with a different extension, so substitute as necessary.)

```
$ gf LangTMP.gf
Lang> gr -depth=6  | l -treebank
Lang: PhrUtt NoPConj (UttS (UseCl (TTAnt TPres ASimul) PPos (PredVP (DetCN (DetQuant DefArt NumSg) (UseN blood_N)) (UseV die_V)))) NoVoc
LangTMP: the blood die
```

There are also a couple of unit tests in the [`unittest`](/unittest) directory. To see how to use them, see the [instructions](https://github.com/GrammaticalFramework/gf-rgl/tree/master/unittest#readme).


# Guided tour: what to implement first?

In this section, I group the RGL functions in clusters and suggest an implementation order. If you have different needs, e.g. you're making the resource grammar for a particular application and need specific RGL functions for that, feel free to prioritise your needs. I'm giving this list as a suggestion for people who just want something to start from.

## 1. N-CN-NP(-AP)

Most of these are in the Noun module. This is the cluster that has most work done in this template.

### Already implemented

With the following functions, it is possible to construct simple noun phrases.

- `DetCN`
- `DetQuant`
- `DefArt`, `IndefArt` (no problem if they are empty strings in your language!)
- `NumSg`, `NumPl`
- `MassNP`
- `UseN`
- `blood_N` (in Lexicon module)

You can see all NPs with the following command:

```bash
Lang> gt -cat=NP | l -treebank
```

### Next steps

#### More morphology
Check the categories and params in `ResTMP`: how well do they apply to your language? Is the initial implementation missing inflectional features that your language has, like case, gender/noun class, other numbers like dual?

If so, then I would suggest adding the missing morphology before implementing any new syntactic functions. Whenever you change a lincat, e.g. by making something that used to be a Str into an inflection table, all the lins that handle that lincat will break. So it's less painful to change the lincats when the amount of lins is still small.

#### More syntax
Once you're happy with the morphology, you can start with other lins and lincats. For instance:

- lincat for `Pron`, lin for `UsePron` and `PossPron`, plus some actual pronouns in Structural
- lincat for `PN`, lin for `UsePN`, plus some actual PNs in Lexicon
- `AdvCN`, plus some actual Advs in Lexicon
- lins for more `Quant`s, `Det`s etc. in Structural

Some things in the Noun module will have to wait for other categories to be done. For instance, `AdjCN` relies on adjectives, `RelCN` on relatives, `NumCard` and `NumNumeral` on numerals, none of which is (properly) implemented in this template. So feel free to leave all the rest for a later pass.

#### Side note: a word about MassNP
In the Noun module, there is a function called `MassNP : CN -> NP`. This is a *mass construction*, which is usually applied to mass nouns like "water".

However, the RGL does not contain a semantic distinction between mass and count nouns, and thus the `MassNP` function can be applied to any CN. Sometimes this results in semantically weird results.

As a resource grammarian, don't worry if `MassNP` applied to count nouns sounds weird. It's the application grammarian's problem to choose when to use MassNP and when DetCN. If `MassNP` sounds good when applied to mass nouns, then you're doing it right.

### How about adjectives?

In some languages, adjectives behave like nouns. In other languages, they behave like verbs. In yet other languages, they behave like neither. If your language happens to be one where adjectives are like nouns, it's pretty cheap to just implement adjectives here as well. The minimal set is as follows:

- lincat for `A` and `AP`
- lin for `PositA` and `AdjCN`

But if adjectives are rather like verbs (e.g. Korean), or just more complex than nouns (e.g. German), feel free to postpone it for later.

## 2. V-VP

### Already implemented

For verbs, we have much fewer things implemented: a single intransitive verb, and a function that elevates an intransitive verb into a VP.

* `UseV`
* `die_V` (in Lexicon module)

You can see all (=1) VPs with the following command.

```bash
Lang> gt -cat=VP | l -treebank
Langs: UseV die_V
LangTMP: die
```

### Next steps

#### Add morphology

Just like with nouns, look at the `VForm` param in the Res module, and add the inflectional features that are missing. If verbs are very complex in your language, it's fine to start with a smaller subset, e.g. only indicative mood, or only a couple of tenses.

Again, you should extend the `VForm` param, and change the lincats of `V` and `VP` in other ways, if needed. In addition, you could implement some morphological paradigms, so that you can add some verbs in the lexicon.

#### Add syntax

In addition to intransitive verbs (`V`), the GF RGL has a large set of verb subcategories. So now you can start adding lincats to `V2` (direct object), `VV` (verbal complement), `VS` (sentence complement) etc.

The most important are the following:

- lincat for `V2` and `VPSlash`
- lin for `SlashV2a` and `ComplSlash`

If you have done a thorough implementation on noun morphology, you might find it useful here. For instance, if verbs mark their arguments with cases, now is a great time to add those cases as *inherent* argument in the verbs.

Another way to make VPs is to use adjectives, noun phrases and adverbials as complements. If you haven't implemented adjectives yet, feel free to skip them at this step. But the other complements should be in reach already, so the next most important steps are the following:

- lincat for `Comp`
- lin for `CompNP`, (`CompAdv`) and `UseComp`
- (If you already have AP: lin for `CompAP`)

These functions don't care whether your language has an explicit copula or not. Just implement whatever strategy that it uses for non-V❋ predication.

## 3. Cl-S-Utt-Phr

At this level, there is rarely new morphology to be added, but there can be interesting decisions about e.g. word order or subordination.

### Already implemented

The following rarely need any changes. By the time an `Utt` is reached, the grammatical decisions should have been already made, and the lincats of `Utt` and `Phr` should be just `{s : Str}`.
- `PhrUtt`
- `NoPConj`, `NoVoc`
- `UttS`

The following functions, and the lincats they operate on, are implemented in the most naive only-strings way, and they need to be changed.
- `UseCl`
- `TTAnt`, `TPres`, `TPast`, `TFut`, `TCond`, `ASimul`, `AAnter`
- `PPos`, `PNeg`
- `PredVP`

### Next steps

If you have added verb inflection in the V❋ and VP categories, then you need to connect them to the Cl category. `PredVP : NP → VP → Cl` picks the correct person inflection from its VP argument, but any tense and polarity is still open. So in most languages, the lincat of `Cl` should have an inflection table, and only `UseCl : Temp → Pol → Cl → S` will choose the final form.

Sometimes even the lincat of `S` has an inflection table or it is discontinuous. That's because `S` can be used in a VP or an Adv, and in those cases, it may have a different word order or inflectional form than as standalone sentence.

If you're not sure whether the lincat of `S` should be still open for something, try to implement the following functions and see if it needs tweaking.

- `ComplVS : VS → S → VP`
- `SubjS : Subj → S → Adv`

### Unused or nonexistent forms?

What if your language has no form that corresponds to e.g. future anterior negative (*won't have walked*)? That's fine, you can put some other form in that slot and go on.

What if your language has tenses, aspects, moods, politeness forms or any others that aren't accessible via the core RGL? That's fine too, you can always create a language-specific extra module with functions that do access them. If you're working towards a specific application that needs such forms, then you should of course prioritise them. But if covering the core RGL that is in the API is the most important, feel free to postpone all the verbal inflection that is not accessible via the core.

# Choose your own adventure: what to implement next

If you've implemented the first cluster, you already have a nice chunk of the RGL! You have tackled many of the hard decisions, so it's natural that these things can take a long time, and you may need to revise often.

The following set doesn't have to be followed in any particular order.

## Questions
Shares similarities with the implementation of declarative clauses and sentences. If you did clauses recently, it's a natural continuation to do questions: here you may need to tackle more word orders.
Forming of IPs (interrogative phrases) may be similar to NPs.

To get started, the easier ones in the Question module are the following:
- lincat for `IP` and `IDet`, lin for `IDetCN` and `IdetQuant`
- lincat for `QCl`, lin for `QuestCl` and `QuestVP`
- lincat for `IAdv`, lin for `QuestIAdv` (+ some `IAdv`s in Structural!)
- lincat for `IComp`, lin for `CompIAdv` and `QuestIComp`

## Adjectives

If you haven't implemented adjectives yet, it's about time! If your adjectives are more of the nouny type, I hope it's rather straightforward to do them. The minimal cluster is the following:

- lincat for `A` and `AP`
- lin for `PositA` and `AdjCN`
- lin for `CompAP`, check whether you have to update lincat for `Comp`

If adjectives behave like verbs, then the lincat for `Comp` and lin for `CompAP` can reuse the lincats and lins of the V-VP cluster. But `AdjCN` can be a bit difficult. Based on previous RGL languages that have verby adjectives, you get a lot of synergy with the Relative module. Basically, APs as modifiers behave just like relative clauses, so `AdjCN` and `RelCN` are similar or even identical.

## Relative clauses

These may be complicated, so feel free to postpone until further. But if your APs are verby, it makes sense to implement these in parallel with `AdjCN`, because you will need some way of making verby/clause-y things into modifiers.

## Numerals

There is a tentative lincat for numerals, and linearisations for the digits `D_0..D_9` and `n2..n9`, as well as the simple coercions `pot0`, `pot0as1`, `pot1as2`, `pot2as3` and `num`. However, it's possible that the simple lincat needs to be changed, and so I haven't implemented any of the lins that do something complex.

The numeral module the oldest piece of code in the RGL, and hence it looks pretty strange compared to the rest of the RGL. If you don't understand it, don't worry–just leave it aside until you have other parts implemented. Nothing depends on it, and in fact, I would recommend that your N-CN-NP cluster is really solid before you do numerals, because then you know better which inflectional features are needed in numerals.

But eventually the time comes to tackle numerals. First tip is to check in https://github.com/GrammaticalFramework/gf-contrib/tree/master/numerals whether someone has already implemented them for your language, or a close relative that behaves similarly. Second tip is to look at the existing implementation of any RGL language that you know, and try to reverse engineer based on that. But even if these tips don't work, please submit your grammar to gf-rgl anyway! A grammar without full numeral implementation is much better than no grammar at all.

Once you have some kind of implementation of the Numeral module, you can connect it to the Noun module by implementing the following. The minimal meaningful set is these two:

- `NumNumeral : Numeral -> Card`
- `NumCard : Card -> Num`

With these, you get a `Num` that can be used in `DetQuant` to make a Det, and that unlocks numerals as determiners, like "two cats".

### Conjunctions



### Idioms



### Functions that are clearly lower priority

What is low or high priority depends on the application. But if you want some general guidelines, these are usually less used, or not in the API at all.