resource ResTMP = open Prelude, Predef in {

--------------------------------------------------------------------------------
-- General notes

-- ** Naming **
{-
I'm using the naming scheme for lincats and opers as explained here:
https://inariksit.github.io/gf/2018/08/28/gf-gotchas.html#my-naming-scheme-for-lincats-and-opers
-}

-- ** File structure **
-- The rest of this module is organised as follows:

      -----------------------------
      -- Grammatical categor(y|ies)

      {-
      General comments on the cat(s)

      params related to the cat(s)

      opers related to the cat(s)
      -}

--------------------------------------------------------------------------------
-- Nouns

{-The param Number comes from common/ParamX, and has the values Sg and Pl.
    * If your language doesn't have number, remove Number from all records.
    * If your language has number with more than 2 values, define your own number in this module
      (e.g. uncomment line 56) and use that Number instead of Number.

  The param Gender is defined here, and has the values Gender1 and Gender2.
  Currently it's only as a suggestion to be an inherent field in LinN.
    * If your language doesn't have gender, remove Gender from all records.
    * If your language has genders/noun classes, replace the placeholder Gender1 and Gender1
      with the actual values of your language (there can be more than 2!), and uncomment
      the g : Gender field from the definition of LinN.

  If your nouns inflect in more things, like case, you can do one of the following
    * Replace the placeholder cases on line 53 and make the table 2-dimensional, like this:
        oper LinN : Type = {s : Number => Case => Str ; …} ;
    * Make your own parameter that combines all the relevant features, like this:
        param NForm = Whatever | You | Need | For | Noun | Inflection ;
        oper LinN : Type = {s : NForm => Str ; …} ;
      This can be a good idea, if your inflection table has some gaps, i.e. not all combinations are in use
      See https://gist.github.com/inariksit/708ab9df2498e88bc63aedf5fc7be2f3#file-tables-gf-L48-L122 for explanation
 -}

param
  Gender = Gender1 | Gender2 ;   -- Just a placeholder, see lines 34-39 above
  Case = Case1 | Case2 | Case3 ; -- Just a placeholder, see lines 41-48 above
  Number = Sg
         | Pl
--       | Dual -- If your language has numbers other than Sg and Pl, add them to the parameter
         ;
  Person = P1 | P2 | P3 ;

oper
  LinN : Type = {
    s :
--      Case =>     -- uncomment if your language has case
        Number =>   -- variable number: table {Sg => "house" ; Pl => "houses"}
        Str ;
  --  g : Gender ;  -- inherent gender/noun class, if your language has that
    } ;

  -- Most often, the lincat for CN is the same as N, with possibly some additional fields.
  -- However, sometimes you need more fields than just the s field, e.g. to keep word order flexible, or to add clitics and make sure they attach to the head, not modifiers.
  -- If you don't know what the previous line means, you can get started with just a single s field.
  -- You'll notice later whether you need such a field or not.
  LinCN : Type = LinN
    -- ** {postmod/premod/… : Str} -- if needed
    ;

  LinPN : Type = {
    s : Str ;
    n : Number ; -- Proper nouns often have already an inherent number; you don't usually say "a Paris / many Parises"
    -- g : Gender ; -- inherent gender/noun class, if your language has that
  } ;

  -- For inflection paradigms, see http://www.grammaticalframework.org/doc/tutorial/gf-tutorial.html#toc56
  mkNoun : Str -> LinN = \str -> {
    s = table {
      _ => str -- TODO: actual morphology
      } ;
      -- If your nouns have gender, it should come here as inherent field.
      -- Usually you need to give the gender as an argument to mkNoun.
    } ;

  linCN : LinCN -> Str = \cn -> cn.s ! Sg
                     --      ++ cn.postmod   -- If there is another field, use here
                                ;

---------------------------------------------
-- Numeral

-- Used in NumeralTMP
  param
    CardOrd = NCard | NOrd ;

  oper
    LinNumeral : Type = {s : CardOrd => Str ; n : Number} ;

    mkNumeral : Str -> LinNumeral = \s -> {
      s = table {
        NCard => s ;
        NOrd  => s + "th"
        } ;
      n = Pl ; -- NB. probably singular for number 1
      } ;

---------------------------------------------
-- Pronoun

{-The param Person comes from common/ParamX, and has the values P1, P2 and P3.
    * If your language doesn't inflect in person, you may be able to remove Person from all records.
      - However, consider if it's really never present? How about e.g. reflexive ("myself", "yourself" etc?)
    * If your language is more fine-grained than {P1,P2,P3} x {Sg,Pl} (for instance gender and familiarity),
      you can define your own param. We provide an example called Agr to take inspiration from—remove if
      not needed, or use and refine if needed.
-}

param
  -- These params are just for inspiration, not used anywhere currently.
  Agr = SgP1             -- I
      | SgP2 Politeness  -- e.g. tū, tum, āp (Hindi) — note that the verb really inflects differently for all three!
      | SgP3 Gender      -- e.g. he, she (verb inflects the same, but distinction in reflexive: himself / herself)
      | FillInTheRestYourself ;
  Politeness = Intimate | Familiar | Polite ;

oper
  LinPron : Type = {
    s : Str ;
    -- Alternative if your language has case and pronouns inflect in case (e.g. English I/me/my, she/her/hers)
    -- s : Case => Str ;
    n : Number ;
    p : Person ;
    -- Alternative to the `n` and `p` fields:
    -- a : Agr -- sketched above, lines 97-103
    } ;

  mkPron : (_ : Str) -> Person -> Number -> LinPron = \str,per,num -> {
    s = str ;
    {- If there is case inflection, you need a table here
        table {
              _ => str -- Pronoun inflection is often irregular, so possibly this constructor requires several forms as argument, even if mkNoun is nice and regular
            } ;
    -}
    p = per ;
    n = num
    } ;

---------------------------------------------
-- NP

{-
In the RGL, a NP may come from a common noun, proper noun or pronoun.
Pronouns are the only ones that have an inherent person (nouns are almost always 3rd person! please give me counterexamples if you can think of any.)
So we can often say that NP's lincat is the same as Prons.

NB. for later, when you want to make Pron into possessives, you may need more fields in LinPron than in LinNP.
That's why I'm copying over the definition below, instead of the neater `LinNP : Type = LinPron`.
-}

  LinNP : Type = {
    s : Str ;
    -- Alternative: If anything inflects in case (nouns, pronouns), NP has to also inflect in case!
    -- s : Case => Str ;
    n : Number ;
    p : Person ;
    -- Alternative to the `n` and `p` fields:
    -- a : Agr -- sketched on lines 97-101
    } ;

  linNP : LinNP -> Str = \np -> np.s ; -- Change when you change LinNP

  emptyNP : LinNP = { -- Change when you change LinNP
    s = [] ;
    n = Sg ;
    p = P3 ;
  } ;

--------------------------------------------------------------------------------
-- Det, Quant, Card, Ord

  -- If your language has a number, it is very very very likely that
  -- Quant has a variable number and Det has inherent number.

  LinQuant : Type = {
    s,  -- quantifier in a context, e.g. 'this (cat) (is nice)'
    sp  -- quantifier as standalone, e.g. 'this (is nice)'
     : Number => Str ;
    } ;

  LinDet : Type = {
    s : Str ;
    n : Number ;
    } ;

  -- Can you reuse your mkNoun? Do nouns and quantifiers inflect the same way?
  mkQuant : Str -> Str -> LinQuant = \this, these -> {
    s,
    sp = table {
      Sg => this ;
      Pl => these } ;
    };

  mkDet : Str -> Number -> LinDet = \str, num -> {
    s = str ;
    n = num
  } ;

--------------------------------------------------------------------------------
-- Adpositions

{- The main use of Prep is in the fun

      PrepNP : Prep -> NP -> Adv

   Despite the name of the RGL category, a 'Prep' can be a preposition, postposition,
   or just an instruction to choose a particular case from the NP.
   A language may use one, two or all these strategies.

-}

oper
  LinPrep : Type = {
    s : Str ;

    -- If your language has cases, and different prepositions choose different cases.
    -- c2 : Case ;

    -- If your language has both pre- and postpositions, you need an inherent parameter in Prep to record which one a given Prep is.
    -- position : PreOrPost ;
    } ;


--------------------------------------------------------------------------------
-- Adjectives

  LinA : Type = SS ;
  LinA2 : Type = LinA ;

  mkAdj : Str -> LinA = \str -> {s = str} ;

  AdjPhrase : Type = LinA ; -- ** {compar : Str} ;
--------------------------------------------------------------------------------
-- Verbs

param
  VForm = TODOVF Number Person ;

oper
  LinV : Type = {
    s : VForm => Str
    } ;

  LinV2 : Type = LinV ** {
    c2 : LinPrep ;
    } ;

  mkVerb : Str -> LinV = \str -> {
    s = table {
      _ => str
      }
    } ;

  copula : LinV = {s = \\_ => "TODO: copula"} ; -- often useful

------------------
-- VP

  LinVP : Type = {
    s : VForm => Str ;
    } ;

  LinVPSlash : Type = LinVP ** {
    c2 : LinPrep ;
    } ;

  linVP : LinVP -> Str = \vp -> vp.s ! TODOVF Sg P3 ;

--------------------------------------------------------------------------------
-- Cl, S

  -- Operations for clauses, sentences
  LinCl : Type = {
    subj : Str ;
    pred : Str ; -- TODO: depend on Temp and Pol
  } ;

  linCl : LinCl -> Str = \cl -> cl.subj ++ cl.pred ;

}
