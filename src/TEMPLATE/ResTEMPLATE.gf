resource ResTEMPLATE = ParamX ** open Prelude, Predef in {

--------------------------------------------------------------------------------
-- General notes

-- ** Naming **
{-
I'm using the naming scheme for lincats and opers as explained here:
https://inariksit.github.io/gf/2018/08/28/gf-gotchas.html#my-naming-scheme-for-lincats-and-opers
-}

-- ** File structure **
-- The rest of this module is organised as follows:

      ------------
      -- GF cat(s)

      {-
      General comments on the cat(s)

      params related to the cat(s)

      opers related to the cat(s)
      -}

--------------------------------------------------------------------------------
-- Nouns

{-The param Number comes from common/ParamX, and has the values Sg and Pl.
    * If your language doesn't have number, remove Number from all records.
    * If your language has number with more than 2 values, define your own number here
      (or in a separate ParamTEMPLATE module if you like) and use that instead of ParamX.Number.

  The param Gender is defined here, and has the values Gender1 and Gender2.
  Currently it's only as a suggestion to be an inherent field in LinN.
    * If your language doesn't have gender, remove Gender from all records.
    * If your language has genders/noun classes, replace the placeholder Gender1 and Gender1
      with the actual values of your language (there can be more than 2!), and uncomment
      the g : Gender field from the definition of LinN.

  If your nouns inflect in more things, like case, you can do one of the following
    * Make the table 2-dimensional, like this:
        param Case = Whatever | Cases | You | Have ;
        oper LinN : Type = {s : ParamX.Number => Case => Str ; …} ;
    * Make your own parameter that combines all the relevant features, like this:
        param NForm = Whatever | You | Need | For | Noun | Inflection ;
        oper LinN : Type = {s : NForm => Str ; …} ;
      This can be a good idea, if your inflection table has some gaps, i.e. not all combinations are in use
      See https://gist.github.com/inariksit/708ab9df2498e88bc63aedf5fc7be2f3#file-tables-gf-L48-L122 for explanation
 -}

param
  Gender = Gender1 | Gender2 ; -- Just a placeholder, see lines 11-16 above

oper
  LinN : Type = {
    s : ParamX.Number => Str ; -- variable number
  --  g : Gender ;        -- inherent gender/noun class
    } ;

  -- Very often, the lincat for CN is the same as N, with possibly some additional fields.
  -- Here we prepare for a postmodifier, just in case.
  -- If you find that you don't need such a field, feel free to remove it and put everything in s.
  LinCN : Type = LinN
    -- ** {postmod : Str}  -- heavy stuff like relative clauses might behave weirdly and maybe you need a different field?
    ;

  LinPN : Type = {
    s : Str ;
    n : ParamX.Number ; -- Proper nouns often have already an inherent number; you don't usually say "a Paris / many Parises"
    -- g : Gender ; -- inherent gender, if your language uses one
  } ;

  -- For inflection paradigms, see http://www.grammaticalframework.org/doc/tutorial/gf-tutorial.html#toc56
  mkNoun : Str -> LinN = \str -> {
    s = table {
      _ => str -- TODO: actual morphology
      } ;
      -- If your nouns have gender, it should come here as inherent field
      -- Usually you need to give the gender as an argument to mkNoun, and then just put it here
    } ;


---------------------------------------------
-- Pronoun

{-The param Person comes from common/ParamX, and has the values P1, P2 and P3.
    * If your language doesn't inflect in person, you may be able to remove Person from all records.
      - However, consider if it's really never present? How about e.g. reflexive ("myself", "yourself" etc?)
    * If your language is more fine-grained than {P1,P2,P3} x {Sg,Pl} (for instance gender and familiarity),
      you can define your own param. We provide an example called Agr to take inspiration from, remove if
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
    s : Str ; -- If there is case in your language, do pronouns inflect in that?
    n : ParamX.Number ;
    p : ParamX.Person ;
    -- Alternative to the `n` and `p` fields:
    -- a : Agr -- sketched above, lines 97-101
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
    s : Str ; -- If anything inflects in case (nouns, pronouns), NP has to also inflect in case.
    n : ParamX.Number ;
    p : ParamX.Person ;
    -- Alternative to the `n` and `p` fields:
    -- a : Agr -- sketched on lines 97-101
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

---- ******* Everything below: TODO

--------------------------------------------------------------------------------
-- Prepositions

  LinPrep : Type = {
    s : Str ;             -- dengan
--    c2 : Case ; -- if your language has cases, and if adverbials can be just like, the word in a case
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
  VForm = TODOVF ;

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

  copula : LinV = {s = \\_ => "TODO"} ; -- often useful

------------------
-- VP

  LinVP : Type = {
    s : VForm => Str ;
    } ;

  VPSlash : Type = LinVP ** {
    c2 : LinPrep ;
    } ;

--  linVP : LinVP -> Str = \vp -> vp.s ! Active ! Pos;


--------------------------------------------------------------------------------
-- Cl, S



--------------------------------------------------------------------------------
-- linrefs

}
