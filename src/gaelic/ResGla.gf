resource ResGla = open Prelude, Predef in {

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


param
  Gender = Masc | Fem ;
  CoreCase = Nom | Gen | Dat ;
  Case = CC CoreCase | Voc ;
  Number = Sg
         | Pl
         ;
  Person = P1 | P2 | P3 ;
  Definiteness = Definite | Indefinite ; -- Some prepositions govern different case when definite vs. indefinite

  NForm =
      Indef Number CoreCase
    | Def Number Case
    | Dual -- only after number 2, only for a handful of nouns. TODO: does it have different cases?
    ;

oper
  getNForm : Number -> Definiteness -> Case -> NForm = \n,d,c ->
     case <d,c> of {
      <Indefinite,Voc>  => Indef n Nom ; ---- ???
      <Indefinite,CC c> => Indef n c ;
      <Definite,c>      => Def n c
    } ;

  LinN : Type = {
    base,                -- tunnag     fuil      loch      fear    litir
    gen,                 -- tunnaige   fala      locha     fir     litreach ("de-palatalised")
    pl,                  -- tunnagan             lochan    fir     litrichean
    -- TODO: for nouns that only use suffixes, should these just show theoretical forms?
    lenited,             -- thunnag    fhuil     loch      fhear
    palatalised,         -- tunnaig    fuil      loch      fir
    lenited_palatalised  -- thunnaig   fhuil     loch      fhir
    : Str ;
    g : Gender
    } ;

  smartN = overload {
    smartN : (nom,gen,pl : Str) -> Gender -> LinN = \loch,locha,lochan,g -> {
      gen = locha ;
      pl = lochan ;
      base,
      lenited,
      palatalised,
      lenited_palatalised = loch ;
      g = g
      }
--      ;
--    smartN : (…,…,…,… : Str)
  } ;

  -- For inflection paradigms, see http://www.grammaticalframework.org/doc/tutorial/gf-tutorial.html#toc56
  mkNoun : (b,g,pl,l,p,lp : Str) -> Gender -> LinN = \b,gen,pl,l,p,lp,g -> {
    base = b ;                 -- tunnag     fuil      loch      fear    litir
    gen = gen ;                  -- tunnaige   fala      locha     fir     litreach
    pl  = pl ;                 -- tunnagan             lochan    fir     litrichean
    lenited = l ;              -- thunnag    fhuil     loch      fhear   litir ?
    palatalised = p ;          -- tunnaig    fuil      loch      fir     litir ?
    lenited_palatalised = lp ; -- thunnaig   fhuil     loch      fhir    litir ?
    g = g ;
    } ;

  -- TODO: no idea if this is even remotely correct
  -- can always replace morphology with Katya's automated tool
  useN : LinN -> LinCN = \n -> n ** {
    s = table {
          Indef Sg Nom => n.base ;
          Indef Sg Gen => n.gen ;
          Indef Sg Dat => fm n.palatalised n.base ;
          Def Sg (CC Nom) => n.base ;
          Def Sg (CC Gen) => fm n.gen n.lenited_palatalised ;
          Def Sg (CC Dat) => fm n.palatalised n.lenited ;
          Def Sg Voc => n.lenited ;
          Indef Pl Nom => n.pl ;
          Indef Pl Gen => n.base ;
          Indef Pl Dat => n.palatalised ;
          Def Pl (CC Nom) => n.pl ;
          Def Pl (CC Gen) => n.base ;
          Def Pl (CC Dat) => n.pl ;
          Def Pl Voc => glue n.lenited "a" ;
          Dual => "TODO FIXME I AM DUAL"}
    } where {
      fm : Str -> Str -> Str = \fem,masc -> case n.g of {
          Fem => fem ;
          Masc => masc
        }
      };

  LinCN : Type = {
    s : NForm =>
        Str ;
    g : Gender ;
    -- ** postmod/premod/… : Str -- if needed? determiners can put stuff after head but it only comes at NP
  } ;

  linCN : LinCN -> Str = \cn -> cn.s ! Indef Sg Nom
                     --      ++ cn.postmod   -- If there is another field, use here
                                ;


-- some test nouns — TODO: do smart paradigms
tunnag_N : LinN = {
    base = "tunnag" ;
    gen = "tunnaige" ;
    pl = "tunnagan" ;
    lenited = "thunnag" ;
    palatalised = "tunnaig" ;
    lenited_palatalised = "thunnaig" ;

    g = Fem ;
    } ;

boireannach_N : LinN = {
    base = "boireannach" ;
    pl,gen = "boireannaich" ;
    lenited = "bhoireannach" ;
    palatalised = "boireannaich" ;
    lenited_palatalised = "bhoireannaich" ;
    g = Masc ;
    } ;

---------------------------------------------
-- Proper noun

oper
  LinPN : Type = {
    s : Str ;
    n : Number ; -- Proper nouns often have already an inherent number; you don't usually say "a Paris / many Parises"
    g : Gender ; -- inherent gender/noun class, if your language has that
  } ;

---------------------------------------------
-- Numeral

-- Used in NumeralGla
  param
    CardOrd = NCard | NOrd ;

  oper
    LinNumeral : Type = {s : CardOrd => Str ; n : Number} ;

    mkNumeral : (card, ord : Str) -> LinNumeral = \card,ord -> {
      s = table {
        NCard => card ; -- aon(a) -- TODO: allomorph of this depends on the following word?
        NOrd  => ord    -- a' chiad
        } ;
      n = Pl ; -- NB. singular for 1, 2, 20 + multiples of 20 and 100 (Lamb, p. 218)
      } ;

---------------------------------------------
-- Pronoun


oper
  LinPron : Type = {
    s : Case => Str ;
    n : Number ;
    p : Person ;
    -- g : Gender ; ?? -- we have already he_Pron and she_Pron in abstract syntax, does this affect inflection?
    } ;

  mkPron : (_ : Str) -> Person -> Number -> LinPron = \str,per,num -> {
    s = \\_ => str ; -- Pronoun inflection is often irregular, so possibly this constructor requires several forms as argument, even if mkNoun is nice and regular
    p = per ;
    n = num ;
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


oper
  LinNP : Type = {
--  art : Str ; -- to be replaced with a combo coming from Prep, if argument of PrepNP? see Lamb p. 225
                -- TODO: is that an issue when the allomorph has been chosen by an inherent param in CN?
                -- does that param need to be kept in LinNP, and Prep need an inflection table from that param?
                -- or do we have an exhaustive list of prepositions that merge, and we can make that into a param and put on a LHS here?

    s : Case => Str ; -- TODO: is lenition a separate dimension from case?

    -- TODO can we make this combo of inherent params leaner?
    n : Number ;
    p : Person ;
    d : Definiteness ;
    } ;

  linNP : LinNP -> Str = \np -> np.s ! (CC Nom) ;

  emptyNP : LinNP = {
    s = \\_ => [] ;
    n = Sg ;
    p = P3 ;
    d = Indefinite ;
  } ;

--------------------------------------------------------------------------------
-- Det, Quant, Card, Ord

  -- If your language has a number, it is very very very likely that
  -- Quant has a variable number and Det has inherent number.

  LinQuant : Type = {
    s,  -- quantifier in a context, e.g. 'this (cat) (is nice)'
    sp  -- quantifier as standalone, e.g. 'this (is nice)'
     : Number => Str ;
    d : Definiteness ;
    } ;

  LinDet : Type = {
    s,s2 : Str ;
    n : Number ;
    d : Definiteness ;
    } ;

  LinNum : Type = {
    s : Str ;
    n : Number ;
    } ;

  -- Can you reuse your mkNoun? Do nouns and quantifiers inflect the same way?
  mkQuant : Str -> Str -> Definiteness -> LinQuant = \this,these,d -> {
    s,
    sp = table {
      Sg => this ;
      _ => these } ;
    d = d ;
    };

  mkDet : (seven, teen : Str) -> Number -> LinDet = \aon, deug, num -> {
    s = aon ;
    s2 = deug ;
    n = num ;
    d = Indefinite -- TODO fix
  } ;

--------------------------------------------------------------------------------
-- Adpositions

{- The main use of Prep is in the fun

      PrepNP : Prep -> NP -> Adv

   Despite the name of the RGL category, a 'Prep' can be a preposition, postposition,
   or just an instruction to choose a particular case from the NP.
   A language may use one, two or all these strategies.

-}

-- TODO: prepositions can merge with articles
-- Lamb, page 210: obair _sa_ cheàrdaich 'working _in+the_ forge'

-- more on preps: Lamb, p.224

oper
  LinPrep : Type = {
    s : Str ;

    c2 : Definiteness => Case ; -- most often dative


    -- If your language has both pre- and postpositions, you need an inherent parameter in Prep to record which one a given Prep is.
    -- position : PreOrPost ;

    -- Some cause lenition—is that separate from case?
    } ;


--------------------------------------------------------------------------------
-- Adjectives
-- Lamb p. 220 basic morphology, degree
-- Lamb p. 246: predicative adjectives

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
-- Lamb p. 229
-- "tense, aspect, modality, voice, person and number. There are contrasts to be seen, as above, between inflected and periphrastic forms and, as a whole, periphrasis is more productive."

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
