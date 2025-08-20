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
  Case = Nom | Gen | Dat | Voc ;
  Number = Sg
         | Pl
         | Dual -- only after number 2
         ;
  Person = P1 | P2 | P3 ;
  Definiteness = Def | Indef ; -- Some prepositions govern different case when definite vs. indefinite


oper
  LinN : Type = {
    base,                -- tunnag     fuil      loch
    lenited,             -- thunnag    fhuil     loch
    palatalised,         -- tunnaig    fuil      loch
    lenited_palatalised, -- thunnaig   fhuil     loch
    suffixE,             -- tunnaige    fuile     loche
    lenited_suffixA,     -- thunnaga   fala      locha
    suffixAn             -- tunnagan  nonExist  lochan
    : Str ;
    g : Gender
    } ;


{-

PL.VOC: lenited + non-palatalized + -a

class 1 masculine noun
    boireannach   SG.*.NOM, SG.INDEF.DAT, PL.DEF.GEN
    boireannaich  SG.INDEF.GEN, PL.*.NOM, PL.*.DAT
    bhoireannach  PL.INDEF.GEN, SG.DEF.DAT
    bhoireannaich SG.DEF.GEN, SG.VOC
    bhoireannacha PL.VOC

  indefinite
                singular	plural
      nominative	boireannach	boireannaich
      genitive	boireannaich	bhoireannach
      dative	boireannach	boireannaich; boireannachaibh✝
  definite
                  singular	    plural
    nominative	(am) boireannach	(na) boireannaich
    genitive	(a') bhoireannaich	(nam) boireannach
    dative	(a') bhoireannach	(na) boireannaich; boireannachaibh✝
    vocative	bhoireannaich	       bhoireannacha


class 2 feminine noun
  not affected by lenition?
    làmh    SG.*.NOM, PL.*.GEN, SG.VOC
           làmh (imagine there was lenition)  PL.INDEF.GEN, SG.VOC
    làimh   SG.*.DAT
    làimhe  SG.*.GEN
    làmhan  PL.*.NOM, PL.*.DAT
    làmha   PL.VOC

  indefinite
                singular	plural
    nominative	làmh	làmhan
    genitive	  làimhe	làmh
    dative	   làimh	làmhan
  definite
                singular	  plural
    nominative	(an) làmh	  (na) làmhan
    genitive	(na) làimhe	  (nan) làmh
    dative	(an) làimh	    (na) làmhan
    vocative	   làmh	        làmha

class 2a feminine noun
    tunnag    SG.*.NOM, PL.DEF.GEN
    thunnag   PL.INDEF.GEN, SG.VOC
    tunnaig   SG.*.DAT, SG.*.GEN (allomorph (secondary?)),
    tunnaige  SG.*.GEN (allomorph (primary?))
    tunnagan  PL.*.NOM, PL.*.DAT
    thunnaga  PL.VOC

  indefinite
                  singular	plural
      nominative	tunnag	tunnagan
      genitive	tunnaige, thunnag
                tunnaig
      dative	tunnaig	    tunnagan
  definite
                    singular	  plural
      nominative	(an) tunnag	  (na) tunnagan
      genitive	(na) tunnaige,  (nan) tunnag
                    tunnaig
      dative	  (an) tunnaig	  (na) tunnagan
      vocative	  thunnag	      thunnaga

class 3


-}

  LinPN : Type = {
    s : Str ;
    n : Number ; -- Proper nouns often have already an inherent number; you don't usually say "a Paris / many Parises"
    g : Gender ; -- inherent gender/noun class, if your language has that
  } ;

  -- For inflection paradigms, see http://www.grammaticalframework.org/doc/tutorial/gf-tutorial.html#toc56
  mkNoun : (b,l,p,lp,se,sa,lsa,san : Str) -> Gender -> LinN = \b,l,p,lp,se,sa,lsa,san,g -> {
    base = b ;                 -- tunnag     fuil      loch
    lenited = l ;              -- thunnag    fhuil     loch
    palatalised = p ;          -- tunnaig    fuil      loch
    lenited_palatalised = lp ; -- thunnaig   fhuil     loch
    suffixE = se ;             -- tunnaige   fuile     loch
    suffixA = sa ;             -- tunnaga    fala      locha
    lenited_suffixA = lsa ;    -- thunnaga   fala      locha
    suffixAn = san ;           -- tunnagan
    g = g ;
    } ;

  -- TODO: no idea if this is even remotely correct
  -- can always replace morphology with Katya's automated tool
  useN : LinN -> LinCN = \n -> n ** {
    s = table {
          Pl => table {
                  Indef => table {
                    Nom|Dat => fm n.suffixAn n.palatalised ;
                    Gen => n.lenited ;
                    Voc => n.lenited_suffixA } ;
                  Def => table {
                    Nom|Dat => fm n.suffixAn n.palatalised ;
                    Gen => n.base ;
                    Voc => fm n.lenited n.lenited_palatalised }
                  } ;
          _ => table { -- Sg and Dl
                  Indef => table {
                    Nom => n.base ;
                    Gen => fm n.suffixE n.palatalised ;
                    Dat => fm n.palatalised n.base ;
                    Voc => fm n.lenited n.lenited_palatalised } ;
                  Def => table {
                    Nom => n.base ;
                    Gen => fm n.suffixE n.lenited_palatalised ;
                    Dat => fm n.palatalised n.lenited ;
                    Voc => fm n.lenited n.lenited_palatalised }
                }
              }
    } where {
      fm : Str -> Str -> Str = \fem,masc -> case n.g of {
          Fem => fem ;
          Masc => masc
        }
      };

  LinCN : Type = {
    s : Number =>
        Definiteness => -- ???? is this needed ??????
        Case =>
        Str ;
    g : Gender ;
    -- ** postmod/premod/… : Str -- if needed? determiners can put stuff after head but it only comes at NP
  } ;

  linCN : LinCN -> Str = \cn -> cn.s ! Sg ! Indef ! Nom
                     --      ++ cn.postmod   -- If there is another field, use here
                                ;


-- some test nouns — TODO: do smart paradigms
tunnag_N : LinN = {
    base = "tunnag" ;
    lenited = "thunnag" ;
    palatalised = "tunnaig" ;
    lenited_palatalised = "thunnaig" ;
    suffixE = "tunnaige" ;
    suffixA = "tunnaga" ;
    lenited_suffixA = "thunnaga" ;
    suffixAn = "tunnagan" ;
    g = Fem ;
    } ;

boireannach_N : LinN = {
    base = "boireannach" ;
    lenited = "bhoireannach" ;
    palatalised = "boireannaich" ;
    lenited_palatalised = "bhoireannaich" ;
    suffixE = "bhoireannaiche" ;
    suffixA = "boireannacha" ;
    lenited_suffixA = "bhoireannacha" ;
    suffixAn = "boireannachan" ;
    g = Masc ;
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

  linNP : LinNP -> Str = \np -> np.s ! Nom ;

  emptyNP : LinNP = {
    s = \\_ => [] ;
    n = Sg ;
    p = P3 ;
    d = Indef ;
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
    s,s2 : Str ;
    n : Number ;
    } ;

  -- Can you reuse your mkNoun? Do nouns and quantifiers inflect the same way?
  mkQuant : Str -> Str -> LinQuant = \this, these -> {
    s,
    sp = table {
      Sg => this ;
      _ => these } ;
    };

  mkDet : (seven, teen : Str) -> Number -> LinDet = \aon, deug, num -> {
    s = aon ;
    s2 = deug ;
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
