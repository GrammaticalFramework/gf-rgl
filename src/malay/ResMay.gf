resource ResMay = ParamMay ** open Prelude, Predef, ParamMay in {

--------------------------------------------------------------------------------
-- Nouns
oper

  Noun : Type = {
    s : NForm => Str
    } ;
  Noun2 : Type = Noun ; -- TODO eventually more parameters?
  Noun3 : Type = Noun ;

  CNoun : Type = Noun ** {
    } ;

  PNoun : Type = Noun ;

  mkNoun : Str -> Noun = \anjing -> {
    s = table {
      NF Sg p => anjing + ParamMay.poss2str p ;
      NF Pl p => duplicate anjing + ParamMay.poss2str p
      }
    } ;

  useN : Noun -> CNoun = \n -> n ;

---------------------------------------------
-- NP

  NounPhrase = {
    s : Str ;
    p : Person -- for relative clauses
    } ;

--------------------------------------------------------------------------------
-- Pronouns

  Pronoun : Type = NounPhrase ** {
    -- poss : { -- for PossPron : Pron -> Quant
    --   } ;
    sp : NForm => Str ;
    } ;


--------------------------------------------------------------------------------
-- Det, Quant, Card, Ord

  BaseQuant : Type = {
    s : Str ;
    isPoss : Bool
    } ;

  Determiner : Type = BaseQuant ** {
    sp : NForm => Str ;
    n : Number ;
--    numtype : NumType ; -- number as in "5" or "Sg/Pl", often makes a difference in lots of things
    } ;

  Quant : Type = BaseQuant ** {
    sp : NForm => Str ;
    } ;

  Num : Type = {
    s : DForm => Str ; -- independent or attribute
    n : Number
    } ;

  baseNum : Num = {
    s = \\_ => [] ;
    n = Sg ;
    numtype = NoNum
    } ;

  Numeral : Type = Num ** {
    ord : Str
    } ;

  baseQuant : BaseQuant = {
    s = [] ;
    isPoss = False ;
    } ;

  mkQuant : Str -> Quant = \str -> baseQuant ** {
    s = str ;
    sp = \\_ => str
    } ;

  --------------------------------------------------------------------------------
-- Prepositions

  Preposition : Type = {
    s : Str ;           -- dengan
    obj : Person => Str -- dengan+nya -- needed in relative clauses to refer to the object
    } ;

  mkPrep : Str -> Preposition = \dengan -> {
    s = dengan ;
    obj = \\p => dengan + poss2str (Poss p)
    } ;

  emptyPrep : Preposition = {
    s = [] ;
    obj = \\_ => []
    } ;

--------------------------------------------------------------------------------
-- Adjectives

  Adjective : Type = Verb ; -- TODO check if meaningful
  Adjective2 : Type = Adjective ;

  mkAdj : Str -> Adjective = \str -> {s = \\_ => str} ;

  AdjPhrase : Type = Adjective ; -- ** {compar : Str} ;
--------------------------------------------------------------------------------
-- Verbs

  Verb : Type = {
    s : VForm => Str
    } ;
  Verb2 : Type = Verb ** {c2 : Preposition} ;
  Verb3 : Type = Verb2 ** {c3 : Preposition} ;

--  VV : Type = Verb ** {vvtype : VVForm} ;

  mkVerb : Str -> Prefix -> Verb = \str,p -> {
    s = table {
      Root => str ;
      Active => prefix p str
      }
  } ;

  copula : Verb = {s = \\_ => "ada"} ; -- TODO
------------------
-- Adv

  Adverb : Type = SS ;

------------------
-- VP

  VerbPhrase : Type = Verb ** {
    -- vComp : Str-- Maybe needed later?
            -- {subjunc : Str ; -- inflected verb complement
            --  inf : Str ; -- infinitive verb complement
            --  subcl : Str} -- clause complement
    } ;

  VPSlash : Type = Verb2 ;

  useV : Verb -> VerbPhrase = \v -> v ** {
     vComp = [] ; -- maybe needed later?
  } ;
--------------------------------------------------------------------------------
-- Cl, S

  Clause : Type = {subj, pred : Str} ;

  Sentence : Type = {s : Str} ;

  predVP : NounPhrase -> VerbPhrase -> Clause = \np,vp -> {
    subj = np.s ;
    pred = vp.s ! Active ; -- TODO
    } ;

--------------------------------------------------------------------------------
-- linrefs

}
