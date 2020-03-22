resource ResKor = ParamKor ** open Prelude, Predef, ParamKor in {

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

  mkNoun : Str -> Noun = \str -> {
    s = \\cas => str + allomorph cas str
    } ;

  useN : Noun -> CNoun = \n -> n ;

---------------------------------------------
-- NP

  -- BaseNP : Type = {
  --   a : Agreement ;
  --   isPron : Bool ;
  --   empty : Str ; -- standard trick for pro-drop
  --   } ;
  --
  -- emptyNP : NounPhrase = {
  --   s = \\_ => [] ;
  --   a = Sg3 Masc ;
  --   isPron = False ;
  --   empty = [] ;
  --   } ;
  --
  -- indeclNP : Str -> NounPhrase = \s -> emptyNP ** {s = \\c => s} ;

  --NounPhrase : Type = BaseNP ** {s : NForm => Str} ;
  NounPhrase = Noun ;

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

--------------------------------------------------------------------------------
-- Postpositions

  Postposition : Type = SS ;

  mkPrep : Str -> Postposition = \str -> ss str ;

--------------------------------------------------------------------------------
-- Adjectives

  Adjective : Type = {s : AForm => Str} ;
  Adjective2 : Type = Adjective ;

  mkAdj : Str -> Adjective = \inf -> let stem = init inf in {
    s = table {
          AAttr => add_N stem ;
          APred VInf => inf ;
          APred (VFin Gnomic Pos) => add_B stem + "니다" ;
          APred (VFin Gnomic Neg) => "안" ++ add_B stem + "니다" ; -- TODO check
          APred _  => stem ++ "TODO: proper adjective inflection"
        }
    } ;

  AdjPhrase : Type = Adjective ** {compar : Str} ;
--------------------------------------------------------------------------------
-- Verbs

  BaseVerb : Type = {
    type : VerbType ;
    } ;
  Verb : Type = BaseVerb ** {
    s : VForm => Str ;
    } ;
  Verb2 : Type = Verb ; -- ** {c2 : Postposition} ;
  Verb3 : Type = Verb ; -- ** {c3 : Postposition} ;

--  VV : Type = Verb ** {vvtype : VVForm} ;

  copula : Verb = {
    s = table {
      VInf => "이다" ;
      VFin Gnomic Pos => "입니다" ;
      VFin Gnomic Neg => "아닙니다" ;
      _ => "TODO:copula" } ;
    type = Copula ;
    } ;

  mkVerb : Str -> Verb = \str -> {
    s = \\_ => str ;
    type = Active
  } ;

------------------
-- Adv

  Adverb : Type = SS ;

------------------
-- VP

  Complement : Type = {
    s : VForm => Str ;
    nComp : Str ;
    -- compar : Str ; -- comparative is discontinuous
    } ;

  emptyComp : Complement = {
    s = \\_ => [] ;
    nComp = [] ;
    -- compar : Str ;
  } ;

  VerbPhrase : Type = BaseVerb ** Complement ** {
    vComp : Str
            -- {subjunc : Str ; -- inflected verb complement
            --  inf : Str ; -- infinitive verb complement
            --  subcl : Str} -- clause complement
    } ;

  VPSlash : Type = VerbPhrase ;

  useV : Verb -> VerbPhrase = \v -> v ** {
    vComp,
    nComp = [] ;
  } ;
--------------------------------------------------------------------------------
-- Cl, S

  Clause : Type = {s : Tense => Anteriority => Polarity => Str} ;

  {- After PredVP, we might still want to add more adverbs (QuestIAdv),
     but we're done with verb inflection.
   -}
  ClSlash : Type = Clause ;

  QClause : Type = Clause ;

  RClause : Type = {s : NForm => Tense => Anteriority => Polarity => Str} ;

  Sentence : Type = {s : Str} ;

  predVPslash = predVP ; -- VP==VPSlash,  Cl==ClSlash

  predVP : NounPhrase -> VerbPhrase -> ClSlash = \np,vp -> vp ** {
    s = \\t,a,p => np.s ! Topic
                ++ vp.nComp -- TODO: embed copula into complement
                ++ vp.s ! VFin Gnomic Pos -- TODO: actual forms
    } ;

--------------------------------------------------------------------------------
-- linrefs

}
