--# -path=.:../abstract:../common:../../prelude

--1 Hungarian auxiliary operations.

-- This module contains operations that are needed to make the
-- resource syntax work.
-- Some parameters, such as $Number$, are inherited from $ParamX$.
resource ResHun = NounMorphoHun ** open Prelude, Predef in {

--------------------------------------------------------------------------------
-- NP

-- Noun morphology is in NounMorphoHun

oper

  NounPhrase : Type = {
    s : Case => Str ;
    agr : Person*Number ;
    isPron : Bool ;
    empty : Str ; -- standard trick for pro-drop
    } ;

  emptyNP : NounPhrase = {
    s = \\_ => [] ;
    agr = <P3,Sg> ;
    isPron = False ;
    empty = [] ;
    } ;

  indeclNP : Str -> NounPhrase = \s -> emptyNP ** {s = \\c => s} ;

--------------------------------------------------------------------------------
-- Pronouns

  Pronoun : Type = NounPhrase ** {
    --poss : Str ; -- for PossPron : Pron -> Quant
    } ;

--------------------------------------------------------------------------------
-- Det, Quant, Card, Ord

  -- Quant has variable number:
  -- e.g. this_Quant has both "this" and "these"
  Quant : Type = {
    s, -- form that comes before noun: "{this} car"
    sp : Number => Case => Str ; -- independent form, "I like {this}" (DetNP)
    isIndefArt : Bool ; -- standard trick to prevent "a one car"
    } ;

  mkQuant : (s,sp : Str) -> Quant = \s,sp -> {
    s = (mkNoun s).s ;
    sp = (mkNoun sp).s ;
    isIndefArt = False ;
    } ;

 -- Det is formed in DetQuant : Quant -> Num -> Det
 -- so it has an inherent number.
  Determiner : Type = {
    s,
    sp : Case => Str ;
    n : Number ;
    numtype : NumType ; -- Whether its Num component is digit, numeral or Sg/Pl
    } ;

  Numeral : Type = {
    s : Place => Str ;  -- Independent or attribute
    numtype : NumType ; -- Digit, numeral or Sg/Pl : makes a difference in many languages
    -- TODO add ordinal
    } ;

  {- Numeral can become Num via
      Noun.gf:    NumNumeral : Numeral -> Card ;
      Noun.gf:    NumCard : Card -> Num ;
  -}
  Num : Type = Numeral ** {
    n : Number ;        -- Singular or plural
  } ;

  baseNum : Num = {
    s = \\_ => [] ;
    n = Sg ;
    numtype = NoNum
    } ;

--------------------------------------------------------------------------------
-- Postpositions

  Postposition : Type = {s : Str ; c : Case} ;

  mkPrep : Str -> Postposition = \str -> {s=str ; c=Nom} ;

  emptyPP : Postposition = mkPrep [] ;

------------------
-- Conj

  Conj : Type = {
    s1 : Str ;
    s2 : Str ;
    n : Number ;
    } ;

--------------------------------------------------------------------------------
-- Adjectives

  Adjective : Type = {s : Number => Str} ;

  mkAdj : Str -> Adjective = \sg -> {
    s = \\n =>
      let plural = case n of {
                     Sg => [] ;
                     Pl => pluralAllomorph sg }
       in sg + plural
    } ;

--------------------------------------------------------------------------------
-- Verbs

  verbEndings : Person*Number => HarmForms = table {
    <P1,Sg> => harm3 "ok" "ek" "ök" ;
    <P2,Sg> => harm1 "sz" ;
    <P3,Sg> => harm1 [] ;
    <P1,Pl> => harm "unk" "ünk" ;
    <P2,Pl> => harm3 "tok" "tek" "tök" ; -- TODO allomorphs -otok, -etek, -ötök
    <P3,Pl> => harm "nak" "nek"  -- TODO allomorphs -anak, -enek
    } ;

  Verb : Type = {
    s : VForm => Str ;
    sc : SubjCase ; -- subject case
    } ;
  Verb2 : Type = Verb ** {
    c2 : Case   -- object case
    } ;
  Verb3 : Type = Verb2 ** {
    -- c3 : Case   -- indirect object case
    } ;

  mkVerb2 : Str -> Verb2 = \sg3 -> vtov2 (mkVerb sg3) ;
  mkVerb3 : Str -> Verb3 = \sg3 -> v2tov3 (mkVerb2 sg3) ;

  vtov2 : Verb -> Verb2 = \v -> v ** {c2 = Acc} ;
  v2tov3 : Verb2 -> Verb3 = \v -> v ** {c3 = Dat} ;

  mkVerb : (sg3 : Str) -> Verb = mkVerbReg "TODO:infinitive" ; -- TODO

  mkVerbReg : (inf, sg3 : Str) -> Verb = \inf,sg3 ->
    let h : Harm = getHarm sg3 ;
        sg1 : Str = sg3 + verbEndings ! <P1,Sg> ! h ;
        sg2 : Str = sg3 + "sz" ;
        pl1 : Str = sg3 + (verbEndings!<P1,Pl>) ! h ;
        pl2 : Str = sg3 + (verbEndings!<P2,Pl>) ! h;
        pl3 : Str = sg3 + (verbEndings!<P3,Pl>) ! h;
     in mkVerbFull sg1 sg2 sg3 pl1 pl2 pl3 inf ;

  mkVerbFull : (x1,_,_,_,_,_,x7 : Str) -> Verb =
    \sg1,sg2,sg3,pl1,pl2,pl3,inf -> {
      s = table {
        VInf => inf ;
        VFin P1 Sg => sg1 ;
        VFin P2 Sg => sg2 ;
        VFin P3 Sg => sg3 ;
        VFin P1 Pl => pl1 ;
        VFin P2 Pl => pl2 ;
        VFin P3 Pl => pl3
      } ;
      sc = SCNom
    } ;

  copula : Verb = mkVerbFull
    "vagyok"
    "vagy"
    "van"
    "vagyunk"
    "vagytok"
    "vannak"
    "lenni" ;

------------------
-- VP

  VerbPhrase : Type = Verb ** {
    obj : Str ;
    adv : Str ;
    } ;  -- TODO more fields

  VPSlash : Type = Verb2 ** {
    adv : Str ;
    } ;

  useV : Verb -> VerbPhrase = \v -> v ** {
    obj,adv = [] ;
    } ;

  useVc : Verb2 -> VPSlash = \v2 -> v2 ** {
    adv = [] ;
    } ;

  insertObj : VPSlash -> NounPhrase -> VerbPhrase = \vps,np -> vps ** {
    obj = np.s ! vps.c2 ;

    -- If verb's subject case is Dat and object Nom, verb agrees with obj.
    s = \\vf => case <vps.sc,vps.c2> of {
                  <SCDat,Nom> => vps.s ! agr2vf np.agr ;
                  _ => vps.s ! vf } ;
    } ;

  insertAdv : VerbPhrase -> SS -> VerbPhrase = \vp,adv -> vp ** {adv = adv.s} ;
  insertAdvSlash : VPSlash -> SS -> VPSlash = \vps,adv -> vps ** {adv = adv.s} ;

--------------------------------------------------------------------------------
-- Cl, S

  Clause : Type = {s : Tense => Anteriority => Polarity => Str} ;

  {- After PredVP, we might still want to add more adverbs (QuestIAdv),
     but we're done with verb inflection.
   -}
  ClSlash : Type = Clause ;

  QClause : Type = Clause ;

  -- RClause : Type = {s : NForm => Tense => Anteriority => Polarity => Str} ;

  Sentence : Type = {s : Str} ;

  predVP : NounPhrase -> VerbPhrase -> ClSlash = \np,vp -> vp ** {
    s = \\t,a,p => let subjcase : Case = case vp.sc of {
                                           SCNom => Nom ;
                                           SCDat => Dat }
                    in np.s ! subjcase
                    ++ np.empty -- standard trick for prodrop
                    ++ vp.s ! agr2vf np.agr
                    ++ vp.obj
                    ++ vp.adv
    } ;

--------------------------------------------------------------------------------
-- linrefs

}
