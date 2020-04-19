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
  BaseNP : Type = {
    agr : Person*Number ;
    objdef : ObjDef ;
    empty : Str ; -- standard trick for pro-drop
    } ;

  NounPhrase : Type = BaseNP ** {
    s : Case => Str ;
    } ;

  emptyNP : NounPhrase = {
    s = \\_ => [] ;
    agr = <P3,Sg> ;
    objdef = Indef ;
    empty = [] ;
    } ;

  indeclNP : Str -> NounPhrase = \s -> emptyNP ** {s = \\c => s} ;

  defNP : Str -> Number -> NounPhrase = \s,n -> emptyNP ** {
    s = (mkNoun s).s ! n ;
    n = n ;
    objdef = Def ;
    } ;
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
    objdef : ObjDef ; -- How V2 agrees if NP with this Det is an object
    } ;

  mkQuant : (s,sp : Str) -> Quant = \s,sp -> {
    s = (mkNoun s).s ;
    sp = (mkNoun sp).s ;
    isIndefArt = False ;
    objdef = Def ;
    } ;

 -- Det is formed in DetQuant : Quant -> Num -> Det
 -- so it has an inherent number.
  Determiner : Type = {
    s,
    sp : Case => Str ;
    n : Number ;
    numtype : NumType ; -- Whether its Num component is digit, numeral or Sg/Pl
    objdef : ObjDef ; -- How V2 agrees if NP with this Det is an object
    } ;

  mkDet : (s : Str) -> ObjDef -> Number -> Determiner = \s,d,n -> {
    s,
    sp = (mkNoun s).s ! n ;
    n = n ;
    numtype = NoNum ;
    objdef = d ;
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
-- Adpositions

  -- TODO: personal suffixes, e.g. felettem, általam, not *felett/által én
  Adposition : Type = {
    pr : Str ; -- Preposition
    s : Str ;  -- Postposition
    c : Case ;
    } ;

  mkPrep : Str -> Adposition = \str -> {s=str ; c=Nom ; pr=[]} ;

  emptyAdp : Adposition = mkPrep [] ;

------------------
-- Conj

  Conj : Type = {
    s1 : Str ;
    s2 : Str ;
    n : Number ;
    } ;

  mkConj : Str -> Number -> Conj = mkDConj [] ;

  mkDConj : (s1,s2 : Str) -> Number -> Conj = \s1,s2,num -> {
    s1 = s1 ;
    s2 = s2 ;
    n = num ;
    } ;
--------------------------------------------------------------------------------
-- Adjectives

  AdjPhrase : Type = {
    s : Number => Str ;
    compar : Str -- Discontinuous: Én *nagyobb* vagyok *nálad*.
    } ;

  emptyAP : AdjPhrase = {
    s = \\_ => [] ;
    compar = [] ;
    } ;

  Adjective : Type = {
    s : Degree => Number => Str
    } ;
  Adjective2 : Type = Adjective ** {
    c2 : Adposition ;
    } ;

  mkAdj : Str -> Adjective = \sg -> {
    s = \\d,n =>
       let adj = case d of {
                   Compar => comparAdj sg ;
                   Superl => "leg" + comparAdj sg ;
                   _ => sg } ;
           plural = case n of {
                         Sg => [] ;
                         Pl => pluralAdj adj }
       in adj + plural
    } ;

  -- https://en.wikisource.org/wiki/Simplified_Grammar_of_the_Hungarian_Language/Adjectives
  comparAdj : Str -> Str = \stem ->
    case stem of {
--      Final a and e become lengthened at the end of a word, if the comparative suffix -bb is joined to it—e.g., drága, drágább; fekete, feketébb, &c.; ó shortens its sound only in jó; jobb, legjobb.
      "szép"   => "szebb" ;
      "könnyű" => "könnyebb" ;
      "ifju"   => "ifjabb" ;
      "hosszú" => "hosszabb" ;
      "sok"    => "több" ;
      "felső"  => "felsőbb" ;
      "belső"  => "belsőbb" ;
      _ + #v   => stem + "bb" ;
      _        => stem + harm "abb" "ebb" ! getHarm stem
    } ;

  pluralAdj : Str -> Str = \stem ->
    case vowFinal stem of {
      True => -- https://en.wikisource.org/wiki/Simplified_Grammar_of_the_Hungarian_Language/Adjectives
        case last stem of { "ü" => "ek" ;
                            "i" => harm "ak" "ek" ! getHarm stem ;
                            _   => "k" } ;

      False => harm3 "ok" "ek" "ök" ! getHarm stem
    } ;
--------------------------------------------------------------------------------
-- Verbs

  VerbEndings : Type = Person*Number => HarmForms ;
  -- TODO: incomplete
  endingsDef : VerbEndings = table {
    <P1,Sg> => harm3 "om" "em" "öm" ;
    <P2,Sg> => harm3 "od" "ed" "öd" ;
    <P3,Sg> => harm "ja" "i" ;
    <P1,Pl> => harm "juk" "jük" ;
    <P2,Pl> => harm "játok" "itek" ;
    <P3,Pl> => harm "ják" "ik"
    } ;

  -- by EG 2009, needs more special cases
  endingsIndef : VerbEndings = table {
    <P1,Sg> => harm3 "ok" "ek" "ök" ;
    <P2,Sg> => harm1 "sz" ;
    <P3,Sg> => harm1 [] ;
    <P1,Pl> => harm "unk" "ünk" ;
    <P2,Pl> => harm3 "tok" "tek" "tök" ; -- TODO allomorphs -otok, -etek, -ötök
    <P3,Pl> => harm "nak" "nek"  -- TODO allomorphs -anak, -enek
    } ;

  BaseVerb : Type = {
    sc : SubjCase ; -- subject case
  } ;
  Verb : Type = BaseVerb ** {
    s : VForm => Str ;
    } ;
  Verb2 : Type = BaseVerb ** {
    s : ObjDef => VForm => Str ;
    c2 : Case   -- object case
    } ;
  Verb3 : Type = Verb2 ** {
    -- c3 : Case   -- indirect object case
    } ;

  datV2 : Verb -> Verb2 = \v -> {
    s = \\_ => v.s ;
    sc = SCDat ;
    c2 = Nom
    } ;

  mkVerb2 : Str -> Verb2 = \sg3 -> vtov2 (mkVerb sg3) ;
  mkVerb3 : Str -> Verb3 = \sg3 -> v2tov3 (mkVerb2 sg3) ;

  vtov2 : Verb -> Verb2 = \v -> v ** {
    s = table {
          Def => let vDef : Verb = mkVerbReg endingsDef (v.s ! VInf) (v.s ! VFin P3 Sg)
                  in vDef.s ;
          Indef => v.s } ;
    c2 = Acc
    } ;
  v2tov3 : Verb2 -> Verb3 = \v -> v ** {c3 = Dat} ;

  mkVerb : (sg3 : Str) -> Verb = mkVerbReg endingsIndef "TODO:infinitive" ; -- TODO

  mkVerbReg : VerbEndings -> (inf, stem : Str) -> Verb = \hf,inf,stem ->
    let h : Harm = getHarm stem ;
        sg1 : Str = stem + hf ! <P1,Sg> ! h ;
        sg2 : Str = stem + hf ! <P2,Sg> ! h ;
        sg3 : Str = stem + hf ! <P3,Sg> ! h ;
        pl1 : Str = stem + hf ! <P1,Pl> ! h ;
        pl2 : Str = stem + hf ! <P2,Pl> ! h ;
        pl3 : Str = stem + hf ! <P3,Pl> ! h ;
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

  megvan : Verb = copula ** {
    s = \\vf => "meg" + copula.s ! vf ;
    } ;

------------------
-- VP

  VerbPhrase : Type = Verb ** {
    obj : Str ;
    adv : Str ;
    c2 : Case ; -- for RelSlash
    } ;  -- TODO more fields

  VPSlash : Type = Verb2 ** {
    adv : Str ;
    } ;

  useV : Verb -> VerbPhrase = \v -> v ** {
    obj,adv = [] ;
    c2 = Acc ; -- TODO check
    } ;

  useVc : Verb2 -> VPSlash = \v2 -> v2 ** {
    adv = [] ;
    } ;

  insertObj : VPSlash -> NounPhrase -> VerbPhrase = \vps,np -> vps ** {
    obj = np.s ! vps.c2 ;

    -- If verb's subject case is Dat and object Nom, verb agrees with obj.
    s = \\vf => case <vps.sc,vps.c2> of {
                  <SCDat,Nom> => vps.s ! np.objdef ! agr2vf np.agr ;
                  _ => vps.s ! np.objdef ! vf } ;
    } ;

  insertAdv : VerbPhrase -> SS -> VerbPhrase = \vp,adv -> vp ** {adv = adv.s} ;
  insertAdvSlash : VPSlash -> SS -> VPSlash = \vps,adv -> vps ** {adv = adv.s} ;

--------------------------------------------------------------------------------
-- Cl, S

  Clause : Type = {s : Tense => Anteriority => Polarity => Str} ;

  {- After PredVP, we might still want to add more adverbs (QuestIAdv),
     but we're done with verb inflection.
   -}
  ClSlash : Type = Clause ** {
    c2 : Case ; -- For RelSlash
    } ;

  QClause : Type = Clause ;


  Sentence : Type = {s : Str} ;

  predVP : NounPhrase -> VerbPhrase -> ClSlash = \np,vp -> vp ** {
    s = \\t,a,p => let subjcase : Case = case vp.sc of {
                                               SCNom => Nom ;
                                               SCDat => Dat }
                        in np.s ! subjcase
                        ++ vp.s ! agr2vf np.agr
                        ++ vp.obj
                        ++ vp.adv
                        ++ np.empty -- standard trick for prodrop+metavariable problem
    } ;

  -- Relative

  RP : Type = {s : Number => Case => Str} ;
  RClause : Type = {s : Tense => Anteriority => Polarity => Number => Case => Str} ;

  np2rp : NounPhrase -> RP ** {agr : Person*Number} = \np -> np ** {
    s = \\n => np.s ;
    } ;

  relVP : RP -> VerbPhrase -> RClause = \rp -> relVP' (rp ** {agr=<P3,Sg>}) ;

  relVP' : RP ** {agr : Person*Number} -> VerbPhrase -> RClause = \rp,vp -> {
    s = \\t,a,p,n,c => let subjcase : Case = case vp.sc of {
                                               SCNom => Nom ;
                                               SCDat => Dat }
                        in rp.s ! n ! subjcase
                        ++ vp.obj
                        ++ vp.adv
                        ++ vp.s ! VFin rp.agr.p1 n -- variable by number
    } ;

  relSlash : RP -> ClSlash -> RClause = \rp,cls -> {
    s = \\t,a,p,n,c => let objcase : Case = case cls.c2 of {
                                              Acc => c ;
                                              _ => cls.c2 }
                    in rp.s ! n ! objcase
                    ++ cls.s ! t ! a ! p
   } ;
--------------------------------------------------------------------------------
-- linrefs

}
