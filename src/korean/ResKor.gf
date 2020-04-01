resource ResKor = ParamKor ** open Prelude, Predef, ParamKor in {

--------------------------------------------------------------------------------
-- Nouns
oper
  Counter : Type = {
    s : Str ;
    origin : NumOrigin
    } ;

  baseCounter : Counter = {
    s = "개" ;
    origin = NK
    } ;

  Noun : Type = {
    s : NForm => Str ;
    p : Phono ;
    c : Counter ;
    } ;
  Noun2 : Type = Noun ; -- TODO eventually more parameters?
  Noun3 : Type = Noun ;

  CNoun : Type = Noun ** {
    rs : Str ; -- Relative clause comes before determiner
    } ;

  PNoun : Type = Noun ;

  mkNoun : Str -> Noun = \str -> {
    s = \\cas => str + allomorph cas str ;
    p = if_then_else Phono (vowFinal str) Vowel Consonant ;
    c = baseCounter
    } ;

  useN : Noun -> CNoun = \n -> n ** {
    rs = []
    } ;

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
    isPoss : Bool ;
    p : Phono
    } ;

  Determiner : Type = BaseQuant ** {
    s : NumOrigin => Str ; -- Chosen by the counter of CN
    sp : NForm => Str ;
    n : Number ;
    numtype : NumType ; -- Whether its Num component is digit, numeral or Sg/Pl
    } ;

  Quant : Type = BaseQuant ** {
    s : Str ;
    sp : NForm => Str ;
    } ;

  Num : Type = {
    s : NumOrigin -- Sino-Korean or native Korean
        => DForm  -- Independent or attribute
        => Str ;
    n : Number ;
    numtype : NumType ; -- Digit, numeral or Sg/Pl
    } ;

  baseNum : Num = {
    s = \\_,_ => [] ;
    n = Sg ;
    numtype = NoNum
    } ;

  Numeral : Type = Num ** {
    ord : Str
    } ;

  baseQuant : BaseQuant = {
    isPoss = False ;
    p = Vowel ;
  } ;

  mkQuant : (s,sp : Str) -> Quant = \s,sp -> baseQuant ** {
    s = s ;
    sp = (mkNoun sp).s ;
    p = (mkNoun sp).p ;
  } ;

  plural : NForm => Str = table {
    Bare => "들" ;
    nf => "들" + allomorph nf "들"
  } ;
--------------------------------------------------------------------------------
-- Postpositions

  Postposition : Type = {s : Str ; attaches : Bool} ;

  mkPrep : Str -> Postposition = \str -> {s=str ; attaches=True} ;

  emptyPP : Postposition = mkPrep [] ** {attaches=False} ;
  datPP : Postposition = mkPrep "에게" ;

--------------------------------------------------------------------------------
-- Adjectives

  Adjective : Type = {s : VForm => Str} ; -- Adjectives are verbs
  Adjective2 : Type = Adjective ;

  mkAdj : Str -> Adjective = \plain ->
   let stem = init plain ;
       verb = mkVerb plain ;
   in {
     s = table {
           VAttr Pos => add_N stem ;  -- Positive Attr form is different in
           vf => verb.s ! vf } -- adjectives, otherwise adj forms == verb forms.
     } ;

  AdjPhrase : Type = Adjective ** {compar : Str} ;
--------------------------------------------------------------------------------
-- Verbs

  BaseVerb : Type = {
    sc : NForm ; -- subject case
    } ;
  Verb : Type = BaseVerb ** {
    s : VForm => Str ;
    } ;
  Verb2 : Type = Verb ** {c2 : NForm ; p2 : Postposition} ;
  Verb3 : Type = Verb2 ** {c3 : NForm ; p3 : Postposition} ;

--  VV : Type = Verb ** {vvtype : VVForm} ;

  mkVerb : (plain : Str) -> Verb = \plain ->
    let stem = init plain ;
        informal = add_eo stem ; -- not used in grammar yet
        polpres = informal + "요" ;
        formalpres = case vowFinal stem of {
                       True  => add_B stem + "니다" ;
                       False => stem + "습니다" } ;
        neg = stem + "지" ;
    in mkVerbReg plain polpres formalpres neg ;

  mkVerb2 : (plain : Str) -> Verb2 = \plain -> vtov2 (mkVerb plain) ;
  mkVerb3 : (plain : Str) -> Verb3 = \plain -> v2tov3 (mkVerb2 plain) ;

  vtov2 : Verb -> Verb2 = \v -> v ** {c2 = Object ; p2 = emptyPP} ;
  v2tov3 : Verb2 -> Verb3 = \v -> v ** {c3 = Bare ; p3 = datPP} ;

  mkVerbReg : (x1,_,_,x4 : Str) -> Verb =
    \plain,polite,formal,neg ->
    let stem    = init plain ;
        attrpos = stem + "는" ; -- TODO: ㄹ-irregulars
        attrneg = neg ++ "않는" ;
        planeg  = neg ++ negForms ! Plain ;
        polneg  = neg ++ negForms ! Polite ;
        formneg = neg ++ negForms ! Formal ;
     in mkVerbFull stem attrpos attrneg plain polite formal planeg polneg formneg ;

  mkVerbFull : (x1,_,_,_,_,_,_,_,x9 : Str) -> Verb =
    \stem,attrpos,attrneg,plain,polite,formal,planeg,polneg,formneg -> {
      s = table {
        VStem => stem ;
        VAttr Pos => attrpos ;
        VAttr Neg => attrneg ;
        VF Plain Pos => plain ;
        VF Plain Neg => planeg ;
        VF Polite Pos => polite ;
        VF Polite Neg => polneg ;
        VF Formal Pos => formal ;
        VF Formal Neg => formneg
      } ;
      sc = Subject
    } ;

  copula : Verb = mkVerbFull
    "이"
    "이는"   -- TODO does this exist?
    "아니는" -- TODO does this exist?
    "이다"
    "이에요"
    "입니다"
    "아니다"
    "아니에요"
    "아닙니다" ;

  copulaAfterVowel : Verb = copula ** {
    s = \\vf => case vf of {
                  VAttr Pos     => "는" ; -- TODO just guessing
                  VF Plain Pos  => "다" ;
                  VF Polite Pos => "예요" ;
                  _ => copula.s ! vf }
  } ;

  have_V : Verb = mkVerbFull
    "있"
    "있는"
    "없는"
    "있다"
    "있어요"
    "있습니다"
    "없다"
    "없어요"
    "없습니다" ;

  do_V : Verb = mkVerbReg
    "하다"
    "해요"
    "합니다"
    "하지" ;

  negForms : Style => Str =
    table { Plain => "않다" ;
            Polite => "않아요" ;
            Formal => "않습니다" } ;

------------------
-- Adv

  Adverb : Type = SS ;

------------------
-- Conj

  Conj : Type = {
    s1 : Str ;
    c : ConjType ; -- if it's And, Or, …
                   -- Need to add conjunction already in ConsX funs.
    n : Number ;
    } ;
------------------
-- VP

  Complement : Type = {
    s : VForm => Str ;
    -- compar : Str ; -- comparative is discontinuous
    } ;

  emptyComp : Complement = {
    s = \\_ => [] ;
    -- compar : Str ;
  } ;

  BaseVP : Type = {
    adv,
    nObj,
    vComp : Str
            -- {subjunc : Str ; -- inflected verb complement
            --  inf : Str ; -- infinitive verb complement
            --  subcl : Str} -- clause complement
    } ;

  baseVP : BaseVP = {
    adv,
    nObj,
    vComp = [] ;
  } ;

  VerbPhrase : Type = BaseVerb ** Complement ** BaseVP ;

  VPSlash : Type = Verb2 ** BaseVP ;

  useV : Verb -> VerbPhrase = \v -> baseVP ** v ;

  useVc : Verb2 -> VPSlash = \v2 -> baseVP ** v2 ;

  insertComp : VPSlash -> NounPhrase -> VerbPhrase = \v2,np -> useV v2 ** {
    nObj = np.s ! v2.c2 ++ v2.p2.s
  } ;

  insertAdv : VerbPhrase -> SS -> VerbPhrase = \vp,adv -> vp ** {adv = adv.s} ;
  insertAdvSlash : VPSlash -> SS -> VPSlash = \v,a -> v ** insertAdv v a ;
--------------------------------------------------------------------------------
-- Cl, S

  Clause : Type = {s : Tense => Anteriority => Polarity => ClType => Str} ;

  {- After PredVP, we might still want to add more adverbs (QuestIAdv),
     but we're done with verb inflection.
   -}
  ClSlash : Type = Clause ;

  QClause : Type = Clause ;

  RClause : Type = {s : Tense => Anteriority => Polarity => Str} ;

  Sentence : Type = {s : ClType => Str} ;

  predVP : NounPhrase -> VerbPhrase -> ClSlash = \np,vp -> vp ** {
    s = \\t,a,p,cltyp =>
           let vf = case cltyp of {
                      Subord   => VAttr p ;
                      WithConj => VStem ;
                      _        => VF Polite p } -- TODO: more tenses, politeness
            in np.s ! vp.sc
            ++ vp.nObj -- an object, not copula complement
            ++ vp.adv
            ++ vp.s ! vf
    } ;

--------------------------------------------------------------------------------
-- linrefs

linVerb : Verb -> Str = \v -> v.s ! linVF ;

}
