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

  mkCounter : Str -> NumOrigin -> Counter = \s,o -> baseCounter ** {
    s = s ;
    origin = o ;
    } ;

  BaseNoun : Type = {
    s : NForm => Str ;
    p : Phono ;
    } ;

  Noun : Type =  BaseNoun ** {
    c : Counter ;
    } ;
  Noun2 : Type = Noun ; -- TODO eventually more parameters?
  Noun3 : Type = Noun ;

  CNoun : Type = Noun ** {
    rs : Str ; -- Relative clause comes before determiner
    } ;

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

  NounPhrase = BaseNoun ** {
    -- empty : Str ; -- standard trick for pro-drop
  } ;

--------------------------------------------------------------------------------
-- Pronouns

  Pronoun : Type = BaseNoun ** {
    poss : Quant ;
    } ;

  mkPron = overload {
    mkPron : (stem,poss : Str) -> Pronoun = \s,poss -> mkNoun s ** {
      poss = mkQuant poss (poss ++ "것") ;
      } ;
    mkPron : (stem : Str) -> Pronoun = \s -> mkNoun s ** {
      poss = mkQuant (s + "의") (s + "의" ++ "것") ;
    }
  } ;
--------------------------------------------------------------------------------
-- Det, Quant, Card, Ord

  BaseQuant : Type = {
    sp : NForm => Str ;
    isPoss : Bool ;
    p : Phono
    } ;

  Determiner : Type = BaseQuant ** {
    s : NumOrigin => Str ; -- Chosen by the counter of CN
    n : Number ;
    numtype : NumType ; -- Whether its Num component is digit, numeral or Sg/Pl
    } ;

  Quant : Type = BaseQuant ** {
    s : Str ;
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
    sp = \\_ => [] ;
    isPoss = False ;
    p = Vowel ;
  } ;

  mkQuant : (s,sp : Str) -> Quant = \s,sp -> baseQuant ** {
    s = s ;
    sp = (mkNoun sp).s ;
    p = (mkNoun sp).p ;
  } ;

  mkDet : Str -> Number -> Determiner = \s,num -> baseQuant ** {
    s = \\_ => (mkNoun s).s ! Bare ; -- NumOrigin irrelevant for non-numbers
    sp = (mkNoun s).s ;
    n = num ;
    numtype = NoNum ;
    } ;

  plural : NForm => Str = table {
    Bare => "들" ;
    nf => "들" + allomorph nf "들"
  } ;
--------------------------------------------------------------------------------
-- Postpositions

  Postposition : Type = {s : Phono => Str ; attaches : Bool} ;

  mkPrep : Str -> Postposition = \str -> {s=\\_ => str ; attaches=True} ;
  mkPrep2 : (ro,euro : Str) -> Postposition = \ro,euro -> {
    s = table {Vowel => ro ; Consonant => euro} ;
    attaches = True
    } ;

  emptyPP : Postposition = mkPrep [] ** {attaches=False} ;
  datPP : Postposition = mkPrep "에게" ;

--------------------------------------------------------------------------------
-- Adjectives

  Adjective : Type = {
    s : VForm => Str ; -- Adjectives are verbs
    p, pNeg : Phono ; -- needed for attaching conjunction
    } ;
  Adjective2 : Type = Adjective ** {c2 : NForm ; p2 : Postposition} ;

  v2a : (attrpos : Str) -> Verb -> Adjective = \attrpos,v -> v ** {
    s = table {
          VAttr Pos => attrpos ; -- Positive Attr is different in
          vf => v.s ! vf } -- adjectives, otherwise adj forms == verb forms.
    } ;

  mkAdj : Str -> Adjective = \plain ->
    let v : Verb = mkVerb plain ;
        stem : Str = v.s ! VStem Pos ;
        attrpos : Str = add_N stem ;
     in v2a attrpos v ;

  mkAdjReg : (x1,_,_,x4 : Str) -> Adjective = \plain,polite,formal,attr ->
    v2a attr (mkVerbReg plain polite formal attr) ;

  atoa2 : Adjective -> Adjective2 = \a -> a ** {c2=Bare ; p2=emptyPP} ;

  AdjPhrase : Type = Adjective ** {compar : Str} ;
--------------------------------------------------------------------------------
-- Verbs

  BaseVerb : Type = {
    sc : NForm ; -- subject case
    p, pNeg : Phono ; -- needed for attaching conjunction
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
        polite = informal + "요" ;
        formal = case vowFinal stem of {
                       True  => add_B stem + "니다" ;
                       False => stem + "습니다" } ;
        attrpos = stem + "는" ;
     in mkVerbReg plain polite formal attrpos ;

  mkVerb2 : (plain : Str) -> Verb2 = \plain -> vtov2 (mkVerb plain) ;
  mkVerb3 : (plain : Str) -> Verb3 = \plain -> v2tov3 (mkVerb2 plain) ;

  vtov2 : Verb -> Verb2 = \v -> v ** {c2 = Object ; p2 = emptyPP} ;
  v2tov3 : Verb2 -> Verb3 = \v -> v ** {c3 = Bare ; p3 = datPP} ;

  -- ㄹ-irregulars, ㅎ-irregular
  mkVerbReg : (x1,_,_,x4 : Str) -> Verb =
    \plain,polite,formal,attrpos ->
    let stem    = init plain ;
        neg     = stem + "지" ;
        attrneg = neg ++ "않는" ;
        planeg  = neg ++ negForms ! Plain ;
        polneg  = neg ++ negForms ! Polite ;
        formneg = neg ++ negForms ! Formal ;
     in mkVerbFull stem attrpos attrneg plain polite formal planeg polneg formneg ;

  mkVerbFull : (x1,_,_,_,_,_,_,_,x9 : Str) -> Verb =
    \stem,attrpos,attrneg,plain,polite,formal,planeg,polneg,formneg -> {
      s = table {
        VStem Pos => stem ;
        VStem Neg => init planeg ;
        VAttr Pos => attrpos ;
        VAttr Neg => attrneg ;
        VF Plain Pos => plain ;
        VF Plain Neg => planeg ;
        VF Polite Pos => polite ;
        VF Polite Neg => polneg ;
        VF Formal Pos => formal ;
        VF Formal Neg => formneg
      } ;
      sc   = Subject ;
      p    = if_then_else Phono (vowFinal stem)          Vowel Consonant ;
      pNeg = if_then_else Phono (vowFinal (init planeg)) Vowel Consonant ;
    } ;

  copula : Verb = mkVerbFull
    "이"
    "인"
    "아닌"
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

  -- For building an adjective. Different attr from do_V.
  do_A : Verb = mkVerbReg
    "하다"
    "해요"
    "합니다"
    "한" ;
  hada_A = do_A ; -- Exposing both names (hada=transliteration, do=translation)

  do_V : Verb = mkVerbReg
    "하다"
    "해요"
    "합니다"
    "하는" ;

  negForms : Style => Str =
    table { Plain => "않다" ;
            Polite => "않아요" ;
            Formal => "않습니다" } ;

------------------
-- Adv

  Adverb : Type = SS ;

  prepNP : NForm -> Postposition -> NounPhrase -> Adverb = \nf,pp,np -> {
    s = case pp.attaches of {
          True => glue (np.s ! nf) (pp.s ! np.p) ;
          False => np.s ! nf ++ (pp.s ! np.p)}
    } ;
------------------
-- Conj

  Conj : Type = {
    s1, s2 : Str ;
    c : ConjType ; -- if it's And, Or, …
                   -- Need to add conjunction already in ConsX funs.
    n : Number ;
    } ;

  -- Do not remove this. Used in a particular application grammar.
  commaConj : Conj = {
    s1, s2 = [] ;
    c = Comma ;
    n = Pl ;
    } ;

------------------
-- VP

  Complement : Type = {
    s : VForm => Str ;
    } ;

  emptyComp : Complement = {
    s = \\_ => [] ;
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
    nObj = np.s ! v2.c2 ++ v2.p2.s ! np.p
  } ;

  insertAdv : VerbPhrase -> SS -> VerbPhrase = \vp,adv -> vp ** {adv = adv.s ++ vp.adv} ;
  insertAdvSlash : VPSlash -> SS -> VPSlash = \v,a -> v ** insertAdv v a ;
--------------------------------------------------------------------------------
-- Cl, S

  Clause : Type = {
    s : Tense => Anteriority => Polarity => ClType => Str ;
    p, pNeg : Phono} ;

  {- After PredVP, we might still want to add more adverbs (QuestIAdv),
     but we're done with verb inflection.
   -}
  ClSlash : Type = Clause ;

  QClause : Type = Clause ;

  RClause : Type = Clause ;

  Sentence : Type = {
    s : ClType => Str ;
    p : Phono -- Needed for attaching conjunction
    } ;

  predVP : NounPhrase -> VerbPhrase -> ClSlash = \np,vp ->
    let npstr : Str = np.s ! vp.sc in predVP' npstr vp ;

  predVP' : (np : Str) -> VerbPhrase -> ClSlash = \np,vp -> vp ** {
    s = \\t,a,p,cltyp =>
           let vf = case cltyp of {
                      Subord   => VAttr p ;
                      WithConj => VStem p ;
                      Statement st => VF st p } -- TODO: more tenses
            in np
            ++ vp.nObj -- an object, not copula complement
            ++ vp.adv
            ++ vp.s ! vf
    } ;

--------------------------------------------------------------------------------
-- linrefs

linVerb : Verb -> Str = \v -> v.s ! linVF ;
linVP : VForm -> VerbPhrase -> Str = \vf,vp -> vp.nObj ++ vp.adv ++ vp.s ! vf ;
linAP : AdjPhrase -> Str = \ap -> ap.compar ++ ap.s ! linVF ;
}
