resource ParadigmsKor = open CatKor, ResKor, ParamKor, NounKor, Prelude in {

oper

--2 Parameters
--
-- To abstract over number, valency and (some) CaseParticle names,
-- we define the following identifiers. The application programmer
-- should always use these constants instead of the constructors
-- defined in $ResKor$.

  CaseParticle : Type ;    -- Arguments to give to V2, V3
  topic : CaseParticle ;   -- 은 or 는
  subject : CaseParticle ; -- 이 or 가
  object : CaseParticle ;  -- 을 or 를
  noCase : CaseParticle ;  -- No case particle

  NumOrigin : Type ;         -- Arguments to give to N
  nativeKorean : NumOrigin ; -- Native Korean variant of numerals: 하나, 둘, 셋 , …
  sinoKorean : NumOrigin ;   -- Sino-Korean variant of numerals: 일, 이, 삼

--2 Nouns

  mkN : overload {
    mkN : (noun : Str) -> N ; -- Predictable nouns with classifier 개. When quantified by a numeral, the numeral is of native Korean origin: 하나/둘/셋 , not 일/이/삼.
    mkN : (noun,counter : Str) -> N ;  -- Noun and classifier given as arguments. Takes numerals of native Korean origin.
    mkN : (noun,counter : Str) -> NumOrigin -> N ; -- Noun, classifier and origin of numerals. E.g. `mkN "사람" "명" nativeKorean`
  } ;

--2 Adjectives

  mkA : overload {
    mkA : (adj : Str) -> A ; -- Regular adjective, given in -다 form
    mkA : (kiga : Str) -> (jakda : A) -> A ; -- Compound adjective, e.g. 키가 작다 'short', literally 'height (is) small'. 키가 'height' given as string, 작다 'small' given as preconstructed A.
    mkA : (jaemi : Str) -> (itda : V) -> A ; -- Compound adjective from 있다/없다 (or any other preconstructed verb), e.g. 재미있다 'amusing; entertaining', literally from parts 'fun' (Str) and 'have' (V).
    mkA : (plain,polite,formal,attr : Str) -> A ; -- Worst case constructor: e.g. mkA "파랗다" "파래요" "파랗습니다" "파란"
  } ;

  mkA2 : overload {
    mkA2 : Str -> A2 ; -- Regular adjective, given in -다 form, no postposition for complement.
    mkA2 : Str -> Str -> A2 ; -- Adjective given in -다 form, postposition given as a string. If you want to use a postposition that has different forms for after vowel/after consonant, use the next constructor that takes a preconstructed Prep.
    mkA2 : A -> Prep -> A2 ; -- Preconstructed adjective and postposition for complement.
  } ;

  mkPN : Str -> PN
   = \s -> lin PN (mkNoun s) ;
--2 Verbs

  -- Verbs
  mkV : overload {
    mkV : (plain : Str) -> V ;    -- Predictable verb. Takes plain, uninflected -다 form, e.g. 가다
    mkV : (nore : Str) -> (hada : V) -> V ; -- Add a prefix to an existing verb, e.g. 노래+하다
    mkV : (plain,polite,formal,attr : Str) -> V ; -- Worst case constructor: e.g. mkV "다르다" "달라요" "다릅니다" "다른"
  } ;

  copula : V ; -- The copula verb ''

  mkV2 : overload {
    mkV2 : (plain : Str) -> V2 ; -- Regular verb. Takes plain, uninflected -다 form, subject particle is 가/이  and object particle is 를/을.
    mkV2 : V -> V2 ; -- Takes preconstructed V, subject particle is 가/이  and object particle is 를/을.
    mkV2 : V -> (subj,obj : CaseParticle) -> V2 ; -- Takes preconstructed V, and subject and object particles. E.g. `mkV2 좋다_V topic subject` for "as for <SUBJ>는, <OBJ>가 is good".
    } ;

  -- mkV3 : overload {
  --   } ;

  -- mkVV : overload {
  --  } ;

  --
  -- mkVA : Str -> VA
  --   = \s -> lin VA (regV s) ;
  -- mkVQ : Str -> VQ
  --   = \s -> lin VQ (regV s) ;
  -- mkVS : Str -> VS
  --   = \s -> lin VS (regV s) ;
  --
  -- mkV2A : Str -> V2A
  --   = \s -> lin V2A (regV s ** {c2 = noPrep}) ;
  -- mkV2V : Str -> V2V
  --   = \s -> lin V2V (regV s ** {c2 = noPrep}) ;
  -- mkV2Q : Str -> V2Q
  --   = \s -> lin V2Q (regV s ** {c2 = noPrep}) ;

  -----

--2 Structural categories

  mkPrep : overload {
    mkPrep : (e : Str) -> Prep ; -- Particle/postposition like 에: same form after vowel and consonant, attaches to the NP. Despite the name Prep, these are always postpositions.
    mkPrep : (ro,euro : Str) -> Prep ; -- Particle like 로/으로: first argument is the form after vowel, second argument after consonant. Attaches to the NP.
    mkPrep : (dwie : Str) -> (attaches : Bool) -> Prep ; -- `mkPrep "뒤에" False` for a postposition that doesn't attach to the NP.
    } ;

  -- mkConj : (_,_ : Str) -> Number -> Conj = \s1,s2,num ->
  --   lin Conj { s = s1 ; s2 = s2 } ;

  -- mkSubj : Str -> Bool -> Subj = \s,b ->
  --   lin Subj { } ;

  mkAdv : Str -> Adv
    = \s -> lin Adv {s = s} ;

  mkAdV : Str -> AdV
    = \s -> lin AdV {s = s} ;

  mkAdA : Str -> AdA
    = \s -> lin AdA {s = s} ;


--.
-------------------------------------------------------------------------------
-- The definitions should not bother the user of the API. So they are
-- hidden from the document.

  CaseParticle : Type = ResKor.NForm ;
  topic = Topic ;
  subject = Subject ;
  object = Object ;
  noCase = Bare ;

  NumOrigin : Type = ResKor.NumOrigin ;
  nativeKorean = NK ;
  sinoKorean = SK ;

  mkN = overload {
    mkN : Str -> N = \s -> lin N (mkNoun s) ;
    mkN : (noun,counter : Str) -> N = \n,c -> mkNCounter n c nativeKorean ;
    mkN : (noun,counter : Str) -> NumOrigin -> N = mkNCounter
    } ;

  mkNCounter : (noun,counter : Str) -> NumOrigin -> N = \n,c,o ->
    let noun : Noun = mkNoun n ;
        counter : Counter = mkCounter c o ;
     in lin N (noun ** {c = counter}) ;

  mkN2 = overload {
    mkN2 : Str -> N2 = \s -> lin N2 (mkNoun s) ;
    mkN2 : N   -> N2 = \n -> lin N2 n ;
   } ;

  -- mkPN = overload {
  --   } ;

  mkA = overload {
    mkA : (adj : Str) -> A = \s -> lin A (mkAdj s) ;
    mkA : (kiga : Str) -> (jakda : A) -> A = \kiga,jakda ->
      jakda ** {s = \\af => kiga ++ jakda.s ! af} ;
    mkA : (plain,polite,formal,attr : Str) -> A
      = \x1,x2,x3,x4 -> lin A (mkAdjReg x1 x2 x3 x4) ;
    mkA : (jaemi : Str) -> (itda : V) -> A
      = \jaemi,itda -> lin A (itda ** {s = \\vf => jaemi ++ itda.s ! vf}) ;
    } ;

  mkA2 = overload {
    mkA2 : Str -> A2 = \s -> lin A2 (atoa2 (mkAdj s)) ;
    mkA2 : Str -> Str -> A2
      = \s,p -> let adj : Adjective = mkAdj s ;
                    prep : Prep = mkPrep p
                 in lin A2 (atoa2 adj ** {p2 = prep}) ;
    mkA2 : A -> Prep -> A2 = \a,p -> lin A2 (atoa2 a ** {p2 = p}) ;
  } ;

  mkV = overload {
    mkV : (plain : Str) -> V = \v -> lin V (mkVerb v) ;
    mkV : (nore : Str) -> (hada : V) -> V = \nore,hada -> hada ** {
      s = \\vf => nore + hada.s ! vf} ;
    mkV : (plain,polite,formal,attr : Str) -> V
     = \x1,x2,x3,x4 -> lin V (mkVerbReg x1 x2 x3 x4) ;
  } ;

  copula = ResKor.copula ;

  -- regV : Str -> Verb = \s -> case s of {
  --   } ;

  mkV2 = overload {
    mkV2 : (plain : Str) -> V2 = \v2 -> lin V2 (mkVerb2 v2) ;
    mkV2 : V -> V2 = vtov2 ;
    mkV2 : V -> (subj,obj : CaseParticle) -> V2 = \v,sc,c2 ->
      vtov2 v ** {sc = sc ; c2 = c2} ;
    } ;

  mkV3 = overload {
    mkV3 : (plain : Str) -> V3 = \v3 -> lin V3 (mkVerb3 v3) ;
    } ;
  --
  -- mkVV = overload {
  --  } ;

  mkPrep = overload {
    mkPrep : (e : Str) -> Prep  -- Particle like 에, attaches to the NP.
      = \e -> lin Prep (ResKor.mkPrep e) ;
    mkPrep : (ro,euro : Str) -> Prep
      = \ro,euro -> lin Prep (ResKor.mkPrep2 ro euro) ;
    mkPrep : (dwie : Str) -> (attaches : Bool) -> Prep -- `mkPrep "뒤에" False` for a postposition that doesn't attach to the NP.
      = \dwie,f -> lin Prep (ResKor.mkPrep dwie ** {attaches = f}) ;
    } ;
--------------------------------------------------------------------------------

}
