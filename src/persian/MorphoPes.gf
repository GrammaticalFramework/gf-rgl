--# -path=.:../../prelude
--
----1 A Simple Persian Resource Morphology
----
----  Shafqat Virk, Aarne Ranta,2010
----
---- This resource morphology contains definitions needed in the resource
---- syntax. To build a lexicon, it is better to use $ParadigmsPes$, which
---- gives a higher-level access to this module.
--
resource MorphoPes = ParamX ** open Prelude,Predef in {

  flags optimize=all ;
   coding = utf8;

---- Orthography

oper
  -- Zero-width non-joiner, used for certain morphemes
  -- See https://en.wikipedia.org/wiki/Persian_alphabet#Word_boundaries
  ZWNJ : Str = "‌" ;
  zwnj : Str -> Str -> Str = \s1,s2 -> s1 + ZWNJ + s2 ;

  -- kasre : Str = "ِ" ; -- To enable vowels for TTS input
  -- fatha : Str = "َ" ;
  kasre,fatha : Str = [] ;

  -- for appComp
  -- runtimeKasre : Str -> Str = \s -> glue s kasre ;
  runtimeKasre : Str -> Str = \s -> s ;


---- Nouns
param
  Animacy = Animate | Inanimate ;
  Mod = Bare | Ezafe | Clitic | Poss ;
  Agr = Ag Number Person ;
  CmpdStatus = IsCmpd | NotCmpd ;
  Degr = Positive | Comparative ;

------------------------------------------
-- Agreement transformations
-----------------------------------------
oper
  toAgr : Number -> Person -> Agr = \n,p -> Ag n p ;

  fromAgr : Agr -> {n : Number ; p : Person } = \agr -> case agr of {
    Ag n p => {n = n ; p = p}
  } ;

  conjAgr : Agr -> Agr -> Agr = \a0,b0 ->
    let a = fromAgr a0 ; b = fromAgr b0
     in toAgr (conjNumber a.n b.n) b.p ;

  giveNumber : Agr -> Number = \a -> case a of {
     Ag n _ => n
  } ;

  defaultAgr : Agr = agrP3 Sg ;
  agrP3 : Number -> Agr = \n -> Ag n P3 ;
  agrP1 : Number -> Agr = \n -> Ag n P1 ;

-------------------------
-- Ezafe construction
------------------------
oper


  mkPossStem : Str -> Str = \str ->
    case str of {
      _ + ("اه"|"او"|"وه")
                    => str + fatha ;
      _ + ("ا"|"و") => str + fatha + "ی" ;
      _ + "ه"       => zwnj str "ا" ;
      _             => str + fatha } ;



  mkEzafe : Str -> Str = \str ->
    case str of {
      st + "اه" => str + kasre ;
      st + "وه" => str + kasre ;
      st + "ه"  => zwnj str "ی" ;-- alt. st + "ۀ" ;
      st + "او" => str + kasre ;
      st + "وو" => str + kasre ;
      st + "و"  => str + "ی" ;
      st + "ا"  => str + "ی" ;
      _ => str + kasre
    };

  mkEnclic : Str -> Str ;
  mkEnclic str = case str of {
      st + ("ا"|"و") => zwnj str "یی" ; -- ی after a long vowel to help pronunciation
      st + "اه"      => str + "ی" ; -- here ه is a consonant, so single ی
      st + ("ی"|"ه") => zwnj str "ای" ; -- after ی or ه as a vowel, add an alif to help pronunciation
      _               =>      str + "ی" -- any other case: just a single ی
    } ;

  modTable : Str -> Mod => Str = \str ->
    table {Bare => str ;
           Ezafe => mkEzafe str ;
           Clitic => mkEnclic str ;
           Poss => mkPossStem str } ;

  -- Can happen that a complement (of N2, or e.g. PossNP) wants one form
  -- and determiner wants another form. Heuristic: whichever wants Bare loses.
  -- Will have to see if this works 100%, the grammar books I've seen
  -- aren't very clear about this, just basing on some data. /IL
  replaceBare : Mod -> (Mod=>Str) -> (Mod=>Str) = \m,tbl ->
    table {
      Bare => tbl ! m ;
      mod  => tbl ! mod
    } ;

  Noun = {
    s : Number => Mod => Str ;
    animacy : Animacy ;
    isCmpd : CmpdStatus -- Affects possession: awkward to use poss. suff. with compound nouns
  } ;

  mkN : (x1,x2 : Str) -> Animacy -> Noun = \sg,pl,ani -> indeclN sg ** {
    s = table {Sg => modTable sg ; Pl => modTable pl}
  } ;

  indeclN : Str -> Noun = \s -> {
    s = \\_,_ => s ;
    animacy = Inanimate ; isCmpd = NotCmpd
  } ;

-- masculine nouns end with alif, choTi_hay, ain Translitration: (a, h, e)
-- Arabic nouns ends with h. also taken as Masc

---------------------
--Determiners
--------------------
  BaseQuant : Type = {
    mod : Mod ;
    isNeg : Bool ;
    isDef : Bool 
  } ;

  Determiner : Type = BaseQuant ** {
    s : Str ;
    sp : Str ; -- stand-alone form for DetNP and possessive pronouns with compound nouns
    n : Number ;
    isNum : Bool
  } ;

  Quant : Type = BaseQuant ** {
    s : Number => CmpdStatus => Str} ;

  makeDet : Str -> Number -> (isNum, isNeg : Bool) -> Determiner = \str,n,isNum,isNeg -> {
    s,sp = str;
    isNum = isNum ;
    isNeg = isNeg ;
    isDef = True ;
    mod = Bare ;
    n = n
  };

  makeQuant : Str -> Str -> Mod -> (isNeg : Bool) -> Quant = \sg,pl,mod,isNeg -> {
    s = table {Sg => \\_ => sg ; Pl => \\_ => pl} ;
    mod = mod ;
    isNeg = isNeg ;
    isDef = True
  };
---------------------------
-- Adjectives
--------------------------
  BaseAdjective : Type = {
    adv : Str ;
    isPre : Bool ;        -- as attributive
    afterPrefix : Bool ;  -- as predicative, does it go between the prefix and the light verb
    } ;

  Adjective : Type = BaseAdjective ** {
    s : Degr => Mod => Str } ;

  AP : Type =  BaseAdjective ** {
    s : Mod => Str } ;

  mkAdj = overload {
    mkAdj : Str -> Str -> Adjective = \adj,adv -> {
       s = table { Positive => modTable adj ;
                   Comparative => modTable (adj + "تر") -- Regular comparative.
                 } ;
       adv = adv ; isPre = False ; afterPrefix = True } ;
    mkAdj : Str -> Str -> Str -> Adjective = \pos,cmp,adv -> {
       s = table { Positive => modTable pos ;
                   Comparative => modTable cmp -- Irregular comparative, e.g. xub-behtar
                 } ;
       adv = adv ; isPre = False ; afterPrefix = True }
    } ;

------------------------------------------------------------------
-- Verbs
------------------------------------------------------------------
param
  VerbForm = Inf       -- kardan
           | PastStem  -- kard -- Also used for future stem
           | PresStem  -- kon -- Also imperative stem
           | PerfStem  -- kardeh  -- Perfect, pluperfect
           | PastPart  -- konandeh
           | ImpPrefix Polarity -- mi/nmi, except for be and have
           | VAor  Polarity Agr  -- konam
           | VPerf Polarity Agr  -- kardeh am/nkardeh am
           | VPast Polarity Agr  -- kardam/nkardam
           | VSubj Polarity Agr  -- bekonam/nakonam
           | VImp Polarity Number -- bekon,bekonid/nakon,nakonid
           ;

  -- Affects clitic placement and passive
  LightVerb = NotLight | Light  -- ateš zadan -> ateš zade šodan
            | Kardan ;          -- gom kardan -> gom   ∅   šodan
oper
  impRoot : Str -> Str = \root -> case root of {
    st + "ی" => st ;
    _        => root
    };

  modifyFiniteForms : (Str -> Str) -> Verb -> Verb = \f,v -> v ** {s =
    table {
      vf@(VAor _ _
         | VPerf _ _
         | VPast _ _
         | VSubj _ _
         | VImp _ _)
         => f (v.s ! vf) ;
      vf => v.s ! vf }
    } ;

  addClitic : LightVerb -> Str -> Verb -> Verb = \light,cl,v -> v ** {s =
    let f : Str -> Str = case light of {
          NotLight => \s -> glue s cl ;
          _ => \s -> BIND ++ cl ++ s } -- hack: put clitic before the verb, so it attaches to the prefix
    in table {
          Inf => glue (v.s ! Inf) cl ;
          vf => (modifyFiniteForms f v).s ! vf }
       } ;

  mkVerb : (inf,pres : Str) -> Verb = \kardan,kon -> {
    s = table {
      Inf => kardan ;
      PastStem => kard ;
      PresStem => kon ;
      PerfStem => kardeh ;
      PastPart => kon + "نده" ;
      ImpPrefix Pos => "می" + ZWNJ ++ BIND ;
      ImpPrefix Neg => "نمی" + ZWNJ ++ BIND ;
      VAor  _   agr => imperfectSuffixD agr kon ; -- for reg verbs, negation comes from prefix
      VPerf pol agr => perfectSuffix agr (addN pol kardeh) ;
      VPast pol agr => imperfectSuffix agr (addN pol kard) ;
      VSubj Pos agr => addBh (imperfectSuffixD agr kon) ;
      VSubj Neg agr => addN (imperfectSuffixD agr kon) ;
      VImp Pos Sg => addBh imp ;
      VImp Pos Pl => addBh kon + "ید" ;
      VImp Neg Sg => addN imp ;
      VImp Neg Pl => addN kon + "ید" } ;
    prefix = [] ;-- For compound verbs
    lightverb = NotLight ;
  } where {
      kard = tk 1 kardan ;
      kardeh = kard + "ه" ;
      imp = impRoot kon ;
  } ;

  invarV : (inv : Str) -> Verb = \s -> -- truly invariable
    let invReg = defectiveVerb s s s in invReg **
     {s = table {ImpPrefix p => invReg.s ! ImpPrefix p ; _ => s}} ;

  defectiveVerb : (inf,pres,past : Str) -> Verb = \bayestan,bayad,bayest ->
    let invReg = mkVerb bayestan bayad in invReg **
      {s = \\vf => case vf of {
            ImpPrefix _ => [] ;
            VAor  pol _ => addN pol bayad ;
            VImp  pol _ => addN pol bayad ;
            VSubj pol _ => addN pol bayad ;
            VPast pol _ => addN pol bayest ;
            VPerf pol _ => addN pol bayest ;
            _ => invReg.s ! vf }
      } ;
--
oper
  Verb = {s : VerbForm => Str ; prefix : Str ; lightverb : LightVerb} ;

  -- Verbs that end in یدن, ادن or ودن
  -- Also some verbs that don't: دانستن with stem دان
  mkVerb1 : (_: Str) -> Verb = \inf -> mkVerb inf (tk 3 inf) ;

  -- Most verbs that end in C+تن or C+دن
  mkVerb2 : (_: Str) -> Verb = \inf -> mkVerb inf (tk 2 inf) ;

-------------------
-- making negatives
-------------------
  addN = overload {
    addN : Str -> Str = addN' ;
    addN : Polarity -> Str -> Str = \p,s ->
      case p of {Pos => s ; Neg => addN' s}
  } ;

  addN' : Str -> Str = \str ->
    case str of {
      "ا" + st => "نی" + str ;
      "آ" + st => "نیا" + st ;
       _        => "ن" + str } ;

  addBh : Str -> Str ;
  addBh str =
    case str of {
      "ا" + st => "بی" + str ;
      "آ" + st => "بیا" + st ;
      _        => "ب" + str
         };

  -- TODO: is this needed anywhere? what does it do? /IL
  addBh2 : Str -> Str ; -- should use drop instead but it gives linking error
  addBh2 str1 =
    case str1 of {
      "می" + str =>
         case str of {
           "ا" + st => Prelude.glue "بی" str ;
           "آ" + st => Prelude.glue "بیا" st ;
            _       => Prelude.glue "ب" str
                      };
      _          => "" -- ????
    };

-------------------
-- Common suffixes
-------------------
  imperfectSuffix : Agr -> Str -> Str = \ag,s -> s +
    case ag of {
      Ag Sg P1 => "م" ;
      Ag Sg P2 => "ی" ;
      Ag Sg P3 => [] ;
      Ag Pl P1 => "یم" ;
      Ag Pl P2 => "ید" ;
      Ag Pl P3 => "ند" } ;

  imperfectSuffixD : Agr -> Str -> Str = \ag,s ->
    case ag of {
      Ag Sg P3 => s + "د" ;
      _ => imperfectSuffix ag s } ;

  perfectSuffix : Agr -> Str -> Str = \ag,s ->
    case ag of {
      Ag Sg P1 => zwnj s "ام" ;
      Ag Sg P2 => zwnj s "ای" ;
      Ag Sg P3 => s ++ "است" ; -- no ZWNJ
      Ag Pl P1 => zwnj s "ایم" ;
      Ag Pl P2 => zwnj s "اید" ;
      Ag Pl P3 => zwnj s "اند" } ;

  pluperfAux : Polarity -> Agr -> Str = \pol,agr ->
    addN pol (imperfectSuffix agr "بود") ;

  futAux : Polarity -> Agr -> Str = \pol,agr ->
    addN pol (imperfectSuffixD agr "خواه") ;

  subjAux : Polarity -> Agr -> Str = \pol,agr ->
    addN pol (imperfectSuffixD agr "باش") ;
----------------------------------
-- Irregular verbs
----------------------------------

  haveVerb : Verb = haveRegV ** {s = table {
    ImpPrefix _ => [] ;
    VAor Neg agr  => imperfectSuffixD agr (addN "دار") ;
    VSubj pol agr => haveRegV.s ! VPerf pol agr ;
    vf          => haveRegV.s ! vf }
  } where { haveRegV = mkVerb "داشتن" "دار" } ;

  beVerb : Verb = beRegV ** {s = table {
    ImpPrefix _ => [] ;
    VAor Pos (Ag Sg P3) => "است" ;
    VAor Pos agr => imperfectSuffix agr "هست" ;
    VAor Neg agr => imperfectSuffix agr "نیست" ;
    VSubj pol agr => addN pol (imperfectSuffixD agr "باش") ;
    VImp Pos Sg => "باش" ;
    VImp Pos Pl => "باشید" ;
    VImp Neg Sg => "نباش" ;
    VImp Neg Pl => "نباشید" ;
    vf          => beRegV.s ! vf }
  } where { beRegV = mkVerb "بودن" "باش" } ;

  doVerb : Verb = doRegV ** {s = table {
    VSubj pol agr => addN pol (imperfectSuffixD agr "کن") ;
    VImp Pos Sg => "کن" ;
    VImp Pos Pl => "کنید" ;
    VImp Neg Sg => "نکن" ;
    VImp Neg Pl => "نکنید" ;
    vf          => doRegV.s ! vf } ;
    lightverb = Kardan
  } where { doRegV = mkVerb "کردن" "کن" } ;

  becomeVerb : Verb = mkVerb "شدن" "شو" ;

}
