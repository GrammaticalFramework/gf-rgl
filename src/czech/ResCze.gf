resource ResCze = open Prelude in {

-- AR March 2020
-- sources:
-- Wiki = https://en.wikipedia.org/wiki/Czech_declension, https://en.wikipedia.org/wiki/Czech_conjugation
-- CEG  = J. Naughton, Czech: an Essential Grammar, Routledge 2005.

-- parameters

param
  Number = Sg | Pl ;

  Animacy = Anim | Inanim ;
  Gender = Masc Animacy | Fem | Neutr ;

  Case = Nom | Gen | Dat | Acc | Voc | Loc | Ins ; -- traditional order

  Person = P1 | P2 | P3 ;

  Agr = Ag Gender Number Person | AgPol Gender | AgQuant Gender ; -- polite singular: plural verb, singular predicate

  CTense = CTPres | CTPast ; ----- TODO complete the tense system to match Czech verb morphology

-- phonology

oper
  hardConsonant    : pattern Str = #("d"|"t"|"g"|"h"|"k"|"n"|"r") ;
  softConsonant    : pattern Str = #("ť"|"ď"|"j"|"ň"|"ř"|"š"|"c"|"č"|"ž") ;
  neutralConsonant : pattern Str = #("b"|"f"|"l"|"m"|"p"|"s"|"v") ;

-- neutral consonants take the hard endings by default (hrad, pán), and so do
-- the foreign "z" and "x"; this is the class to test when choosing a paradigm
  hardishConsonant : pattern Str =
    #("d"|"t"|"g"|"h"|"k"|"n"|"r" | "b"|"f"|"l"|"m"|"p"|"s"|"v" | "z"|"x") ;

  consonant : pattern Str =
    #(
      "d" | "t" | "g" | "h" | "k" | "n" | "r" |
      "ť" | "ď" | "j" | "ň" | "ř" | "š" | "c" | "č" | "ž" |
      "b" | "f" | "l" | "m" | "p" | "s" | "v"
      ) ;

  dropFleetingE : Str -> Str = \s -> case s of {
    x + "e" + c@("k"|"c"|"n") => x + c ;
    x + "e" + "ň" => x + "n" ;
    _ => s
    } ;

  shortenVowel : Str -> Str = \s -> case s of {
    x + "á" + y => x + "a" + y ;
    x + "é" + y => x + "e" + y ;
    x + "í" + y => x + "i" + y ;
    x + "ý" + y => x + "y" + y ;
    x + "ó" + y => x + "o" + y ;
    x + "ú" + y => x + "u" + y ;
    x + "ů" + y => x + "o" + y ;
    _ => s
    } ;

  addI : Str -> Str = \s -> case s of {
    klu + "k"  => klu + "ci" ;
    vra + "h"  => vra + "zi" ;
    ce  + "ch" => ce  + "ši" ;
    dokto + "r" => dokto + "ři" ;
    pan => pan + "i"
    } ;

  addAdjI : Str -> Str = \s -> case s of {
    angli + "ck"  => angli + "čtí" ;
    ce    + "sk"  => ce    + "ští" ;
    _ => init (addI s) + "í"
    } ;

  -- Before i/í/ě the vowel letter carries the dental's palatalization.
  dentalStem : Str -> Str = \s -> case s of {
    stem + "ň" => stem + "n" ; stem + "ť" => stem + "t" ;
    stem + "ď" => stem + "d" ; _ => s
    } ;

  -- The žena ending is spelled i after a soft consonant, otherwise y.
  addY : Str -> Str = \s -> case s of {
    _ + #softConsonant => dentalStem s + "i" ; _ => s + "y"
    } ;

  -- 3.4.10, in particular when also final 'a' is dropped
  addE : Str -> Str = \s -> case s of {
    re + "k"   => re + "ce" ;
    pra + ("g"|"h") => pra + "ze" ;
    stre + "ch" => stre  + "še" ;
    sest + "r" => sest + "ře" ;
    stem + "l" => stem + "le" ;
    stem + "z" => stem + "ze" ;
    stem + "s" => stem + "se" ;
    _ + ("ň"|"ť"|"ď") => dentalStem s + "ě" ;
    _ + #softConsonant => s + "e" ;
    pan => pan + "ě"
    } ;

  addEch : Str -> Str = \s -> case s of {
    klu + "k" => klu + "cich" ;
    vra + ("h"|"g") => vra + "zich" ;
    ce  + "ch" => ce + "šich" ;
    pan => pan + "ech"
    } ;

  shortFemPlGen : Str -> Str = \s -> case s of {
    ul  + "ice" => ul + "ic" ;
    koleg + "yně" => koleg + "yň" ;
    ruz + "e" => ruz + "í" ;
    _ => "" + s  -- Predef.error ("shortFemPlGen does not apply to" ++ s)
    } ;

---------------
-- Nouns
---------------

-- novel idea (for RGL): lexical items stored as records rather than tables
-- advantages:
-- - easier to make exceptions to paradigms (by ** {})
-- - easier to keep the number of forms minimal
-- - easier to see what is happening than with lots of anonymous arguments to mkN, mkA, mkV

-- so this is the lincat of N

  NounForms : Type = {snom,sgen,sdat,sacc,svoc,sloc,sins, pnom,pgen,pdat,pacc,ploc,pins : Str ; g,gPl : Gender} ;

-- But traditional tables make agreement easier to handle in syntax
-- so this is the lincat of CN

  Noun : Type = {s : Number => Case => Str ; g,gPl : Gender} ;

-- this is used in UseN

  nounFormsNoun : NounForms -> Noun
    = \forms -> {
      s = table {
        Sg => table {
	  Nom => forms.snom ;
	  Gen => forms.sgen ;
	  Dat => forms.sdat ;
	  Acc => forms.sacc ;
	  Voc => forms.svoc ;
	  Loc => forms.sloc ;
	  Ins => forms.sins
	  } ;
        Pl => table {
	  Nom | Voc => forms.pnom ;
	  Gen => forms.pgen ;
	  Dat => forms.pdat ;
	  Acc => forms.pacc ;
	  Loc => forms.ploc ;
	  Ins => forms.pins
	  }
	} ;
      g = forms.g ; gPl = forms.gPl
      } ;

-- terminology of CEG
  DeclensionType : Type = Str -> NounForms ;

  declensionNounForms : (nom,gen : Str) -> Gender -> NounForms
    = \nom,gen,g ->
-- the oblique stem, for the paradigms that cannot derive it from the nominative
    let stem : Str = Predef.tk 1 gen ;
        decl : DeclensionType = case <g, nom, gen> of {
      <Masc Anim,   _ + "tel"         , _ + "e"> => declMUZstem stem ;
      <Masc Anim,   _ + "ce"          , _ + "e"> => declSOUDCE ;
      <Masc Anim,   _ + ("us"|"os")   , _ + "a"> => declLATINUSA ;
      <Masc Anim,   _ + "a"           , _ + "a"> => declPREDSEDA ;
      <Masc Anim,   _ + (#softConsonant|#hardishConsonant), _ + ("e"|"ě")> => declMUZstem stem ;
      <Masc Anim,   _ + #hardishConsonant, _ + "a"> => declPAN ;
      <Masc Inanim, _ + ("us"|"os")   , _ + "u"> => declLATINUS ;
      <Masc Inanim, _ + "ý"           , _ + "ého"> => declADJM ;
      <Masc Inanim, _ + (#softConsonant|#hardishConsonant), _ + ("e"|"ě")> => declSTROJ ;
      <Masc Inanim, _ + #hardishConsonant, _ + "u"> => declHRADstem stem ;
      <Masc Inanim, _ + #hardishConsonant, _ + "a"> => declHRADAstem stem ;
      <Fem,         _ + "a"           , _ + "y"> => declZENA ;
      <Fem,         _ + "á"           , _ + "é"> => declADJF ;
      <Fem,         _ + ("e"|"ě")     , _ + ("e"|"ě")> => declRUZE ;
      <Fem,         _ + (#softConsonant|#hardishConsonant), _ + "i"> => declKOST ;  --- also many other "st" 3.6.3
      <Fem,         _ + (#softConsonant|#hardishConsonant), _ + ("e"|"ě")> => declPISEN ;
      <Neutr,       _ + "um"          , _ + "a"> => declLATINUM ;
      <Neutr,       _ + "ma"          , _ + ("matu"|"mata")> => declGREEKMA ;
      <Neutr,       _ + "o"           , _ + "a"> => declMESTO ;
      <Neutr,       _ + "e"           , _+"ete"> => declKURE ;
      <Neutr,       _ + "í"           , _ + "í"> => declSTAVENI ;
      <Neutr,       _ + ("e"|"ě")     , _ + ("e"|"ě")> => declMORE ;
      <Masc Inanim, _ + ("é"|"i"|"y"|"e"), _ + ("é"|"i"|"y"|"e")> => declINVAR (Masc Inanim) ;
      <Neutr,       _ + ("é"|"i"|"y")  , _ + ("é"|"i"|"y")> => declINVAR Neutr ;
      _ => (\s -> declSTROJ ("" + s)) -- Predef.error ("cannot infer declension type for" ++ nom ++ gen)
      }
    in decl nom ;

-- the "smartest" one-argument mkN

  guessNounForms : Str -> NounForms
    = \s -> case s of {
      _ + "ost"          => declKOST s ;
      _ + "tel"          => declMUZ s ;
      _ + "us"           => declLATINUS s ;
      _ + "um"           => declLATINUM s ;
      _ + #hardishConsonant => declHRAD s ;
      _ + #softConsonant => declSTROJ s ;
      _ + "a"            => declZENA s ;
      _ + "o"            => declMESTO s ;
      _ + "ce"           => declSOUDCE s ;
      _ + ("e"|"ě")      => declMORE s ;
      _ + "í"            => declSTAVENI s ;
      _ => declSTROJ ("" + s) -- Predef.error ("cannot guess declension type for" ++ s)
      } ;

-- the traditional declensions, in both CEG and Wiki
-- they are also exported in ParadigmsCze with names panN etc

  declPAN : DeclensionType = \pan ->  --- plural nom ové|i|é can be changed with ** {pnom = ...} CEG 3.5.1
    {
      snom      = pan ;
      sgen,sacc = pan + "a" ;
      sdat,sloc = pan + "ovi" ; --- pánu
      svoc      = shortenVowel pan + "e" ; --- "irregular shortening" 3.5.1
      sins      = pan + "em" ;

      pnom      = addI pan ;       -- pani, kluk-kluci --- panové, host-hosté
      pgen      = pan + "ů" ;
      pdat      = pan + "ům" ;
      pacc,pins = pan + "y" ;
      ploc      = addEch pan ;
      g,gPl = Masc Anim
      } ;

  declPREDSEDA : DeclensionType = \predseda -> --- 3.5.4: sgen y/i
    let predsed = init predseda
    in
    {
      snom      = predseda ;
      sgen      = predsed + "y" ; -- pacc,pins --- i
      sdat,sloc = predsed + "ovi" ;
      sacc      = predsed + "u" ;
      svoc      = predsed + "o" ;
      sins      = predsed + "ou" ;

      pnom      = case predseda of {
        tur + "ista" => tur + "isté" ;
        _ => predsed + "ové"
	} ;
      pgen      = predsed + "ů" ;
      pdat      = predsed + "ům" ;
      pacc,pins = predsed + "y" ;
      ploc      = addEch predsed ;
      g,gPl = Masc Anim
      } ;

-- the oblique stem is a separate argument, because it cannot always be
-- derived from the nominative: uzel-uzlu but člen-členu
  declHRADstem : Str -> DeclensionType = \hrd,hrad ->
    {
      snom,sacc = hrad ;
      sgen,sdat = hrd + "u" ; --- Berlín-a
      sloc      = hrd + "u" ; --- addE hrad ;  -- stůl-stole
      svoc      = hrd + "e" ;
      sins      = hrd + "em" ;

      pnom,pacc,pins = hrd + "y" ;
      pgen           = hrd + "ů" ;
      pdat           = hrd + "ům" ;
      ploc           = addEch hrd ;
      g,gPl = Masc Inanim
      } ;

  declHRAD : DeclensionType = \hrad -> --- 3.5.2: sloc u/ě/e  extra arg, sport-u, hrad-ě ; sgen u/a
    declHRADstem (dropFleetingE hrad) hrad ;

  declZENA : DeclensionType = \zena -> --- 3.6.1 sge y/i ; pgen sometimes shortening
    let zen = init zena
    in
    {
      snom      = zena ;
      sgen      = addY zen ;
      sdat,sloc = addE zen ;
      sacc      = zen + "u" ;
      svoc      = shortenVowel zen + "o" ; ---- shorten ?
      sins      = zen + "ou" ;

      pnom,pacc = addY zen ;
      pgen      = zen ; --- sometimes with vowel shortening
      pdat      = zen + "ám" ;
      ploc      = zen + "ách" ;
      pins      = zen + "ami" ;
      g,gPl = Fem
      } ;

  declMESTO : DeclensionType = \mesto -> --- 3.7.1 sloc u/e ; pgen vowel shortening sometimes ; ploc variations
    let mest = init mesto
    in
    {
      snom,sacc,svoc = mesto ;
      sgen      = mest + "a" ;
      sdat      = mest + "u" ;
      sloc      = mest + "u" ; --- "ě"
      sins      = mest + "em" ;

      pnom,pacc = mest + "a" ;
      pgen      = mest ;  --- léta - let
      pdat      = mest + "ům" ;
      ploc      = mest + "ech" ; --- with variations
      pins      = mest + "y" ;
      g,gPl = Neutr
      } ;

-- Latin masculines in -us: the ending is dropped outside the nominative
-- (algoritmus - algoritmu), otherwise they follow hrad
  declLATINUS : DeclensionType = \algoritmus ->
    let algoritm = Predef.tk 2 algoritmus
    in declHRAD algoritm ** {
      snom, sacc = algoritmus ;
      svoc       = algoritm + "e"
      } ;

  declLATINUSA : DeclensionType = \genius ->
    declLATINUS genius ** {g,gPl = Masc Anim} ;

-- Latin neuters in -um: the ending is dropped outside the nominative
-- (kontinuum - kontinua), otherwise they follow město
  declLATINUM : DeclensionType = \kompaktum ->
    let kompakt = Predef.tk 2 kompaktum
    in declMESTO (kompakt + "o") ** {
      snom, sacc, svoc = kompaktum
      } ;

-- Greek neuters in -ma, with the stem extended by -t- (schéma - schématu)
  declGREEKMA : DeclensionType = \schema ->
    let schemat = schema + "t"
    in {
      snom,sacc,svoc = schema ;
      sgen,sdat,sloc = schemat + "u" ;
      sins           = schemat + "em" ;

      pnom,pacc = schemat + "a" ;
      pgen      = schemat ;
      pdat      = schemat + "ům" ;
      ploc      = schemat + "ech" ;
      pins      = schemat + "y" ;
      g,gPl = Neutr
      } ;

-- the hrad type with genitive -a instead of -u (les - lesa, zákon - zákona)
  declHRADAstem : Str -> DeclensionType = \les_,les ->
    declHRADstem les_ les ** {sgen = les_ + "a"} ;

  declHRADA : DeclensionType = \les ->
    declHRADAstem (dropFleetingE les) les ;

-- nouns that are adjectives in form: proměnná - proměnné, nultý - nultého
  declADJF : DeclensionType = \promenna ->
    let a = mladyAdjForms (init promenna + "ý")
    in {
      snom,svoc = a.fsnom ;
      sgen      = a.fsgen ;
      sdat,sloc = a.fsdat ;
      sacc      = a.fsacc ;
      sins      = a.fsins ;
      pnom,pacc = a.fpnom ;
      pgen,ploc = a.pgen ;
      pdat      = a.msins ;
      pins      = a.pins ;
      g,gPl = Fem
      } ;

  declADJM : DeclensionType = \nulty ->
    let a = mladyAdjForms nulty
    in {
      snom,sacc,svoc = a.msnom ;
      sgen      = a.msgen ;
      sdat      = a.msdat ;
      sloc      = a.msloc ;
      sins      = a.msins ;
      pnom,pacc = a.fpnom ;
      pgen,ploc = a.pgen ;
      pdat      = a.msins ;
      pins      = a.pins ;
      g,gPl = Masc Inanim
      } ;

-- indeclinable loans: bombé, tamari, software
  declINVAR : Gender -> DeclensionType = \g,s -> {
    snom,sgen,sdat,sacc,svoc,sloc,sins = s ;
    pnom,pgen,pdat,pacc,ploc,pins      = s ;
    g,gPl = g
    } ;

  declMUZ : DeclensionType = \muz_ -> --- 3.5.3 : sdat,sloc ; pnom
    declMUZstem (dropFleetingE muz_) muz_ ;

  declMUZstem : Str -> DeclensionType = \muz,muz_ ->
    {
      snom      = muz_ ;
      sgen,sacc = muz + "e" ;   --- pacc
      sdat,sloc = muz + "i" ;   --- muzovi
      svoc      = case muz_ of {
        chlap + "ec" => chlap + "če" ;
        _ => muz + "i"
	} ;
      sins      = muz + "em" ;

      pnom = case muz_ of {
        uci + "tel" => uci + "telé" ;
        _ => muz + "i"  --- muzové
	} ;
      pgen = muz + "ů" ;
      pacc = muz + "e" ;
      pdat = muz + "ům" ;
      ploc = muz + "ích" ;
      pins = muz + "i" ;
      g,gPl = Masc Anim
      } ;

  declSOUDCE : DeclensionType = \soudce ->   --- 3.5.3: sdat/sloc i,ovi ; pnom i/ové
    let soudc = init soudce
    in
    {
      snom,sgen,sacc,svoc = soudce ;      ---- pacc
      sdat,sloc           = soudc + "i" ; --- soudcovi
      sins                = soudc + "em" ;

      pnom                = soudc + "i" ; --- soudcové
      pgen                = soudc + "ů" ;
      pdat                = soudc + "ům" ;
      pacc                = soudce ;
      ploc                = soudc + "ích" ;
      pins                = soudc + "i" ;
      g,gPl = Masc Anim
      } ;

  declSTROJ : DeclensionType = \stroj ->
    {
      snom,sacc      = stroj ;
      sgen           = stroj + "e" ; --- pnom,pacc
      sdat,svoc,sloc = stroj + "i" ; --- pins ---- svoc shorten?
      sins           = stroj + "em" ;

      pnom,pacc      = stroj + "e" ;
      pgen           = stroj + "ů" ;
      pdat           = stroj + "ům" ;
      ploc           = stroj + "ích" ;
      pins           = stroj + "i" ;
      g,gPl = Masc Inanim
      } ;

  declRUZE : DeclensionType = \ruze -> --- 3.6.2: pgen ulice-ulic, chvile-cvil
    let ruz = init ruze
    in
    {
      snom,sgen,svoc      = ruze ; --- pnom,pacc
      sdat,sacc,sloc = ruz + "i" ;
      sins = ruz + "í" ;

      pnom,pacc = ruze ;
      pgen      = shortFemPlGen ruze ;
      pdat      = ruz + "ím" ;
      ploc      = ruz + "ích" ;
      pins      = ruz + "emi" ;
      g,gPl = Fem
      } ;

  declPISEN : DeclensionType = \pisen ->
    let pisn = dentalStem (dropFleetingE pisen)
    in
    {
      snom,sacc      = pisen ;
      sgen           = pisn + "ě" ;
      sdat,svoc,sloc = pisn + "i" ; -- not shortened
      sins           = pisn + "í" ;

      pnom,pacc      = pisn + "ě" ;
      pgen           = pisn + "í" ;
      pdat           = pisn + "ím" ;
      ploc           = pisn + "ích" ;
      pins           = pisn + "ěmi" ;
      g,gPl = Fem
      } ;

  declKOST : DeclensionType = \kost ->
    let stem = dentalStem kost
    in
    {
      snom,sacc           = kost ;
      sgen,sdat,svoc,sloc = stem + "i" ; --- pnom,pacc
      sins                = stem + "í" ; --- pgen

      pnom,pacc      = stem + "i" ;
      pgen           = stem + "í" ;
      pdat           = stem + "em" ;
      ploc           = stem + "ech" ;
      pins           = kost + "mi" ;
      g,gPl = Fem
      } ;

  declKURE : DeclensionType = \kure ->
    let kur = init kure
    in
    {
      snom,sacc,svoc = kure ;
      sgen           = kur  + "ete" ;
      sdat,sloc      = kur  + "eti" ;
      sins           = kur  + "etem" ;

      pnom,pacc = kur + "ata" ;
      pgen      = kur + "at" ;
      pdat      = kur + "atům" ;
      ploc      = kur + "atech" ;
      pins      = kur + "aty" ;
      g,gPl = Neutr
      } ;

  declMORE : DeclensionType = \more -> --- 3.7.2 pgen zero sometimes
    let mor = init more
    in
    {
      snom,sgen,sacc,svoc = more ;      --- pnom
      sdat,sloc           = mor + "i" ; --- pins
      sins                = mor + "em" ;

      pnom,pacc = more ;
      pgen      = mor + "í" ;  ---
      pdat      = mor + "ím" ;
      ploc      = mor + "ích" ;
      pins      = mor + "i" ;
      g,gPl = Neutr
      } ;

  declSTAVENI : DeclensionType = \staveni ->
    {
      snom,sgen,sdat,sacc,svoc,sloc = staveni ;
      sins                          = staveni + "m" ;

      pnom,pgen,pacc = staveni ;
      pdat           = staveni + "m" ;
      ploc           = staveni + "ch" ;
      pins           = staveni + "mi" ;
      g,gPl = Neutr
      } ;

---------------------------
-- Adjectives

-- to be used for AP: 56 forms for each degree
  Adjective : Type = {s : Gender => Number => Case => Str} ;

  -- Long predicates agree with the counted noun; short predicates instead
  -- use neuter singular with a quantified subject.
  longPredicate : Adjective -> Agr => Str = \ap -> \\a => case a of {
    Ag g n _ => ap.s ! g ! n ! Nom ;
    AgPol g => ap.s ! g ! Sg ! Nom ;
    AgQuant g => ap.s ! g ! Pl ! Gen
    } ;
  shortPredicate : Adjective -> Agr => Str = \ap -> \\a => case a of {
    AgQuant _ => ap.s ! Neutr ! Sg ! Nom ;
    _ => longPredicate ap ! a
    } ;

-- to be used for A, in three degrees: 15 forms in each
  DegreeForms : Type = AdjForms ** {compar,superl : AdjForms} ;

  positiveAdj : AdjForms -> DegreeForms = \a -> a ** {
    compar,superl = invarAdjForms nonExist
    } ;

  AdjForms : Type = {
    msnom, fsnom, nsnom : Str ; -- svoc = snom
    msgen, fsgen : Str ;        -- nsgen = msgen, pacc = fsgen
    msdat, fsdat : Str ;        -- nsdat = msdat
    fsacc : Str ;               -- amsacc = msgen, imsacc = msnom, nsacc = nsnom
    msloc : Str ;               -- fsloc = fsdat, nsloc = msloc
    msins, fsins : Str ;        -- nsins = msins, pdat = msins

    mpnom,fpnom : Str ;         -- pvoc = pnom, impnom = fpnom, npnom = fsnom
    pgen : Str ;                -- ploc = pgen
    pins : Str ;
    } ;

invarAdjForms : Str -> AdjForms = \s -> {
    msnom, fsnom, nsnom, msgen, fsgen, msdat, fsdat, fsacc,
    msloc, msins, fsins, mpnom, fpnom, pgen, pins = s ;
    } ;

-- used in PositA but will also work in Compar and Superl by calling their record fields

adjFormsAdjective : AdjForms -> Adjective = \afs -> {
  s = \\g,n,c => case <n,c,g> of {

    <Sg, Nom|Voc, Masc _>
      | <Sg, Acc, Masc Inanim>   => afs.msnom ;
    <Sg, Nom|Voc, Fem>
      | <Pl, Nom|Acc|Voc, Neutr> => afs.fsnom ;
    <Sg, Nom|Acc|Voc, Neutr>     => afs.nsnom ;

    <Sg, Gen, Masc _ | Neutr>
      | <Sg,Acc,Masc Anim>     => afs.msgen ;
    <Sg, Gen, Fem>
      | <Pl,Acc,Masc _|Fem>    => afs.fsgen ;

    <Sg, Dat, Masc _|Neutr>    => afs.msdat ;
    <Sg, Dat|Loc, Fem>         => afs.fsdat ;

    <Sg, Acc, Fem>             => afs.fsacc ;

    <Sg, Loc, Masc _|Neutr>    => afs.msloc ;

    <Sg, Ins, Masc _|Neutr>
      | <Pl,Dat,_>             => afs.msins ;
    <Sg, Ins, Fem>             => afs.fsins ;

    <Pl, Nom|Voc, Masc Anim>       => afs.mpnom ;
    <Pl, Nom|Voc, Masc Inanim|Fem> => afs.fpnom ;

    <Pl, Gen|Loc,_> => afs.pgen ;
    <Pl, Ins,_>     => afs.pins
    }

    } ;

  -- Regular comparison plus common lexical exceptions. Spelling cannot
  -- determine semantic gradability or every stem alternation: callers can
  -- supply a comparative or nonExist explicitly.
  guessComparative : Str -> Str = \s -> case s of {
    "dobrý" => "lepší" ; "špatný" | "zlý" => "horší" ;
    "malý" => "menší" ; "velký" => "větší" ; "dlouhý" => "delší" ;
    "mladý" => "mladší" ; "starý" => "starší" ;
    "chudý" => "chudší" ; "tvrdý" => "tvrdší" ; "bledý" => "bledší" ;
    "bílý" => "bělejší" ; "hnědý" => "hnědší" ; "hezký" => "hezčí" ;
    stem + "cký" => stem + "čtější" ;
    stem + "ský" => stem + "štější" ;
    stem + ("ný" | "ní") => stem + "nější" ;
    stem + "lý" => stem + "lejší" ;
    stem + "rý" => stem + "řejší" ;
    stem + "vý" => stem + "vější" ;
    stem + "dý" => stem + "dější" ;
    stem + "tý" => stem + "tější" ;
    stem + "pý" => stem + "pější" ;
    stem + "bý" => stem + "bější" ;
    stem + "mý" => stem + "mější" ;
    stem + "zí" => stem + "zejší" ;
    stem + "ží" => stem + "žejší" ;
    _ => nonExist
    } ;

  degreeAdjForms : Str -> Str -> DegreeForms = \p,c ->
    (guessAdjForms p) ** {
      compar = guessAdjForms c ; superl = guessAdjForms ("nej" + c)
      } ;

  guessAdjForms : Str -> AdjForms = \s -> case s of {
        _ + "ý"  => mladyAdjForms s ;
        _ + "í"  => jarniAdjForms s ;
        _ + "ův" => otcuvAdjForms s ;
        _ + "in" => matcinAdjForms s ;
        _ => matcinAdjForms ("" + s) -- Predef.error ("no mkA for" ++ s)
        } ;

-- hard declension

  mladyAdjForms : Str -> AdjForms = \mlady ->
    let mlad = init mlady
    in {
      msnom                    = mlad + "ý" ;
      fsnom                    = mlad + "á" ;
      nsnom,fsgen,fsdat,fpnom  = mlad + "é" ;
      msgen                    = mlad + "ého" ;
      msdat                    = mlad + "ému" ;
      fsacc,fsins              = mlad + "ou" ;
      msloc                    = mlad + "ém" ;
      msins,pdat               = mlad + "ým" ;
      mpnom                    = addAdjI mlad ;
      pgen                     = mlad + "ých" ;
      pins                     = mlad + "ými" ;
      } ;

-- soft declension

  jarniAdjForms : Str -> AdjForms = \jarni ->
    {
      msnom,fsnom,nsnom,
      fsgen,fsdat,fsacc,fsins,
      mpnom,fpnom              = jarni ;
      msgen                    = jarni + "ho" ;
      msdat                    = jarni + "mu" ;
      msloc,msins              = jarni + "m" ;
      pgen                     = jarni + "ch" ;
      pins                     = jarni + "mi" ;
      } ;

-- masculine possession: the same endings as in feminine

  otcuvAdjForms : Str -> AdjForms = \otcuv ->
    let otcov = Predef.tk 2 otcuv + "ov"
    in
    matcinAdjForms otcov ** {msnom = otcuv} ;

-- feminine possession

  matcinAdjForms : Str -> AdjForms = \matcin ->
     {
      msnom                    = matcin ;
      fsnom,msgen              = matcin + "a" ;
      nsnom                    = matcin + "o" ;
      fsgen,fpnom              = matcin + "y" ;
      msdat,fsacc              = matcin + "u" ;
      fsdat,msloc              = matcin + "ě" ;
      msins                    = matcin + "ým" ;
      fsins                    = matcin + "ou" ;
      mpnom                    = matcin + "i" ;
      pgen                     = matcin + "ých" ;
      pins                     = matcin + "ými" ;
      } ;

---------------------
-- Verbs

  -- Public input schema of ParadigmsCze.VerbPrincipalParts. Keep existing
  -- record literals valid: derived forms belong in VerbForms; additional
  -- principal parts need a new constructor or overload.
  PositiveVerbForms : Type = {
    inf,
    impsg2, imppl1, imppl2,
    pressg1, pressg2, pressg3,
    prespl1, prespl2, prespl3,
    pastpartsg, pastpartpl : Str
    } ;

  VerbForms : Type = PositiveVerbForms ** {
    negpressg1,negpressg2,negpressg3,negprespl1,negprespl2,negprespl3,
    negimpsg2,negimppl1,negimppl2,refl : Str ; isRefl : Bool
    } ;

  -- Prefix at lexical construction time, so ordinary spelling also parses.
  withNeg : PositiveVerbForms -> VerbForms = \v -> v ** {
    refl = [] ; isRefl = False ;
    negpressg1 = "ne" + v.pressg1 ; negpressg2 = "ne" + v.pressg2 ;
    negpressg3 = "ne" + v.pressg3 ;
    negprespl1 = "ne" + v.prespl1 ; negprespl2 = "ne" + v.prespl2 ;
    negprespl3 = "ne" + v.prespl3 ;
    negimpsg2 = "ne" + v.impsg2 ; negimppl1 = "ne" + v.imppl1 ; negimppl2 = "ne" + v.imppl2
    } ;

  ComplementCase : Type = {s : Str ; c : Case ; hasPrep : Bool} ;

  verbAgr : VerbForms -> Agr -> Bool -> Str
    = \vf,a,b -> case <a,b> of {
      <Ag _ Sg P1,True> => vf.pressg1 ; <Ag _ Sg P1,False> => vf.negpressg1 ;
      <Ag _ Sg P2,True> => vf.pressg2 ; <Ag _ Sg P2,False> => vf.negpressg2 ;
      <Ag _ Sg P3 | AgQuant _,True> => vf.pressg3 ; <Ag _ Sg P3 | AgQuant _,False> => vf.negpressg3 ;
      <Ag _ Pl P1,True> => vf.prespl1 ; <Ag _ Pl P1,False> => vf.negprespl1 ;
      <Ag _ Pl P2 | AgPol _,True> => vf.prespl2 ; <Ag _ Pl P2 | AgPol _,False> => vf.negprespl2 ;
      <Ag _ Pl P3,True> => vf.prespl3 ; <Ag _ Pl P3,False> => vf.negprespl3
      } ;

  imperativeAgr : VerbForms -> Agr -> Bool -> Str = \v,a,pos -> case <a,pos> of {
    <Ag _ Sg _,True> => v.impsg2 ; <Ag _ Sg _,False> => v.negimpsg2 ;
    <Ag _ Pl P1,True> => v.imppl1 ; <Ag _ Pl P1,False> => v.negimppl1 ;
    <_,True> => v.imppl2 ; <_,False> => v.negimppl2
    } ;


  copulaVerbForms : VerbForms = (withNeg {
    inf = "být" ;
    impsg2 = "buď" ; imppl1 = "buďme" ; imppl2 = "buďte" ;
    pressg1 = "jsem" ;
    pressg2 = "jsi" ;
    pressg3 = "je" ;
    prespl1 = "jsme" ;
    prespl2 = "jste" ;
    prespl3 = "jsou" ;
    pastpartsg = "byl" ;
    pastpartpl = "byli" ;
    }) ** {negpressg3 = "není"} ;

  haveVerbForms : VerbForms = withNeg {
    inf = "mít" ;
    impsg2 = "měj" ; imppl1 = "mějme" ; imppl2 = "mějte" ;
    pressg1 = "mám" ;
    pressg2 = "máš" ;
    pressg3 = "má" ;
    prespl1 = "máme" ;
    prespl2 = "máte" ;
    prespl3 = "mají" ;
    pastpartsg = "měl" ;
    pastpartpl = "měli" ;
    } ;

-- just an example of a traditional paradigm
---- TODO other traditional paradigms

  iii_kupovatVerbForms : Str -> VerbForms = \kupovat ->
   let
     kupo = Predef.tk 3 kupovat ;
     kupu = Predef.tk 1 kupo + "u"
   in
   withNeg {
    inf = kupovat ;
    impsg2 = kupu + "j" ; imppl1 = kupu + "jme" ; imppl2 = kupu + "jte" ;
    pressg1 = kupu + "ji" ; --- kupuju
    pressg2 = kupu + "ješ" ;
    pressg3 = kupu + "je" ;
    prespl1 = kupu + "jeme" ;
    prespl2 = kupu + "jete" ;
    prespl3 = kupu + "jí" ; --- kupujou
    pastpartsg = kupo + "val" ;
    pastpartpl = kupo + "vali" ;
    } ;

  iii_krýtVerbForms : Str -> VerbForms = \krýt ->
   let
     kry = shortenVowel (Predef.tk 1 krýt) ;
   in
   withNeg {
    inf = krýt ;
    impsg2 = kry + "j" ; imppl1 = kry + "jme" ; imppl2 = kry + "jte" ;
    pressg1 = kry + "ji" ;
    pressg2 = kry + "ješ" ;
    pressg3 = kry + "je" ;
    prespl1 = kry + "jeme" ;
    prespl2 = kry + "jete" ;
    prespl3 = kry + "jí" ;
    pastpartsg = kry + "l" ;
    pastpartpl = kry + "li" ;
    } ;

---------------------------
-- Pronouns

  PronForms : Type = {
    nom, cnom,       -- cnom is the pro-drop subject
    gen, cgen,pgen,  -- bare, clitic, prepositional
    acc, cacc,pacc,
    dat, cdat,pdat,
    loc,
    ins,pins        : Str ;
    a : Agr
    } ;

  personalPron : Agr -> PronForms = \a ->
    {a = a ; cnom = []} **
    case a of {
      AgQuant _ => {nom,gen,cgen,pgen,acc,cacc,pacc,dat,cdat,pdat,loc,ins,pins = nonExist} ;
      Ag _ Sg P1 => {
        nom = "já" ;
        gen,acc,pgen,pacc = "mne" ;
        cgen,cacc = "mě" ;
        dat,pdat,loc = "mně" ;
        cdat = "mi" ;
        ins,pins = "mnou"
        } ;
      Ag _ Sg P2 => {
        nom = "ty" ;
        gen,acc,pgen,pacc = "tebe" ;
        cgen,cacc = "tě" ;
        dat,pdat,loc = "tobě" ;
        cdat = "ti" ;
        ins,pins = "tebou"
        } ;
      Ag (Masc _) Sg P3 => {
        nom = "on" ;
        gen,acc = "jeho" ;
        cgen,cacc = "ho" ;
        pgen,pacc = "něho" ;
        dat = "jemu" ;
	cdat = "mu" ;
	pdat = "němu" ;
	loc = "něm" ;
	ins = "jím" ;
	pins = "ním" ;
        } ;
      Ag Fem Sg P3 => {
        nom = "ona" ;
        gen,dat,cgen,cdat,ins = "jí" ;
        acc,cacc = "ji" ;
        pacc = "ni" ;
        pgen,pdat,loc,pins = "ní" ;
        } ;
      Ag Neutr Sg P3 => {
        nom = "ono" ;
        gen = "jeho" ;
        cgen,cacc = "ho" ;
        pgen = "něho" ;
        dat = "jemu" ;
	acc = "je" ;
        pacc = "ně" ;
	cdat = "mu" ;
	pdat = "němu" ;
	loc = "něm" ;
	ins = "jím" ;
	pins = "ním" ;
        } ;
      Ag _ Pl P1 => {
        nom = "my" ;
        gen,acc,
          cgen,cacc,
          pgen,pacc,
	  loc  = "nás" ;
        dat,cdat,pdat = "nám" ;
	ins,pins = "námi" ;
        } ;
      Ag _ Pl P2 | AgPol _ => {
        nom = "vy" ;
        gen,acc,
          cgen,cacc,
          pgen,pacc,
	  loc  = "vás" ;
        dat,cdat,pdat = "vám" ;
	ins,pins = "vámi" ;
        } ;
      Ag g Pl P3 => {
        nom = case g of {
	  Masc Anim => "oni" ;
          Masc Inanim => "ony" ;
	  Fem => "ony" ;
	  Neutr => "ona"
	  } ;
	gen,cgen = "jich" ;
	pgen = "nich" ;
	dat,cdat = "jim" ;
	pdat = "nim" ;
	acc,cacc = "je" ;
	pacc = "ně" ;
	loc = "nich" ;
	ins = "jimi" ;
	pins = "nimi" ;
	}

      } ;

  possessivePron : Agr -> DemPronForms = \a -> case a of {
      Ag _ Sg P1 => mladyAdjForms "my" ** {msnom = "můj" ; pdat = "mým"} ;  --- alts: moje, moji,...
      Ag _ Sg P2 => mladyAdjForms "tvy" ** {msnom = "tvůj" ; pdat = "tvým"} ;
      
      Ag _ Pl P1   => jarniAdjForms "naše" ** {
        msnom = "náš" ;
	fsgen,mpnom = "naši" ;
	fsins = "naší" ;
	pdat, msins = "našim" ;
	pgen = "našich" ;
	pins = "našimi" ;
	} ;
      Ag _ Pl P2 | AgPol _ => jarniAdjForms "vaše" ** {
        msnom = "váš" ;
	fsgen,mpnom = "vaši" ;
	fsins = "vaší" ;
	pdat, msins = "vašim" ;
	pgen = "vašich" ;
	pins = "vašimi" ;
	} ;
	
      Ag Fem Sg P3 => jarniAdjForms "její" ** {pdat = "jejím"} ;

      Ag (Masc _ | Neutr) Sg P3 => invarDemPronForms "jeho" ** {pdat = "jeho"} ;
      Ag _ Pl P3 | AgQuant _ => invarDemPronForms "jejich" ** {pdat = "jejich"}


    } ;

  reflPossessivePron : DemPronForms = mladyAdjForms "svy" ** {msnom = "svůj" ; pdat = "svým"} ;

  mkPron : Agr -> PronForms ** {poss : DemPronForms} = \a ->
    personalPron a ** {poss = possessivePron a} ;
  
--------------------------------
-- demonstrative pronouns, used for Quant and Det

oper
  DemPronForms : Type = {
    msnom, fsnom, nsnom,
    msgen, fsgen,
    msdat, -- fsdat = fsgen unlike AdjForms
    fsacc,
    msloc,
    msins, fsins,
    mpnom, fpnom, -- mpacc = fpacc = fpnom
    pgen,
    pdat,  -- NOT msins like AdjForms
    pins : Str
    } ;

  demPronFormsAdjective : DemPronForms -> Str -> Adjective =
    \dem,s ->
    let
      demAdj = dem ** {fsdat = dem.fsgen} ;
      adjAdj = adjFormsAdjective demAdj
    in {
      s = \\g,n,c => case <g,n,c> of {
        <_,Pl,Dat> => dem.pdat ;
	<Masc _ | Fem, Pl, Acc> => dem.fpnom ;
        _ => adjAdj.s ! g ! n ! c
        } + s
      } ;
      
  justDemPronFormsAdjective : DemPronForms -> Adjective =
    \dem ->
    let
      demAdj = dem ** {fsdat = dem.fsgen} ;
      adjAdj = adjFormsAdjective demAdj
    in {
      s = \\g,n,c => case <g,n,c> of {
        <_,Pl,Dat> => dem.pdat ;
	<Masc _ | Fem, Pl, Acc> => dem.fpnom ;
        _ => adjAdj.s ! g ! n ! c
        }
      } ;


  Determiner : Type = {
    s : Gender => Case => Str ;
    size : NumSize
    } ;

  mkDemPronForms : Str -> DemPronForms = \t -> {
    msnom = t + "en" ;
    fsnom = t + "a" ;
    nsnom = t + "o" ;
    msgen = t + "oho" ;
    fsgen = t + "é" ;
    msdat = t + "omu" ;
    fsacc = t + "u" ;
    msloc = t + "om" ;
    msins = t + "ím" ;
    fsins = t + "ou" ;
    mpnom = t + "i" ;
    fpnom = t + "y" ;
    pgen  = t + "ěch" ;
    pdat  = t + "ěm" ;
    pins  = t + "ěmi" ;
    } ;

  invarDemPronForms : Str -> DemPronForms = \s -> {
    msnom, fsnom, nsnom, msgen, fsgen,
    msdat, fsacc, msloc, msins, fsins,
    mpnom, fpnom, pgen, pdat, pins = s ;
    } ;

-- interrogatives

 kdoForms : Case => Str = table {
   Nom => "kdo" ;
   Gen | Acc | Voc => "koho" ;
   Dat => "komu" ;
   Loc => "kom" ;
   Ins => "kým"
   } ;

 coForms : Case => Str = table {
   Nom|Acc|Voc => "co" ;
   Gen => "čeho" ;
   Dat => "čemu" ;
   Loc => "čem" ;
   Ins => "čím"
   } ;

-- Numerals

  -- singular forms of demonstratives
  NumeralForms : Type = {
    msnom, fsnom, nsnom,
    msgen, fsgen,
    msdat,
    fsacc,
    msloc,
    msins, fsins : Str
    } ;

  numeralFormsDeterminer : NumeralForms -> NumSize -> Determiner =
    \nume,size ->
    let
      dem = nume **
        {mpnom, fpnom, pgen, pdat, pins = nume.msnom} ; --- plural forms not used
      demAdj = dem ** {fsdat = dem.fsgen} ;
      adjAdj = adjFormsAdjective demAdj
    in {
      s = \\g,c => adjAdj.s ! g ! Sg ! c ;
      size = size
      } ;

  -- example: number 1
  oneNumeral : Determiner = numeralFormsDeterminer ((mkDemPronForms "jedn") ** {msnom = "jeden"}) Num1 ;

  -- numbers 2,3,4 ---- to check if everything comes out right with the determiner type
  twoNumeral : Determiner =
    let forms = {
      msnom = "dva" ; fsnom, nsnom, fsacc = "dvě" ;
      msgen, fsgen, msloc = "dvou" ;
      msdat, msins, fsins = "dvěma"
      }
    in numeralFormsDeterminer forms Num2_4 ;

  threeNumeral : Determiner =
    let forms = {
      msnom, fsnom, nsnom, fsacc, msgen, fsgen = "tři" ;
      msdat = "třem" ;
      msloc = "třech" ;
      msins,fsins = "třemi" ;
      }
    in numeralFormsDeterminer forms Num2_4 ;

  fourNumeral : Determiner =
    let forms = {
      msnom, fsnom, nsnom, fsacc = "čtyři" ;
      msgen, fsgen = "čtyř" ;
      msdat = "čtyřem" ;
      msloc = "čtyřech" ;
      msins,fsins = "čtyřmi" ;
      }
    in numeralFormsDeterminer forms Num2_4 ;

  -- for the numbers 5 upwards
  regNumeral : Str -> Str -> Determiner = \pet,peti ->
    let forms = {
      msnom,fsnom,nsnom = pet ;
      msgen, fsgen, msdat, fsacc, msloc, msins, fsins = peti
      }
    in numeralFormsDeterminer forms Num5 ;

  invarDeterminer : Str -> NumSize -> Determiner = \sto,size ->
    regNumeral sto sto ;

  invarNumeral : Str -> Determiner = \s -> invarDeterminer s Num5 ;

--------------------------------
-- combining nouns with numerals

param
  NumSize = Num1 | Num2_4 | Num5 ; -- CEG 6.1

oper
  nounGender : Noun -> Number -> Gender = \cn,n -> case n of {
    Sg => cn.g ; Pl => cn.gPl
    } ;

  numSizeForm : (Number => Case => Str) -> NumSize -> Case -> Str
    = \cns,n,c -> case n of {
        Num1   => cns ! Sg ! c ;
	Num2_4 => cns ! Pl ! c ;
	Num5   => case c of {
	  Nom | Acc => cns ! Pl ! Gen ;
	  _ => cns ! Pl ! c
	  }
	} ;

  numSizeAgr : Gender -> NumSize -> Person -> Agr
    = \g,ns,p -> case ns of {
        Num5   => Ag Neutr Sg p ; -- essential grammar 6.1.4
	Num2_4 => Ag g Pl p ;
	Num1   => Ag g Sg p
	} ;

  numSizeNumber : NumSize -> Number = \ns -> case ns of {
    Num1 => Sg ;
    _ => Pl      ---- TO CHECK
    } ;
}
