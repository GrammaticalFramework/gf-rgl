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

  Agr = Ag Gender Number Person ;

  CTense = CTPres | CTPast ; ----- TODO complete the tense system to match Czech verb morphology

-- phonology

oper
  hardConsonant    : pattern Str = #("d"|"t"|"g"|"h"|"k"|"n"|"r") ;
  softConsonant    : pattern Str = #("ť"|"ď"|"j"|"ň"|"ř"|"š"|"c"|"č"|"ž") ;
  neutralConsonant : pattern Str = #("b"|"f"|"l"|"m"|"p"|"s"|"v") ;

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

  -- 3.4.10, in particular when also final 'a' is dropped
  addE : Str -> Str = \s -> case s of {
    re + "k"   => re + "ce" ;
    pra + ("g"|"h") => pra + "ze" ;
    stre + "ch" => stre  + "še" ;
    sest + "r" => sest + "ře" ;
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

  NounForms : Type = {snom,sgen,sdat,sacc,svoc,sloc,sins, pnom,pgen,pdat,pacc,ploc,pins : Str ; g : Gender} ;

-- But traditional tables make agreement easier to handle in syntax
-- so this is the lincat of CN

  Noun : Type = {s : Number => Case => Str ; g : Gender} ;

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
      g = forms.g
      } ;

-- terminology of CEG
  DeclensionType : Type = Str -> NounForms ;

  declensionNounForms : (nom,gen : Str) -> Gender -> NounForms
    = \nom,gen,g ->
    let decl : DeclensionType = case <g, nom, gen> of {
      <Masc Anim,   _ + #hardConsonant, _ + "a"> => declPAN ;
      <Masc Anim,   _ + "a"           , _ + "a"> => declPREDSEDA ;
      <Masc Inanim, _ + #hardConsonant, _ + "u"> => declHRAD ;
      <Fem,         _ + "a"           , _ + "y"> => declZENA ;
      <Neutr,       _ + "o"           , _ + "a"> => declMESTO ;
      <Masc Anim,   _ + #softConsonant, _ + "e"> => declMUZ ;
      <Masc Anim,   _ + "tel"         , _ + "e"> => declMUZ ;
      <Masc Anim,   _ + "ce"          , _ + "e"> => declSOUDCE ;
      <Masc Inanim, _ + #softConsonant, _ + "e"> => declSTROJ ;
      <Fem,         _ + ("e"|"ě")     , _ + ("e"|"ě")> => declRUZE ;
      <Fem,         _ + #softConsonant, _ + "e"> => declPISEN ;
      <Fem,         _ + "ost"         , _ + "i"> => declKOST ;  --- also many other "st" 3.6.3
      <Neutr,       _ + "e"           , _+"ete"> => declKURE ;
      <Neutr,       _ + "e"           , _ + "e"> => declMORE ;
      <Neutr,       _ + "í"           , _ + "í"> => declSTAVENI ;
      _ => (\s -> declSTROJ ("" + s)) -- Predef.error ("cannot infer declension type for" ++ nom ++ gen)
      }
    in decl nom ;

-- the "smartest" one-argument mkN

  guessNounForms : Str -> NounForms
    = \s -> case s of {
      _ + "ost"          => declKOST s ;
      _ + "tel"          => declMUZ s ;
      _ + #hardConsonant => declHRAD s ;
      _ + #softConsonant => declSTROJ s ;
      _ + "a"            => declZENA s ;
      _ + "o"            => declMESTO s ;
      _ + "ce"           => declSOUDCE s ;
      _ + "e"            => declMORE s ;
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
      g = Masc Anim
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
      g = Masc Anim
      } ;

  declHRAD : DeclensionType = \hrad -> --- 3.5.2: sloc u/ě/e  extra arg, sport-u, hrad-ě ; sgen u/a
    let hrd = dropFleetingE hrad
    in
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
      g = Masc Inanim
      } ;

  declZENA : DeclensionType = \zena -> --- 3.6.1 sge y/i ; pgen sometimes shortening
    let zen = init zena
    in
    {
      snom      = zena ;
      sgen      = zen + "y" ;  --- i after soft cons sometimes
      sdat,sloc = zen + "ě" ;  --- i after soft cons sometimes ; skol+e
      sacc      = zen + "u" ;
      svoc      = shortenVowel zen + "o" ; ---- shorten ?
      sins      = zen + "ou" ;

      pnom,pacc = zen + "y" ;  --- also sgen
      pgen      = zen ; --- sometimes with vowel shortening
      pdat      = zen + "ám" ;
      ploc      = zen + "ách" ;
      pins      = zen + "ami" ;
      g = Fem
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
      g = Neutr
      } ;

  declMUZ : DeclensionType = \muz_ -> --- 3.5.3 : sdat,sloc ; pnom
    let muz = dropFleetingE muz_
    in
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
      g = Masc Anim
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
      g = Masc Anim
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
      g = Masc Inanim
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
      g = Fem
      } ;

  declPISEN : DeclensionType = \pisen ->
    let pisn = dropFleetingE pisen
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
      g = Fem
      } ;

  declKOST : DeclensionType = \kost ->
    {
      snom,sacc           = kost ;
      sgen,sdat,svoc,sloc = kost + "i" ; --- pnom,pacc
      sins                = kost + "í" ; --- pgen

      pnom,pacc      = kost + "i" ;
      pgen           = kost + "í" ;
      pdat           = kost + "em" ;
      ploc           = kost + "ech" ;
      pins           = kost + "mi" ;
      g = Fem
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
      g = Neutr
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
      g = Neutr
      } ;

  declSTAVENI : DeclensionType = \staveni ->
    {
      snom,sgen,sdat,sacc,svoc,sloc = staveni ;
      sins                          = staveni + "m" ;

      pnom,pgen,pacc = staveni ;
      pdat           = staveni + "m" ;
      ploc           = staveni + "ch" ;
      pins           = staveni + "mi" ;
      g = Neutr
      } ;

---------------------------
-- Adjectives

-- to be used for AP: 56 forms for each degree
  Adjective : Type = {s : Gender => Number => Case => Str} ;

-- to be used for A, in three degrees: 15 forms in each
---- TODO other degrees than positive

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

  VerbForms : Type = {          ---- TODO more forms to add
    inf,
    pressg1, pressg2, pressg3,
    prespl1, prespl2, prespl3,
    pastpartsg, pastpartpl,
----    passpart,
    negpressg3 : Str   -- matters only for copula
    } ;

  ComplementCase : Type = {s : Str ; c : Case ; hasPrep : Bool} ;

  verbAgr : VerbForms -> Agr -> Bool -> Str   ---- TODO tenses
    = \vf,a,b -> case a of {
      Ag _ Sg P1 => vf.pressg1 ;
      Ag _ Sg P2 => vf.pressg2 ;
      Ag _ Sg P3 => case b of {
        True  => vf.pressg3 ;
	False => vf.negpressg3 -- matters only for copula
	} ;
      Ag _ Pl P1 => vf.prespl1 ;
      Ag _ Pl P2 => vf.prespl2 ;
      Ag _ Pl P3 => vf.prespl3
      } ;

  copulaVerbForms : VerbForms = {
    inf = "být" ;
    pressg1 = "jsem" ;
    pressg2 = "jsi" ;
    pressg3 = "je" ;
    prespl1 = "jsme" ;
    prespl2 = "jste" ;
    prespl3 = "jsou" ;
    pastpartsg = "byl" ;
    pastpartpl = "byli" ;
    negpressg3 = "ní" ;  -- ne is added to this
    } ;

  haveVerbForms : VerbForms = {
    inf = "mít" ;
    pressg1 = "mám" ;
    pressg2 = "máš" ;
    pressg3, negpressg3 = "má" ;
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
   {
    inf = kupovat ;
    pressg1 = kupu + "ji" ; --- kupuju
    pressg2 = kupu + "ješ" ;
    pressg3, negpressg3 = kupu + "je" ;
    prespl1 = kupu + "jeme" ;
    prespl2 = kupu + "jete" ;
    prespl3 = kupu + "jí" ; --- kupujou
    pastpartsg = kupo + "val" ;
    pastpartpl = kupo + "vali" ;
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

---- TODO: possessives

  personalPron : Agr -> PronForms = \a ->
    {a = a ; cnom = []} **
    case a of {
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
        gen = "její" ;
        dat,acc,cgen,cacc,cdat,ins = "ji" ;
        pgen,pdat,pacc,loc,pins = "ní" ;
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
      Ag _ Pl P2 => {
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
	  Masc _ => "oni" ;
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
