resource ResSlo = open Prelude in {

-- AR March 2020
-- sources:
-- Wiki = https://en.wikipedia.org/wiki/Czech_declension, https://en.wikipedia.org/wiki/Czech_conjugation
-- CEG  = J. Naughton, Czech: an Essential Grammar, Routledge 2005.

-- parameters

param
  Number = Sg | Pl ;

  Animacy = Anim | Inanim ;
  Gender = Masc Animacy | Fem | Neutr ;

  Case = Nom | Gen | Dat | Acc | Loc | Ins ; -- traditional order

  Person = P1 | P2 | P3 ;

  Agr = Ag Gender Number Person ;

  CTense = CTPres | CTPast ; ----- TODO complete the tense system to match Czech verb morphology

-- phonology

oper
  hardConsonant    : pattern Str = #("d"|"t"|"g"|"h"|"k"|"n"|"r") ;
  softConsonant    : pattern Str = #("ť"|"ď"|"j"|"ň"|"š"|"c"|"č"|"ž"|"ľ"|"ĺ"|"ŕ"|"dz") ;
  neutralConsonant : pattern Str = #("b"|"f"|"l"|"m"|"p"|"s"|"v") ;

  consonant : pattern Str =
    #(
      "d" | "t" | "g" | "h" | "k" | "n" | "r" |
      "ť" | "ď" | "j" | "ň" | "š" | "c" | "č" | "ž" |
      "b" | "f" | "l" | "m" | "p" | "s" | "v" | "ľ" | "ĺ" | "ŕ" | "dz"
      ) ;

  accentedVowel : pattern Str = #("á"|"é"|"í"|"ú"|"ý") ;

  diphthong : pattern Str = #("ia"|"ie"|"iu"|"ô") ;

  addAccented : (stem,long,short : Str) -> Str = \stem,long,short ->
    case stem of {
      _ + (#accentedVowel | #diphthong) + ? + ? => stem + short ;
      _ + (#accentedVowel | #diphthong) + ?     => stem + short ;
      _ => stem + long
      } ;

palatal : Str -> Str = \s -> case s of {
  x + "ď" => x + "d" ;
  x + "ť" => x + "t" ;
  x + "ň" => x + "n" ;
  x + "ľ" => x + "l" ;
  _ => s
  } ;

---------------
-- Nouns
---------------

-- https://en.wikipedia.org/wiki/Slovak_declension
-- http://www.angelfire.com/sk3/quality/Slovak_declension.html

-- novel idea (for RGL): lexical items stored as records rather than tables
-- advantages:
-- - easier to make exceptions to paradigms (by ** {})
-- - easier to keep the number of forms minimal
-- - easier to see what is happening than with lots of anonymous arguments to mkN, mkA, mkV

-- so this is the lincat of N

  NounForms : Type = {snom,sgen,sdat,sacc,sloc,sins, pnom,pgen,pdat,pacc,ploc,pins : Str ; g : Gender} ;

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
	  Loc => forms.sloc ;
	  Ins => forms.sins
	  } ;
        Pl => table {
	  Nom => forms.pnom ;
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

  declensionNounForms : (snom,pgen : Str) -> Gender -> NounForms
    = \snom,pgen,g -> case <g, snom, pgen> of {
      <Masc Anim,   _ + "a"           , _ + "ov">   => hrdinaN snom ;
      <Masc _,      _ + ("i"|"y"|"e") , _ + "ov">   => ponyN snom ; ----
      <Masc Anim,   _                 , _ + "ov">   => chlapN snom ;
      
      <Masc Inanim, _ + #softConsonant,   _ + "ov"> => strojN snom ;
      <Masc Ianim,  _ + #hardConsonant,   _ + "ov"> => dubN snom ;
      <Masc Ianim,  _ + #neutralConsonant,_ + "ov"> => dubN snom ;

      <Fem,         _ + #hardConsonant + "a",    _ + #consonant>  => zenaN snom ;
      <Fem,         _ + #neutralConsonant + "a", _ + #consonant>  => zenaN snom ;
      <Fem,         _ + #softConsonant + "a",    _ + #consonant>  => ulicaN snom ;
      <Fem,         _ + ("ia"|"ya"),             _>               => ulicaN snom ;
      <Fem,         _ + ("c"|"s"|"p"|"v"|"sť"),  _ + "í">         => kostN snom pgen ;
      <Fem,         _ + #consonant            ,  _ + "í">         => dlanN snom pgen ;

      <Neutr,       _ + "o"                   ,  _      >         => mestoN snom ;
      <Neutr,       _ + "ie"                  ,  _ + "í">         => vysvedcenieN snom ;
      <Neutr,       _ + "e"                   ,  _      >         => srdceN snom ;
      <Neutr,       _ + ("a"|"ä")             ,  _ + "iec">       => dievceniecN snom ;
      <Neutr,       _ + ("a"|"ä")             ,  _        >       => dievcaN snom ;

      _ => dubN (""+snom) ** {pgen = pgen} ---- Predef.error ("cannot infer declension type for" ++ snom ++ pgen)
      } ** {pgen = pgen ; g = g} ;

-- the "smartest" one-argument mkN

  guessNounForms : Str -> NounForms
    = \snom -> case snom of {
        _ + ("i"|"y"|"e")           => ponyN snom ;
        _ + #softConsonant          => strojN snom ;
        _ + #hardConsonant          => dubN snom ;
        _ + #neutralConsonant       => dubN snom ;
        _ + #hardConsonant + "a"    => zenaN snom ;
        _ + #neutralConsonant + "a" => zenaN snom ;
        _ + #softConsonant + "a"    => ulicaN snom ;
        _ + ("ia"|"ya")             => ulicaN snom ;
        _ + "o"                     => mestoN snom ;
        _ + "ie"                    => vysvedcenieN snom ;
        _ + "e"                     => srdceN snom ;
        _ + "ä"                     => dievcaN snom ;

        _ => dubN (""+snom) ---- Predef.error ("cannot guess declension type for" ++ snom)
      } ;


-- the traditional declensions, in both CEG and Wiki
-- they are also exported in ParadigmsSlo with names chlapN etc

  chlapN : DeclensionType = \chlap ->
    {
      snom      = chlap ;
      sgen,sacc = chlap + "a" ;
      sdat,sloc = chlap + "ovi" ;
      sins      = chlap + "om" ;

      pnom      = palatal chlap + "i" ;
      pgen,pacc = chlap + "ov" ;
      pdat      = chlap + "om" ;
      ploc      = chlap + "och" ;
      pins      = chlap + "mi" ;
      g = Masc Anim
      } ;

  hrdinaN : DeclensionType = \hrdina -> 
    let hrdin = init hrdina
    in
    {
      snom      = hrdina ;
      sgen,sacc = hrdin + "u" ;
      sdat,sloc = hrdin + "ovi" ;
      sins      = hrdin + "om" ;

      pnom      = hrdin + "ovia" ;
      pgen,pacc = hrdin + "ov" ;
      pdat      = hrdin + "om" ;
      ploc      = hrdin + "och" ;
      pins      = hrdin + "ami" ;

      g = Masc Anim
      } ;
      
  dubN : DeclensionType = \dub -> 
    {
      snom      = dub ;
      sgen      = dub + "a" ;
      sdat      = dub + "u" ;
      sacc      = dub + "" ;
      sloc      = dub + "e" ;
      sins      = dub + "om" ;

      pnom      = dub + "y" ;
      pgen      = dub + "ov" ;
      pdat      = dub + "om" ;
      pacc      = dub + "y" ;
      ploc      = dub + "och" ;
      pins      = dub + "mi" ;

      g = Masc Inanim
      } ;

  strojN : DeclensionType = \stroj ->
    let pstroj = palatal stroj in
    {
      snom      = stroj ;
      sgen      = stroj + "a" ;
      sdat      = stroj + "u" ;
      sacc      = stroj + "" ;
      sloc      = stroj + "i" ;
      sins      = stroj + "om" ;

      pnom      = pstroj + "e" ;
      pgen      = stroj + "ov" ;
      pdat      = pstroj + "om" ;
      pacc      = stroj + "e" ;
      ploc      = stroj + "och" ;
      pins      = stroj + "mi" ;

      g = Masc Inanim
      } ;

-- "fifth type"
  ponyN : DeclensionType = \pony -> 
    {
      snom      = pony ;
      sgen      = pony + "ho" ;
      sdat      = pony + "mu" ;
      sacc      = pony + "ho" ;
      sloc      = pony + "m" ;
      sins      = pony + "m" ;

      pnom      = pony + "ovia" ;
      pgen      = pony + "ov" ;
      pdat      = pony + "om" ;
      pacc      = pony + "ov" ;
      ploc      = pony + "och" ;
      pins      = pony + "mi" ;

      g = Masc Anim
      } ;
      
  zenaN : DeclensionType = \zena -> 
    let
      zen = init zena ;
      zien : Str = case zen of {
        z + "e" + c@? => z + "ie" + c ;
	_ => zen ---- TODO: many more rules
        } ;
      zenaa = addAccented zen "á" "a" ;
    in
    {
      snom      = zena ;
      sgen      = zen + "y" ;
      sdat      = zen + "e" ;
      sacc      = zen + "u" ;
      sloc      = zen + "e" ;
      sins      = zen + "ou" ;

      pnom      = zen + "y" ;
      pgen      = zien ;
      pdat      = zenaa + "m" ;
      pacc      = zen + "y" ;
      ploc      = zenaa + "ch" ;
      pins      = zen + "ami" ;

      g = Fem
      } ;

  ulicaN : DeclensionType = \ulica -> 
    let
      ulic = init ulica ;
      ulíc : Str = case ulic of {
        ul + "i" + c@? => ul + "í" + c ;
	_ => ulic
        } ;
      pulic = palatal ulic ;
    in
    {
      snom      = ulica ;
      sgen      = pulic + "e" ;
      sdat      = pulic + "i" ;
      sacc      = ulic + "u" ;
      sloc      = pulic + "i" ;
      sins      = ulic + "ou" ;

      pnom      = pulic + "e" ;
      pgen      = ulíc ;
      pdat      = ulic + "iam" ;
      pacc      = pulic + "e" ;
      ploc      = pulic + "iach" ;
      pins      = ulic + "ami" ;

      g = Fem
      } ;

  dlanN : Str -> DeclensionType = \dlanj,dlani -> 
    let
      dlan : Str = init dlani ;
      pdlan = palatal dlan ;
    in
    {
      snom      = dlanj ;
      sgen      = pdlan + "e" ;
      sdat      = pdlan + "i" ;
      sacc      = dlanj ;
      sloc      = pdlan + "i" ;
      sins      = dlanj + "ou" ;

      pnom      = pdlan + "e" ;
      pgen      = dlani ;
      pdat      = pdlan + "iam" ;
      pacc      = pdlan + "e" ;
      ploc      = pdlan + "iach" ;
      pins      = dlanj + "ami" ;

      g = Fem
      } ;

  kostN : Str -> DeclensionType = \kost',kosti -> 
    let
      kost = init kosti ;
    in
    {
      snom      = kost' ;
      sgen      = kost + "i" ;
      sdat      = kost + "i" ;
      sacc      = kost' ;
      sloc      = kost + "i" ;
      sins      = kost' + "ou" ;

      pnom      = kost + "i" ;
      pgen      = kosti ;
      pdat      = kost + "iam" ;
      pacc      = kost + "i" ;
      ploc      = kost + "iach" ;
      pins      = kost' + "ami" ;

      g = Fem
      } ;

  mestoN : DeclensionType = \mesto -> 
    let
      mest = init mesto ;
      miest : Str = case mest of {
        m + "e" + c@(? | (? + ?)) => m + "ie" + c ;
	_ => mest ---- TODO: many more rules
        } ;
      mesta = addAccented mest "á" "a" ;
      pmest = palatal mest ;

    in
    {
      snom      = mesto ;
      sgen      = mest + "a" ;
      sdat      = mest + "u" ;
      sacc      = mesto ;
      sloc      = pmest + "e" ;
      sins      = mest + "om" ;

      pnom      = mesta ;
      pgen      = miest ;
      pdat      = mesta + "m" ;
      pacc      = mesta ;
      ploc      = mesta + "ch" ;
      pins      = mest + "ami" ;

      g = Neutr
      } ;

  srdceN : DeclensionType = \srdce -> 
    let
      srdc = init srdce ;
      psrdc = palatal srdc ;
    in
    {
      snom      = srdce ;
      sgen      = srdc + "a" ;
      sdat      = srdc + "u" ;
      sacc      = srdce ;
      sloc      = psrdc + "i" ;
      sins      = srdc + "om" ;

      pnom      = psrdc + "ia" ;
      pgen      = srdc ; ---- TODO sŕdc
      pdat      = psrdc + "iam" ;
      pacc      = psrdc + "ia" ;
      ploc      = psrdc + "iach" ;
      pins      = srdc + "ami" ;

      g = Neutr
      } ;

  vysvedcenieN : DeclensionType = \vysvedcenie -> 
    let
      vysvedceni = init vysvedcenie ;
      vysvedcen  = init vysvedceni
    in
    {
      snom      = vysvedcenie ;
      sgen      = vysvedceni + "a" ;
      sdat      = vysvedceni + "u" ;
      sacc      = vysvedcenie ;
      sloc      = vysvedcen + "í" ;
      sins      = vysvedcen + "ím" ;

      pnom      = vysvedceni + "a" ;
      pgen      = vysvedcen + "í" ;
      pdat      = vysvedceni + "am" ;
      pacc      = vysvedceni + "a" ;
      ploc      = vysvedceni + "ach" ;
      pins      = vysvedceni + "ami" ;

      g = Neutr
      } ;

  dievcaN : DeclensionType = \dievca ->
    let dievc = init dievca
    in
    {
      snom      = dievca ;
      sgen      = dievca + "ťa" ;
      sdat      = dievca + "ťu" ;
      sacc      = dievca ;
      sloc      = dievca + "ti" ;
      sins      = dievca + "ťom" ;

      pnom      = dievca + "tá" ;
      pgen      = dievc + "iat" ;
      pdat      = dievca + "tám" ;
      pacc      = dievca + "tá" ;
      ploc      = dievca + "tách" ;
      pins      = dievca + "tami" ;

      g = Neutr
      } ;

  -- with variant plural forms of the previous
  dievceniecN : DeclensionType = \dievca ->
    let dievc = init dievca
    in dievcaN dievca ** { 
      pnom      = dievc + "ence" ;
      pgen      = dievc + "eniec" ;
      pdat      = dievc + "encom" ;
      pacc      = dievc + "ence" ;
      ploc      = dievc + "encoch" ;
      pins      = dievc + "encami" ;
      } ;

---------------------------
-- Adjectives

-- to be used for AP: 56 forms for each degree
  Adjective : Type = {s : Gender => Number => Case => Str} ;

-- to be used for A, in three degrees: 15 forms in each
---- TODO other degrees than positive

  AdjForms : Type = {
    msnom, fsnom, nsnom : Str ; 
    msgen, fsgen : Str ;        -- nsgen = msgen
    msdat : Str ;               -- nsdat = msdat
    fsacc : Str ;               -- amsacc = msgen, imsacc = msnom, nsacc = nsnom
    msloc : Str ;               -- fsloc = fsdat, nsloc = msloc
    msins, fsins : Str ;        -- nsins = msins, pdat = msins

    ampnom : Str ;              -- *pnom = nsnom
    pgen : Str ;                --
                                -- pdat = msins, ampacc = pgen, *pacc = nsnom, ploc = pgen
    pins : Str ;
    } ;

invarAdjForms : Str -> AdjForms = \s -> {
    msnom, fsnom, nsnom, msgen, fsgen, msdat, fsacc,
    msloc, msins, fsins, ampnom, pgen, pins = s ;
    } ;

-- used in PositA but will also work in Compar and Superl by calling their record fields

adjFormsAdjective : AdjForms -> Adjective = \afs -> {
  s = \\g,n,c => case <n,c,g> of {

    <Sg, Nom, Masc _>
      | <Sg, Acc, Masc Inanim>  => afs.msnom ;
    <Sg, Nom, Fem>              => afs.fsnom ;
    <Sg, Nom|Acc, Neutr>    
      | <Pl, Nom|Acc, Masc Inanim|Fem|Neutr> => afs.nsnom ;
    <Sg, Gen, Masc _ | Neutr>
      | <Sg,Acc,Masc Anim>     => afs.msgen ;
    <Sg, Gen|Dat|Loc, Fem>     => afs.fsgen ;
    <Sg, Dat, Masc _|Neutr>    => afs.msdat ;

    <Sg, Acc, Fem>             => afs.fsacc ;
    <Sg, Loc, Masc _|Neutr>    => afs.msloc ;
    <Sg, Ins, Masc _|Neutr>
      | <Pl,Dat,_>             => afs.msins ;
    <Sg, Ins, Fem>             => afs.fsins ;

    <Pl, Nom, Masc Anim>       => afs.ampnom ;
    <Pl, Gen|Loc,_> 
      | <Pl, Acc, Masc Anim>   => afs.pgen ;
    <Pl, Ins,_>     => afs.pins
    }

    } ;

  guessAdjForms : Str -> AdjForms
    = \s -> case s of {
        _ + "ý"  => peknyA s ;
        _ + "y"  => krasnyA s ;
        _ + "í"  => cudziA s ;
        _ + "i"  => rydziA s ;
        _ + ("ov"|"in") => otcovA s ;
        _ => otcovA (""+s)  ---- Predef.error ("no mkA for" ++ s)
        } ;


-- hard consonant + y

  peknyA : Str -> AdjForms = \pekny ->
    let pekn = init pekny
    in {
      msnom   = pekn + "ý" ;
      fsnom   = pekn + "á" ;
      nsnom   = pekn + "é" ;
      msgen   = pekn + "ého" ;
      fsgen   = pekn + "ej" ;
      msdat   = pekn + "ému" ;
      fsacc   = pekn + "ú" ;
      msloc   = pekn + "om" ;
      msins   = pekn + "ým" ;
      fsins   = pekn + "ou" ;
      ampnom  = pekn + "í" ;
      pgen    = pekn + "ých" ;
      pins    = pekn + "ými" ;
      } ;

-- if the penultimate has accent, e.g. krásny, the last accent disappears
  krasnyA : Str -> AdjForms = \krasny ->
    let
      krasn = init krasny ;
    in peknyA krasny ** {
      msnom   = krasn + "y" ;
      fsnom   = krasn + "a" ;
      nsnom   = krasn + "e" ;
      msgen   = krasn + "eho" ;
      msdat   = krasn + "emu" ;
      fsacc   = krasn + "u" ;
      msins   = krasn + "ym" ;
      ampnom  = krasn + "i" ;
      pgen    = krasn + "ych" ;
      pins    = krasn + "ymi" ;
      } ;

-- soft consonant + i

  cudziA : Str -> AdjForms = \cudzi ->
    let
      cudz = init cudzi ;
      pcudz = palatal cudz ;
    in {
      msnom   = pcudz + "í" ;
      fsnom   = pcudz + "ia" ;
      nsnom   = pcudz + "ie" ;
      msgen   = pcudz + "ieho" ;
      fsgen   = pcudz + "ej" ;
      msdat   = pcudz + "iemu" ;
      fsacc   = pcudz + "iu" ;
      msloc   = cudz + "om" ;
      msins   = pcudz + "ím" ;
      fsins   = cudz + "ou" ;
      ampnom  = pcudz + "í" ;
      pgen    = pcudz + "ích" ;
      pins    = pcudz + "ími" ;
      } ;

-- accented vowel + soft consonant + i
  rydziA : Str -> AdjForms = \rydzi ->
    let
      rydz = init rydzi ;
      prydz = palatal rydz ;
    in peknyA rydzi ** {
      msnom   = prydz + "i" ;
      fsnom   = rydz + "a" ;
      nsnom   = prydz + "e" ;
      msgen   = prydz + "eho" ;
      msdat   = prydz + "emu" ;
      fsacc   = rydz + "u" ;
      msins   = prydz + "im" ;
      ampnom  = prydz + "i" ;
      pgen    = prydz + "ich" ;
      pins    = prydz + "imi" ;
      } ;

-- masculine possession: the same endings as in feminine

  otcovA : Str -> AdjForms = \otcov ->
     {
      msnom   = otcov ;
      fsnom   = otcov + "a" ;
      nsnom   = otcov + "o" ;
      msgen   = otcov + "ho" ;
      fsgen   = otcov + "ej" ;
      msdat   = otcov + "mu" ;
      fsacc   = otcov + "u" ;
      msloc   = otcov + "om" ;
      msins   = otcov + "ým" ;
      fsins   = otcov + "ou" ;
      ampnom  = otcov + "i" ;
      pgen    = otcov + "ých" ;
      pins    = otcov + "ými" ;
      } ;

  paviA : Str -> AdjForms = \pavi ->
    let
      pav = init pavi ;
    in {
      msnom = pav + "í" ;
      fsnom = pav + "ia" ;
      nsnom = pav + "ie" ;
      msgen = pav + "ieho" ;
      fsgen = pav + "ej" ;
      msdat = pav + "iemu" ;
      fsacc = pav + "iu" ;
      msloc = pav + "om" ;
      msins = pav + "ím" ; 
      fsins = pav + "ou" ;
      ampnom = pav + "í" ; ----
      pgen = pav + "ich" ; ----
      pins = pav + "imi" ; ----
      } ;
 
---------------------
-- Verbs
-- https://en.wikipedia.org/wiki/Slovak_language#Verbs

  VerbForms : Type = {          ---- TODO more forms to add ?
    inf,
    pressg1, pressg2, pressg3,
    prespl1, prespl2, prespl3,
    pastpmasc, pastpfem, pastpneutr : Str
    } ;

  ComplementCase : Type = {s : Str ; c : Case ; hasPrep : Bool} ;

  verbAgr : VerbForms -> Agr -> Bool -> Str   ---- TODO tenses
    = \vf,a,b -> case a of {
      Ag _ Sg P1 => vf.pressg1 ;
      Ag _ Sg P2 => vf.pressg2 ;
      Ag _ Sg P3 => vf.pressg3 ;
      Ag _ Pl P1 => vf.prespl1 ;
      Ag _ Pl P2 => vf.prespl2 ;
      Ag _ Pl P3 => vf.prespl3
      } ;

  copulaVerbForms : VerbForms = {
    inf = "byť" ;
    pressg1 = "som" ;
    pressg2 = "si" ;
    pressg3 = "je" ;
    prespl1 = "sme" ;
    prespl2 = "ste" ;
    prespl3 = "sú" ;
    pastpmasc = "bol" ;
    pastpfem = "bola" ;
    pastpneutr = "bolo" ;
    } ;

  haveVerbForms : VerbForms = {
    inf = "mať" ;
    pressg1 = "mám" ;
    pressg2 = "máš" ;
    pressg3 = "má" ;
    prespl1 = "máme" ;
    prespl2 = "máte" ;
    prespl3 = "majú" ;
    pastpmasc = "mal" ;
    pastpfem = "mala" ;
    pastpneutr = "malo" ;
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
    pressg1 = kupu + "jem" ;
    pressg2 = kupu + "ješ" ;
    pressg3 = kupu + "je" ;
    prespl1 = kupu + "jeme" ;
    prespl2 = kupu + "jete" ;
    prespl3 = kupu + "jú" ;
    pastpmasc = "kupoval" ;
    pastpfem = "kupovala" ;
    pastpneutr = "kupovalo" ;    
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
        nom = "ja" ;
        gen,acc,pgen,pacc = "mňa" ;
        cgen,cacc = "ma" ;
        dat,pdat,loc = "mne" ;
        cdat = "mi" ;
        ins,pins = "mnou"
        } ;
      Ag _ Sg P2 => {
        nom = "ty" ;
        gen,acc,pgen,pacc = "teba" ;
        cgen,cacc = "ťa" ;
        dat,pdat,loc = "tebe" ;
        cdat = "ti" ;
        ins,pins = "tebou"
        } ;
      Ag (Masc _) Sg P3 => {
        nom = "on" ;
        gen,acc = "jeho" ;
        cgen,cacc = "ho" ;
        pgen,pacc = "neho" ;
        ---- pgen = "ňho"; ---- bind, only with prepositions "do", "u"
        ---- pgen = "ň"; ---- bind, only with preposition "do", "u"
        dat = "jemu" ;
        ---- pacc = "eň" ; ---- bind, only with preposition "cez" 
        ---- pacc = "ňho" ; ---- bind, only with prepositions "na", "pre", "za"
        ---- pacc = "ň" ; ---- bind, only with preposition "na", "pre", "za"
	cdat = "mu" ;
	pdat = "nemu" ;
	loc = "ňom" ;
	ins,pins = "ním" ;
        } ;
      Ag Fem Sg P3 => {
        nom = "ona" ;
        gen,cgen,pgen,acc,cacc,pacc = "ju" ;
        cdat = "jej" ;
	dat,pdat,loc = "nej" ;
        ins,pins = "ní" ;
        } ;
      Ag Neutr Sg P3 => {
        nom = "ono" ;
        gen, acc = "jeho" ;
        cgen,cacc = "ho" ;
        pgen = "neho" ;
        ---- pgen = "ňho"; ---- bind, only with prepositions "do", "u"
        ---- pgen = "ň"; ---- bind, only with preposition "do", "u"
        dat = "jemu" ; 
        ---- pacc = "eň" ; ---- bind, only with preposition "cez" 
        ---- pacc = "ňho" ; ---- bind, only with prepositions "na", "pre", "za"
        ---- pacc = "ň" ; ---- bind, only with preposition "na", "pre", "za"
        pacc = "jeho"; ---- doublecheck
	cdat = "mu" ;
	pdat = "nemu" ;
	loc = "ňom" ;
	ins,pins = "ním" ;
        } ;
      Ag _ Pl P1 => {
        nom = "my" ;
        gen,acc,
          cgen,cacc,
          pgen,pacc,
	  loc  = "nás" ;
        dat,cdat,pdat = "nám" ;
	ins,pins = "nami" ;
        } ;
      Ag _ Pl P2 => {
        nom = "vy" ;
        gen,acc,
          cgen,cacc,
          pgen,pacc,
	  loc  = "vás" ;
        dat,cdat,pdat = "vám" ;
	ins,pins = "vami" ;
        } ;
      Ag (Masc Anim) Pl P3 => {
        nom = "oni" ;
	gen,cgen,acc,cacc = "ich" ;
	pgen,pacc = "nich" ;
	dat,cdat = "im" ;
	pdat = "nim" ;
	loc = "nich" ;
	ins,pins = "nimi" ;
	} ;
      Ag _ Pl P3 => {
        nom = "ony" ;
	gen,cgen,acc,cacc = "ich" ;
	pgen,pacc = "ne" ;
	dat,cdat = "im" ;
	pdat = "nim" ;
	loc = "nich" ;
	ins,pins = "nimi" ;
	}

      } ;

--------------------------------
-- demonstrative pronouns, used for Quant and Det

oper
  DemPronForms : Type = {
    msnom, fsnom, nsnom, 
    msgen, fsgen, pgen,
    msdat, -- fsdat = fsgen unlike AdjForms
    fsacc,
    msloc,
    msins, fsins,
    ampnom, fpnom, -- mpacc = fpacc = fpnom
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
	<Masc Anim,         Pl,     Acc> => dem.pgen ;
	<Masc Inanim | Fem | Neutr, Pl, Nom|Acc> => dem.fpnom ;
        _ => adjAdj.s ! g ! n ! c
        } + s
      } ;

  Determiner : Type = {
    s : Gender => Case => Str ;
    size : NumSize
    } ;

  mkDemPronForms : Str -> DemPronForms = \jedn -> {
      msnom   = jedn + "y" ; -- should be "jeden"
      fsnom   = jedn + "a" ;
      nsnom   = jedn + "o" ;
      msgen   = jedn + "ého" ;
      fsgen   = jedn + "ej" ;
      msdat   = jedn + "ému" ;
      fsacc   = jedn + "u" ;
      msloc   = jedn + "om" ;
      msins   = jedn + "ým" ;
      fsins   = jedn + "ou" ;
      ampnom  = jedn + "i" ;
      fpnom   = jedn + "y" ;
      pgen    = jedn + "ých" ;
      pdat    = jedn + "ým" ;
      pins    = jedn + "ými" ;
    } ;
    
  tenDemPronForms : Str -> DemPronForms = \tam -> {
      msnom   = tam + "ten" ;
      fsnom   = tam + "tá" ;
      nsnom   = tam + "to" ;
      msgen   = tam + "toho" ;
      fsgen   = tam + "tej" ;
      msdat   = tam + "tomu" ;
      fsacc   = tam + "tú" ;
      msloc   = tam + "tom" ;
      msins   = tam + "tým" ;
      fsins   = tam + "tou" ;
      ampnom  = tam + "tí" ;
      fpnom   = tam + "tie" ;
      pgen    = tam + "tých" ;
      pdat    = tam + "tým" ;
      pins    = tam + "tými" ;
    } ;

  invarDemPronForms : Str -> DemPronForms = \s -> {
    msnom, fsnom, nsnom, msgen, fsgen,
    msdat, fsacc, msloc, msins, fsins,
    ampnom, fpnom, pgen, pdat, pins = s ;
    } ;

-- interrogatives

 ktoForms : Case => Str = table {
   Nom => "kto" ;
   Gen | Acc => "koho" ;
   Dat => "komu" ;
   Loc => "kom" ;
   Ins => "kým"
   } ;

 coForms : Case => Str = table {
   Nom|Acc => "čo" ;
   Gen => "čoho" ;
   Dat => "čomu" ;
   Loc => "čom" ;
   Ins => "čím"
   } ;

-- Numerals

  -- singular forms of demonstratives
  NumeralForms : Type = {
 ----   amsnom,
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
        {ampnom, fpnom, pgen, pdat, pins = nume.msnom} ; --- plural forms not used
      demAdj = dem ** {fsdat = dem.fsgen} ;
      adjAdj = adjFormsAdjective demAdj
    in {
      s = \\g,c => case <g,c> of {
----         <Masc Anim, Nom> => nume.amsnom ;
         _ => adjAdj.s ! g ! Sg ! c
	 } ;
      size = size
      } ;

  -- example: number 1
  oneNumeral : Determiner = numeralFormsDeterminer ((mkDemPronForms "jedn") ** {msnom = "jeden"}) Num1 ;

  -- numbers 2,3,4 ---- to check if everything comes out right with the determiner type
  twoNumeral : Determiner =
    let forms = {
----    amsnom = "dvaja" ;
      msnom = "dva" ; fsnom, nsnom, fsacc = "dve" ; 
      msgen, fsgen, msloc = "dvoch" ;
      msdat = "dvom" ;
      msins, fsins = "dvoma"
      }
    in numeralFormsDeterminer forms Num2_4 ;

  threeNumeral : Determiner =
    let forms = {
      ---- amsnom = "traja" ;
      msnom, fsnom, nsnom, fsacc = "tri" ; ---- amsacc = "troch"
      msgen, fsgen = "troch" ;
      msdat = "trom" ;
      msloc = "troch" ;
      msins,fsins = "tromi" ;
      }
    in numeralFormsDeterminer forms Num2_4 ;

  fourNumeral : Determiner =
      let forms = {
      ---- amsnom = "štyria" ;
      msnom, fsnom, nsnom, fsacc = "štyri" ; ---- amsacc = "štyroch"
      msgen, fsgen = "štyroch" ;
      msdat = "štyrom" ;
      msloc = "štyroch" ;
      msins,fsins = "štyrmi" ;
      }
    in numeralFormsDeterminer forms Num2_4 ;

  -- for the numbers 5 upwards
  regNumeral : Str -> Str -> Str -> Str -> Determiner = \pät,piatich,piatim,piatimi ->
    let forms = {
      msnom,fsnom,nsnom, fsacc = pät ;
      msgen, fsgen, msloc = piatich ;
      msdat = piatim ;
      msins, fsins = piatimi ;
      }
    in numeralFormsDeterminer forms Num5 ;

  invarDeterminer : Str -> NumSize -> Determiner = \sto,size ->
    regNumeral sto sto sto sto ;

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
