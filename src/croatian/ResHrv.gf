resource ResHrv = open Prelude in {

-- AR September 2022
-- sources:
-- Wiki = https://en.wikipedia.org/wiki/Serbo-Croatian_grammar
-- BCMS = Bosnian, Croatian, Montenegrin and Serbian: An Essential Grammar (Routledge Essential Grammars) 1st Edition, by Željko Vrabec

-- parameters

param
  Number = Sg | Pl ;

  Animacy = Anim | Inanim ;
  Gender = Masc Animacy | Fem | Neutr ;

  Case = Nom | Gen | Dat | Acc | Voc | Loc | Ins ; -- traditional order

  Person = P1 | P2 | P3 ;

  Agr = Ag Gender Number Person ;

  CTense = CTPres | CTPast ; ----- TODO complete the tense system to match BCS verb morphology

-- phonology

oper
  hardConsonant : pattern Str = #("d"|"t"|"g"|"h"|"k"|"n"|"r") ; ----
  softConsonant : pattern Str = #("c"|"č"|"ć"|"đ"|"j"|"lj"|"nj"|"š"|"ž"|"št") ;
  --- wiki "and sometimes r"; BCMS slightly different

  ifSoft : Str -> (soft,hard : Str) -> Str = \word, ssoft, shard ->
    case word of {
      _ + #softConsonant => ssoft ;
      _ => shard
      } ;


palatalize : Str -> Str = \s -> case s of {
  x + "ki" => x + "ci" ;
  x + "ke" => x + "če" ;
  x + "gi" => x + "zi" ;
  x + "ge" => x + "že" ;
  x + "hi" => x + "si" ;
  x + "he" => x + "še" ;
  x + "ce" => x + "če" ; ---- TODO stric but check sins: stricem, klincem, pacc klince
  _ => s
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
	  Nom => forms.pnom ;
	  Gen => forms.pgen ;
	  Dat => forms.pdat ;
	  Acc => forms.pacc ;
	  Voc => forms.pnom ;
	  Loc => forms.ploc ;
	  Ins => forms.pins
	  }
	} ;
      g = forms.g
      } ;


-- terminology of CEG
  DeclensionType : Type = Str -> NounForms ;
{-
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

-}

  ifAnim : Animacy -> (anim, inanim : Str) -> Str = \anim, sanim, sinanim ->
    case anim of {
      Anim => sanim ;
      Inanim => sinanim
      } ;

-- the traditional declensions, following Wiki
-- they are also exported in ParadigmsHrv with names izvorN etc

  izvorN : Animacy -> DeclensionType = \anim, izvor ->
    {
      snom      = izvor ;
      sgen      = izvor + "a" ;
      sdat,sloc = izvor + "u" ;
      sacc      = ifAnim anim (izvor + "a") izvor ;
      svoc      = palatalize (izvor + "e") ;
      sins      = ifSoft izvor
                    (palatalize (izvor + "e") + "m") 
                    (izvor + "om") ;

      pnom,pvoc = palatalize (izvor + "i") ;
      pgen      = izvor + "a" ;
      pdat,ploc,pins =
        ifSoft izvor
	  (palatalize (izvor + "e") + "vima")
	  (palatalize (izvor + "i") + "ma") ;
      pacc      = palatalize (izvor + "e") ;
      g = Masc anim
      } ;

   nokatN : Animacy -> DeclensionType = \anim, nokat ->
      let
        nokt = Predef.tk 2 nokat + last nokat
      in izvorN anim nokt ** {
        snom = nokat ;
	sacc = ifAnim anim (nokt + "a") nokat ;
	pgen = nokat + "a" ;
        } ;
	
   gradaninN : Animacy -> DeclensionType = \anim, gradanin ->
      let
        gradan = Predef.tk 2 gradanin ;
        gradanN = izvorN anim gradan 
      in izvorN anim gradanin ** {
        pnom,pvoc = gradanN.pnom ;
        pgen      = gradanN.pgen ;
        pdat,ploc,pins = gradanN.pdat ;
        pacc      = gradanN.pacc ;
        } ;
  
   -- vojnik, bubreg, trbuh, stric by izvorN, čvórak, klinac by nokatN
   ---- TODO 








{-


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

  possessivePron : Agr -> DemPronForms = \a -> case a of {

      Ag _ Sg P1 => otcovA "moj" ** {
        msnom = "môj" ; msgen = "môjho" ; msdat = "môjmu" ;
	msins = "mojím" ;
	ampnom = "moji" ;
	nsnom, fpnom = "moje" ;
	pgen = "mojich" ;
	pdat = "mojim" ;
	pins = "mojimi" ;
	} ;
	
      Ag _ Sg P2 => otcovA "tvoj" ** {
        msnom = "tvoj" ; msgen = "tvojho" ; msdat = "tvojmu" ;
	msins = "tvojím" ;
	ampnom = "tvoji" ;
	nsnom, fpnom = "tvoje" ;
	pgen = "tvojich" ;
	pdat = "tvojim" ;
	pins = "tvojimi" ;
	} ;

      Ag _ Pl P1 => otcovA "naš" ** {
        msnom = "náš" ; msgen = "nášho" ; msdat = "nášmu" ;
	msins = "naším" ;
	ampnom = "naši" ;
	nsnom, fpnom = "naše" ;
	pgen = "našich" ;
	pdat = "našim" ;
	pins = "našimi" ;
	} ;
	
      Ag _ Pl P2 => otcovA "vaš" ** {
        msnom = "váš" ; msgen = "vášho" ; msdat = "vášmu" ;
	msins = "vaším" ;
	ampnom = "vaši" ;
	nsnom, fpnom = "vaše" ;
	pgen = "vašich" ;
	pdat = "vašim" ;
	pins = "vašimi" ;
	} ;
	
      Ag (Masc _ | Neutr) Sg P3 => invarDemPronForms "jeho" ** {pdat = "jeho"} ;
      Ag Fem Sg P3 => invarDemPronForms "jej" ** {pdat = "jej"} ;
      Ag _ Pl P3 => invarDemPronForms "ich" ** {pdat = "ich"}

    } ;

  reflPossessivePron : DemPronForms = otcovA "svoj" ** {
        msnom = "svoj" ; msgen = "svojho" ; msdat = "svojmu" ;
	msins = "svojím" ;
	ampnom = "svoji" ;
	nsnom, fpnom = "svoje" ;
	pgen = "svojich" ;
	pdat = "svojim" ;
	pins = "svojimi" ;
	} ;

  mkPron : Agr -> PronForms ** {poss : DemPronForms} = \a ->
    personalPron a ** {poss = possessivePron a} ;

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
      
  justDemPronFormsAdjective : DemPronForms -> Adjective =
    \dem ->
    let
      demAdj = dem ** {fsdat = dem.fsgen} ;
      adjAdj = adjFormsAdjective demAdj
    in {
      s = \\g,n,c => case <g,n,c> of {
        <_,Pl,Dat> => dem.pdat ;
	<Masc Anim,         Pl,     Acc> => dem.pgen ;
	<Masc Inanim | Fem | Neutr, Pl, Nom|Acc> => dem.fpnom ;
        _ => adjAdj.s ! g ! n ! c
        }
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
-}
}
