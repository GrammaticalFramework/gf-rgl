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

  VForm =
      VInf
    | VPres Number Person
    | VPastPart Gender Number
    ;
    ---- TODO aorist, imperfect

  Agr = Ag Gender Number Person ;

  CTense = CTPres | CTPast ; ----- TODO complete the tense system to match BCS verb morphology

-- phonology

oper
  softConsonant : pattern Str = #("c"|"č"|"ć"|"đ"|"j"|"lj"|"nj"|"š"|"ž"|"št") ;
  --- wiki "and sometimes r"; BCMS slightly different

  ifSoft : Str -> (soft,hard : Str) -> Str = \word, ssoft, shard ->
    case word of {
      _ + #softConsonant => ssoft ;
      _ => shard
      } ;

   animate = Masc Anim ;
   inanimate = Masc Inanim ;
   feminine = Fem ;
   neuter = Neutr ;

palatalize : Str -> Str = \s -> case s of {
  x + "ki" => x + "ci" ;
  x + "ke" => x + "če" ;
  x + "gi" => x + "zi" ;
  x + "ge" => x + "že" ;
  x + "hi" => x + "si" ;
  x + "he" => x + "še" ;
  x + "ce" => x + "če" ;
  _ => s
  } ;

voicing : Str -> Str = \s -> case s of {
  x + "b"  => x + "p" ;
  x + "d"  => x + "t" ;
  x + "đ"  => x + "ć" ;
  x + "z"  => x + "s" ;
  x + "dž" => x + "č" ;
  x + "ž"  => x + "š" ;
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

  NounForms : Type = {snom,sgen,sdat,sacc,svoc,sins,pnom,pgen,pdat,pacc : Str} ;

-- But traditional tables make agreement easier to handle in syntax
-- so this is the lincat of CN

  Noun : Type = {s : Number => Case => Str ; g : Gender} ;

-- this is used in UseN

  nounFormsNoun : NounForms -> Gender -> Noun
    = \forms, g -> {
      s = table {
        Sg => table {
	  Nom => forms.snom ;
	  Gen => forms.sgen ;
	  Dat => forms.sdat ;
	  Acc => case g of {
	    Masc Anim | Fem => forms.sacc ;
	    _ => forms.snom
	    } ;
	  Voc => forms.svoc ;
	  Loc => forms.sdat ;
	  Ins => forms.sins
	  } ;
        Pl => table {
	  Nom => forms.pnom ;
	  Gen => forms.pgen ;
	  Dat => forms.pdat ;
	  Acc => forms.pacc ;
	  Voc => forms.pnom ;
	  Loc => forms.pdat ;
	  Ins => forms.pdat
	  }
	} ;
      g = g
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

-- the traditional declensions, following Wiki
-- they are also exported in ParadigmsHrv with names izvorN etc

  izvorN : DeclensionType = \izvor ->
    {
      snom = izvor ;
      sgen = izvor + "a" ;
      sdat = izvor + "u" ;
      sacc = izvor + "a" ;
      svoc = ifSoft izvor
                    (izvor + "u")
		    (palatalize (izvor + "e")) ;
      sins = ifSoft izvor
                    (izvor + "em") 
                    (izvor + "om") ;

      pnom = palatalize (izvor + "i") ;
      pgen = izvor + "a" ;
      pdat =
        ifSoft izvor
	  (palatalize (izvor + "e") + "vima")
	  (palatalize (izvor + "i") + "ma") ;
      pacc      = izvor + "e" ;
      } ;

   nokatN : DeclensionType = \nokat ->
      let
        nokt = Predef.tk 2 nokat + last nokat
      in izvorN nokt ** {
        snom = nokat ;
	sacc = nokt + "a" ;
	pgen = nokat + "a" ;
        } ;
	
   gradaninN : DeclensionType = \gradanin ->
      let
        gradan = Predef.tk 2 gradanin ;
        gradanN = izvorN gradan 
      in numbersNounForms (izvorN gradanin) gradanN ;

   numbersNounForms : (sg, pl : NounForms) -> NounForms =
     \sg, pl -> sg ** {
        pnom = pl.pnom ;
        pgen = pl.pgen ;
        pdat = pl.pdat ;
        pacc = pl.pacc ;
        } ;
  
   vojnikN : DeclensionType = izvorN ;
   bubregN : DeclensionType = izvorN ;
   trbuhN : DeclensionType = izvorN ;
   cvorakN : DeclensionType = nokatN ;

   panjN : DeclensionType = \panj ->
     numbersNounForms (izvorN panj) (izvorN (palatalize (panj + "e") + "v")) ;

   suzanjN : DeclensionType = \suzanj ->
     let
       suznj = Predef.tk 3 suzanj + Predef.dp 2 suzanj
     in
     numbersNounForms
       (izvorN suzanj)
       (izvorN suznj ** {
         pgen = "sužanja" ;
         pdat = "sužnjima"
         }) ;

    pristN : DeclensionType = panjN ;
    
    stricN : DeclensionType = \stric ->
      panjN stric ** {
        svoc = palatalize (stric + "e")
	} ;

    klinacN : DeclensionType = \klinac ->
      let
        klinc = Predef.tk 2 klinac + last klinac
      in nokatN klinac ** {
        svoc = palatalize (klinc + "e") ;
	pdat = klinc + "ima" ;
	} ;

    posjetilacN : DeclensionType = \posjetilac ->
      let
        posjetioc = Predef.tk 3 posjetilac + "oc"
      in izvorN posjetioc ** {
        snom = posjetilac ;
	svoc = palatalize (posjetioc + "e") ;
	sins = palatalize (posjetioc + "e") + "m" ;
	pgen = posjetilac + "a" ;
	pdat = palatalize (posjetioc + "i") + "ma" ;
        } ;

    pepeoN : DeclensionType = \pepeo ->
      let
        pepel = init pepeo + "l"
      in izvorN pepel ** {snom = pepeo} ;

    ugaoN : DeclensionType = \ugao ->
      let
        ugal = init ugao + "l" ;
	ugl = Predef.tk 2 ugal + last ugal
      in numbersNounForms
           (nokatN ugal ** {snom = ugao})
	   (izvorN (ugl + "ov")) ;
	   
    bifeN : DeclensionType = \bife ->
      izvorN bife ** {svoc = bife + "u"} ;
      
    ziriN : DeclensionType = \ziri ->
      bifeN (ziri + "j") ** {
        snom = ziri ;
	pdat = ziri + "jima" ;
	} ;

    taksiN : DeclensionType = ziriN ;

---- Danilo, Hrvoje, raščupànko skipped

    koljenoN : DeclensionType = \koljeno ->
      let
        koljen = init koljeno
      in izvorN koljen ** {
        snom, sacc, svoc = koljeno ;
	pnom, pacc, pvoc = koljen + "a" ;
        } ;

    jedroN : DeclensionType = \jedro ->
      let
        jed = Predef.tk 2 jedro ;
	r = last (init jedro) ; 
      in koljenoN jedro ** {
        pgen = jed + "a" + r + "a" ;
        } ;

---- drvo - drveta skipped, can also decline as koljeno
---- oči, čudo skipped

    poljeN : DeclensionType = \polje ->
      koljenoN polje ** {
        pdat = init polje + "ima" ;
        } ;
    
---- sunce, uže, zvonce, rame, podne, doba

---- no tables given in the sources for feminine nouns, so guessing from ending tables

    zenaN : DeclensionType = \zena -> 
      let
        zen = init zena
      in {
        snom = zena ;
	sgen = zen + "e" ;
	sdat = zen + "i" ;
	sacc = zen + "u" ;
	svoc = zen + "o" ; ---- o/a in Wiki ; o in https://sh.wiktionary.org/wiki/%C5%BEena
	sins = zen + "om" ;
	pnom, pacc, pvoc = zen + "e" ;
	pdat = zen + "ama" ;
	pgen = zen + "a" ;
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
    fsdat : Str ;               -- fsloc = fsdat
    fsacc : Str ;               -- 
    msloc : Str ;               -- nsloc, fsins = msloc
    msins : Str ;        -- nsins, pdat, ploc, pins = msins
    mpnom : Str ;               -- mpvoc = mpnom
    pgen : Str ;                --
    } ;

invarAdjForms : Str -> AdjForms = \s -> {
    msnom, fsnom, nsnom, msgen, fsgen, msdat,
    fsdat, fsacc, msloc, msins, fsins, mpnom, pgen = s ;
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
      | <Sg, Acc, Masc Anim>     => afs.msgen ;
    <Sg, Gen, Fem>
      | <Pl, Nom|Acc|Voc, Fem>
      | <Pl, Acc, Masc _>        => afs.fsgen ;
    <Sg, Dat, Masc _|Neutr>      => afs.msdat ;
    <Sg, Dat|Loc, Fem>           => afs.fsdat ;
    <Sg, Acc, Fem>               => afs.fsacc ;
    <Sg, Loc, Masc _|Neutr>
      | <Sg, Ins, Fem>           => afs.msloc ;
    <Sg, Ins, Masc _|Neutr>
      | <Pl,Dat|Loc|Ins, _>      => afs.msins ;
    <Pl, Nom|Voc, Masc _>        => afs.mpnom ;
    <Pl, Gen,_>                  => afs.pgen
    }
    } ;

{-
  guessAdjForms : Str -> AdjForms
    = \s -> case s of {
        _ + "ý"  => peknyA s ;
        _ + "y"  => krasnyA s ;
        _ + "í"  => cudziA s ;
        _ + "i"  => rydziA s ;
        _ + ("ov"|"in") => otcovA s ;
        _ => otcovA (""+s)  ---- Predef.error ("no mkA for" ++ s)
        } ;
-}

  velikA : Str -> AdjForms = \velik ->
    let
      velk : Str = case velik of {
        vel + "stan" => vel + "sn" ;
        vel + "ao" => vel + "l" ;
        vel + "ak" => voicing vel + "k" ;
        vel + "a" + k@? => vel + k ;
	_ => velik
        }
    in {
      msnom   = velik ;
      fsnom   = velk + "a" ;
      nsnom   = ifSoft velik
                  (velk + "e")
                  (velk + "o") ;
      msgen   = velk + "og" ;
      fsgen   = velk + "e" ;
      msdat   = velk + "omu" ;
      fsdat   = velk + "oj" ;
      fsacc   = velk + "u" ;
      msloc   = velk + "om" ;
      msins   = velk + "im" ;
      mpnom   = velk + "i" ;
      pgen    = velk + "ih" ;
      } ;

---------------------
-- Verbs
-- Wiki

  VerbForms : Type = VForm => Str ;

  ComplementCase : Type = {s : Str ; c : Case ; hasPrep : Bool} ;

  verbAgr : VerbForms -> Agr -> CTense -> Str   ---- TODO tenses
    = \vf,a,b -> case <a,b> of {
      <Ag _ n p, CTPres> => vf ! VPres n p ;
      <Ag g n _, CTPast> => vf ! VPastPart g n
      } ;
{-
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
-}

-- just an example of a traditional paradigm
---- TODO other traditional paradigms

  aeiVerbForms : Str -> Str -> Str -> VerbForms = \citati, citam, citao ->
   let
     cita = init citam ;
     u = case last cita of {
        "a" => "aju" ;
	"e" => "u" ;
	"i" => "e"
        } ;
     cital = init citao + "l" ;
   in table {
      VInf => citati ;
      VPres Sg P1 => cita + "m" ;
      VPres Sg P2 => cita + "š" ;
      VPres Sg P3 => cita ;
      VPres Pl P1 => cita + "mo" ;
      VPres Pl P2 => cita + "te" ;
      VPres pl P3 => init cita + u ;
      VPastPart (Masc _) Sg => citao ;
      VPastPart Fem Sg => cital + "a" ;
      VPastPart Neutr Sg => cital + "o" ;
      VPastPart (Masc _) Pl => cital + "i" ;
      VPastPart Fem Pl => cital + "e" ;
      VPastPart Neutr Pl => cital + "a"
    } ;


-- copula

  jesam_Copula : {short, long, negative : Number => Person => Str} =
    let
      sam : Number => Person => Str = table {
        Sg => table {
	  P1 => "sam" ;
	  P2 => "si" ;
	  P3 => "je"
	  } ;
        Pl => table {
	  P1 => "smo" ;
	  P2 => "ste" ;
	  P3 => "su"
	  }
	}
     in {
       short = sam ;
       long = \\n,p => case <n,p> of {
         <Sg,P3> => "jeste" ; --- wiki: jest(e)
	 _ => sam ! n ! p
	 } ;
       negative = \\n,p => "ni" + sam ! n ! p
       } ;

    biti_VerbForms : VerbForms = aeiVerbForms "biti" "budem" "bio" ;

    imati_VerbForms : VerbForms = aeiVerbForms "imati" "imam" "imao" ;


---------------------------
-- Pronouns

  PronForms : Type = {
    nom, 
    gen, cgen,  -- bare, clitic (also as Acc)
    dat, cdat,  -- also as Loc
    ins        : Str ;
    a : Agr
    } ;

  personalPron : Agr -> PronForms = \a ->
    {a = a ; cnom = []} **
    case a of {
      Ag _ Sg P1 => {
        nom = "ja" ;
        gen = "mene" ;
        cgen = "ma" ;
        dat = "meni" ;
        cdat = "mi" ;
        ins = "mnom"
        } ;
      Ag _ Sg P2 => {
        nom = "ti" ;
        gen = "tebe" ;
        cgen = "te" ;
        dat = "tebi" ;
        cdat = "ti" ;
        ins = "tobom"
        } ;
      Ag Fem Sg P3 => {
        nom = "ona" ;
        gen = "nje" ;
        cgen = "je" ;
        dat = "njoj" ;
        cdat = "joj" ;
        ins = "njom"
        } ;
      Ag g Sg P3 => {
        nom = case g of {
	  Masc _ => "on" ;
	  _ => "ono"
	  } ;
        gen = "njega" ;
        cgen = "ga" ;
        dat = "njemu" ;
        cdat = "mu" ;
        ins = "njim"
        } ;

      Ag _ Pl P1 => {
        nom = "mi" ;
        gen, cgen = "nas" ;
        dat, ins = "nama" ;
        cdat = "nam"
        } ;
      Ag _ Pl P2 => {
        nom = "vi" ;
        gen, cgen = "vas" ;
        dat, ins = "vama" ;
        cdat = "vam"
        } ;
      Ag g Pl P3 => {
        nom = case g of {
	  Masc _ => "oni" ;
	  Fem => "one" ;
	  Neutr => "ona"
	  } ;
        gen = "njih" ;
        cgen = "ih" ;
        dat, ins = "njima" ;
        cdat = "im" 
        }
      } ;

  possessivePron : Agr -> AdjForms = \a -> case a of {
      Ag _ Sg P1 => velikA "moj" ;
      Ag _ Sg P2 => velikA "tvoj" ;
      Ag (Masc _) Sg P3 => velikA "njegov" ;
      Ag (Fem|Neutr) Sg P3 => velikA "njezin" ; -- BCMS: Hrv: otherwise njen
      Ag _ Pl P1 => velikA "naš" ;
      Ag _ Pl P2 => velikA "vaš" ;
      Ag _ Pl P3 => velikA "njihov"
    } ;

{-
  reflPossessivePron : DemPronForms = otcovA "svoj" ** {
        msnom = "svoj" ; msgen = "svojho" ; msdat = "svojmu" ;
	msins = "svojím" ;
	ampnom = "svoji" ;
	nsnom, fpnom = "svoje" ;
	pgen = "svojich" ;
	pdat = "svojim" ;
	pins = "svojimi" ;
	} ;
-}

  mkPron : Agr -> PronForms ** {poss : AdjForms} = \a ->
    personalPron a ** {poss = possessivePron a} ;
    
{-
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
