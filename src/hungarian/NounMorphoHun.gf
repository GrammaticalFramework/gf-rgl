resource NounMorphoHun = ParamHun ** open Prelude, Predef in {

oper
  Noun = {s : NumCaseStem => Str ; h : Harm} ;

  -- Paradigm functions
  -- http://www.cse.chalmers.se/~aarne/articles/smart-preprint.pdf

  -- Words like "alma, kefe, apa, anya, fa":
  dAlma : (nom : Str) -> (acc : Str) -> Noun = \alma,almát ->
    let almá : Str = init almát ;

        -- Apply mkNoun to the lengthened stem "almá" or "kefé"
        nAlmá : Noun = mkNoun almá ;
     in nAlmá ** {
          s = \\nc => case nc of {
                -- Singular nominative uses the given form, e.g. "alma" or "kefe"
                SgNom => alma ;

                PossdSg_PossrP3 => almá + "j" ;

                -- The rest of the forms are formed with the regular constructor,
                -- using "almá" or "kefé" as the stem.
                _ => nAlmá.s ! nc }
          } ;

  --Handles words like "ló, lé, kő" which are "lovak, levek, kövek" in plural.
  --Also handles "tó, hó" which are "tavak, havak" in plural!
  -- <Sg,Sup> "lovon" instead of "lón" fixed
  dLó : (nom : Str) -> (acc : Str) -> Noun = \ló, lovat ->
    let lova = init lovat ;
        lov = init lova ;
        nLov = mkNoun lov ;
        nLova = mkNoun lova ;
        nLó = mkNoun ló ;
    in nLova ** {
         s = \\nc => case nc of {

              -- All plural forms and Sg Acc use the "lova" stem
              PlStem | PlAcc | SgAccStem
               => nLova.s ! nc ;

              SgSup | -- Sg Sup has vowel o/ö, not a/e
              PossdSg_PossrP3 -- Consonant stem before P3 suffixes
               => nLov.s ! nc ;

              PossdSg_PossrPl1 -- Round vowel, part of Pl1 suffix
               => lov + harm "u" "ü" ! nLov.h ;

              PossdPl => lova + "i" ;

              -- The rest of the forms are formed with the regular constructor,
              -- using "ló" as the stem.
              _ => nLó.s ! nc }
         } ;

    -- NB. arguments are Sg Nom, Pl Nom
    -- handles words like: falu, daru, tetű -> falvak, darvak, tetvek
    dFalu : (nomsg : Str) -> (nompl : Str) -> Noun = \falu,falvak ->
      let falva = init falvak ;
          nFalva = mkNoun falva ;
          nFalu = mkNoun falu ;
       in nFalu ** {
            s = \\nc => case nc of {

               -- All plural forms and Sg Acc, Sg Sup use the "falu" stem
               PlStem | PlAcc => nFalva.s ! nc ;

               -- The plural morpheme before possessive suffixes is i
               PossdPl => nFalu.s ! nc + "i" ;

               -- The form before P3 possessive suffixes: faluj|a, faluj|uk
               -- Forms before other possessive suffixes follow SgAccStem.
               PossdSg_PossrP3 => nFalu.s ! nc + "j" ;

               -- The rest of the forms are formed with the regular constructor,
               -- using "falu" as the stem.
               _ => nFalu.s ! nc }
            } ;


  --Handles words like "gyomor, majom, retek" which are "gyomrot, majmot, retket" in accusative (wovel dropping base)
  --More examples: "ajak,  bokor,  cukor,  csokor,  eper,  fészek,  fodor,  gödör,  haszon,  iker,  izom,  kölyök,  köröm,  méreg,  piszok,  sarok,  selyem,  szeder,  szobor,  takony,  terem,  titok,  torok,  torony,  tükör,  vödör" ->
  --               "ajkat, bokrot, cukrot, csokrot, epret, fészket, fodrot, gödröt, hasznot, ikret, izmot, kölyköt, körmet, mérget, piszkot, sarkot, selymet, szedret, szobrot, taknyot, termet, titkot, torkot, tornyot, tükröt, vödröt"
  --ALso handles words like "sátor, álom, alkalom, farok, halom, vászon"
  --                        "sátrat, álmat, alkalmat, farkat, halmat, vásznat"
  --<Sg,Sup> case handled
  dMajom : (nom : Str) -> (acc : Str) -> Noun = \majom, majmot ->
    let majmo = init majmot ;
        majm = init majmo ;
        nMajmo = mkNoun majmo ;
        nMajom = mkNoun majom ;
    in nMajmo ** {
         s = \\nc => case nc of {

            SgSup -- All plural forms and Sg Acc and Sg Sup use the "majmo" stem
          | PlAcc
          | PlStem
          | SgAccStem => nMajmo.s ! nc ;

            -- The plural morpheme before possessive suffixes: majmai
            PossdPl => majm + harm "a" "e" ! nMajmo.h + "i" ;

            -- The form before P3 possessive suffixes: majm|a, majm|uk
            -- Forms before other possessive suffixes follow SgAccStem.
            PossdSg_PossrP3 => majm ;

            -- The rest of the forms are formed with the regular constructor,
            -- using "majom" as the stem.
            _ => nMajom.s ! nc
            }
          } ;


          -- Handles regular wovel ending words, with j added in possesive forms
          -- Examples: "hajó, hajója, zseni, zsenije, kestyű, kestyűje"
  dHajó : (nom : Str) -> (acc : Str) -> Noun = \hajó,hajót ->
    let nHajó = mkNoun hajó ;
        hajój = hajó + "j" ;
     in nHajó ** {
          s = \\nc => case nc of {
             -- All plural forms and Sg Acc use the "tolla" stem
             PlStem | PlAcc | SgAccStem => nHajó.s ! nc ;

             PossdSg_PossrPl1 => hajój + harm "u" "ü" ! nHajó.h ;

             PossdSg_PossrP3 => hajój ;

             -- The plural morpheme before possessive suffixes: madarai
             PossdPl => hajó + harm "a" "e" ! nHajó.h + "i" ;

             -- The rest of the forms are formed with the regular constructor,
             -- using "toll" as the stem.
             _ => nHajó.s ! nc
            }
          } ;

  -- Handles many possesive forms
  dToll : (nom : Str) -> (acc : Str) -> Noun = \toll,tollat ->
    let tolla = init tollat ;
        nTolla = mkNoun tolla ;
        nToll = mkNoun toll ;
        diákj = case ifTok Bool toll tolla True False of {
                     True => toll + "j" ;
                     False => case tolla of {
                                    x + #v => x ;
                                    x + #v + ("sz"|"z"|"s"|"zs"|"j"|"ly"|"l"|"r"|"n"|"ny"|"ssz"
                                             |"zz"|"ss"|"ll"|"rr"|"nn"|"ns"|"nsz"|"nz") => tolla ;
                                    _      => tolla + "j" } 
                                  } ;
     in nTolla ** {
          s = \\nc => case nc of {
             -- All plural forms and Sg Acc use the "tolla" stem
             PlStem | PlAcc | SgAccStem => nTolla.s ! nc ;

             PossdSg_PossrPl1 => diákj + harm "u" "ü" ! nToll.h ;

             PossdSg_PossrP3 => diákj ;

             -- The plural morpheme before possessive suffixes: madarai
             PossdPl => diákj + harm "a" "e" ! nToll.h + "i" ;

             -- The rest of the forms are formed with the regular constructor,
             -- using "toll" as the stem.
             _ => nToll.s ! nc
            }
          } ;

  -- Handles words like "madár, nyár, név, bogár" with shortened stem vowel in plural.
  dMadár : (nom : Str) -> (acc : Str) -> Noun = \madár,madarat ->
    let madara = init madarat ;
        nMadara = mkNoun madara ;
        nMadár = mkNoun madár ;
     in nMadara ** {
          s = \\nc => case nc of {
             -- All plural forms and Sg Acc use the "tolla" stem
             PlStem | PlAcc | SgAccStem => nMadara.s ! nc ;

             PossdSg_PossrPl1 => madara + harm "u" "ü" ! nMadár.h ;

             PossdSg_PossrP3 => madara ;

             -- The plural morpheme before possessive suffixes: madarai
             PossdPl => madara + harm "a" "e" ! nMadár.h + "i" ;

             -- The rest of the forms are formed with the regular constructor,
             -- using "toll" as the stem.
             _ => nMadár.s ! nc
            }
          } ;

  -- More words not covered by current paradigms:
  -- https://cl.lingfil.uu.se/~bea/publ/megyesi-hungarian.pdf
  -- TODO: teher ~ terhet (consonant-crossing)
  -- TODO: do we need possessive forms? e.g. fiú ~ fia{m,d,tok}

  -- All regNoun* are /smart paradigms/: they take one or a couple of forms,
  -- and decides which (non-smart) paradigm is the most likely to match.
regNounNomAccPl : (nomsg, accsg, nompl : Str) -> Noun = \nsg,asg,npl ->
  case <nsg,asg,npl> of {
    <_ + ("u"|"ú"|"ü"|"ű"|"ó"), -- falu, falut, falvak ; szó, szót, szavak
     _ + ("u"|"ú"|"ü"|"ű"|"ó") + "t",
     _ + "v" +          #v + "k"> => dFalu nsg npl ;

    -- Fall back to 2-argument smart paradigm
    _ => regNounNomAcc nsg asg
  } ;

regNounNomAcc : (nom : Str) -> (acc : Str) -> Noun = \n,a ->
  case <n,a> of {

    -- Stem 1: Sg Nom
    -- Stem 2: Everything else
    -- alma, almát
    <_ + "a", _ + "át">
   |<_ + "e" ,_ + "ét"> => dAlma n a ;

   -- Stem 1: Sg Nom
   -- Stem 2: Sg Gen, Sg Sup, Pl *
    <_ + #shortv + #c, -- majom, majmot
     _ + #c + #shortv + "t"> => dMajom n a ;

   -- Stem 1: Sg Nom
   -- Stem 2: Sg Sup
   -- Stem 3: Sg Gen, Pl *
    <_ + "ó", -- ló, lovat
     _ + "o" + #c + #v + "t">

   |<_ + "ó", -- tó, tavat
     _ + "a" + #c + #v + "t">

   |<_ + "ő", -- kő, követ
     _ + "ö" + #c + #v + "t">

   |<_ + "ű", -- fű, füvet
     _ + "ü" + #c + #v + "t">

   |<_ + "é", -- lé, levet
     _ + "e" + #c + #v + "t"> => dLó n a ;

    -- Stem 1: Sg Nom, Sg * - [Gen]
    -- Stem 2: Sg Gen, Pl *
    <_ + "á" + #c, _ + "a" + #c + "at">
   |<_ + "é" + #c,_ + "e" + #c + "et"> => dMadár n a ;

    _ => dToll n a
  } ;

  -- 1-argument smart paradigm
  -- Here we guess the genitive form and give it to appropriate 2-arg paradigm
  regNoun : Str -> Noun = \sgnom -> case sgnom of {
    _  + ("a"|"e")         => dAlma sgnom (lengthen sgnom + "t") ;
    -- Words like nyár, név need to use 2-arg smart paradigm
    (#c|"")+("á"|"é")+ #c  => mkNoun sgnom ;
    _  + ("á"|"é") + #c    => dToll sgnom (név2nevet sgnom) ;
    _  + ("ó"|"é"|"ő"|"ű") => dLó sgnom (ló2lovat sgnom) ;
    _  + #v + #c + #v + #c => dMajom sgnom (majom2majmo sgnom);
    _ => mkNoun sgnom -- Fall back to the regular paradigm
  } where {
    név2nevet : Str -> Str = \név ->
      let né_v : Str*Str = case név of {né + v@#c => <né,v>} ;
          né = né_v.p1 ;
          v  = né_v.p2 ;
          ne = shorten né ;
          e = case last ne of {
              "i" => "e" ;
              _   => last ne } ;
      in ne + v + e + "t" ;
    ló2lovat : Str -> Str = \ló ->
      let lo = shorten ló ;
          lov = lo + "v" ;
          at : Str = case ló of {
            _ + "ó" => "at" ;
            _       => "et" } ;
       in lov + at ;
    majom2majmo : Str -> Str = \majom ->
      let majo_m : Str*Str = case majom of {majo + m@#c => <majo,m>} ;
          majo = majo_m.p1 ;
          m = majo_m.p2 ;
          mo = m + last majo ;
          maj = init majo ;
      in maj + mo
  } ;

--TODO: Special cases (enter these words manually to not complicate the paradigms):
--dLó: special case <Sg,Sup> "lén" not "leven"

--endCaseConsAcc: "falat, fület, várat, könnyet",
--also special in superessive case "falon, fülek, vizen"
--------------------------------------------------------------------------------
-- Following code by EG in 2009 (?), comments and some additions by IL 2020

param
  -- Harmony types
  Harm = H_a | H_e | H_o ;

oper

  -- Vowels as a pattern.
  v : pattern Str = #("a" | "e" | "i" | "o" | "u" | "ö" | "ü" |
                      "á" | "é" | "í" | "ó" | "ú" | "ő" | "ű") ;
  shortv : pattern Str = #("a" | "e" | "i" | "o" | "u" | "ö" | "ü") ;

  back : pattern Str = #("a" | "á" | "o" | "ó" | "u" | "ú") ;

  front_rounded : pattern Str = #("ö" | "ő" | "ü" | "ű") ;

  -- front and back rounded
  -- rounded : pattern Str = #("ö" | "ő" | "ü" | "ű" | "o" | "ó" | "u" | "ú")

  c : pattern Str = #("b"|"c"|"d"|"f"|"g"|"h"|"j"|"k"|"l"|"m"|
                      "n"|"p"|"q"|"r"|"s"|"t"|"v"|"w"|"x"|"z"|
                      "cs"|"dz"|"gy"|"ly"|"ny"|"sz"|"ty"|"zs"|
                      "dzs") ;

  dupl : pattern Str = #("bb"|"cc"|"dd"|"ff"|"gg"|"hh"|"jj"|"kk"|"ll"|"mm"|
                         "nn"|"pp"|"qq"|"rr"|"ss"|"tt"|"vv"|"ww"|"xx"|"zz"|
                         "ddzs"|"ccs"|"ddz"|"ggy"|"lly"|"nny"|"ssz"|"tty"|"zzs") ;

  -- Only single consonants
  unigraph : pattern Str = #("b"|"c"|"d"|"f"|"g"|"h"|"j"|"k"|"l"|"m"|
                             "n"|"p"|"q"|"r"|"s"|"t"|"v"|"w"|"x"|"z") ;

  -- Digraphs
  digraph : pattern Str = #("cs"|"dz"|"gy"|"ly"|"ny"|"sz"|"ty"|"zs") ;

  -- Trigraphs
  trigraph : pattern Str = #("dzs") ;

  duplicateLast : Str -> Str = \str -> case str of {
    x + "dzs" => x + "ddzs" ;
    x + "cs" => x + "ccs" ;
    x + "dz" => x + "ddz" ;
    x + "gy" => x + "ggy" ;
    x + "ly" => x + "lly" ;
    x + "ny" => x + "nny" ;
    x + "sz" => x + "ssz" ;
    x + "ty" => x + "tty" ;
    x + "zs" => x + "zzs" ;
    x + #dupl => str ; -- Don't duplicate already long consonant

    -- Base case: just duplicate the single letter
    x + s@?  => x + s + s } ;

  -- Function to test if a string ends in a vowel
  vowFinal : Str -> Bool = \str ->
    case str of {
      _ + #v => True ; -- Matching a string against a pattern.
      _ => False
    } ;

  lengthen : Str -> Str = \str -> case str of {
    x + "a" => x + "á" ;
    x + "e" => x + "é" ;
    x + "i" => x + "í" ;
    x + "o" => x + "ó" ;
    x + "u" => x + "ú" ;
    x + "ö" => x + "ő" ;
    x + "ü" => x + "ű" ;
    _   => Predef.error "Lengthening not applicable to" ++ str
  } ;

  shorten : Str -> Str = \str -> case str of {
    x + "á" => x + "a" ;
    x + "é" => x + "e" ;
    x + "í" => x + "i" ;
    x + "ó" => x + "o" ;
    x + "ú" => x + "u" ;
    x + "ő" => x + "ö" ;
    x + "ű" => x + "ü" ;
    _   => Predef.error "Shortening not applicable to" ++ str
  } ;

  -- Function to get a harmony from a string
  getHarm : Str -> Harm = \s -> case s of {
    _ + #back + _          => H_a ;
    _ + #front_rounded + (#c|"") + (#c|"") => H_o ;
    _ => H_e
    } ;

  -- Used as a table of allomorphs for a give case.
  HarmForms : Type = Harm => Str ;

  -- Functions for constructing a HarmForms table.
  harm3 : Str -> Str -> Str -> HarmForms = \a,e,o -> table {
    H_a => a ;
    H_e => e ;
    H_o => o
    } ;
  harm  : Str -> Str -> HarmForms = \a,e -> harm3 a e e ;
  harm1 : Str -> HarmForms = \i -> harm i i ;

  -- Variant of case forms when the noun stem ends in consonant.
  endCaseCons : NumCaseStem -> HarmForms = \c -> case c of {
    SgSup => harm3 "on" "en" "ön" ;
    PlAcc => harm3 "ot" "et" "öt" ;
    SgAccStem => harm3 "o" "e" "ö" ;
    PossdPl => harm1 "i" ; -- TODO figure out allomorphs
    _ => harm1 []
    } ;


  -- Variant where accusative has the allomorph -at
  endCaseConsAccAt : NumCaseStem -> HarmForms = \c -> case c of {
    SgAccStem => harm3 "a" "e" "ö" ;
    PlAcc => harm3 "at" "et" "öt" ;
    _   => endCaseCons c
    } ;

  -- Variant where accusative has the allomorph -t for consonants
  -- Examples: "pénz, bor, orr, szín, lány, kés, dal"
  endCaseConsAcc : NumCaseStem -> HarmForms = \c -> case c of {
    SgAccStem => harm1 "" ;
    _   => endCaseCons c
  } ;

  -- Variant of case forms when the noun stem ends in vowel.
  endCaseVow : NumCaseStem -> HarmForms = \c -> case c of {
    SgAccStem => harm1 "" ;
    SgSup => harm1 "n" ;
    SgInsStem => harm1 "v" ;
    -- Other forms are shared with endCaseCons.
    _   => endCaseCons c
  } ;

  -- Function to return a plural allomorph given the stem (e.g. név, almá).
  pluralAllomorph : (stem : Str) -> Str = pluralAllomorphLowStem False ;

  -- Function to return a plural allomorph given lowering stem or not
  -- Examples of lowering stems: ág, ágy, ár, fal, fog, gyár, hal, has, hát, ház, hold, láz, lyuk, nyak, olaj, oldal, toll, ujj, vonal
  -- Examples of lowering stems: férj, fej, hely, fül, könny, könyv, mell, szög
  pluralAllomorphLowStem : (low : Bool) -> (stem : Str) -> Str = \low,stem ->
    case <low,vowFinal stem> of {
      <_,True>  => "k" ;
      <True, _> => harm "ak" "ek" ! getHarm stem ;
      _ => harm3 "ok" "ek" "ök" ! getHarm stem
    } ;


  -- Harmony and plural allomorph read from the singular nominative
  mkNoun : Str -> Noun = \w ->
    mkNounHarm (getHarm w) (pluralAllomorph w) w ;

  -- Harmony and plural allomorph given explicitly (check if the True makes it bad)
  mkNounHarm : Harm -> (plural : Str) -> Str -> Noun = mkNounHarmAcc True ;

  mkNounHarmAcc : (useAt : Bool) -> Harm -> (plural : Str) -> Str -> Noun = \useAt,h,plural,w ->
    let endCaseSg : NumCaseStem -> HarmForms =
          case <useAt, w> of {
            <_,_ + #v> => endCaseVow ;
            <_,_ + #v + ("sz"|"z"|"s"|"zs"|"j"|"ly"|"l"|"r"|"n"|"ny"|"ssz"
            |"zz"|"ss"|"ll"|"rr"|"nn"|"ns"|"nsz"|"nz")> => endCaseConsAcc ;
            <True,_>  => endCaseConsAccAt ;
            _ => endCaseCons } ;
        endCasePl : NumCaseStem -> HarmForms =
          case <plural, useAt> of {
            <"ak",_> => endCaseConsAccAt ;
            <_,True> => endCaseConsAccAt ;
            _    => endCaseCons } ;
        -- Last consonant doubles before instrumental and translative
        duplConsStem : Str = case vowFinal w of {
                           True  => w ;
                           False => duplicateLast w } ;

        -- Noun is {s : NumCaseStem => Str}
     in {h = h ;
         s = table {
               -- Before Sg Ins, Tra:
               --  * Double the last letter if consonant
               --  * Add v if vowel (comes from endCaseSg)
               SgInsStem        => duplConsStem + endCaseSg SgInsStem ! h ;

               -- endCaseCons, because we only use -k as plural morpheme.
               -- If we add possessive forms with allomorph -i, then revise.
               c@(PlStem|PlAcc) => w + plural + endCasePl c ! h ;

               -- All other singular forms and stems
               c                => w +          endCaseSg c ! h } ;

        } ;


-- This is used in ResHun.caseFromStem, which makes NP out of CN.
-- Ns only have stems, and these forms are attached to the stems.

    endCase : Case -> HarmForms = \c -> case c of {
      Nom => harm1 [] ;
      Acc => harm3 "ot" "et" "öt" ;
      Dat => harm "nak" "nek" ;
      Ins => harm "al" "el" ;
      Tra => harm "á" "é" ;
      Ill => harm "ba" "be" ;
      Ine => harm "ban" "ben" ;
      Ela => harm "ból" "ből" ;
      All => harm3 "hoz" "hez" "höz" ;
      Ade => harm "nál" "nél" ;
      Abl => harm "tól" "től" ;
      Sub => harm "ra" "re" ;
      Sup => harm3 "on" "en" "ön" ;
      Del => harm "ról" "ről" ;
      Cau => harm1 "ért"
      -- Ess => harm "stul" "stül" ;  -- Essive-modal 'with <the noun> and its parts'
      -- Ter => harm1 "ig" ; -- Terminative 'as far as <the noun>'
      -- For => harm1 "ként" ; -- Formal 'as <the noun>'
      -- Tem => harm1 "kor" -- Temporal 'at <numeral>'. Only used with numerals.
      } ;

    endCasePossVow : Case -> HarmForms = \c -> case c of {
      Acc => harm1 "t" ;
      Sup => harm1 "n" ;
      Tra => harm "vá" "vé" ;
      Ins => harm "val" "vel" ;
      _ => endCase c
    } ;

}
