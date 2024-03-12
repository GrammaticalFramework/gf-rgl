resource InflectionRus = open Prelude, ParamRus in {
flags coding=utf8 ; optimize=all ;

param
  AlterType    = No | Ast | Deg  ;    -- Alternation: eg 1a, 1*a, 1°a
  StressSchema = A | A' | B | B' | C | C' | C'' | D | D' | E | F | F' | F'' ;
  ZCirc        = NoC | ZC1 | ZC2 | ZC12 ;
  ZNIndex       = ZN0 | ZN DeclType AlterType StressSchema ZCirc ;
  Stressedness = Stressed | Unstressed ;

  AdjStressSchema = A_ | A'_ | B_ | B'_ | C_ | A_A | A_A' | A_B | A_C | A_A' | A_B' | A_C' | A_C'' | B_A | B_B | B_C | B_A' | B_B' | B_C' | B_C'' ;
  ZAIndex      = ZA0 | ZA DeclType AlterType AdjStressSchema ZCirc ;

  VerbSS         = _A | _B | _C | _C' | _C'' ;
  Conjug         = I | I' | II ;   -- first, first with stressed ending, second conjugation
  VerbStressSchema = VSS VerbSS VerbSS ;   -- Pres / Imp and Past forms respectively. By default, _A as second
  ZVIndex      = ZV ConjType AlterType VerbStressSchema ;

oper

--------
-- Nouns

  -- This correspond to the abbreviated Zaliznyak index for nouns.
  -- Complete index contains a lot of additions.

  EndingSpec : Type = {p1, p2: Str} ;

  NounEndForms, StemForms : Type = {
    snom, sgen, sdat, sacc, sins, sprep,
    pnom, pgen, pdat, pacc, pins, pprep : Str ;
  } ;

  NounEndFormsS1 : Type = {
    snom, sgen, sdat, sacc, sins, sprep,
    pnom, pgen, pdat, pacc, pins, pprep : EndingSpec ;
  } ;

  immutableCases : NounEndForms = {
    snom="";pnom="";sgen="";pgen="";sdat="";pdat="";sacc="";pacc="";sins="";pins="";sprep="";pprep=""
  } ;

  immutableNounCases : Str -> Gender -> Animacy -> NounFormsBase
    = \s, g, anim -> {
      snom=s;pnom=s;sgen=s;pgen=s;sdat=s;pdat=s;sacc=s;pacc=s;sins=s;pins=s;sprep=s;pprep=s;
      anim=anim;
      g=g;
      rel=immutableAdjectiveCases s;
      rt=GenType;
    } ;

  immutableCasesS1 : NounEndFormsS1 = {
    snom=<"","">;pnom=<"","">;sgen=<"","">;pgen=<"","">;sdat=<"","">;pdat=<"","">;sacc=<"","">;pacc=<"","">;sins=<"","">;pins=<"","">;sprep=<"","">;pprep=<"","">
  } ;

  digitToDeclType : Str -> DeclType
    = \s ->
      case s of {
        "0" => 0 ;
        "1" => 1 ;
        "2" => 2 ;
        "3" => 3 ;
        "4" => 4 ;
        "5" => 5 ;
        "6" => 6 ;
        "7" => 7 ;
        "8" => 8
      } ;

  numToConjType : Str -> ConjType
    = \s ->
      case s of {
        "1" => 1 ;
        "2" => 2 ;
        "3" => 3 ;
        "4" => 4 ;
        "5" => 5 ;
        "6" => 6 ;
        "7" => 7 ;
        "8" => 8 ;
        "9" => 9 ;
        "10" => 10 ;
        "11" => 11 ;
        "12" => 12 ;
        "13" => 13 ;
        "14" => 14 ;
        "15" => 15 ;
        "16" => 16
      } ;

  toStressSchema : Str -> StressSchema
    = \s ->
      case s of {
        "a" => A ;
        "a'" => A' ;
        "b" => B ;
        "b'" => B' ;
        "c" => C ;
        "c'" => C' ;
        "c''" => C'' ;
        "d" => D ;
        "d'" => D' ;
        "e" => E ;
        "f" => F ;
        "f'" => F' ;
        "f''" => F'' ;
        _ => A
      } ;

  toAlterType : Str -> AlterType
    = \s ->
      case s of {
        "*" => Ast ;
        "°" => Deg ;
        _ => No
      } ;

  toZCirc: Str -> ZCirc
    = \s ->
      case s of {
        "①" | "(1)" => ZC1 ;
        "①②" | "②①" | "(1)(2)" | "(2)(1)" => ZC12 ;
        "②" | "(2)" => ZC2 ;
        _ => NoC
      } ;

  parseIndex : Str -> ZNIndex
    = \s ->
      case s of {
        "0" => ZN0 ;
        dt@(#digit) + at@("*"|"°"|"") + ss@(#stress_schema) + zc@("①"|"①②"|"②①"|"②"|"(1)"|"(1)(2)"|"(2)(1)"|"(2)")
          => ZN (digitToDeclType dt) (toAlterType at) (toStressSchema ss) (toZCirc zc) ;
        dt@(#digit) + at@("*"|"°"|"") + ss@(#stress_schema)
          => ZN (digitToDeclType dt) (toAlterType at) (toStressSchema ss) NoC;
        _ => Predef.error "Error: incorrect ZNIndex"
      } ;

  mobileOne : Str -> NounEndForms -> DeclType -> StressSchema -> StemForms
   = \s, nef, dt, ss ->
     let snom = s + nef.snom in
     let cmp_base : Str = case snom of {
       _ + "ь" => s ;
       _ => snom
     } in
     let last = Predef.dp 1 cmp_base in
     let butLast = Predef.tk 1 cmp_base in
     let secondLast = Predef.dp 1 butLast in
     let butTwolast = Predef.tk 2 cmp_base in
     let thirdLast = Predef.dp 1 butTwolast in
     let s1 : Str = case <dt, cmp_base, thirdLast, secondLast> of {   -- what if more than one consonant or sign? eg день
       <6, _ + #vowel + #consonant, _, "е"|"ё"> => butTwolast + "ь" ;
       <_, _ + #vowel + #consonant, #vowel, "е"|"ё"> => butTwolast + "й" + last ;  --?
       <3, _ + #vowel + #consonant, #consonant_minus, "е"|"ё"> => butTwolast + "ь" + last ;  -- королёк, but овражек
       <1|2|4|5|7|8, _ + #vowel + #consonant, "л", "е"|"ё"> => butTwolast + "ь" + last ;
       <6, _ + #vowel + #consonant, _, "и"> => butTwolast + "ь" + last  ;  --?
       <_, _ + #vowel + #consonant, _, "е"|"ё"> => butTwolast + last ;
       <_, _ + #vowel + #consonant, _, "о"> => butTwolast + last  ;  -- клочок
       _ => s
     } in
     {
      snom = s + nef.snom ;
      pnom = s1 + nef.pnom ;
      sgen = s1 + nef.sgen ;
      pgen = s1 + nef.pgen ;
      sdat = s1 + nef.sdat ;
      pdat = s1 + nef.pdat ;
      sacc = s1 + nef.sacc ;
      pacc = s1 + nef.pacc ;
      sins = s1 + nef.sins ;
      pins = s1 + nef.pins ;
      sprep= s1 + nef.sprep ;
      pprep= s1 + nef.pprep ;
    } ;

  PlGenAlter : Str -> Str -> DeclType -> StressSchema -> Str
   = \s, end, dt, ss ->
      let stem1 = Predef.tk 1 s in
      let stem2 = Predef.tk 2 s in
      let stemEnd1 = Predef.dp 1 s in
      let pgenStressed = stressTable ss "pgen" in
      case <dt, pgenStressed, s> of {
        <6, Stressed, _>  => stem1 + "е" + end;
        <6, _, _>         => stem1 + "и" + end;
        <5, _, _ + ("ь"|"й") + #consonant> => stem2 + "е" + end;
        <3, _, _ + "й" + #consonant> => stem2 + "е" + stemEnd1 + end;  -- стройка
        <3, _, _ + ("ж"|"ц"|"ч"|"ш"|"щ") + #consonant> => stem1 + "е" + stemEnd1 + end;  -- бабушка
        <3, _, _ + #consonant> => stem1 + "о" + stemEnd1 + end ;  -- ^жшчщц - голубка
        <1, Stressed, _ + ("ь"|"й") + #consonant> => stem2 + "ё" + stemEnd1 + end ;
        <1, _, _ + ("ь"|"й") + #consonant> => stem2 + "е" + stemEnd1 + end ;
        <_, Unstressed, _ + ("ь"|"й") + #consonant> => stem2 + "е" + end ;
        <_, _, _ + ("ь"|"й") + #consonant> => stem2 + "ё" + end ;
        <_, _, _ + ("г"|"к"|"х") + #consonant> => stem1 + "о" + stemEnd1 + end ;
        <2, Unstressed, _ + "н"> => stem1 + "е" + stemEnd1 + (case end of {"ь"=>"";_=>end}) ; -- ????? песня - песен//ь
        <2, Stressed, _ + "н"> => s + "ё" + (Predef.dp 1 end) ;
        <1|2, Unstressed, _> => stem1 + "е" + stemEnd1 + end;
        <5, _, _>            => stem1 + "е" + stemEnd1 + end ;
        <_, Stressed, _ + ("ж"|"ч"|"ш"|"щ") + #consonant> => stem1 + "о" + stemEnd1 + end ; -- shorted stem?
        <_, Stressed, _> => stem1 + "ё" + stemEnd1 + end ; -- shorted stem?
        _ => s + end
   } ;

  mobileTwo : Str -> NounEndForms -> DeclType -> StressSchema -> StemForms
   = \s, nef, dt, ss -> {
      snom = s + nef.snom;
      pnom = s + nef.pnom;
      sgen = s + nef.sgen;
      pgen = PlGenAlter s nef.pgen dt ss ;
      sdat = s + nef.sdat ;
      pdat = s + nef.pdat ;
      sacc = s + nef.sacc ;
      pacc = s + nef.pacc ;
      sins = s + nef.sins ;
      pins = s + nef.pins ;
      sprep= s + nef.sprep;
      pprep= s + nef.pprep;
    } ;

  mobileThree : Str -> NounEndForms -> DeclType -> StressSchema -> StemForms
   = \s, nef, dt, ss ->
     let snom = s + nef.snom in
     let last = Predef.dp 1 s in
     let butLast = Predef.tk 1 s in
     let secondLast = Predef.dp 1 butLast in
     let butTwolast = Predef.tk 2 s in
     let thirdLast = Predef.dp 1 butTwolast in
     let s1 : Str = case <dt, s, thirdLast, secondLast> of {   -- what if more than one consonant or sign?
       <6, _ + #vowel + #consonant, _, "е"|"ё"> => butTwolast + "ь" ;
       <_, _ + #vowel + #consonant, #vowel, "е"|"ё"> => butTwolast + "й" + last ;  --?
       <3, _ + #vowel + #consonant, #consonant_minus, "е"|"ё"> => butTwolast + "ь" + last ;  -- королёк, but овражек
       <1|2|4|5|7|8, _ + #vowel + #consonant, "л", "е"|"ё"> => butTwolast + "ь" + last ;
       <6, _ + #vowel + #consonant, _, "и"> => butTwolast + "ь" + last  ;  --?
       -- <_, _ + #vowel + #consonant, _, "е"|"ё"> => butTwolast + last ;
       -- <_, _ + #vowel + #consonant, _, "о"> => butTwolast + last  ;  -- клочок
       _ => butTwolast + last
     } in
     {  -- TODO
      snom = s + nef.snom ;
      pnom = s1 + nef.pnom ;
      sgen = s1 + nef.sgen ;
      pgen = s1 + nef.pgen ;
      sdat = s1 + nef.sdat ;
      pdat = s1 + nef.pdat ;
      sacc = s + nef.sacc ;
      pacc = s1 + nef.pacc ;
      sins = s + nef.sins ;
      pins = s1 + nef.pins ;
      sprep= s1 + nef.sprep ;
      pprep= s1 + nef.pprep ;
    } ;


  alterStems : Str -> NounEndForms -> Gender -> DeclType -> StressSchema -> StemForms
    = \s, nef, g, dt, ss ->
      case <g, dt> of {
        <Masc, _> => mobileOne s nef dt ss ;
        <Neut, _> => mobileTwo s nef dt ss ;
        <Fem, 8> => mobileThree s nef dt ss ;
        <Fem, _> => mobileTwo s nef dt ss
      } ;

  doAlternations : Str -> NounEndForms -> Gender -> Animacy -> DeclType -> StressSchema -> AdjForms -> NRelType -> NounFormsBase
    = \s, nef, g, anim, dt, ss, rel, rt ->
      (alterStems s nef g dt ss) ** {g=g; anim=anim; rel=rel; rt=rt} ;

  alterForms : Str -> NounEndForms -> Gender -> Animacy -> DeclType -> AlterType -> StressSchema -> AdjForms -> NRelType -> NounFormsBase
    = \s, nef, g, anim, dt, at, ss, rel, rt ->
      case at of {
        Ast => doAlternations s nef g anim dt ss rel rt;
        _ => {
          snom = s + nef.snom ;
          pnom = s + nef.pnom ;
          sgen = s + nef.sgen ;
          pgen = s + nef.pgen ;
          sdat = s + nef.sdat ;
          pdat = s + nef.pdat ;
          sacc = s + nef.sacc ;
          pacc = s + nef.pacc ;
          sins = s + nef.sins ;
          pins = s + nef.pins ;
          sprep= s + nef.sprep ;
          pprep= s + nef.pprep ;
          g=g ;
          anim=anim ;
          rel=rel;
          rt=rt ;
        }
    } ;


  makeNoun : Str -> Gender -> Animacy -> AdjForms -> NRelType -> ZNIndex -> NounFormsBase
    = \word, g, anim, rel, rt, z ->
    case z of {
      ZN0 => immutableNounCases word g anim ;
      ZN 3 Deg ss NoC => formsSelectionOnok word g anim 3 Deg ss rel rt NoC ;
      ZN 1 Deg ss ci => formsSelectionAnin word g anim 3 Deg ss rel rt ci ;
      ZN 8 Deg ss NoC => formsSelectionMya word g anim 8 Deg ss rel rt NoC ;
      ZN dt at ss ci => formsSelectionNoun word g anim dt at ss rel rt ci
    } ;

  myaCases : Str -> NounEndForms
    = \stem ->
    let suffix="ен" in
    let pGenSuf=case stem of {("сем"|"стрем") => "ян"; _  => "ён"} in
    {
      snom=stem + "я";
      pnom=stem + suffix + "а";
      sgen=stem + suffix + "и";
      pgen=stem + pGenSuf ;
      sdat=stem + suffix + "и";
      pdat=stem + suffix + "ам";
      sacc=stem + "я";
      pacc=stem + suffix + "а";
      sins=stem + suffix + "ем";
      pins=stem + suffix + "ами";
      sprep=stem + suffix + "и";
      pprep=stem + suffix + "ах"
  } ;

  formsSelectionMya : Str -> Gender -> Animacy -> DeclType -> AlterType -> StressSchema -> AdjForms -> NRelType -> ZCirc -> NounFormsBase
    = \word, g, anim, dt, at, ss, rel, rt, ci ->
      let butLast = Predef.tk 1 word in
      (myaCases butLast) ** {anim=anim; g=g; rel=rel; rt=rt} ;

  formsSelectionOnok : Str -> Gender -> Animacy -> DeclType -> AlterType -> StressSchema -> AdjForms -> NRelType -> ZCirc -> NounFormsBase
    = \word, g, anim, dt, at, ss, rel, rt, ci ->
      let sgForms = formsSelectionNoun word g anim dt Ast ss rel rt ci in
      case word of {
          _ + "ёнок" => combineDiffSgPlStems sgForms (formsSelectionNoun (Predef.tk 4 word + "ята") Neut anim 8 Ast ss rel rt NoC) ;
          _ + "онок" => combineDiffSgPlStems sgForms(formsSelectionNoun (Predef.tk 4 word + "ата") Neut anim 8 Ast ss rel rt NoC) ;
          _ + "ёночек" => combineDiffSgPlStems sgForms (formsSelectionNoun (Predef.tk 6 word + "ятка") Fem anim 3 Ast ss rel rt NoC) ;
          _ + "оночек" => combineDiffSgPlStems sgForms (formsSelectionNoun (Predef.tk 6 word + "атка") Fem anim 3 Ast ss rel rt NoC) ;
          _ => sgForms
      } ;

  formsSelectionAnin : Str -> Gender -> Animacy -> DeclType -> AlterType -> StressSchema -> AdjForms -> NRelType -> ZCirc -> NounFormsBase
    = \word, g, anim, dt, at, ss, rel, rt, ci ->
      let butTwolast = Predef.tk 2 word in
      let sgForms = formsSelectionNoun word g anim dt Ast ss rel rt ci in
      case word of {
          _ + ("анин"|"янин") => combineDiffSgPlStems sgForms (formsSelectionNoun (butTwolast + "н") Neut anim 8 Ast ss rel rt NoC)
            ** {pnom=butTwolast + "е"};
          _ => sgForms
      } ;

  combineDiffSgPlStems : NounFormsBase -> NounFormsBase -> NounFormsBase
    = \sgn, pln -> sgn ** {
      pnom =  pln.pnom ;
      pgen =  pln.pgen ;
      pdat =  pln.pdat ;
      pacc =  pln.pacc ;
      pins =  pln.pins ;
      pprep=  pln.pprep
    } ;

  formsSelectionNoun : Str -> Gender -> Animacy -> DeclType -> AlterType -> StressSchema -> AdjForms -> NRelType -> ZCirc -> NounFormsBase
    = \word, g, anim, dt, at, ss, rel, rt, ci ->
      let stem = stemFromNoun word g dt in
      let nef = endingsSelectionNoun word g anim dt at ss ci in
      let nef' = specialEndingsNoun word stem nef g dt in
      let alternated = alterForms stem nef' g anim dt at ss rel rt in
      animacySelectionNoun dt alternated nef' g anim
    ;

  stemFromNoun : Str -> Gender -> DeclType -> Str
    = \word, g, dt ->
      let end1 = (gDtBasedSelectionNoun word g dt).snom.p1 in
      case end1 of {
        "" => word ;
        _ => Predef.tk 1 word
      }
    ;

  SgAcc : Gender -> Animacy -> DeclType -> NounFormsBase -> Str -> Str
    = \g, anim, dt, frm, sacc -> case <g, dt, anim, sacc> of {
      <Neut, (3 | 4 | 5 | 6 | 7 | 8), Animate, "?"> => frm.snom ;
      <_, _, Animate, "?"> => frm.sgen ;
      <_, _, Inanimate, "?"> => frm.snom ;
      _ => frm.sacc
    } ;

  PlAcc : Gender -> Animacy -> DeclType -> NounFormsBase -> Str
    = \g, anim, dt, frm -> case <g, dt, anim> of {
      <Neut, (5 | 7), Animate> => frm.pnom ;
      <Neut, 6, Animate> => frm.pnom ;  -- does not exist
      <_, _, Animate> => frm.pgen ;
      <_, _, Inanimate> => frm.pnom ;
      _ => frm.pacc
    } ;

  animacySelectionNoun : DeclType -> NounFormsBase -> NounEndForms -> Gender -> Animacy -> NounFormsBase
    = \dt, frm, nef, g, anim -> frm ** {
        sacc=SgAcc g anim dt frm nef.sacc;
        pacc=PlAcc g anim dt frm ;
        sins=frm.sins  -- TODO: there can be variants {}  ю in addition to й
    } ;

  endingsSelectionNoun : Str -> Gender -> Animacy -> DeclType -> AlterType -> StressSchema -> ZCirc -> NounEndForms
    = \word, g, anim, dt, at, ss, ci ->
    let gDtBased = gDtBasedSelectionNoun word g dt in
    let gDtBasedCirc = circCorrectionNoun word gDtBased g dt ci in
    gDtSsBasedSelectionNoun gDtBasedCirc ss
  ;

  specialEndingsNoun : Str -> Str -> NounEndForms -> Gender -> DeclType -> NounEndForms
    = \word, stem, nef1, g, dt ->
    let stemEnds = Predef.dp 1 stem in
    let wordEnds = Predef.dp 1 word in
    case <g, dt, stemEnds, wordEnds> of {
      <_, 8, ("ж"|"ч"|"ш"|"щ"), _> => nef1 ** {pdat="ам";pins="ами";pprep="ах"} ;
      <Neut, 2|6|7, _, "ё"> => nef1 ** {snom="ё"} ;
      _ => nef1
    } ;

  circCorrectionNoun : Str -> NounEndFormsS1 -> Gender -> DeclType -> ZCirc -> NounEndFormsS1
    = \word, nef1, g, dt, ci ->
      let trans1 : NounEndFormsS1 = case <g, ci> of {
        <Masc, ZC1|ZC12> => nef1 ** {pnom=(gDtBasedSelectionNoun word Neut dt).pnom} ;
        <Neut, ZC1|ZC12> => nef1 ** {pnom=(gDtBasedSelectionNoun word Masc dt).pnom} ;
        _ => nef1
      } in
      case <g, ci> of {
        <Masc, ZC2|ZC12> => trans1 ** {pgen=(gDtBasedSelectionNoun word Neut dt).pgen} ;
        <Neut, ZC2|ZC12> => trans1 ** {pgen=(gDtBasedSelectionNoun word Masc dt).pgen} ;
        <Fem,  ZC2|ZC12> => trans1 ** {pgen=(gDtBasedSelectionNoun word Masc dt).pgen} ;
        _ => trans1
      } ;

  selStress : EndingSpec -> Stressedness -> Str
    = \es, sness ->
    case <es, sness> of {
      <<v, v'>, Unstressed> => v ;
      <<v, v'>, Stressed> => v' ;
      <<s>, _> => s
    } ;

  gDtSsBasedSelectionNoun : NounEndFormsS1 -> StressSchema -> NounEndForms
    = \nef1, ss ->
      {
        snom=stressSelection nef1.snom ss "snom";
        pnom=stressSelection nef1.pnom ss "pnom";
        sgen=stressSelection nef1.sgen ss "sgen";
        pgen=stressSelection nef1.pgen ss "pgen";
        sdat=stressSelection nef1.sdat ss "sdat";
        pdat=stressSelection nef1.pdat ss "pdat";
        sacc=stressSelection nef1.sacc ss "sacc";
        pacc=stressSelection nef1.pacc ss "pacc";
        sins=stressSelection nef1.sins ss "sins";
        pins=stressSelection nef1.pins ss "pins";
        sprep=stressSelection nef1.sprep ss "sprep";
        pprep=stressSelection nef1.pprep ss "pprep"
    } ;

  stressSelection : EndingSpec -> StressSchema -> Str -> Str
    = \es, ss, c ->
    selStress es (stressTable ss c) ;

  stressTable : StressSchema -> Str -> Stressedness
    = \ss, c ->
    case <ss, c> of {
      <B, "snom"|"sgen"|"sdat"|"sacc"|"sins"|"sprep"|"pnom"|"pgen"|"pdat"|"pins"|"pprep"> => Stressed ;
      <C, "pnom"|"pgen"|"pdat"|"pins"|"pprep"> => Stressed ;
      <D, "snom"|"sgen"|"sdat"|"sacc"|"sins"|"sprep"> => Stressed ;
      <E, "pgen"|"pdat"|"pins"|"pprep"> => Stressed ;
      <F, "snom"|"sgen"|"sdat"|"sacc"|"sins"|"sprep"|"pgen"|"pdat"|"pins"|"pprep"> => Stressed ;
      <B', "snom"|"sgen"|"sdat"|"sacc"|"sprep"|"pnom"|"pgen"|"pdat"|"pins"|"pprep"> => Stressed ;
      <D', "snom"|"sgen"|"sdat"|"sins"|"sprep"> => Stressed ;
      <F', "snom"|"sgen"|"sdat"|"sins"|"sprep"|"pgen"|"pdat"|"pins"|"pprep"> => Stressed ;
      <F'', "snom"|"sgen"|"sdat"|"sacc"|"sprep"|"pgen"|"pdat"|"pins"|"pprep"> => Stressed ;
      <_, _> => Unstressed
    } ;

  gDtBasedSelectionNoun : Str -> Gender -> DeclType -> NounEndFormsS1
    = \word, g, dt -> case <word, g, dt> of {
      <_, _, 0> => immutableCasesS1 ;
      <_ + "а", Masc, 1> => {snom=<"а","а">;pnom=<"ы","ы">;sgen=<"ы","ы">;pgen=<"","">;sdat=<"е","е">;pdat=<"ам","ам">;sacc=<"у","у">;pacc=<"?","?">;sins=<"ой","ой">;pins=<"ами","ами">;sprep=<"е","е">;pprep=<"ах","ах">} ;
      <_ + "я", Masc, 1> =>  {snom=<"я","я">;pnom=<"и","и">;sgen=<"и","и">;pgen=<"ь","ей">;sdat=<"е","е">;pdat=<"ям","ям">;sacc=<"ю","ю">;pacc=<"?","?">;sins=<"ей","ёй">;pins=<"ями","ями">;sprep=<"е","е">;pprep=<"ях","ях">} ;
      <_, Masc, 1> => {snom=<"","">;pnom=<"ы","ы">;sgen=<"а","а">;pgen=<"ов","ов">;sdat=<"у","у">;pdat=<"ам","ам">;sacc=<"?","?">;pacc=<"?","?">;sins=<"ом","ом">;pins=<"ами","ами">;sprep=<"е","е">;pprep=<"ах","ах">} ;
      <_, Masc, 2> => {snom=<"ь","ь">;pnom=<"и","и">;sgen=<"я","я">;pgen=<"ей","ей">;sdat=<"ю","ю">;pdat=<"ям","ям">;sacc=<"?","?">;pacc=<"?","?">;sins=<"ем","ём">;pins=<"ями","ями">;sprep=<"е","е">;pprep=<"ях","ях">} ;
      <_, Masc, 3> => {snom=<"","">;pnom=<"и","и">;sgen=<"а","а">;pgen=<"ов","ов">;sdat=<"у","у">;pdat=<"ам","ам">;sacc=<"?","?">;pacc=<"?","?">;sins=<"ом","ом">;pins=<"ами","ами">;sprep=<"е","е">;pprep=<"ах","ах">} ;
      <_, Masc, 4> => {snom=<"","">;pnom=<"и","и">;sgen=<"а","а">;pgen=<"ей","ей">;sdat=<"у","у">;pdat=<"ам","ам">;sacc=<"?","?">;pacc=<"?","?">;sins=<"ем","ом">;pins=<"ами","ами">;sprep=<"е","е">;pprep=<"ах","ах">} ;
      <_, Masc, 5> => {snom=<"","">;pnom=<"ы","ы">;sgen=<"а","а">;pgen=<"ев","ов">;sdat=<"у","у">;pdat=<"ам","ам">;sacc=<"?","?">;pacc=<"?","?">;sins=<"ем","ом">;pins=<"ами","ами">;sprep=<"е","е">;pprep=<"ах","ах">} ;
      <_, Masc, 6> => {snom=<"й","й">;pnom=<"и","и">;sgen=<"я","я">;pgen=<"ев","ёв">;sdat=<"ю","ю">;pdat=<"ям","ям">;sacc=<"?","?">;pacc=<"?","?">;sins=<"ем","ём">;pins=<"ями","ями">;sprep=<"е","е">;pprep=<"ях","ях">} ;
      <_, Masc, 7> => {snom=<"й","й">;pnom=<"и","и">;sgen=<"я","я">;pgen=<"ев","ёв">;sdat=<"ю","ю">;pdat=<"ям","ям">;sacc=<"?","?">;pacc=<"?","?">;sins=<"ем","ём">;pins=<"ями","ями">;sprep=<"и","е">;pprep=<"ях","ях">} ;
      <_, Masc, 8> => {snom=<"ь","ь">;pnom=<"и","и">;sgen=<"и","и">;pgen=<"ей","ей">;sdat=<"и","и">;pdat=<"ям","ям">;sacc=<"?","?">;pacc=<"?","?">;sins=<"ем","ём">;pins=<"ями","ями">;sprep=<"и","и">;pprep=<"ях","ях">} ;
      <_, Fem, 1> => {snom=<"а","а">;pnom=<"ы","ы">;sgen=<"ы","ы">;pgen=<"","">;sdat=<"е","е">;pdat=<"ам","ам">;sacc=<"у","у">;pacc=<"?","?">;sins=<"ой","ой">;pins=<"ами","ами">;sprep=<"е","е">;pprep=<"ах","ах">} ;
      <_, Fem, 2> => {snom=<"я","я">;pnom=<"и","и">;sgen=<"и","и">;pgen=<"ь","ей">;sdat=<"е","е">;pdat=<"ям","ям">;sacc=<"ю","ю">;pacc=<"?","?">;sins=<"ей","ёй">;pins=<"ями","ями">;sprep=<"е","е">;pprep=<"ях","ях">} ;
      <_, Fem, 3> => {snom=<"а","а">;pnom=<"и","и">;sgen=<"и","и">;pgen=<"","">;sdat=<"е","е">;pdat=<"ам","ам">;sacc=<"у","у">;pacc=<"?","?">;sins=<"ой","ой">;pins=<"ами","ами">;sprep=<"е","е">;pprep=<"ах","ах">} ;
      <_, Fem, 4> => {snom=<"а","а">;pnom=<"и","и">;sgen=<"и","и">;pgen=<"","ей">;sdat=<"е","е">;pdat=<"ам","ам">;sacc=<"у","у">;pacc=<"?","?">;sins=<"ей","ой">;pins=<"ами","ами">;sprep=<"е","е">;pprep=<"ах","ах">} ;
      <_, Fem, 5> => {snom=<"а","а">;pnom=<"ы","ы">;sgen=<"ы","ы">;pgen=<"","">;sdat=<"е","е">;pdat=<"ам","ам">;sacc=<"у","у">;pacc=<"?","?">;sins=<"ей","ой">;pins=<"ами","ами">;sprep=<"е","е">;pprep=<"ах","ах">} ;
      <_, Fem, 6> => {snom=<"я","я">;pnom=<"и","и">;sgen=<"и","и">;pgen=<"й","й">;sdat=<"е","е">;pdat=<"ям","ям">;sacc=<"ю","ю">;pacc=<"?","?">;sins=<"ей","ёй">;pins=<"ями","ями">;sprep=<"е","е">;pprep=<"ях","ях">} ;
      <_, Fem, 7> => {snom=<"я","я">;pnom=<"и","и">;sgen=<"и","и">;pgen=<"й","й">;sdat=<"и","е">;pdat=<"ям","ям">;sacc=<"ю","ю">;pacc=<"?","?">;sins=<"ей","ёй">;pins=<"ями","ями">;sprep=<"и","е">;pprep=<"ях","ях">} ;
      <_, Fem, 8> => {snom=<"ь","ь">;pnom=<"и","и">;sgen=<"и","и">;pgen=<"ей","ей">;sdat=<"и","и">;pdat=<"ям","ям">;sacc=<"ь","ь">;pacc=<"?","?">;sins=<"ью","ью">;pins=<"ями","ями">;sprep=<"и","и">;pprep=<"ях","ях">} ;
      <_, Neut, 1> => {snom=<"о","о">;pnom=<"а","а">;sgen=<"а","а">;pgen=<"","">;sdat=<"у","у">;pdat=<"ам","ам">;sacc=<"?","?">;pacc=<"?","?">;sins=<"ом","ом">;pins=<"ами","ами">;sprep=<"е","е">;pprep=<"ах","ах">} ;
      <_, Neut, 2> => {snom=<"е","е">;pnom=<"я","я">;sgen=<"я","я">;pgen=<"ь","ей">;sdat=<"ю","ю">;pdat=<"ям","ям">;sacc=<"?","?">;pacc=<"?","?">;sins=<"ем","ём">;pins=<"ями","ями">;sprep=<"е","е">;pprep=<"ях","ях">} ;
      <_, Neut, 3> => {snom=<"о","о">;pnom=<"а","а">;sgen=<"а","а">;pgen=<"","">;sdat=<"у","у">;pdat=<"ам","ам">;sacc=<"?","?">;pacc=<"?","?">;sins=<"ом","ом">;pins=<"ами","ами">;sprep=<"е","е">;pprep=<"ах","ах">} ;
      <_, Neut, 4> => {snom=<"е","о">;pnom=<"а","а">;sgen=<"а","а">;pgen=<"","ей">;sdat=<"у","у">;pdat=<"ам","ам">;sacc=<"?","?">;pacc=<"?","?">;sins=<"ем","ом">;pins=<"ами","ами">;sprep=<"е","е">;pprep=<"ах","ах">} ;
      <_, Neut, 5> => {snom=<"е","о">;pnom=<"а","а">;sgen=<"а","а">;pgen=<"","">;sdat=<"у","у">;pdat=<"ам","ам">;sacc=<"?","?">;pacc=<"?","?">;sins=<"ем","ом">;pins=<"ами","ами">;sprep=<"е","е">;pprep=<"ах","ах">} ;
      <_, Neut, 6> => {snom=<"е","е">;pnom=<"я","я">;sgen=<"я","я">;pgen=<"й","й">;sdat=<"ю","ю">;pdat=<"ям","ям">;sacc=<"?","?">;pacc=<"?","?">;sins=<"ем","ём">;pins=<"ями","ями">;sprep=<"е","е">;pprep=<"ях","ях">} ;
      <_, Neut, 7> => {snom=<"е","е">;pnom=<"я","я">;sgen=<"я","я">;pgen=<"й","й">;sdat=<"ю","ю">;pdat=<"ям","ям">;sacc=<"?","?">;pacc=<"?","?">;sins=<"ем","ём">;pins=<"ями","ями">;sprep=<"и","е">;pprep=<"ях","ях">} ;
      <_, Neut, 8> => {snom=<"о","о">;pnom=<"а","а">;sgen=<"а","а">;pgen=<"","">;sdat=<"у","у">;pdat=<"ам","ам">;sacc=<"?","?">;pacc=<"?","?">;sins=<"ом","ом">;pins=<"ами","ами">;sprep=<"и","и">;pprep=<"ах","ах">}
    } ;

-------------
-- Adjectives

  toAdjStressSchema : Str -> AdjStressSchema
    = \s ->
      case s of {
        "b/c''" => B_C'' ;
        "a/c''" => A_C'' ;
        "a/b'" => A_B' ;
        "a/c'" => A_C' ;
        "b/a'" => B_A' ;
        "a/a'" => A_A' ;
        "b/b'" => B_B' ;
        "b/c'" => B_C' ;
        "b/c" => B_C ;
        "b/a" => B_A ;
        "b/b" => B_B ;
        "a/a" => A_A ;
        "a/c" => A_C ;
        "a/b" => A_B ;
        "a'" => A'_ ;
        "b'" => B'_ ;
        "a" => A_ ;
        "b" => B_ ;
        "c" => C_ ;
        _ => A_
      } ;

  parseAdjIndex : Str -> ZAIndex
    = \s ->
      case s of {
        "0" => ZA0 ;
        dt@(#digit) + at@("*"|"°"|"") + ss@(#adj_stress_schema) + zc@("①"|"①②"|"②①"|"②"|"(1)"|"(1)(2)"|"(2)(1)"|"(2)")
          => ZA (digitToDeclType dt) (toAlterType at) (toAdjStressSchema ss) (toZCirc zc) ;
        dt@(#digit) + at@("*"|"°"|"") + ss@(#adj_stress_schema)
          => ZA (digitToDeclType dt) (toAlterType at) (toAdjStressSchema ss) NoC ;
        _ => Predef.error "Error: incorrect ZAIndex"
      } ;


  AdjectiveEndFormsS1 : Type = {
      msnom, fsnom, nsnom, pnom, msgen, fsgen, pgen, msdat, fsacc, msins, fsins, pins, msprep, sm, sf, sn, sp, comp : EndingSpec ;
  } ;

  AdjectiveImmutableCasesS1 : AdjectiveEndFormsS1 = {
    msnom=<"","">;fsnom=<"","">;nsnom=<"","">;pnom=<"","">;msgen=<"","">;fsgen=<"","">;pgen=<"","">;msdat=<"","">;fsacc=<"","">;msins=<"","">;fsins=<"","">;pins=<"","">;msprep=<"","">;sm=<"","">;sf=<"","">;sn=<"","">;sp=<"","">;comp=<"","">;
  } ;

  onlyParticipleForms : AdjForms -> AdjForms
    -- To prevent shadowing homonymic forms while parsing or empty, here asterisk has been to incorect forms
    = \af -> af ** {sm=af.sm+"*"; sf=af.sf+"*"; sn=af.sn+"*"; sp=af.sp+"*"; comp=af.comp+"*"} ;

  immutableAdjectiveCases : Str -> AdjForms
    = \s -> {
      msnom=s;fsnom=s;nsnom=s;pnom=s;msgen=s;fsgen=s;pgen=s;msdat=s;fsacc=s;msins=s;fsins=s;pins=s;msprep=s;sm=s;sf=s;sn=s;sp=s;comp=[];
      preferShort=PreferFull ;
      p=False
    } ;

  makeAdjective : Str -> ZAIndex -> ShortFormPreference -> AdjForms
    = \word, z, sfp ->
    case z of {
      ZA0 => immutableAdjectiveCases word ;
      ZA dt at ss ci => formsSelectionAdjective word dt at ss ci sfp
    } ;

  formsSelectionAdjective : Str -> DeclType -> AlterType -> AdjStressSchema -> ZCirc -> ShortFormPreference -> AdjForms
    = \word, dt, at, ss, ci, sfp ->
      let stem = stemFromAdjective word dt in
      let aef = endingsSelectionAdj dt at ss sfp in
      let alternated = alterFormsAdj stem aef dt at ss ci in    -- TODO: alternation, fix comparative for dt=3
      alternated
    ;

  stemFromAdjective : Str -> DeclType -> Str
    = \word, dt ->
      let end1 = (gDtBasedSelectionAdj dt).msnom.p1 in
      case end1 of {
        "" => word ;
        _ => Predef.tk 2 word
      }
    ;

  alterFormsAdj : Str -> AdjForms -> DeclType -> AlterType -> AdjStressSchema -> ZCirc -> AdjForms
    = \s, aef, dt, at, ss, ci ->
      case at of {
        Ast => doAlternationsAdj s aef dt ss ci ;
        _ => noAlternationsAdj s aef dt ss ci
    } ;

  mobileShortMascAdj : Str -> DeclType -> AlterType -> AdjStressSchema -> ZCirc -> Str
    = \s, dt, at, ss, ci ->
      let last = Predef.dp 1 s in
      let butLast = case ci of {NoC => Predef.tk 1 s; ZC1 | ZC2 | ZC12 => Predef.tk 2 s } in
      let butLastCirc = case ci of {NoC => butLast; ZC1 | ZC2 | ZC12 => Predef.tk 3 s } in
      let secondLast = Predef.dp 1 butLast in
      let butTwolast = Predef.tk 1 butLast in
      let thirdLast = Predef.dp 1 butTwolast in
      let smStressed = stressTableAdj ss "sm" in
      case <dt, at, s, secondLast, last, smStressed, ci> of {   -- what if more than one consonant or sign? eg день
         <1, Ast, _, "й"|"ь", _, _, _> => butTwolast + "е" + last ;
         <1, Ast, _, "ж"|"ш"|"ч"|"щ", _, Stressed, _> => butLast + "о" + last ;
         <1|2, Ast, _, _, _, Stressed, ZC1 | ZC2 | ZC12> => butLastCirc + "ё" + last ;
         <1|2, Ast, _, _, _, _, ZC1 | ZC2 | ZC12> => butLastCirc + "е" + last ;
         <1, Ast, _, _, _, Stressed, _> => butLast + "ё" + last ;
         <1, Ast, _, _, _, _, _> => butLast + "е" + last ;
         <2, No,  _, _, _, _, _> => s + "ь";
         <2, Ast, _, _, _, _, _> => butLast + "е" + last ;
         <3, Ast, _, _, _, _, _> => butLast + "о" + last ;   -- долг(ий) - долог
         _ => s
      } ;

  mobileShortAdj : Str -> DeclType -> AlterType -> AdjStressSchema -> ZCirc -> Str
    = \s, dt, at, ss, ci ->
      case ci of {
        NoC => s ;
        ZC1 | ZC2 | ZC12 => (Predef.tk 3 s) + "ен"
      } ;

  mobileCompAdj : Str -> DeclType -> Str
    = \s, dt ->
      let last = Predef.dp 1 s in
      let butLast = Predef.tk 1 s in
      let secondLast = Predef.dp 1 butLast in
      let butTwolast = Predef.tk 2 s in
      let thirdLast = Predef.dp 1 butTwolast in
      case <dt, s> of {   -- what if more than one consonant or sign? eg день
         <3, _ + "к"> => butLast + "ч" ;
         <3, _ + "г"> => butLast + "ж" ;
         <3, _ + "х"> => butLast + "ш" ;
         _ => s
      } ;

  noAlternationsAdj : Str -> AdjForms -> DeclType -> AdjStressSchema -> ZCirc -> AdjForms
    = \s, aef, dt, ss, ci ->
      let sms = mobileShortMascAdj s dt No ss ci in
      let sstem = mobileShortAdj s dt No ss ci in
      let comps = mobileCompAdj s dt in
      {
        msnom = s + aef.msnom  ;
        fsnom = s + aef.fsnom  ;
        nsnom = s + aef.nsnom  ;
        pnom  = s + aef.pnom   ;
        msgen = s + aef.msgen  ;
        fsgen = s + aef.fsgen  ;
        pgen  = s + aef.pgen   ;
        msdat = s + aef.msdat  ;
        fsacc = s + aef.fsacc  ;
        msins = s + aef.msins  ;
        fsins = s + aef.fsins  ;
        pins  = s + aef.pins   ;
        msprep= s + aef.msprep ;
        sm    = sms + aef.sm   ;
        sf    = sstem + aef.sf ;
        sn    = sstem + aef.sn ;
        sp    = sstem + aef.sp ;
        comp  = comps + aef.comp ;
        preferShort = aef.preferShort ;
        p = aef.p
      } ;

  doAlternationsAdj : Str -> AdjForms -> DeclType -> AdjStressSchema -> ZCirc -> AdjForms
    = \s, aef, dt, ss, ci ->
      let sms = mobileShortMascAdj s dt Ast ss ci in
      let sstem = mobileShortAdj s dt Ast ss ci in
      let comps = mobileCompAdj s dt in
      {
        msnom = s + aef.msnom  ;
        fsnom = s + aef.fsnom  ;
        nsnom = s + aef.nsnom  ;
        pnom  = s + aef.pnom   ;
        msgen = s + aef.msgen  ;
        fsgen = s + aef.fsgen  ;
        pgen  = s + aef.pgen   ;
        msdat = s + aef.msdat  ;
        fsacc = s + aef.fsacc  ;
        msins = s + aef.msins  ;
        fsins = s + aef.fsins  ;
        pins  = s + aef.pins   ;
        msprep= s + aef.msprep ;
        sm    = sms + aef.sm   ;
        sf    = sstem + aef.sf ;
        sn    = sstem + aef.sn ;
        sp    = sstem + aef.sp ;
        comp  = comps + aef.comp ;
        preferShort = aef.preferShort ;
        p = False
      } ;

  endingsSelectionAdj : DeclType -> AlterType -> AdjStressSchema -> ShortFormPreference -> AdjForms
    = \dt, at, ss, sfp ->
    let gDtBased = gDtBasedSelectionAdj dt in
    gDtSsBasedSelectionAdj gDtBased ss sfp
  ;

  gDtSsBasedSelectionAdj : AdjectiveEndFormsS1 -> AdjStressSchema -> ShortFormPreference -> AdjForms
    = \aef1, ss, sfp ->
      {
        msnom  = stressSelectionAdj aef1.msnom  ss "msnom" ;
        fsnom  = stressSelectionAdj aef1.fsnom  ss "fsnom" ;
        nsnom  = stressSelectionAdj aef1.nsnom  ss "nsnom" ;
        pnom   = stressSelectionAdj aef1.pnom   ss "pnom" ;
        msgen  = stressSelectionAdj aef1.msgen  ss "msgen" ;
        fsgen  = stressSelectionAdj aef1.fsgen  ss "fsgen" ;
        pgen   = stressSelectionAdj aef1.pgen   ss "pgen " ;
        msdat  = stressSelectionAdj aef1.msdat  ss "msdat" ;
        fsacc  = stressSelectionAdj aef1.fsacc  ss "fsacc" ;
        msins  = stressSelectionAdj aef1.msins  ss "msins" ;
        fsins  = stressSelectionAdj aef1.fsins  ss "fsins" ;
        pins   = stressSelectionAdj aef1.pins   ss "pins" ;
        msprep = stressSelectionAdj aef1.msprep ss "msprep" ;
        sm     = stressSelectionAdj aef1.sm     ss "sm" ;
        sf     = stressSelectionAdj aef1.sf     ss "sf" ;
        sn     = stressSelectionAdj aef1.sn     ss "sn" ;
        sp     = stressSelectionAdj aef1.sp     ss "sp" ;
        comp   = stressSelectionAdj aef1.comp   ss "comp" ;
        preferShort = sfp ;
        p = False
    } ;

  stressSelectionAdj : EndingSpec -> AdjStressSchema -> Str -> Str
    = \es, ss, c ->
    selStress es (stressTableAdj ss c) ;

  stressTableAdj : AdjStressSchema -> Str -> Stressedness
    = \ss, c ->
    case <ss, c> of {
      <A'_, "sf"> => Stressed ;
      <B_, "msnom"|"fsnom"|"nsnom"|"msgen"|"fsgen"|"msdat"|"fsacc"|"msins"|"fsins"|"msprep"|"pnom"|"pgen"|"pins"|"sf"|"sn"|"sp"|"sm"> => Stressed ;
      <B'_, "msnom"|"fsnom"|"nsnom"|"msgen"|"fsgen"|"msdat"|"fsacc"|"msins"|"fsins"|"msprep"|"pnom"|"pgen"|"pins"|"sf"|"sn"|"sp"> => Stressed ;
      <A_B, "sf"|"sn"|"sp"|"sm"> => Stressed ;
      <A_C, "sf"> => Stressed ;
      <A_A', "sf"> => Stressed ;
      <A_B', "sf"|"sn"|"sp"> => Stressed ;
      <A_C', "sf"|"sp"> => Stressed ;
      <A_C'', "sf"|"sn"|"sp"> => Stressed ;
      <B_A, "msnom"|"fsnom"|"nsnom"|"msgen"|"fsgen"|"msdat"|"fsacc"|"msins"|"fsins"|"msprep"|"pnom"|"pgen"|"pins"> => Stressed ;
      <B_B, "msnom"|"fsnom"|"nsnom"|"msgen"|"fsgen"|"msdat"|"fsacc"|"msins"|"fsins"|"msprep"|"pnom"|"pgen"|"pins"|"sf"|"sn"|"sp"> => Stressed ;
      <B_C, "msnom"|"fsnom"|"nsnom"|"msgen"|"fsgen"|"msdat"|"fsacc"|"msins"|"fsins"|"msprep"|"pnom"|"pgen"|"pins"|"sf"> => Stressed ;
      <B_A', "msnom"|"fsnom"|"nsnom"|"msgen"|"fsgen"|"msdat"|"fsacc"|"msins"|"fsins"|"msprep"|"pnom"|"pgen"|"pins"|"sf"> => Stressed ;
      <B_B', "msnom"|"fsnom"|"nsnom"|"msgen"|"fsgen"|"msdat"|"fsacc"|"msins"|"fsins"|"msprep"|"pnom"|"pgen"|"pins"|"sf"|"sn"|"sp"> => Stressed ;
      <B_C', "msnom"|"fsnom"|"nsnom"|"msgen"|"fsgen"|"msdat"|"fsacc"|"msins"|"fsins"|"msprep"|"pnom"|"pgen"|"pins"|"sf"|"sp"> => Stressed ;
      <B_C'', "msnom"|"fsnom"|"nsnom"|"msgen"|"fsgen"|"msdat"|"fsacc"|"msins"|"fsins"|"msprep"|"pnom"|"pgen"|"pins"|"sf"|"sn"|"sp"> => Stressed ;
      <_, _> => Unstressed
    } ;

  gDtBasedSelectionAdj : DeclType -> AdjectiveEndFormsS1
    = \dt -> case dt of {
      0 => AdjectiveImmutableCasesS1 ;
      1 => {msnom=<"ый","ой">;msgen=<"ого","ого">;msdat=<"ому","ому">;msins=<"ым","ым">;msprep=<"ом","ом">;sm=<"","">;fsnom=<"ая","ая">;fsgen=<"ой","ой">;fsacc=<"ую","ую">;fsins=<"ой","ой">;sf=<"а","а">;nsnom=<"ое","ое">;sn=<"о","о">;pnom=<"ые","ые">;pgen=<"ых","ых">;pins=<"ыми","ыми">;sp=<"ы","ы">;comp=<"ее","ее">} ;
      2 => {msnom=<"ий","ий">;msgen=<"его","его">;msdat=<"ему","ему">;msins=<"им","им">;msprep=<"ем","ем">;sm=<"","">;fsnom=<"яя","яя">;fsgen=<"ей","ей">;fsacc=<"юю","юю">;fsins=<"ей","ей">;sf=<"я","я">;nsnom=<"ее","ее">;sn=<"е","ё">;pnom=<"ие","ие">;pgen=<"их","их">;pins=<"ими","ими">;sp=<"и","и">;comp=<"ее","ее">} ;
      3 => {msnom=<"ий","ой">;msgen=<"ого","ого">;msdat=<"ому","ому">;msins=<"им","им">;msprep=<"ом","ом">;sm=<"","">;fsnom=<"ая","ая">;fsgen=<"ой","ой">;fsacc=<"ую","ую">;fsins=<"ой","ой">;sf=<"а","а">;nsnom=<"ое","ое">;sn=<"о","о">;pnom=<"ие","ие">;pgen=<"их","их">;pins=<"ими","ими">;sp=<"и","и">;comp=<"е","е">} ;
      4 => {msnom=<"ий","ой">;msgen=<"его","ого">;msdat=<"ему","ому">;msins=<"им","им">;msprep=<"ем","ом">;sm=<"","">;fsnom=<"ая","ая">;fsgen=<"ей","ой">;fsacc=<"ую","ую">;fsins=<"ей","ой">;sf=<"а","а">;nsnom=<"ее","ое">;sn=<"е","о">;pnom=<"ие","ие">;pgen=<"их","их">;pins=<"ими","ими">;sp=<"и","и">;comp=<"ее","ее">} ;
      5 => {msnom=<"ый","ой">;msgen=<"его","ого">;msdat=<"ему","ому">;msins=<"ым","ым">;msprep=<"ем","ом">;sm=<"","">;fsnom=<"ая","ая">;fsgen=<"ей","ой">;fsacc=<"ую","ую">;fsins=<"ей","ой">;sf=<"а","а">;nsnom=<"ее","ое">;sn=<"е","о">;pnom=<"ые","ые">;pgen=<"ых","ых">;pins=<"ыми","ыми">;sp=<"ы","ы">;comp=<"ее","ее">} ;
      6 => {msnom=<"ий","ий">;msgen=<"его","его">;msdat=<"ему","ему">;msins=<"им","им">;msprep=<"ем","ем">;sm=<"й","й">;fsnom=<"яя","яя">;fsgen=<"ей","ей">;fsacc=<"юю","юю">;fsins=<"ей","ей">;sf=<"я","я">;nsnom=<"ее","ее">;sn=<"е","ё">;pnom=<"ие","ие">;pgen=<"их","их">;pins=<"ими","ими">;sp=<"и","и">;comp=<"ее","ее">} ;
      7|8 => Predef.error "Error: adjectives do not have class 7 or 8" ;
      _ => {msnom=<"ий","ий">;msgen=<"его","его">;msdat=<"ему","ему">;msins=<"им","им">;msprep=<"ем","ем">;sm=<"ь","ь">;fsnom=<"яя","яя">;fsgen=<"ей","ей">;fsacc=<"юю","юю">;fsins=<"ей","ей">;sf=<"я","я">;nsnom=<"ее","ее">;sn=<"е","ё">;pnom=<"ие","ие">;pgen=<"их","их">;pins=<"ими","ими">;sp=<"и","и">;comp=<"ее","ее">}
    } ;

-----------
-- Pronouns

  -- these are needed for numerals
  pronounAdj1A : Str -> PronForms
    = \word -> makeAdjective word (ZA 1 No A_ NoC) PreferFull ;

  pronounAdj1B : Str -> PronForms
    = \word -> makeAdjective word (ZA 1 No B_ NoC) PreferFull ;

  pronounAdj1AstA : Str -> PronForms
    = \word -> makeAdjective word (ZA 1 Ast A_ NoC) PreferFull ;

  pronoun1A : Str -> PronForms
    = \word -> -- Христов
      let stem = word in
      {
        msnom=stem ;
        fsnom=stem  +"а" ;
        nsnom=stem  +"о" ;
        pnom=stem   +"ы" ;
        msgen=stem  +"а" ;
        fsgen=stem  +"ой" ;
        pgen=stem   +"ых" ;
        msdat=stem  +"у" ;
        fsacc=stem  +"у" ;
        msins=stem  +"ым" ;
        fsins=stem  +"ой" ; -- ою
        pins=stem   +"ыми" ;
        msprep=stem +"ом" ;
    } ;

  pronoun2AstB : Str -> PronForms
    = \word -> -- весь
      let cmp_base : Str = case word of {s + "ь" => s ; _ => word} in --
      let last = Predef.dp 1 cmp_base in
      let butTwolast = Predef.tk 2 cmp_base in  --
      let stem = cmp_base in
      let stem2 = butTwolast + last in
      {
        msnom=stem   +"ь" ;
        fsnom=stem2  +"я" ;
        nsnom=stem2  +"ё" ;
        pnom=stem2   +"е" ;
        msgen=stem2  +"его" ;
        fsgen=stem2  +"ей" ;
        pgen=stem2   +"ех" ;
        msdat=stem2  +"ему" ;
        fsacc=stem2  +"ю" ;
        msins=stem2  +"ем" ;
        fsins=stem2  +"ей" ; -- ею
        pins=stem2   +"еми" ;
        msprep=stem2 +"ем" ;  --ём
    } ;

  pronoun6AstA : Str -> PronForms
    = \word -> -- третий
      let cmp_base : Str = case word of {_ => word} in --
      let butTwolast = Predef.tk 2 cmp_base in --
      let stem = cmp_base in  --
      let stem2 = butTwolast + "ь" in --
      {
        msnom=stem   ;
        fsnom=stem2  +"я" ;
        nsnom=stem2  +"е" ;
        pnom=stem2   +"и" ;
        msgen=stem2  +"его" ;
        fsgen=stem2  +"ей" ;
        pgen=stem2   +"их" ;
        msdat=stem2  +"ему" ;
        fsacc=stem2  +"ю" ;
        msins=stem2  +"им" ;
        fsins=stem2  +"ей" ;
        pins=stem2   +"ими" ;
        msprep=stem2 +"ем" ;
    } ;


--------
-- Verbs

  toVerbStressSchema : Str -> VerbStressSchema
    = \s ->
      case s of {
        "a/c'" => VSS _A _C' ;
        "b/c'" => VSS _B _C' ;
        "c/c'" => VSS _B _C' ;
        "a/b" => VSS _A _B ;
        "a/c" => VSS _A _C ;
        "b/b" => VSS _B _B ;
        "b/c" => VSS _B _C ;
        "c/b" => VSS _B _B ;
        "c/c" => VSS _B _C ;
        "a" => VSS _A _A ;
        "b" => VSS _B _A ;
        "c" => VSS _C _A ;
        _ => VSS _A _A
      } ;

  VerbInherent : Type = {
    fut : SpecialFuture ;
    refl : Reflexivity ;
    asp : Aspect ;
    tran : Transitivity
    } ;

  VerbInf : Type = {
    inf, infrefl : Str
    } ;

  VerbPresFut : Type = {
    prsg1, prsg2, prsg3, prpl1, prpl2, prpl3 : Str
    } ;

  VerbPast : Type = {
    psgm, psgs : Str
    } ;

  VerbImp : Type = {
    isg2, ipl1, isg2refl : Str
    } ;

  VerbPassPastPart : Type = {
    ppps : Str ;  -- full form stem
    pppss : Str   -- short stem
    } ;

  VerbTransgressive : Type = {
    prtr, ptr: Str
    } ;

  infStemFromVerb : Str -> Str * Reflexivity
    = \v ->
      case v of {
        s + ("ть" | "ти" | "чь") => <s, NonReflexive> ;
        s + ("ться" | "тись" | "чься") => <s, Reflexive> ;
        _ => Predef.error "Error: incorrect infinitive"
      } ;

  dropRefl : Str -> Str
    = \v -> case v of {s + ("ся" | "сь") => s ; _ => v} ;

  sg1StemFromVerb : Str -> Str
    = \v ->
      case v of {
        s + ("у" | "ю" | "усь" | "юсь") => s ;
        _ => Predef.error ("Error: incorrect Sg P1 Pres/Fut:" + v)
      } ;

  sg3StemAndConjFromVerb : Str -> Str * Conjug
    = \v ->
      case v of {
        s + ("ет" | "ется") => <s, I> ;
        s + ("ёт" | "ётся") => <s, I'> ;
        s + ("ит" | "ится") => <s, II> ;
        _ => Predef.error ("Error: incorrect Sg P3 Pres/Fut:" + v)
      } ;

  parseVerbIndex : Str -> ZVIndex
    = \s ->
      case s of {
        dt@(#small_num) + at@("*"|"°"|"") + ss@(#verb_stress_schema)
          => ZV (numToConjType dt) (toAlterType at) (toVerbStressSchema ss) ;
        _ => Predef.error ("Error: incorrect ZVIndex" ++ s)
      } ;

  guessRegularIndex : Str -> Str -> Str -> ZVIndex * Reflexivity
    = \inf,sgP1PresFut,sgP3PresFut ->
      let inf1 = dropRefl inf in
      let stem_info = infStemFromVerb inf in
      let inf_s = stem_info.p1 in
      let refl = stem_info.p2 in
      let sg1 = sg1StemFromVerb sgP1PresFut in
      let sg3 = sg3StemAndConjFromVerb sgP3PresFut in
      let conjtype : ConjType = case <sg3.p2,inf1,sg1,sg3.p1> of {
        <I | I', i+("овать"|"евать"), s2+"у", s3+"у"> => 2 ;
        <I | I', i+"евать", s2+"ю", s3+"ю"> => 2 ;
        <I | I', i+"авать", s2+"а", s3+"а"> => 13 ;
        <I | I', i+("ереть"), s2+"р", s3+"р"> => 9 ;
        <I | I', i+("олоть"), s2+"ол", s3+"ол"> => 10 ;
        <I | I', i+("ороть"), s2+"ор", s3+"ор"> => 10 ;
        <I | I', i+("зти"|"зть"), s2 + "з", s3 + "з"> => 7 ;
        <I | I', i+("сти"|"сть"), s2 + ("с"|"д"|"ст"|"т"|"б"), s3 + ("с"|"д"|"ст"|"т"|"б")> => 7 ;
        <I | I', i+"ать", s2+"а", s3+"а"> => 1 ;
        <I | I', i+"ять", s2+"я", s3+"я"> => 1 ;
        <I     , i+"еть", s2+"е", s3+"е"> => 1 ;
        <    I', i+"еть", s2+"ё", s3+"ё"> => 1 ; -- ?
        <I | I', i+"нуть", s2+"н", s3+"н"> => 3 ;
        <I | I', i+"чь", s2+"г", s3+"ж"> => 8 ;
        <I | I', i+"чь", s2+"к", s3+"ч"> => 8 ;
        <I | I', i+"ить", s2+"ь", s3+"ь"> => 11 ;
        <I | I', i+"ыть", s2+"о", s3+"о"> => 12 ;  -- also some known others
        <I | I', i+"уть", s2+"у", s3+"у"> => 12 ;
        <I | I', i+"ить", s2+"и", s3+"и"> => 12 ;
        <I | I', i+("ать"|"ять"), s2+"н", s3+"н"> => 14 ;
        <I | I', i+("ать"|"ять"), s2+"м", s3+"м"> => 14 ;
        <I | I', i+("ать"|"ять"), s2+"им", s3+"им"> => 14 ;
        <I | I', i+"ть", s2+"н", s3+"н"> => 15 ;
        <I | I', i+"ть", s2+"в", s3+"в"> => 16 ;
        <I | I', i+("ать"|"ять"), s2, s3> => 6 ;
        <II, i+"ить", s2, s3> => 4 ;  -- после шип  -- here and below alternations possible
        <II, i+("ать"|"ять"|"еть"), s2, s3> => 5 ;
       -- _ => 1
         _ => Predef.error ("Error: guessing verb conjugation does not work for:" + sg1 + sg3.p1 + ":" + inf + ":" + sgP1PresFut + ":" + sgP3PresFut)
      } in <ZV conjtype No (VSS (case sg3.p2 of {I' => _B; _ => _A}) _A), refl> ;

  makeVerb : Str -> Str -> Str -> ZVIndex -> Aspect -> Transitivity -> Reflexivity -> VerbForms
    = \inf, sgP1PresFut, sgP3PresFut, zv, asp, tran, refl ->
      let infs = (infStemFromVerb (dropRefl inf)).p1 in
      let sg1 = sg1StemFromVerb sgP1PresFut in
      let sg3 = sg3StemAndConjFromVerb sgP3PresFut in
      let pl3 = case sg3.p2 of {(I|I') => sg1; II => sg3.p1} in
      let conjtype = case zv of {ZV ct _ _ => ct} in
      let alt = case zv of {ZV _ at _ => at} in
      let prss = case zv of {ZV _ _ (VSS x _) => x} in
      let inff = makeVerbInf (dropRefl inf) refl in
      let past = makeVerbPast infs sg1 conjtype alt in
      let presfut = makeVerbPresFut (dropRefl sgP1PresFut) (dropRefl sgP3PresFut) sg3 in
      let imp = makeVerbImp conjtype prss infs sg3.p1 pl3 presfut.prpl1 in
      let ppp = makeVerbPassPastPart conjtype infs sg1 sg3.p1 past.psgm in
      let tr = makeVerbTransgressive conjtype infs pl3 past.psgm in {
        fut=NormalFuture ;
        asp=asp ;
        refltran = reflTran refl tran ; -- from API params to internal
        -- refl=refl ;
        -- tran=tran ;
        inf=inff.inf ;
        infrefl=inff.infrefl ;
        prsg1=presfut.prsg1 ;
        prsg2=presfut.prsg2 ;
        prsg3=presfut.prsg3 ;
        prpl1=presfut.prpl1 ;
        prpl2=presfut.prpl2 ;
        prpl3=presfut.prpl3 ;
        psgm=past.psgm ;
        psgs=past.psgs ;
        isg2=imp.isg2 ;
        isg2refl=imp.isg2refl ;
        ipl1=imp.ipl1 ;
        ppps=ppp.ppps ;
        pppss=ppp.pppss ;
        prtr=tr.prtr ;
        ptr=tr.ptr
      } ;

  addRefl : Str -> Str
    = \v -> case v of {s + ("ь"|#consonant) => v + "ся" ; _ => v + "сь"} ;

  makeVerbInf : Str -> Reflexivity -> VerbInf
    = \inf, refl -> {
      inf=inf ;
      infrefl=addRefl inf
    } ;

  makeVerbPast : Str -> Str -> ConjType -> AlterType -> VerbPast
    = \infs, sg1, conjtype, alt ->
      case <conjtype,alt,infs,sg1> of {
        <7|8,_,_,s+("т"|"д")> => {psgm=s + "л"; psgs=s} ;
        <7|8,_,_,_> => {psgm=sg1; psgs=sg1} ;
        <9,_,s+("е"|"ё"),_> => {psgm=s; psgs=s} ;
        <3,Deg,s + "ну",_> => {psgm=s; psgs=s} ;
        <_,_,s+#consonant,_> => {psgm=infs; psgs=infs} ;
        _ => {psgm=infs + "л"; psgs=infs}
      } ;

  makeVerbPresFut : Str -> Str -> (Str * Conjug) -> VerbPresFut
    = \sgP1PresFut, sgP3PresFut, sg3 ->
    let sg3s = sg3.p1 in {
      prsg1=sgP1PresFut ;
      prsg2=case sg3.p2 of {I=>sg3s+"ешь" ; I' => sg3s+"ёшь" ; II=>sg3s+"ишь"} ;
      prsg3=sgP3PresFut ;
      prpl1=case sg3.p2 of {I=>sg3s+"ем" ; I' => sg3s+"ём" ; II=>sg3s+"им"} ;
      prpl2=case sg3.p2 of {I=>sg3s+"ете" ; I' => sg3s+"ёте" ; II=>sg3s+"ите"} ;
      prpl3=case sg3 of {<_,I|I'>=>sgP1PresFut+"т" ; <_+("ж"|"ш"|"ч"|"щ"),II> => sg3s+"ат" ; _=>sg3s+"ят"}
      } ;

  makeVerbImp : ConjType -> VerbSS -> Str -> Str -> Str -> Str -> VerbImp =
    \ct, prss, infs, sg3, pl3, prpl1 ->
      let imps=case ct of {13=>infs;_=>pl3} in
      let isg2 : Str =case <ct,prss,imps,sg3> of {
        <4,_B|_C,s + #vowel,_> => imps + "й" ;
        <4,_A,"вы" + s + #vowel,_> => imps + "и" ;
        <11,_,_,s+"ь"> => (Predef.tk 1 sg3) + "ей";
        <_,_,s + #vowel,_> => imps + "й" ;
        <_,_B|_C,s + #consonant,_> => imps + "и" ;
        <_,_A,"вы" + s + #consonant,_> => imps + "и" ;
        <_,_A,s + "щ",_> => imps + "и" ;
        <_,_A,s + #consonant + #consonant,_> => imps + "и" ;
        <_,_A,s + #consonant + "ь" + #consonant,_> => imps + "и" ;
        _ => imps + "ь"
        } in {
      isg2=isg2 ;
      ipl1=prpl1 ;
      isg2refl=addRefl isg2
      } ;

  makeVerbPassPastPart : ConjType -> Str -> Str -> Str -> Str -> VerbPassPastPart =
    \ct, infs, sg1, sg3, psgm -> {
      ppps=case <ct,infs> of { -- TODO
        <9|11|12|14|15|16,_> => case psgm of {s+"л"=>s+"т"; _=>psgm+"т"} ;
        <4,_> => sg1 + "енн" ;   -- TODO: ён
        <5,s+("е"|"ё")> => sg1 + "енн" ;  -- TODO: ён
        <7|8,_> => sg3 + "енн" ;  -- TODO: ён
        <3|10,_> => infs + "т" ;
        _ => infs + "нн"
        } ;
      pppss=case <ct,infs> of {
        <9|11|12|14|15|16,_> => case psgm of {s+"л"=>s+"т"; _=>psgm+"т"} ;
        <4,_> => sg1 + "ен" ;   -- TODO: ён
        <5,s+("е"|"ё")> => sg1 + "ен" ;  -- TODO: ён
        <7|8,_> => sg3 + "ен" ;  -- TODO: ён
        <3|10,_> => infs + "т" ;
        _ => infs + "н"
        } ;
      } ;

  makeVerbTransgressive : ConjType -> Str -> Str -> Str -> VerbTransgressive
    = \ct, infs, pl3, psgm -> {
    prtr=case <ct,pl3> of {
      <13,_> => infs + "я" ;
      <_,s + ("ж" | "ш" | "ч" | "щ")> => pl3 + "а" ;
      _ => pl3 + "я"
      } ;
    ptr=case <ct,psgm> of {
      -- <7, s+"л"> => s+"тши" ;  -- TODO: fix , use sg1 in some cases
      <_, s+"л"> => s+"вши" ;
      <_, s> => psgm+"ши"
      } ;
    } ;

  makeVerbKhotet6 : Aspect -> Transitivity -> Str -> VerbForms
    = \asp,tran,inf ->
      let inf1 = dropRefl inf in
      let stem_info = infStemFromVerb inf in
      let inf_s : Str = stem_info.p1 in
      let com : Str = case inf_s of {c + "те" => c ; _ => inf_s} in
      let refl = stem_info.p2 in {
        inf=inf1 ;
        infrefl=inf1 + "ся" ;
        prsg1=com + "чу";
        prsg2=com + "чешь";
        prsg3=com + "чет";
        prpl1=com + "тим";
        prpl2=com + "тите";
        prpl3=com + "тят";
        fut=NormalFuture ; -- ?
        psgm=com + "тел";
        psgs=com + "те";
        isg2=com + "ти";
        isg2refl=com + "тись";
        ipl1=[];
        ppps=com + "тим";  -- incorrect, but prevents empty
        pppss=com + "тим";  -- incorrect, but prevents empty
        prtr=com + "тя";
        ptr=com + "тев";
        asp=asp;
        refltran = reflTran refl tran ;
        } ;

  makeVerbBezhat6 : Aspect -> Transitivity -> Str -> VerbForms
    = \asp,tran,inf ->
      let inf1 = dropRefl inf in
      let stem_info = infStemFromVerb inf in
      let inf_s : Str = stem_info.p1 in
      let com : Str = case inf_s of {c + "жа" => c ; _ => inf_s} in
      let refl = stem_info.p2 in {
        inf=inf1 ;
        infrefl=inf1 + "ся" ;
        prsg1=com + "гу";
        prsg2=com + "жишь";
        prsg3=com + "жит";
        prpl1=com + "жим";
        prpl2=com + "жите";
        prpl3=com + "гут";
        fut=NormalFuture ; -- ?
        psgm=com + "жал";
        psgs=com + "жа";
        isg2=com + "ги";
        isg2refl=com + "гись";
        ipl1=[];
        ppps=com + "ган"; -- incorrect, but prevents parsing problems
        pppss=com + "ган"; -- incorrect, but prevents parsing problems
        prtr=com + "жа"; -- *
        ptr=com + "жав";
        asp=asp;
        refltran = reflTran refl tran ;
        } ;

  makeVerbEst : Aspect -> Transitivity -> Str -> VerbForms
    = \asp,tran,inf ->
      let inf1 = dropRefl inf in
      let stem_info = infStemFromVerb inf in
      let inf_s : Str = stem_info.p1 in
      let com : Str = case inf_s of {c + "с" => c ; _ => inf_s} in
      let refl = stem_info.p2 in {
        inf=inf1 ;
        infrefl=inf1 + "ся" ;
        prsg1=com + "м";
        prsg2=com + "шь";
        prsg3=com + "ст";
        prpl1=com + "дим";
        prpl2=com + "дите";
        prpl3=com + "дят";
        fut=NormalFuture ;
        psgm=com + "л";
        psgs=com ;
        isg2=com + "шь";
        isg2refl=com + "шься";
        ipl1=[];
        ppps=com + "денн";  -- *
        pppss=com + "ден";  -- *
        prtr=com + "дя";
        ptr=com + "в";
        asp=asp;
        refltran = reflTran refl tran ;
        } ;

  makeVerbDat6 : Aspect -> Transitivity -> Str -> VerbForms
    = \asp,tran,inf ->
      let inf1 = dropRefl inf in
      let stem_info = infStemFromVerb inf in
      let com : Str = stem_info.p1 in
      let refl = stem_info.p2 in {
        inf=inf1 ;
        infrefl=inf1 + "ся" ;
        prsg1=com + "м";
        prsg2=com + "шь";
        prsg3=com + "ст";
        prpl1=com + "дим";
        prpl2=com + "дите";
        prpl3=com + "дут";
        fut=NormalFuture ;
        psgm=com + "л";
        psgs=com ;
        isg2=com + "й";
        isg2refl=com + "йся";
        ipl1=[];
        ppps=com + "нн"; -- *
        pppss=com + "н"; -- *
        prtr=com + "вая";
        ptr=com + "в";
        asp=asp;
        refltran = reflTran refl tran ;
        } ;

  makeVerbByt6 : Aspect -> Transitivity -> Str -> VerbForms
    = \asp,tran,inf ->
      let inf1 = dropRefl inf in
      let stem_info = infStemFromVerb inf in
      let inf_s : Str = stem_info.p1 in
      let com : Str = case inf_s of {c + "ы" => c ; _ => inf_s} in
      let refl = stem_info.p2 in {
        inf=inf1 ;
        infrefl=inf1 + "ся" ;
        prsg1=com + "уду";
        prsg2=com + "удешь";
        prsg3=com + "удет";
        prpl1=com + "удем";
        prpl2=com + "удете";
        prpl3=com + "удут";
        fut=NormalFuture ;
        psgm=com + "ыл";
        psgs=com + "ы";
        isg2=com + "удь";
        isg2refl=com + "удься";
        ipl1=[];
        ppps=com + "ыт";  -- *
        pppss=com + "ыт";  -- *
        prtr=com + "ывая";
        ptr=com + "ыв";
        asp=asp;
        refltran = reflTran refl tran ;
        } ;

  makeVerbJti: Aspect -> Transitivity -> Str -> Str -> VerbForms
    = \asp,tran,inf,e ->
      let inf1 = dropRefl inf in
      let stem_info = infStemFromVerb inf in
      let inf_s : Str = stem_info.p1 in
      let com : Str = case inf_s of {"ид" => "и" ; c + "й" => inf_s ; _ => inf_s} in
      let comPast : Str = case inf_s of {"ид" => "ш" ; _ => (Predef.tk 1 inf_s) + "ш" } in
      let refl = stem_info.p2 in {
        inf=inf1 ;
        infrefl=inf1 + "сь" ;
        prsg1=com + "ду";
        prsg2=com + "д" + e + "шь";
        prsg3=com + "д" + e + "т";
        prpl1=com + "д" + e + "м";
        prpl2=com + "д" + e + "те";
        prpl3=com + "дут";
        fut=NormalFuture ;
        psgm=comPast + e + "л";
        psgs=comPast ;
        isg2=com + "ди";
        isg2refl=com + "дись";
        ipl1=[];
        ppps=com + "денн"; -- *
        pppss="com + ден"; -- *
        prtr=com + "дя";
        ptr=[];
        asp=asp;
        refltran = reflTran refl tran ;
        } ;
}
