resource ParadigmsJpn = CatJpn **
  open ResJpn, CatJpn, Prelude in {

flags coding = utf8 ;

oper

  VerbGroup : Type ; -- Parameter for mkV*
  gr1 : VerbGroup ; -- Basic form ends in -u, consonant stem.
  gr2 : VerbGroup ; -- Basic form ends in -iru/-eru, vowel stem.
  suru : VerbGroup ; -- Irregular: kuru
  kuru : VerbGroup ; -- Irregular: suru

  Animacy : Type ; -- Parameter for mkN*
  animate : Animacy ;
  inanimate : Animacy ;

  mkN = overload {
    mkN : (man : Str) -> N -- Inanimate noun. Counter is つ and doesn't replace the noun.
      = \n -> lin N (regNoun n Inanim "つ" False True) ;
    mkN : (man : Str) -> (anim : Animacy) -> N -- Animacy given as argument. Counter is つ and doesn't replace the noun.
      = \n,a -> lin N (regNoun n a "つ" False True) ;
    mkN : (kane,okane : Str) -> (anim : Animacy) -> N -- Style variation (plain, respectful) and animacy given. Counter is つ and doesn't replace the noun.
      = \kane,okane,a -> lin N (styleNoun kane okane a "つ" False True) ;
    mkN : (man : Str) -> (anim : Animacy) -> (counter : Str) -> (counterReplace : Bool) -> N -- No style variation. Arguments are animacy, counter and whether counter replaces the noun.
      = \n,a,c,b -> lin N (regNoun n a c b False) ;
    mkN : (man : Str) -> (anim : Animacy) -> (counterReplace : Bool) -> N -- No style variation. Arguments are animacy and whether counter replaces the noun (here the counter and the noun coincide, e.g. 'day' 日, 'two days' 二日, not 二日の日).
      = \n,a,b -> lin N (regNoun n a n b False) ; -- Liza 16/02/2020
    mkN : (man : Str) -> (anim : Animacy) -> (counter : Str) -> N -- No style variation. Arguments are animacy and counter, which does not replace the noun.
      = \n,a,c -> lin N (regNoun n a c False False) ; -- Liza 16/02/2020
    mkN : (man : Str) -> (anim : Animacy) -> (counter : Str) -> (counterReplace : Bool) -> (men : Str) -> N -- Like previous, but unpredictable plural.
      = \n,a,c,b,pl -> lin N (numberNoun n a c b pl False) ;
    mkN : (kane,okane : Str) -> (anim : Animacy) -> (counter : Str) -> (counterReplace : Bool) -> N -- Plain form, respectful form, animacy, counter and whether counter replaces the noun.
      = \kane,okane,a,c,b -> lin N (styleNoun kane okane a c b False) ;
    mkN : (tsuma,okusan : Str) -> (anim : Animacy) -> (counter : Str) -> (counterReplace : Bool) -> (tsumatachi : Str) -> N  -- Worst case paradigm: plain form, respectful form, animacy, counter, whether counter replaces the noun, and plural form.
      = \tsuma,okusan,a,c,b,tsumatachi ->
             lin N (mkNoun tsuma okusan tsumatachi tsumatachi a c b False)
    } ;

  mkN2 : (man : Str) -> (anim : Animacy) -> (counter : Str) -> (counterReplace : Bool) -> (men : Str) -> (prep : Str) -> N2 = \n,a,c,b,pl,pr ->
          lin N2 (numberNoun n a c b pl False) ** {prep = pr ; object = \\st => []} ;

  mkN3 : (distance : Str) -> (prep1: Str) -> (prep2: Str) -> (anim : Animacy) -> N3
      = \n,p1,p2,a -> lin N3 (regNoun n a "つ" False True) ** {prep1 = p1; prep2 = p2} ;

  mkPN = overload {
    mkPN : (paris : Str) -> PN
      = \n -> lin PN (regPN n) ;
    mkPN : (jon,jonsan : Str) -> PN
      = \jon,jonsan -> lin PN (personPN jon jonsan)
    } ;

  mkGN : Str -> GN = \s -> lin GN (regPN s);
  mkLN : Str -> LN = \s -> lin LN (regPN s);
  mkSN : Str -> SN = \s -> lin SN (regPN s);

  mkPron = overload {
    mkPron : (kare : Str) -> (Pron1Sg : Bool) -> (anim : Animacy) -> Pron
      = \kare,b,a -> lin Pron (regPron kare b a) ;
    mkPron : (boku,watashi : Str) -> (Pron1Sg : Bool) -> (anim : Animacy) -> Pron
      = \boku,watashi,b,a -> lin Pron (stylePron boku watashi b a)
    } ;

  mkA = overload {
    mkA : (ookina : Str) -> A -- One form for both predicative and attribute
      = \a -> lin A (regAdj a) ;
    mkA : (kekkonshiteiru,kikonno : Str) -> A -- Verbal adjective, arguments are predicative and attributive
      = \pred,attr -> lin A (VerbalA pred attr)
    } ;

  mkA2 = overload {
    mkA2 : (yasui : Str) -> (prep : Str) -> A2 -- 2-place adjective. Arguments: adjective (same for predicative and attributive) and object marker.
      = \a,p -> lin A2 (regAdj a) ** {prep = p} ;
    mkA2 : (pred : Str) -> (attr : Str) -> (prep : Str) -> A2 -- Predicative, attributive and object marker.
      = \pred,attr,pr -> lin A2 (VerbalA pred attr) ** {prep = pr}
    } ;

  mkAdV : Str -> AdV = \s -> lin AdV {s = s} ;

  mkAdA : Str -> AdA = \s -> lin AdA {s = s} ;

  mkAdN : Str -> AdN = \s -> lin AdN {s = s; postposition = False} ;

  mkV = overload {
    mkV : (yomu : Str) -> V -- Default: group 1 verb
      = \yomu -> lin V (mkVerb (cleanVerb yomu) (guessVerbGroup (cleanVerb yomu))) ;
    mkV : (yomu : Str) -> (group : VerbGroup) -> V
      = \yomu,gr -> lin V (mkVerb (cleanVerb yomu) gr) ; -- Base form and verb group given
    } ;

  mkV2 = overload {
    mkV2 : (yomu : Str) -> V2 -- Group 1 verb, を as object marker
        = \yomu -> lin V2 (mkVerb2 (cleanVerb yomu) "を" (guessVerbGroup (cleanVerb yomu))) ;
    mkV2 : (yomu, prep : Str) -> (group : VerbGroup) -> V2 -- Base form, object marker and verb group given
        = \yomu,p,gr -> lin V2 (mkVerb2 (cleanVerb yomu) p gr) ;
    } ;

  mkV3 = overload {
    mkV3 : (yomu : Str) -> V3 -- Group 1 verb, に and を as object markers
      = \yomu -> lin V3 (mkVerb3 (cleanVerb yomu) "に" "を" (guessVerbGroup (cleanVerb yomu))) ;
    mkV3 : (uru, p1, p2 : Str) -> (group : VerbGroup) -> V3 -- Base form, object markers and verb group given
      = \uru,p1,p2,gr -> lin V3 (mkVerb3 (cleanVerb uru) p1 p2 gr) ;
    } ;

  mkVS : (yomu : Str) -> VS
    = \yomu -> lin VS (mkVerb2 (cleanVerb yomu) "ことを" (guessVerbGroup (cleanVerb yomu))) ;

  mkVV : (yomu : Str) -> VV
    = \yomu -> lin VV (mkVerbV (cleanVerb yomu) (guessVerbGroup (cleanVerb yomu))) ;

  mkV2V : (yomu : Str) -> V2V
    = \yomu -> lin V2V (mkVerb (cleanVerb yomu) (guessVerbGroup (cleanVerb yomu))) ;

  mkV2S : (yomu : Str) -> V2S
    = \yomu -> lin V2S (mkVerb (cleanVerb yomu) (guessVerbGroup (cleanVerb yomu))) ;

  mkVQ : (yomu : Str) -> VQ
    = \yomu -> lin VQ (mkVerb2 (cleanVerb yomu) "を" (guessVerbGroup (cleanVerb yomu))) ;

  mkVA : (yomu : Str) -> VA
     = \yomu -> lin VA (mkVerb (cleanVerb yomu) (guessVerbGroup (cleanVerb yomu))) ;

  mkV2A : (yomu : Str) -> V2A
    = \yomu -> lin V2A (mkVerb (cleanVerb yomu) (guessVerbGroup (cleanVerb yomu))) ;

  mkAdv : Str -> Adv
    = \s -> lin Adv (ResJpn.mkAdv (cleanLemma s)) ;

  mkPrep : Str -> Prep
    = \s -> lin Prep (ResJpn.mkPrep (cleanLemma s)) ;

  mkDet : Str -> Det = \d -> lin Det (ResJpn.mkDet d d ResJpn.Sg) ;

  mkConj : Str -> Conj = \c -> lin Conj (ResJpn.mkConj c ResJpn.And) ;

  guessVerbGroup : Str -> VerbGroup = \v -> case v of {
    stem + "する" => Suru ;
    stem + "来る" => Kuru ;
    stem + "える" => Gr2 ; stem + "ける" => Gr2 ; stem + "げる" => Gr2 ;
    stem + "せる" => Gr2 ; stem + "ぜる" => Gr2 ; stem + "てる" => Gr2 ;
    stem + "でる" => Gr2 ; stem + "ねる" => Gr2 ; stem + "べる" => Gr2 ;
    stem + "める" => Gr2 ; stem + "れる" => Gr2 ; stem + "いる" => Gr2 ;
    stem + "きる" => Gr2 ; stem + "ぎる" => Gr2 ; stem + "じる" => Gr2 ;
    stem + "ちる" => Gr2 ; stem + "にる" => Gr2 ; stem + "びる" => Gr2 ;
    stem + "みる" => Gr2 ; stem + "りる" => Gr2 ;
    stem + ("見る"|"得る"|"着る"|"出る"|"寝る"|"居る"|"似る"|"経る") => Gr2 ;
    stem + "ずる" => Gr2 ;
    _ => Gr1
    } ;

  cleanVerb : Str -> Str = \v -> case v of {
    stem + "+する" => stem + "する" ;
    stem + "ずる" => stem + "じる" ;
    _ => cleanLemma v
    } ;

  cleanLemma : Str -> Str = \s -> case s of {
    "..." + rest => rest ;
    "…" + rest => rest ;
    "〜" + rest => rest ;
    "～" + rest => rest ;
    _ => s
    } ;

  mkInterj : Str -> Interj
    = \s -> lin Interj (ss s) ;

  mkgoVV : VV = lin VV {s = \\sp => mkGo.s ; te = \\sp => mkGo.te ;
             a_stem = \\sp => mkGo.a_stem ;
             i_stem = \\sp => mkGo.i_stem ;
             ba = \\sp => mkGo.ba ;
             te_neg = \\sp => "行かないで" ;
             ba_neg = \\sp => "行かなければ" ;
             sense = Abil} ;

--.
  -- Hidden definitions

  Animacy : Type = ResJpn.Animateness ;
  animate : Animacy = ResJpn.Anim ;
  inanimate : Animacy = ResJpn.Inanim ;

  VerbGroup : Type = ResJpn.VerbGroup ;
  gr1 : VerbGroup = Gr1 ;
  gr2 : VerbGroup = Gr2 ;
  suru : VerbGroup = Suru ;
  kuru : VerbGroup = Kuru ;
}
