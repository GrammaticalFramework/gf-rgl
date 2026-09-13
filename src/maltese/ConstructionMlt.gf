concrete ConstructionMlt of Construction = CatMlt ** open
  Prelude,
  ResMlt,
  GrammarMlt,
  ParadigmsMlt,
  (S = StructuralMlt) in {

  lincat
    Timeunit = N ;
    Hour = {s : Str} ;
    Weekday = N ;
    Monthday = NP ;
    Month = N ;
    Year = NP ;
    Language = N ;

  oper
    mkHour : Str -> Hour = \s -> lin Hour {s = s} ;
    mkYearNP : Str -> NounPhrase = \s -> {
      s = \\_ => s ;
      a = agrP3 Sg Masc ;
      isPron = False ;
      isDefn = False
      } ;
    amountStr : Card -> CN -> Str = \card,unit ->
      card.s ! NumAdj ++ unit.s ! numform2nounnum card.n ;
    containerCN : N -> NP -> Noun = \container,np -> {
      s = \\num => container.s ! num ++ prepNP prep_ta np ;
      g = container.g ;
      hasColl = container.hasColl ;
      hasDual = container.hasDual ;
      takesPron = container.takesPron
      } ;

  lin
    hungry_VP = UseComp (CompAP (PositA (mkA "bil-ġuħ"))) ;
    thirsty_VP = UseComp (CompAP (PositA (mkA "bil-għatx"))) ;
    tired_VP = UseComp (CompAP (PositA (mkA "għajjien"))) ;
    scared_VP = UseComp (CompAP (PositA (mkA "imbeżża'"))) ;
    ill_VP = UseComp (CompAP (PositA (mkA "marid"))) ;
    ready_VP = UseComp (CompAP (PositA (mkA "lest"))) ;

    has_age_VP card =
      ComplSlash (SlashV2a S.have_V2) {
        s = \\_ => card.s ! NumNom ++ "sena" ;
        a = agrP3 Pl Fem ;
        isPron = False ;
        isDefn = False
        } ;

    weather_adjCl ap = PredVP (UsePron S.it_Pron) (UseComp (CompAP ap)) ;
    is_right_VP = UseComp (CompAP (PositA (mkA "għandu raġun"))) ;
    is_wrong_VP = UseComp (CompAP (PositA (mkA "żbaljat"))) ;

    have_name_Cl x y =
      mkClause ("l-isem" ++ prepNP prep_ta x) (agrP3 Sg Masc)
        (insertObj (\\_ => y.s ! NPAcc) CopulaVP) ;
    married_Cl x y =
      PredVP x (UseComp (CompAP (ComplA2 (mkA2 (mkA "miżżewweġ") S.with_Prep) y))) ;
    what_name_QCl x = {
      s = \\_,_,_,_ => "x'inhu l-isem" ++ prepNP prep_ta x
      } ;
    how_old_QCl x = {
      s = \\_,_,_,_ => "kemm għandu żmien" ++ x.s ! NPNom
      } ;
    how_far_QCl x = {
      s = \\_,_,_,_ => "kemm qiegħed 'il bogħod" ++ x.s ! NPNom
      } ;

    n_units_AP card unit a = {
      s = \\gn => amountStr card unit ++ a.s ! APosit gn ;
      isPre = False
      } ;
    n_units_of_NP card unit np = {
      s = \\_ => amountStr card unit ++ prepNP prep_ta np ;
      a = agrP3 (numform2num card.n) unit.g ;
      isPron = False ;
      isDefn = False
      } ;
    n_unit_CN card unit cn = {
      s = \\num => amountStr card unit ++ cn.s ! num ;
      g = cn.g ;
      hasColl = cn.hasColl ;
      hasDual = cn.hasDual ;
      takesPron = cn.takesPron
      } ;

    bottle_of_CN np = containerCN (mkN "flixkun") np ;
    cup_of_CN np = containerCN (mkN "kikkra") np ;
    glass_of_CN np = containerCN (mkN "tazza") np ;
    few_X_short_of_Y np x y = {
      s = np.s ! NPNom ++ "jonqsu" ++ x.s ! Plural ++ "biex ikun" ++ y.s ! Singulative
      } ;

    timeunitAdv n unit = advSS (n.s ! NumNom ++ unit.s ! Singulative) ;
    timeunitRange l u unit = advSS (l.s ! NumNom ++ "sa" ++ u.s ! NumNom ++ unit.s ! Plural) ;

    oneHour = mkHour "1" ;
    twoHour = mkHour "2" ;
    threeHour = mkHour "3" ;
    fourHour = mkHour "4" ;
    fiveHour = mkHour "5" ;
    sixHour = mkHour "6" ;
    sevenHour = mkHour "7" ;
    eightHour = mkHour "8" ;
    nineHour = mkHour "9" ;
    tenHour = mkHour "10" ;
    elevenHour = mkHour "11" ;
    twelveHour = mkHour "12" ;
    thirteenHour = mkHour "13" ;
    fourteenHour = mkHour "14" ;
    fifteenHour = mkHour "15" ;
    sixteenHour = mkHour "16" ;
    seventeenHour = mkHour "17" ;
    eighteenHour = mkHour "18" ;
    nineteenHour = mkHour "19" ;
    twentyHour = mkHour "20" ;
    twentyOneHour = mkHour "21" ;
    twentyTwoHour = mkHour "22" ;
    twentyThreeHour = mkHour "23" ;
    twentyFourHour = mkHour "24" ;

    timeHour h = advSS ("fis-" ++ h.s) ;
    timeHourMinute h m = advSS ("fis-" ++ h.s ++ m.s ! NumNom) ;

    weekdayPunctualAdv w = advSS ("nhar" ++ w.s ! Singulative) ;
    weekdayHabitualAdv w = advSS ("nhar ta'" ++ w.s ! Plural) ;
    weekdayLastAdv w = advSS (w.s ! Singulative ++ "li għadda") ;
    weekdayNextAdv w = advSS (w.s ! Singulative ++ "li ġej") ;

    monthAdv m = advSS (makePreVowel "fi" "f'" ++ m.s ! Singulative) ;
    yearAdv y = PrepNP S.in_Prep y ;
    dayMonthAdv d m = advSS (d.s ! NPNom ++ m.s ! Singulative) ;
    monthYearAdv m y = advSS (m.s ! Singulative ++ y.s ! NPNom) ;
    dayMonthYearAdv d m y = advSS (d.s ! NPNom ++ m.s ! Singulative ++ y.s ! NPNom) ;

    intYear i = lin NP (mkYearNP i.s) ;
    intMonthday i = lin NP (mkYearNP i.s) ;

    InLanguage l = PrepNP S.in_Prep (MassNP (UseN l)) ;

    weekdayN w = w ;
    monthN m = m ;
    weekdayPN w = mkPN (w.s ! Singulative) ;
    monthPN m = mkPN (m.s ! Singulative) ;
    languageNP l = MassNP (UseN l) ;
    languageCN l = UseN l ;

    second_Timeunit = mkN "sekonda" ;
    minute_Timeunit = mkN "minuta" ;
    hour_Timeunit = mkN "siegħa" ;
    day_Timeunit = mkN "jum" ;
    week_Timeunit = mkN "ġimgħa" ;
    month_Timeunit = mkN "xahar" ;
    year_Timeunit = mkN "sena" ;

    monday_Weekday = mkN "Tnejn" ;
    tuesday_Weekday = mkN "Tlieta" ;
    wednesday_Weekday = mkN "Erbgħa" ;
    thursday_Weekday = mkN "Ħamis" ;
    friday_Weekday = mkN "Ġimgħa" ;
    saturday_Weekday = mkN "Sibt" ;
    sunday_Weekday = mkN "Ħadd" ;

    january_Month = mkN "Jannar" ;
    february_Month = mkN "Frar" ;
    march_Month = mkN "Marzu" ;
    april_Month = mkN "April" ;
    may_Month = mkN "Mejju" ;
    june_Month = mkN "Ġunju" ;
    july_Month = mkN "Lulju" ;
    august_Month = mkN "Awwissu" ;
    september_Month = mkN "Settembru" ;
    october_Month = mkN "Ottubru" ;
    november_Month = mkN "Novembru" ;
    december_Month = mkN "Diċembru" ;

    afrikaans_Language = mkN "Afrikans" ;
    amharic_Language = mkN "Amħariku" ;
    arabic_Language = mkN "Għarbi" ;
    bulgarian_Language = mkN "Bulgaru" ;
    catalan_Language = mkN "Katalan" ;
    chinese_Language = mkN "Ċiniż" ;
    danish_Language = mkN "Daniż" ;
    dutch_Language = mkN "Olandiż" ;
    english_Language = mkN "Ingliż" ;
    estonian_Language = mkN "Estonjan" ;
    finnish_Language = mkN "Finlandiż" ;
    french_Language = mkN "Franċiż" ;
    german_Language = mkN "Ġermaniż" ;
    greek_Language = mkN "Grieg" ;
    hebrew_Language = mkN "Ebrajk" ;
    hindi_Language = mkN "Ħindi" ;
    japanese_Language = mkN "Ġappuniż" ;
    italian_Language = mkN "Taljan" ;
    latin_Language = mkN "Latin" ;
    latvian_Language = mkN "Latvjan" ;
    maltese_Language = mkN "Malti" ;
    nepali_Language = mkN "Nepali" ;
    norwegian_Language = mkN "Norveġiż" ;
    persian_Language = mkN "Persjan" ;
    polish_Language = mkN "Pollakk" ;
    punjabi_Language = mkN "Punġabi" ;
    romanian_Language = mkN "Rumen" ;
    russian_Language = mkN "Russu" ;
    sindhi_Language = mkN "Sindi" ;
    spanish_Language = mkN "Spanjol" ;
    swahili_Language = mkN "Swaħili" ;
    swedish_Language = mkN "Svediż" ;
    thai_Language = mkN "Tajlandiż" ;
    turkish_Language = mkN "Tork" ;
    urdu_Language = mkN "Urdu" ;

}
