concrete ConstructionHun of Construction = CatHun ** open
  ParadigmsHun, ResHun, NounHun, VerbHun, AdjectiveHun, Prelude in {

lincat
  Timeunit = N ;
  Hour = {s : Str} ;
  Weekday = N ;
  Monthday = NP ;
  Month = N ;
  Year = NP ;
  Language = N ;

lin
  hungry_VP = UseComp (CompAP (PositA (mkA "éhes"))) ;
  thirsty_VP = UseComp (CompAP (PositA (mkA "szomjas"))) ;
  tired_VP = UseComp (CompAP (PositA (mkA "fáradt"))) ;
  scared_VP = UseComp (CompAP (PositA (mkA "ijedt"))) ;
  ill_VP = UseComp (CompAP (PositA (mkA "beteg"))) ;
  ready_VP = UseComp (CompAP (PositA (mkA "kész"))) ;

  has_age_VP card = useV (copula ** {
    s = \\vf => case vf of {
      VPres P3 _ => [] ;
      _          => copula.s ! vf
      }
    }) ** {
      adv = card.s ! Indep ++ "éves"
    } ;

  n_units_AP card cn a =
    let ap : AP = PositA a in ap ** {
      s = \\n,c => card.s ! Attrib ++ cn.s ! SgNom ++ ap.s ! n ! c
      } ;

  n_units_of_NP card cn np = lin NP (indeclNP
    (card.s ! Attrib ++ cn.s ! SgNom ++ np.s ! NoPoss ! Nom ++ np.postmod)) ;

  n_unit_CN card unit cn = cn ** {
    s = \\ncs => card.s ! Attrib ++ unit.s ! SgNom ++ cn.s ! ncs
    } ;

  bottle_of_CN np = (UseN (mkN "palack")) ** {
    compl = \\_,_ => np.s ! NoPoss ! Nom ++ np.postmod
    } ;
  cup_of_CN np = (UseN (mkN "csésze")) ** {
    compl = \\_,_ => np.s ! NoPoss ! Nom ++ np.postmod
    } ;
  glass_of_CN np = (UseN (mkN "pohár")) ** {
    compl = \\_,_ => np.s ! NoPoss ! Nom ++ np.postmod
    } ;

  timeunitAdv n time = mkAdv (n.s ! Attrib ++ time.s ! SgNom) ;
  timeunitRange l u time =
    mkAdv (l.s ! Attrib ++ BIND ++ "-" ++ BIND ++ u.s ! Attrib ++ time.s ! SgNom) ;

  oneHour = {s = "1"} ;
  twoHour = {s = "2"} ;
  threeHour = {s = "3"} ;
  fourHour = {s = "4"} ;
  fiveHour = {s = "5"} ;
  sixHour = {s = "6"} ;
  sevenHour = {s = "7"} ;
  eightHour = {s = "8"} ;
  nineHour = {s = "9"} ;
  tenHour = {s = "10"} ;
  elevenHour = {s = "11"} ;
  twelveHour = {s = "12"} ;
  thirteenHour = {s = "13"} ;
  fourteenHour = {s = "14"} ;
  fifteenHour = {s = "15"} ;
  sixteenHour = {s = "16"} ;
  seventeenHour = {s = "17"} ;
  eighteenHour = {s = "18"} ;
  nineteenHour = {s = "19"} ;
  twentyHour = {s = "20"} ;
  twentyOneHour = {s = "21"} ;
  twentyTwoHour = {s = "22"} ;
  twentyThreeHour = {s = "23"} ;
  twentyFourHour = {s = "24"} ;

  timeHour h = mkAdv (h.s ++ "órakor") ;
  timeHourMinute h m = mkAdv (h.s ++ BIND ++ ":" ++ BIND ++ m.s ! Attrib) ;

  weekdayPunctualAdv w = mkAdv (caseFromStem glue w Sup Sg) ;
  weekdayHabitualAdv w = mkAdv (caseFromStem glue w Sup Sg) ;
  weekdayNextAdv w = mkAdv ("jövő" ++ caseFromStem glue w Sup Sg) ;
  weekdayLastAdv w = mkAdv ("múlt" ++ caseFromStem glue w Sup Sg) ;

  monthAdv m = mkAdv (caseFromStem glue m Ine Sg) ;
  yearAdv y = mkAdv (y.s ! NoPoss ! Nom ++ BIND ++ "-ban") ;
  dayMonthAdv d m =
    mkAdv (d.s ! NoPoss ! Nom ++ BIND ++ "." ++ m.s ! SgNom) ;
  monthYearAdv m y =
    mkAdv (m.s ! SgNom ++ y.s ! NoPoss ! Nom ++ BIND ++ "-ban") ;
  dayMonthYearAdv d m y =
    mkAdv (d.s ! NoPoss ! Nom ++ BIND ++ "." ++ m.s ! SgNom ++ y.s ! NoPoss ! Nom ++ BIND ++ "-ban") ;

  intYear i = lin NP (indeclNP i.s) ;
  intMonthday i = lin NP (indeclNP i.s) ;

  InLanguage l = mkAdv (l.s ! SgNom ++ "nyelven") ;

  weekdayN w = w ;
  monthN m = m ;

  weekdayPN w = lin PN (MassNP (UseN w)) ;
  monthPN m = lin PN (MassNP (UseN m)) ;

  languageCN l = UseN l ;
  languageNP l = lin NP (MassNP (UseN l)) ;

  second_Timeunit = mkN "másodperc" ;
  minute_Timeunit = mkN "perc" ;
  hour_Timeunit = mkN "óra" ;
  day_Timeunit = mkN "nap" ;
  week_Timeunit = mkN "hét" ;
  month_Timeunit = mkN "hónap" ;
  year_Timeunit = mkN "év" ;

  monday_Weekday = mkN "hétfő" ;
  tuesday_Weekday = mkN "kedd" ;
  wednesday_Weekday = mkN "szerda" ;
  thursday_Weekday = mkN "csütörtök" ;
  friday_Weekday = mkN "péntek" ;
  saturday_Weekday = mkN "szombat" ;
  sunday_Weekday = mkN "vasárnap" ;

  january_Month = mkN "január" ;
  february_Month = mkN "február" ;
  march_Month = mkN "március" ;
  april_Month = mkN "április" ;
  may_Month = mkN "május" ;
  june_Month = mkN "június" ;
  july_Month = mkN "július" ;
  august_Month = mkN "augusztus" ;
  september_Month = mkN "szeptember" ;
  october_Month = mkN "október" ;
  november_Month = mkN "november" ;
  december_Month = mkN "december" ;

  afrikaans_Language = mkN "afrikaans" ;
  amharic_Language = mkN "amhara" ;
  arabic_Language = mkN "arab" ;
  bulgarian_Language = mkN "bolgár" ;
  catalan_Language = mkN "katalán" ;
  chinese_Language = mkN "kínai" ;
  danish_Language = mkN "dán" ;
  dutch_Language = mkN "holland" ;
  english_Language = mkN "angol" ;
  estonian_Language = mkN "észt" ;
  finnish_Language = mkN "finn" ;
  french_Language = mkN "francia" ;
  german_Language = mkN "német" ;
  greek_Language = mkN "görög" ;
  hebrew_Language = mkN "héber" ;
  hindi_Language = mkN "hindi" ;
  japanese_Language = mkN "japán" ;
  italian_Language = mkN "olasz" ;
  latin_Language = mkN "latin" ;
  latvian_Language = mkN "lett" ;
  maltese_Language = mkN "máltai" ;
  nepali_Language = mkN "nepáli" ;
  norwegian_Language = mkN "norvég" ;
  persian_Language = mkN "perzsa" ;
  polish_Language = mkN "lengyel" ;
  punjabi_Language = mkN "pandzsábi" ;
  romanian_Language = mkN "román" ;
  russian_Language = mkN "orosz" ;
  sindhi_Language = mkN "szindhi" ;
  spanish_Language = mkN "spanyol" ;
  swahili_Language = mkN "szuahéli" ;
  swedish_Language = mkN "svéd" ;
  thai_Language = mkN "thai" ;
  turkish_Language = mkN "török" ;
  urdu_Language = mkN "urdu" ;
{-
lin

  timeunitAdv n time =
  weekdayPunctualAdv w = ;  -- on Sunday
  weekdayHabitualAdv w = ;  -- on Sundays
  weekdayNextAdv w =        -- next Sunday
  weekdayLastAdv w =        -- last Sunday

  monthAdv m = mkAdv in_Prep (mkNP m) ;
  yearAdv y = mkAdv in_Prep y ;
  dayMonthAdv d m  =  ; -- on 17 May
  monthYearAdv m y =  ; -- in May 2012
  dayMonthYearAdv d m y =  ; -- on 17 May 2013

  intYear = symb ;
  intMonthday = symb ;

lincat Language = N ;

lin InLanguage l = mkAdv ???_Prep (mkNP l) ;

lin
  weekdayN w = w ;
  monthN m = m ;

  weekdayPN w = mkPN w ;
  monthPN m = mkPN m ;

  languageCN l = mkCN l ;
  languageNP l = mkNP l ;


oper mkLanguage : Str -> N = \s -> mkN s ;

----------------------------------------------
---- lexicon of special names

lin second_Timeunit = mkN "second" ;
lin minute_Timeunit = mkN "minute" ;
lin hour_Timeunit = mkN "hour" ;
lin day_Timeunit = mkN "day" ;
lin week_Timeunit = mkN "week" ;
lin month_Timeunit = mkN "month" ;
lin year_Timeunit = mkN "year" ;

lin monday_Weekday = mkN "Monday" ;
lin tuesday_Weekday = mkN "Tuesday" ;
lin wednesday_Weekday = mkN "Wednesday" ;
lin thursday_Weekday = mkN "Thursday" ;
lin friday_Weekday = mkN "Friday" ;
lin saturday_Weekday = mkN "Saturday" ;
lin sunday_Weekday = mkN "Sunday" ;

lin january_Month = mkN "January" ;
lin february_Month = mkN "February" ;
lin march_Month = mkN "March" ;
lin april_Month = mkN "April" ;
lin may_Month = mkN "May" ;
lin june_Month = mkN "June" ;
lin july_Month = mkN "July" ;
lin august_Month = mkN "August" ;
lin september_Month = mkN "September" ;
lin october_Month = mkN "October" ;
lin november_Month = mkN "November" ;
lin december_Month = mkN "December" ;

lin afrikaans_Language = mkLanguage "Afrikaans" ;
lin amharic_Language = mkLanguage "Amharic" ;
lin arabic_Language = mkLanguage "Arabic" ;
lin bulgarian_Language = mkLanguage "Bulgarian" ;
lin catalan_Language = mkLanguage "Catalan" ;
lin chinese_Language = mkLanguage "Chinese" ;
lin danish_Language = mkLanguage "Danish" ;
lin dutch_Language = mkLanguage "Dutch" ;
lin english_Language = mkLanguage "Euslish" ;
lin estonian_Language = mkLanguage "Estonian" ;
lin finnish_Language = mkLanguage "Finnish" ;
lin french_Language = mkLanguage "French" ;
lin german_Language = mkLanguage "German" ;
lin greek_Language = mkLanguage "Greek" ;
lin hebrew_Language = mkLanguage "Hebrew" ;
lin hindi_Language = mkLanguage "Hindi" ;
lin japanese_Language = mkLanguage "Japanese" ;
lin italian_Language = mkLanguage "Italian" ;
lin latin_Language = mkLanguage "Latin" ;
lin latvian_Language = mkLanguage "Latvian" ;
lin maltese_Language = mkLanguage "Maltese" ;
lin nepali_Language = mkLanguage "Nepali" ;
lin norwegian_Language = mkLanguage "Norwegian" ;
lin persian_Language = mkLanguage "Persian" ;
lin polish_Language = mkLanguage "Polish" ;
lin punjabi_Language = mkLanguage "Punjabi" ;
lin romanian_Language = mkLanguage "Romanian" ;
lin russian_Language = mkLanguage "Russian" ;
lin sindhi_Language = mkLanguage "Sindhi" ;
lin spanish_Language = mkLanguage "Spanish" ;
lin swahili_Language = mkLanguage "Swahili" ;
lin swedish_Language = mkLanguage "Swedish" ;
lin thai_Language = mkLanguage "Thai" ;
lin turkish_Language = mkLanguage "Turkish" ;
lin urdu_Language = mkLanguage "Urdu" ;
-}

} ;
