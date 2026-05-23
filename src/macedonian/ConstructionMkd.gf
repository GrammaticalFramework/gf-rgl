concrete ConstructionMkd of Construction = CatMkd **
  open ParadigmsMkd, ResMkd, GrammarMkd, Prelude in {

lincat
  Timeunit = N ;
  Hour = {s : Str} ;
  Weekday = N ;
  Monthday = NP ;
  Month = N ;
  Year = NP ;
  Language = N ;

lin
  hungry_VP = adjVP (mkA "гладен" "гладна") ;
  thirsty_VP = adjVP (mkA "жеден" "жедна") ;
  tired_VP = adjVP (mkA "уморен" "уморна") ;
  scared_VP = adjVP (mkA "исплашен" "исплашена") ;
  ill_VP = adjVP (mkA "болен" "болна") ;
  ready_VP = adjVP (mkA "подготвен" "подготвена") ;

  has_age_VP card = (UseV (mkV "има")) ** {
    compl = \\_ => card.s ++ "години"
  } ;

  have_name_Cl p name = PredVP p ((UseV (medialV (mkV "вика") accusative)) ** {
    compl = \\_ => name.s ! RSubj
  }) ;

  married_Cl p1 p2 =
    PredVP p1 (AdvVP (adjVP (mkA "венчан" "венчана")) (PrepNP (mkPrep "со") p2)) ;

  what_name_QCl p = {
    s = \\_,_,_ => "како" ++ "се" ++ "вика" ++ p.s ! RSubj
  } ;
  how_old_QCl p = {
    s = \\_,_,_ => "колку години има" ++ p.s ! RSubj
  } ;
  how_far_QCl p = {
    s = \\_,_,_ => "колку далеку е" ++ p.s ! RSubj
  } ;

  weather_adjCl ap = {
    s = \\_,_,_,_ => "е" ++ ap.s ! Indef ! GSg Neuter
  } ;

  is_right_VP = adjVP (mkA "прав" "права") ;
  is_wrong_VP = adjVP (mkA "погрешен" "погрешна") ;

  n_units_AP card cn a = {
    s = \\sp,gn => a.s ! sp ! gn ++ card.s ++ cn.count_form ;
    isPre = False
  } ;

  n_units_of_NP card cn np = {
    s = \\_ => card.s ++ cn.count_form ++ "од" ++ np.s ! RSubj ;
    vocative = card.s ++ cn.count_form ++ "од" ++ np.vocative ;
    a = {g = GPl; p = P3}
  } ;

  n_unit_CN card unit cn = {
    s = \\sp,n => card.s ++ unit.count_form ++ cn.s ! sp ! n ;
    count_form = card.s ++ unit.count_form ++ cn.count_form ;
    vocative = \\n => card.s ++ unit.count_form ++ cn.vocative ! n ;
    g = cn.g
  } ;

  bottle_of_CN np = containerCN (mkN "шише" "шишиња") np ;
  cup_of_CN np = containerCN (mkN "чаша" "чаши") np ;
  glass_of_CN np = containerCN (mkN "чаша" "чаши") np ;

  few_X_short_of_Y np x y = {
    s = np.s ! RSubj ++ "нема доволно" ++ x.count_form ++ "за" ++ y.s ! Indef ! Sg
  } ;

  timeunitAdv card unit = {
    s = "за" ++ card.s ++ unit.count_form
  } ;
  timeunitRange from to unit = {
    s = "од" ++ from.s ++ "до" ++ to.s ++ unit.count_form
  } ;

  oneHour = hour "1" ;
  twoHour = hour "2" ;
  threeHour = hour "3" ;
  fourHour = hour "4" ;
  fiveHour = hour "5" ;
  sixHour = hour "6" ;
  sevenHour = hour "7" ;
  eightHour = hour "8" ;
  nineHour = hour "9" ;
  tenHour = hour "10" ;
  elevenHour = hour "11" ;
  twelveHour = hour "12" ;
  thirteenHour = hour "13" ;
  fourteenHour = hour "14" ;
  fifteenHour = hour "15" ;
  sixteenHour = hour "16" ;
  seventeenHour = hour "17" ;
  eighteenHour = hour "18" ;
  nineteenHour = hour "19" ;
  twentyHour = hour "20" ;
  twentyOneHour = hour "21" ;
  twentyTwoHour = hour "22" ;
  twentyThreeHour = hour "23" ;
  twentyFourHour = hour "24" ;

  timeHour h = {s = "во" ++ h.s ++ "часот"} ;
  timeHourMinute h m = {s = "во" ++ h.s ++ ":" ++ m.s} ;

  weekdayPunctualAdv w = {s = "во" ++ w.s ! Indef ! Sg} ;
  weekdayHabitualAdv w = {s = "во" ++ w.s ! Indef ! Pl} ;
  weekdayLastAdv w = {s = "минатиот" ++ w.s ! Indef ! Sg} ;
  weekdayNextAdv w = {s = "следниот" ++ w.s ! Indef ! Sg} ;

  monthAdv m = {s = "во" ++ m.s ! Indef ! Sg} ;
  yearAdv y = {s = "во" ++ y.s ! RSubj} ;
  dayMonthAdv d m = {s = "на" ++ d.s ! RSubj ++ m.s ! Indef ! Sg} ;
  monthYearAdv m y = {s = "во" ++ m.s ! Indef ! Sg ++ y.s ! RSubj} ;
  dayMonthYearAdv d m y = {s = "на" ++ d.s ! RSubj ++ m.s ! Indef ! Sg ++ y.s ! RSubj} ;

  intYear n = symbNP n.s ;
  intMonthday n = symbNP n.s ;

  InLanguage l = {s = "на" ++ l.s ! Indef ! Sg} ;
  languageCN l = UseN l ;
  languageNP l = MassNP (UseN l) ;

  weekdayN w = w ;
  monthN m = m ;

  weekdayPN w = mkPN (w.s ! Indef ! Sg) ;
  monthPN m = mkPN (m.s ! Indef ! Sg) ;

  second_Timeunit = mkN "секунда" "секунди" ;
  minute_Timeunit = mkN "минута" "минути" ;
  hour_Timeunit = mkN "час" "часови" ;
  day_Timeunit = mkN "ден" "денови" ;
  week_Timeunit = mkN "недела" "недели" ;
  month_Timeunit = mkN "месец" "месеци" ;
  year_Timeunit = mkN "година" "години" ;

  monday_Weekday = mkN "понеделник" "понеделници" ;
  tuesday_Weekday = mkN "вторник" "вторници" ;
  wednesday_Weekday = mkN "среда" "среди" ;
  thursday_Weekday = mkN "четврток" "четвртоци" ;
  friday_Weekday = mkN "петок" "петоци" ;
  saturday_Weekday = mkN "сабота" "саботи" ;
  sunday_Weekday = mkN "недела" "недели" ;

  january_Month = mkMonth "јануари" ;
  february_Month = mkMonth "февруари" ;
  march_Month = mkMonth "март" ;
  april_Month = mkMonth "април" ;
  may_Month = mkMonth "мај" ;
  june_Month = mkMonth "јуни" ;
  july_Month = mkMonth "јули" ;
  august_Month = mkMonth "август" ;
  september_Month = mkMonth "септември" ;
  october_Month = mkMonth "октомври" ;
  november_Month = mkMonth "ноември" ;
  december_Month = mkMonth "декември" ;

  afrikaans_Language = mkLanguage "африканс" ;
  amharic_Language = mkLanguage "амхарски" ;
  arabic_Language = mkLanguage "арапски" ;
  bulgarian_Language = mkLanguage "бугарски" ;
  catalan_Language = mkLanguage "каталонски" ;
  chinese_Language = mkLanguage "кинески" ;
  danish_Language = mkLanguage "дански" ;
  dutch_Language = mkLanguage "холандски" ;
  english_Language = mkLanguage "англиски" ;
  estonian_Language = mkLanguage "естонски" ;
  finnish_Language = mkLanguage "фински" ;
  french_Language = mkLanguage "француски" ;
  german_Language = mkLanguage "германски" ;
  greek_Language = mkLanguage "грчки" ;
  hebrew_Language = mkLanguage "хебрејски" ;
  hindi_Language = mkLanguage "хинди" ;
  japanese_Language = mkLanguage "јапонски" ;
  italian_Language = mkLanguage "италијански" ;
  latin_Language = mkLanguage "латински" ;
  latvian_Language = mkLanguage "латвиски" ;
  maltese_Language = mkLanguage "малтешки" ;
  nepali_Language = mkLanguage "непалски" ;
  norwegian_Language = mkLanguage "норвешки" ;
  persian_Language = mkLanguage "персиски" ;
  polish_Language = mkLanguage "полски" ;
  punjabi_Language = mkLanguage "пенџапски" ;
  romanian_Language = mkLanguage "романски" ;
  russian_Language = mkLanguage "руски" ;
  sindhi_Language = mkLanguage "синди" ;
  spanish_Language = mkLanguage "шпански" ;
  swahili_Language = mkLanguage "свахили" ;
  swedish_Language = mkLanguage "шведски" ;
  thai_Language = mkLanguage "тајландски" ;
  turkish_Language = mkLanguage "турски" ;
  urdu_Language = mkLanguage "урду" ;

oper
  adjVP : A -> VP = \a -> UseComp (CompAP (PositA a)) ;

  containerCN : N -> NP -> CN = \n,np -> lin CN {
    s = \\sp,num => n.s ! sp ! num ++ "од" ++ np.s ! RSubj ;
    count_form = n.count_form ++ "од" ++ np.s ! RSubj ;
    vocative = \\num => n.vocative ! num ++ "од" ++ np.vocative ;
    g = n.g
  } ;

  hour : Str -> Hour = \s -> lin Hour {s = s} ;

  symbNP : Str -> NP = \s -> lin NP {
    s = \\_ => s ;
    vocative = s ;
    a = {g = GSg Masc; p = P3}
  } ;

  mkMonth : Str -> N = \s -> mkN s s ;
  mkLanguage : Str -> N = \s -> mkN s s ;
}
