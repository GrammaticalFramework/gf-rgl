concrete ConstructionBel of Construction = CatBel ** open ResBel, ParadigmsBel, (R = ParamX) in {

lincat
  Timeunit, Hour, Weekday, Month, Monthday, Year, Language = {s : Str} ;

oper
  adjVP : Adj -> VPhrase = \a -> {
    s = \\t,p,agr => copula t p agr ++ a.s ! Nom ! genNum agr.g agr.n ;
    inf = "быць" ++ a.s ! Nom ! GSg Masc ;
    imp = \\p,_ => neg p ++ "будзь" ++ a.s ! Nom ! GSg Masc
  } ;

lin
  hungry_VP = adjVP (mkA "галодны") ;
  thirsty_VP = adjVP (mkA "спрагнены") ;
  tired_VP = adjVP (mkA "стомлены") ;
  scared_VP = adjVP (mkA "спалоханы") ;
  ill_VP = adjVP (mkA "хворы") ;
  ready_VP = adjVP (mkA "гатовы") ;
  has_age_VP card = {
    s = \\t,p,a => copula t p a ++ card.s ++ "гадоў" ;
    inf = card.s ++ "гадоў" ;
    imp = \\p,_ => neg p ++ card.s ++ "гадоў"
  } ;

  have_name_Cl np name = {s = \\t,p => np.s ! Gen ++ "імя" ++ name.s ! Nom} ;
  married_Cl np spouse = {s = \\t,p => np.s ! Nom ++ copula t p np.a ++ "жанаты" ++ "з" ++ spouse.s ! Instr} ;
  what_name_QCl np = {s = \\t,p => "як" ++ "завуць" ++ np.s ! Acc} ;
  how_old_QCl np = {s = \\t,p => "колькі гадоў" ++ np.s ! Dat} ;
  how_far_QCl np = {s = \\t,p => "як далёка" ++ np.s ! Nom} ;

  weather_adjCl ap = {s = \\t,p => copula t p defaultAgr ++ ap.s ! Nom ! GSg Neuter} ;
  is_right_VP = adjVP (mkA "правы") ;
  is_wrong_VP = adjVP (mkA "няправы") ;

  n_units_AP card cn a = {s = \\c,gn => card.s ++ cn.s ! Gen ! Pl ++ a.s ! c ! gn} ;
  n_units_of_NP card cn np = mkSimpleNP (card.s ++ cn.s ! Gen ! Pl ++ np.s ! Gen) Neuter Sg P3 ;
  n_unit_CN card unit cn = nounFromStr (card.s ++ unit.s ! Nom ! Pl ++ cn.s ! Nom ! Sg) cn.g ;

  bottle_of_CN np = nounFromStr ("бутэлька" ++ np.s ! Gen) Fem ;
  cup_of_CN np = nounFromStr ("кубак" ++ np.s ! Gen) Masc ;
  glass_of_CN np = nounFromStr ("шклянка" ++ np.s ! Gen) Fem ;
  few_X_short_of_Y np x y = {s = np.s ! Nom ++ "бракуе" ++ x.s ! Gen ! Pl ++ "да" ++ y.s ! Gen ! Sg} ;

  timeunitAdv card unit = {s = card.s ++ unit.s} ;
  timeunitRange c1 c2 unit = {s = c1.s ++ "да" ++ c2.s ++ unit.s} ;
  oneHour = {s = "1"} ; twoHour = {s = "2"} ; threeHour = {s = "3"} ; fourHour = {s = "4"} ;
  fiveHour = {s = "5"} ; sixHour = {s = "6"} ; sevenHour = {s = "7"} ; eightHour = {s = "8"} ;
  nineHour = {s = "9"} ; tenHour = {s = "10"} ; elevenHour = {s = "11"} ; twelveHour = {s = "12"} ;
  thirteenHour = {s = "13"} ; fourteenHour = {s = "14"} ; fifteenHour = {s = "15"} ; sixteenHour = {s = "16"} ;
  seventeenHour = {s = "17"} ; eighteenHour = {s = "18"} ; nineteenHour = {s = "19"} ; twentyHour = {s = "20"} ;
  twentyOneHour = {s = "21"} ; twentyTwoHour = {s = "22"} ; twentyThreeHour = {s = "23"} ; twentyFourHour = {s = "24"} ;
  timeHour h = {s = "а" ++ h.s} ;
  timeHourMinute h m = {s = "а" ++ h.s ++ ":" ++ m.s} ;

  weekdayPunctualAdv d = {s = "у" ++ d.s} ;
  weekdayHabitualAdv d = {s = "па" ++ d.s} ;
  weekdayLastAdv d = {s = "мінулай" ++ d.s} ;
  weekdayNextAdv d = {s = "наступнай" ++ d.s} ;
  monthAdv m = {s = "у" ++ m.s} ;
  yearAdv y = {s = "у" ++ y.s} ;
  dayMonthAdv d m = {s = d.s ++ m.s} ;
  monthYearAdv m y = {s = m.s ++ y.s} ;
  dayMonthYearAdv d m y = {s = d.s ++ m.s ++ y.s} ;
  intYear i = {s = i.s} ;
  intMonthday i = {s = i.s} ;

  InLanguage l = {s = "па-" ++ l.s} ;
  weekdayN d = nounFromStr d.s Masc ;
  monthN m = nounFromStr m.s Masc ;
  weekdayPN d = mkPN d.s ;
  monthPN m = mkPN m.s ;
  languageNP l = mkSimpleNP l.s Neuter Sg P3 ;
  languageCN l = nounFromStr l.s Neuter ;

  second_Timeunit = {s = "секунд"} ;
  minute_Timeunit = {s = "хвілін"} ;
  hour_Timeunit = {s = "гадзін"} ;
  day_Timeunit = {s = "дзён"} ;
  week_Timeunit = {s = "тыдняў"} ;
  month_Timeunit = {s = "месяцаў"} ;
  year_Timeunit = {s = "гадоў"} ;

  monday_Weekday = {s = "панядзелак"} ;
  tuesday_Weekday = {s = "аўторак"} ;
  wednesday_Weekday = {s = "серада"} ;
  thursday_Weekday = {s = "чацвер"} ;
  friday_Weekday = {s = "пятніца"} ;
  saturday_Weekday = {s = "субота"} ;
  sunday_Weekday = {s = "нядзеля"} ;

  january_Month = {s = "студзень"} ;
  february_Month = {s = "люты"} ;
  march_Month = {s = "сакавік"} ;
  april_Month = {s = "красавік"} ;
  may_Month = {s = "май"} ;
  june_Month = {s = "чэрвень"} ;
  july_Month = {s = "ліпень"} ;
  august_Month = {s = "жнівень"} ;
  september_Month = {s = "верасень"} ;
  october_Month = {s = "кастрычнік"} ;
  november_Month = {s = "лістапад"} ;
  december_Month = {s = "снежань"} ;

  afrikaans_Language = {s = "афрыкаанс"} ;
  amharic_Language = {s = "амхарску"} ;
  arabic_Language = {s = "арабску"} ;
  bulgarian_Language = {s = "балгарску"} ;
  catalan_Language = {s = "каталанску"} ;
  chinese_Language = {s = "кітайску"} ;
  danish_Language = {s = "дацку"} ;
  dutch_Language = {s = "нідэрландску"} ;
  english_Language = {s = "англійску"} ;
  estonian_Language = {s = "эстонску"} ;
  finnish_Language = {s = "фінску"} ;
  french_Language = {s = "французску"} ;
  german_Language = {s = "нямецку"} ;
  greek_Language = {s = "грэцку"} ;
  hebrew_Language = {s = "іўрыце"} ;
  hindi_Language = {s = "хіндзі"} ;
  japanese_Language = {s = "японску"} ;
  italian_Language = {s = "італьянску"} ;
  latin_Language = {s = "лацінску"} ;
  latvian_Language = {s = "латышску"} ;
  maltese_Language = {s = "мальтыйску"} ;
  nepali_Language = {s = "непальску"} ;
  norwegian_Language = {s = "нарвежску"} ;
  persian_Language = {s = "персідску"} ;
  polish_Language = {s = "польску"} ;
  punjabi_Language = {s = "панджабі"} ;
  romanian_Language = {s = "румынску"} ;
  russian_Language = {s = "руску"} ;
  sindhi_Language = {s = "сіндхі"} ;
  spanish_Language = {s = "іспанску"} ;
  swahili_Language = {s = "суахілі"} ;
  swedish_Language = {s = "шведску"} ;
  thai_Language = {s = "тайску"} ;
  turkish_Language = {s = "турэцку"} ;
  urdu_Language = {s = "урду"} ;

}
