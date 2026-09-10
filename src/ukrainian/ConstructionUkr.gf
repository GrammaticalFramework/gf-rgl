concrete ConstructionUkr of Construction = CatUkr ** open ResUkr, (R = ParamX), ParadigmsUkr in {

lincat
  Timeunit, Hour, Weekday, Month, Monthday, Year, Language = {s : Str} ;

oper
  stateVP : Str -> VP = \s -> lin VP {
    s = \\t,pol,g,n,p => copula t pol g n p ++ s ;
    inf = "бути" ++ s ;
    imp = \\pol,n => neg pol ++ "будь" ++ s
  } ;

lin
  hungry_VP = stateVP "голодний" ;
  thirsty_VP = stateVP "спраглий" ;
  tired_VP = stateVP "втомлений" ;
  scared_VP = stateVP "наляканий" ;
  ill_VP = stateVP "хворий" ;
  ready_VP = stateVP "готовий" ;
  has_age_VP card = {
    s = \\t,pol,g,n,p => copula t pol g n p ++ card.s ++ "років" ;
    inf = "мати" ++ card.s ++ "років" ;
    imp = \\pol,n => neg pol ++ "май" ++ card.s ++ "років"
  } ;
  have_name_Cl np name = {s = \\t,pol => np.s ! Gen ++ "ім'я" ++ name.s ! Nom} ;
  married_Cl x y = {s = \\t,pol => x.s ! Nom ++ copula t pol x.g x.n x.p ++ "одружений з" ++ y.s ! Instr} ;
  what_name_QCl np = {s = \\t,pol => "як звати" ++ np.s ! Acc} ;
  how_old_QCl np = {s = \\t,pol => "скільки років" ++ np.s ! Dat} ;
  how_far_QCl np = {s = \\t,pol => "як далеко" ++ np.s ! Nom} ;
  weather_adjCl ap = {s = \\t,pol => copula t pol Neuter Sg P3 ++ ap.s ! Nom ! GSg Neuter} ;
  is_right_VP = stateVP "правий" ;
  is_wrong_VP = stateVP "неправий" ;
  n_units_AP card cn a = {s=\\c,gn => card.s ++ cn.s ! Gen ! Pl ++ a.s ! c ! gn} ;
  n_units_of_NP card cn np = {s=\\c=>card.s ++ cn.s ! Gen ! Pl ++ np.s ! Gen; g=np.g; n=np.n; p=np.p} ;
  n_unit_CN card unit cn = cn ** {s=\\c,n=>card.s ++ unit.s ! Gen ! Pl ++ cn.s ! c ! n} ;
  bottle_of_CN np = constN ("пляшка" ++ np.s ! Gen) Fem ;
  cup_of_CN np = constN ("чашка" ++ np.s ! Gen) Fem ;
  glass_of_CN np = constN ("склянка" ++ np.s ! Gen) Fem ;
  few_X_short_of_Y np x y = {s = np.s ! Dat ++ "бракує кількох" ++ x.s ! Gen ! Pl ++ "до" ++ y.s ! Gen ! Sg} ;

  timeunitAdv card unit = {s = card.s ++ unit.s} ;
  timeunitRange c1 c2 unit = {s = c1.s ++ "-" ++ c2.s ++ unit.s} ;
  timeHour h = {s = "о" ++ h.s} ;
  timeHourMinute h m = {s = "о" ++ h.s ++ m.s} ;
  weekdayPunctualAdv d = {s = "у" ++ d.s} ;
  weekdayHabitualAdv d = {s = "щопонеділка" ++ d.s} ;
  weekdayLastAdv d = {s = "минулого" ++ d.s} ;
  weekdayNextAdv d = {s = "наступного" ++ d.s} ;
  monthAdv m = {s = "у" ++ m.s} ;
  yearAdv y = {s = "у" ++ y.s} ;
  dayMonthAdv d m = {s = d.s ++ m.s} ;
  monthYearAdv m y = {s = m.s ++ y.s} ;
  dayMonthYearAdv d m y = {s = d.s ++ m.s ++ y.s} ;
  intYear i = {s = i.s} ;
  intMonthday i = {s = i.s} ;
  InLanguage l = {s = "українською" ++ l.s} ;
  weekdayN d = constN d.s Masc ;
  monthN m = constN m.s Masc ;
  weekdayPN d = {s=d.s} ;
  monthPN m = {s=m.s} ;
  languageNP l = {s=\\_=>l.s; g=Fem; n=Sg; p=P3} ;
  languageCN l = constN l.s Fem ;

  oneHour = {s="одна"} ;
  twoHour = {s="дві"} ;
  threeHour = {s="три"} ;
  fourHour = {s="чотири"} ;
  fiveHour = {s="п'ять"} ;
  sixHour = {s="шість"} ;
  sevenHour = {s="сім"} ;
  eightHour = {s="вісім"} ;
  nineHour = {s="дев'ять"} ;
  tenHour = {s="десять"} ;
  elevenHour = {s="одинадцять"} ;
  twelveHour = {s="дванадцять"} ;
  second_Timeunit = {s="секунд"} ;
  minute_Timeunit = {s="хвилин"} ;
  hour_Timeunit = {s="годин"} ;
  day_Timeunit = {s="днів"} ;
  week_Timeunit = {s="тижнів"} ;
  month_Timeunit = {s="місяців"} ;
  year_Timeunit = {s="років"} ;
  monday_Weekday = {s="понеділок"} ;
  tuesday_Weekday = {s="вівторок"} ;
  wednesday_Weekday = {s="середу"} ;
  thursday_Weekday = {s="четвер"} ;
  friday_Weekday = {s="п'ятницю"} ;
  saturday_Weekday = {s="суботу"} ;
  sunday_Weekday = {s="неділю"} ;
  january_Month = {s="січні"} ;
  february_Month = {s="лютому"} ;
  march_Month = {s="березні"} ;
  april_Month = {s="квітні"} ;
  may_Month = {s="травні"} ;
  june_Month = {s="червні"} ;
  july_Month = {s="липні"} ;
  august_Month = {s="серпні"} ;
  september_Month = {s="вересні"} ;
  october_Month = {s="жовтні"} ;
  november_Month = {s="листопаді"} ;
  december_Month = {s="грудні"} ;
}
