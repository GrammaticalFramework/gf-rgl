concrete ConstructionMon of Construction = CatMon **
  open Prelude, ResMon, ParadigmsMon in {

  lincat Timeunit, Hour, Weekday, Month, Monthday, Year = {s : Str} ;

  oper
    fixedN : Str -> N = \s -> lin N {s = \\_ => s} ;

  lin
    ready_VP = insertObj (\\_ => "бэлэн") (predV (auxToVerb auxBe)) ;
    has_age_VP card = insertObj (\\_ => card.s ++ "настай") (predV (auxToVerb auxBe)) ;
    n_units_AP card cn ap = {s = card.s ++ cn.s ! Sg ! NNom ++ ap.s} ;
    n_units_of_NP card cn np = np ** {
      s = \\rc => card.s ++ cn.s ! Sg ! NNom ++ np.s ! rc
      } ;
    cup_of_CN np = {s = \\_,nc => "нэг аяга" ++ np.s ! toRCase nc Definite} ;

    monday_Weekday = {s = "даваа"} ;
    tuesday_Weekday = {s = "мягмар"} ;
    wednesday_Weekday = {s = "лхагва"} ;
    thursday_Weekday = {s = "пүрэв"} ;
    friday_Weekday = {s = "баасан"} ;
    saturday_Weekday = {s = "бямба"} ;
    sunday_Weekday = {s = "ням"} ;

    january_Month = {s = "нэгдүгээр сар"} ;
    february_Month = {s = "хоёрдугаар сар"} ;
    march_Month = {s = "гуравдугаар сар"} ;
    april_Month = {s = "дөрөвдүгээр сар"} ;
    may_Month = {s = "тавдугаар сар"} ;
    june_Month = {s = "зургаадугаар сар"} ;
    july_Month = {s = "долдугаар сар"} ;
    august_Month = {s = "наймдугаар сар"} ;
    september_Month = {s = "есдүгээр сар"} ;
    october_Month = {s = "аравдугаар сар"} ;
    november_Month = {s = "арван нэгдүгээр сар"} ;
    december_Month = {s = "арван хоёрдугаар сар"} ;

    weekdayPunctualAdv weekday = {s = weekday.s ++ "гарагт"} ;
    weekdayHabitualAdv weekday = {s = "долоо бүрийн" ++ weekday.s ++ "гарагт"} ;
    weekdayN weekday = fixedN (weekday.s ++ "гараг") ;
    monthN month = fixedN month.s ;
    intYear i = {s = i.s} ;
    yearAdv year = {s = year.s ++ "онд"} ;
}
