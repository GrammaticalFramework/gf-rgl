concrete ConstructionSwa of Construction = CatSwa **
  open Prelude, ResSwa, ParadigmsSwa, GrammarSwa in {

  flags coding=utf8 ;

  lincat Timeunit, Hour, Weekday, Month, Monthday, Year = {s : Str} ;

  lin
    ready_VP = UseComp (CompAP (PositA (mkA "tayari"))) ;
    has_age_VP card = UseComp (CompAdv (mkAdv ("mwenye umri wa" ++ card.s ! G1))) ;
    n_units_AP card cn a = {s = \\g,n => card.s ! g ++ cn.s ! Pl ! Nom ++ a.s ! AAdj g n} ;
    n_units_of_NP card cn np = {
      s = \\c => card.s ! G5 ++ cn.s ! Pl ! Nom ++ "za" ++ np.s ! c ;
      a = Ag cn.g Pl P3
      } ;
    cup_of_CN np = {
      s = \\n,c => case n of {Sg => "kikombe" ; Pl => "vikombe"} ++
        case n of {Sg => "cha" ; Pl => "vya"} ++ np.s ! NPAcc ;
      s2 = \\_ => [] ; g = G4
      } ;

    weekdayN d = {s = \\_,_ => d.s ; g = G5} ;
    monthN m = {s = \\_,_ => m.s ; g = G5} ;
    weekdayPN d = {s = \\_ => d.s ; g = G5} ;
    monthPN m = {s = \\_ => m.s ; g = G5} ;
    weekdayPunctualAdv d = mkAdv d.s ;
    weekdayHabitualAdv d = mkAdv ("kila" ++ d.s) ;
    weekdayLastAdv d = mkAdv (d.s ++ "iliyopita") ;
    weekdayNextAdv d = mkAdv (d.s ++ "ijayo") ;
    monthAdv m = mkAdv ("mwezi wa" ++ m.s) ;
    yearAdv y = mkAdv ("mwaka" ++ y.s) ;
    intYear i = {s = i.s} ;
    intMonthday i = {s = i.s} ;
    dayMonthAdv d m = mkAdv (d.s ++ m.s) ;
    monthYearAdv m y = mkAdv (m.s ++ y.s) ;
    dayMonthYearAdv d m y = mkAdv (d.s ++ m.s ++ y.s) ;

    monday_Weekday = {s = "Jumatatu"} ;
    tuesday_Weekday = {s = "Jumanne"} ;
    wednesday_Weekday = {s = "Jumatano"} ;
    thursday_Weekday = {s = "Alhamisi"} ;
    friday_Weekday = {s = "Ijumaa"} ;
    saturday_Weekday = {s = "Jumamosi"} ;
    sunday_Weekday = {s = "Jumapili"} ;
    january_Month = {s = "Januari"} ;
    february_Month = {s = "Februari"} ;
    march_Month = {s = "Machi"} ;
    april_Month = {s = "Aprili"} ;
    may_Month = {s = "Mei"} ;
    june_Month = {s = "Juni"} ;
    july_Month = {s = "Julai"} ;
    august_Month = {s = "Agosti"} ;
    september_Month = {s = "Septemba"} ;
    october_Month = {s = "Oktoba"} ;
    november_Month = {s = "Novemba"} ;
    december_Month = {s = "Desemba"} ;
}
