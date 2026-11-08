--# -path=.:../abstract:../common:../prelude

concrete ConstructionIna of Construction = CatIna **
  open GrammarIna, ParadigmsIna, ResIna, Prelude in {

lin
  ready_VP = UseComp (CompAP (PositA (mkA "preste"))) ;

  has_age_VP card =
    AdvVP (UseV (mkV "haber")) (lin Adv {s = card.s ++ "annos"}) ;

  n_units_AP card cn a =
    AdAP (lin AdA {s = card.s ++ cn.s ! card.n}) (PositA a) ;

  n_units_of_NP card cn np =
    AdvNP (DetCN (DetQuant IndefArt (NumCard card)) cn)
          (PrepNP part_Prep np) ;

  cup_of_CN np =
    AdvCN (UseN (mkN "tassa")) (PrepNP part_Prep np) ;

lincat
  Weekday = N ;
  Month = N ;
  Year = {s : Str} ;
  Monthday = {s : Str} ;

lin
  weekdayPunctualAdv w = {s = "le" ++ w.s ! Sg} ;
  weekdayHabitualAdv w = {s = "omne" ++ w.s ! Sg} ;
  weekdayLastAdv w = {s = "le" ++ w.s ! Sg ++ "passate"} ;
  weekdayNextAdv w = {s = "le proxime" ++ w.s ! Sg} ;

  monthAdv m = {s = "in" ++ m.s ! Sg} ;
  yearAdv y = {s = "in" ++ y.s} ;
  dayMonthAdv d m = {s = "le" ++ d.s ++ "de" ++ m.s ! Sg} ;
  monthYearAdv m y = {s = "in" ++ m.s ! Sg ++ y.s} ;
  dayMonthYearAdv d m y =
    {s = "le" ++ d.s ++ "de" ++ m.s ! Sg ++ y.s} ;

  intYear i = {s = i.s} ;
  intMonthday i = {s = i.s} ;

  weekdayN w = w ;
  monthN m = m ;
  weekdayPN w = {s = w.s ! Sg} ;
  monthPN m = {s = m.s ! Sg} ;

  monday_Weekday = mkN "lunedi" ;
  tuesday_Weekday = mkN "martedi" ;
  wednesday_Weekday = mkN "mercuridi" ;
  thursday_Weekday = mkN "jovedi" ;
  friday_Weekday = mkN "venerdi" ;
  saturday_Weekday = mkN "sabbato" ;
  sunday_Weekday = mkN "dominica" ;

  january_Month = mkN "januario" ;
  february_Month = mkN "februario" ;
  march_Month = mkN "martio" ;
  april_Month = mkN "april" ;
  may_Month = mkN "maio" ;
  june_Month = mkN "junio" ;
  july_Month = mkN "julio" ;
  august_Month = mkN "augusto" ;
  september_Month = mkN "septembre" ;
  october_Month = mkN "octobre" ;
  november_Month = mkN "novembre" ;
  december_Month = mkN "decembre" ;
}
