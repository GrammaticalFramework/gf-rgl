--# -path=.:../abstract:../common:../prelude

concrete ConstructionCze of Construction = CatCze **
  open ResCze, Prelude, (P = ParadigmsCze), (G = GrammarCze) in {

lincat
  Timeunit, Hour, Monthday, Year, Language = {s : Str} ;
  Weekday, Month = {s : Str ; n : NounForms} ;

lin
  ready_VP = G.UseComp (G.CompAP (G.PositA (P.mkA "připravený"))) ;
  has_age_VP card = {
    verb = copulaVerbForms ; clitPresent = False ; clit = \\_ => [] ;
    compl = \\a => card.s ! Neutr ! Nom ++ "let" ++ "starý"
    } ;
  cup_of_CN np = G.PossNP (G.UseN (P.mkN "šálek")) np ;
  n_units_AP card cn adj =
    G.AdvAP (G.PositA adj) {s = card.s ! Neutr ! Nom ++ cn.s ! Pl ! Gen} ;
  n_units_of_NP card cn np =
    G.AdvNP np {s = card.s ! Neutr ! Nom ++ cn.s ! Pl ! Gen} ;

  intYear year = {s = year.s} ;
  yearAdv year = {s = "v roce" ++ year.s} ;
  weekdayN day = day.n ;
  monthN month = month.n ;
  weekdayPunctualAdv day = {s = "v" ++ day.s} ;
  weekdayHabitualAdv day = {s = "každý" ++ day.s} ;

  monday_Weekday = {s = "pondělí" ; n = P.mkN "pondělí"} ;
  tuesday_Weekday = {s = "úterý" ; n = P.mkN "úterý"} ;
  wednesday_Weekday = {s = "středa" ; n = P.mkN "středa"} ;
  thursday_Weekday = {s = "čtvrtek" ; n = P.mkN "čtvrtek"} ;
  friday_Weekday = {s = "pátek" ; n = P.mkN "pátek"} ;
  saturday_Weekday = {s = "sobota" ; n = P.mkN "sobota"} ;
  sunday_Weekday = {s = "neděle" ; n = P.mkN "neděle"} ;

  january_Month = {s = "leden" ; n = P.mkN "leden"} ;
  february_Month = {s = "únor" ; n = P.mkN "únor"} ;
  march_Month = {s = "březen" ; n = P.mkN "březen"} ;
  april_Month = {s = "duben" ; n = P.mkN "duben"} ;
  may_Month = {s = "květen" ; n = P.mkN "květen"} ;
  june_Month = {s = "červen" ; n = P.mkN "červen"} ;
  july_Month = {s = "červenec" ; n = P.mkN "červenec"} ;
  august_Month = {s = "srpen" ; n = P.mkN "srpen"} ;
  september_Month = {s = "září" ; n = P.mkN "září"} ;
  october_Month = {s = "říjen" ; n = P.mkN "říjen"} ;
  november_Month = {s = "listopad" ; n = P.mkN "listopad"} ;
  december_Month = {s = "prosinec" ; n = P.mkN "prosinec"} ;

}
