concrete ConstructionKor of Construction = CatKor **
  open Prelude, ResKor, (P=ParadigmsKor), (G=GrammarKor) in {

lincat
  Timeunit = N ;
  Weekday = N ;
  Monthday = NP ;
  Month = N ;
  Year = NP ;

lin
  ready_VP = useV (mkVerb "준비되다") ;
  has_age_VP card =
    let age = glue (card.s ! SK ! Attrib) "살" in
    useV copula ** {
      s = \\vf => glue age (copula.s ! vf) ;
      past = \\p => glue age (copula.past ! p) ;
      pastAttr = \\p => glue age (copula.pastAttr ! p) ;
      futureAttr = \\p => glue age (copula.futureAttr ! p)
    } ;

  n_units_AP card unit adj = adj ** {
    s = \\vf => card.s ! unit.c.origin ! Attrib ++ unit.s ! Bare ++ adj.s ! vf ;
    compar = []
    } ;
  n_units_of_NP card unit np = np ** {
    s = \\nf => card.s ! unit.c.origin ! Attrib ++ unit.s ! Bare ++ np.s ! nf
    } ;
  cup_of_CN np = useN (P.mkN "컵") ** {rs = np.s ! Bare} ;

  weekdayPunctualAdv w = {s = glue (w.s ! Bare) "에"} ;
  weekdayHabitualAdv w = {s = "매주" ++ glue (w.s ! Bare) "에"} ;
  yearAdv y = {s = glue (y.s ! Bare) "에"} ;
  intYear i = lin NP {s = \\_ => glue i.s "년" ; p = Consonant} ;
  intMonthday i = lin NP {s = \\_ => glue i.s "일" ; p = Consonant} ;

  weekdayN w = w ;
  monthN m = m ;

  monday_Weekday = P.mkN "월요일" ;
  tuesday_Weekday = P.mkN "화요일" ;
  wednesday_Weekday = P.mkN "수요일" ;
  thursday_Weekday = P.mkN "목요일" ;
  friday_Weekday = P.mkN "금요일" ;
  saturday_Weekday = P.mkN "토요일" ;
  sunday_Weekday = P.mkN "일요일" ;

  january_Month = P.mkN "1월" ;
  february_Month = P.mkN "2월" ;
  march_Month = P.mkN "3월" ;
  april_Month = P.mkN "4월" ;
  may_Month = P.mkN "5월" ;
  june_Month = P.mkN "6월" ;
  july_Month = P.mkN "7월" ;
  august_Month = P.mkN "8월" ;
  september_Month = P.mkN "9월" ;
  october_Month = P.mkN "10월" ;
  november_Month = P.mkN "11월" ;
  december_Month = P.mkN "12월" ;
}
