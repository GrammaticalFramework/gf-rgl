--# -path=.:../abstract:../common:../prelude

concrete ConstructionLav of Construction = CatLav ** open
  ResLav, ParadigmsLav, StructuralLav, GrammarLav, Prelude in {

lincat
  Timeunit = N ;
  Hour = {s : Str} ;
  Weekday = N ;
  Monthday = NP ;
  Month = N ;
  Year = NP ;
  Language = N ;

lin
  hungry_VP = UseComp (CompAP (PositA (mkA "izsalcis"))) ;
  thirsty_VP = UseComp (CompAP (PositA (mkA "izslāpis"))) ;
  tired_VP = UseComp (CompAP (PositA (mkA "noguris"))) ;
  scared_VP = UseComp (CompAP (PositA (mkA "nobijies"))) ;
  ill_VP = UseComp (CompAP (PositA (mkA "slims"))) ;
  ready_VP = UseComp (CompAP (PositA (mkA "gatavs"))) ;

  has_age_VP card = UseComp {s = \\agr =>
    card.s ! (fromAgr agr).gend ! Acc ++ "gadus" ++
    (mkA "vecs").s ! (AAdj Posit Indef (fromAgr agr).gend
      (fromAgr agr).num Nom)} ;

  n_units_AP card unit adj = {
    s = \\d,g,n,c => card.s ! g ! c ++ unit.s ! Indef ! Pl ! Gen ++
                       adj.s ! (AAdj Posit d g n c)
  } ;
  n_units_of_NP card unit np = {
    s = \\c => card.s ! unit.gend ! c ++ unit.s ! Indef ! Pl ! Gen ++
                 np.s ! Gen ;
    agr = AgrP3 card.num unit.gend ; pol = Pos ; isRel = False ; isPron = False
  } ;
  n_unit_CN card unit cn = {
    s = \\d,n,c => card.s ! cn.gend ! c ++ unit.s ! Indef ! Sg ! Gen ++ cn.s ! d ! n ! c ;
    gend = cn.gend ; isRel = cn.isRel
  } ;

  bottle_of_CN np = containerCN "pudele" np ;
  cup_of_CN np = containerCN "tase" np ;
  glass_of_CN np = containerCN "glāze" np ;

  weekdayPunctualAdv w = {s = w.s ! Sg ! Loc ; isPron = False} ;
  weekdayHabitualAdv w = {s = w.s ! Pl ! Loc ; isPron = False} ;
  weekdayLastAdv w = {s = "pagājušajā" ++ w.s ! Sg ! Loc ; isPron = False} ;
  weekdayNextAdv w = {s = "nākamajā" ++ w.s ! Sg ! Loc ; isPron = False} ;

  monthAdv m = {s = m.s ! Sg ! Loc ; isPron = False} ;
  yearAdv y = {s = y.s ! Loc ++ "gadā" ; isPron = False} ;
  dayMonthAdv d m = {s = d.s ! Loc ++ m.s ! Sg ! Loc ; isPron = False} ;
  monthYearAdv m y = {s = m.s ! Sg ! Loc ++ y.s ! Loc ++ "gadā" ; isPron = False} ;
  dayMonthYearAdv d m y = {
    s = d.s ! Loc ++ m.s ! Sg ! Loc ++ y.s ! Loc ++ "gadā" ; isPron = False
  } ;

  intYear i = mkSymbolNP i.s ;
  intMonthday i = mkSymbolNP i.s ;

  weekdayN w = w ;
  monthN m = m ;
  weekdayPN w = {s = w.s ! Sg ; gend = w.gend ; num = Sg} ;
  monthPN m = {s = m.s ! Sg ; gend = m.gend ; num = Sg} ;

  monday_Weekday = mkN "pirmdiena" ;
  tuesday_Weekday = mkN "otrdiena" ;
  wednesday_Weekday = mkN "trešdiena" ;
  thursday_Weekday = mkN "ceturtdiena" ;
  friday_Weekday = mkN "piektdiena" ;
  saturday_Weekday = mkN "sestdiena" ;
  sunday_Weekday = mkN "svētdiena" ;

  january_Month = mkN "janvāris" ;
  february_Month = mkN "februāris" ;
  march_Month = mkN "marts" ;
  april_Month = mkN "aprīlis" ;
  may_Month = mkN "maijs" ;
  june_Month = mkN "jūnijs" ;
  july_Month = mkN "jūlijs" ;
  august_Month = mkN "augusts" ;
  september_Month = mkN "septembris" ;
  october_Month = mkN "oktobris" ;
  november_Month = mkN "novembris" ;
  december_Month = mkN "decembris" ;

oper
  containerCN : Str -> NP -> CN = \x,np -> lin CN {
    s = \\d,n,c => (mkN x).s ! n ! c ++ np.s ! Gen ;
    gend = Fem ; isRel = False
  } ;

  mkSymbolNP : Str -> NP = \s -> lin NP {
    s = \\_ => s ; agr = AgrP3 Sg Masc ; pol = Pos ;
    isRel = False ; isPron = False
  } ;

}
