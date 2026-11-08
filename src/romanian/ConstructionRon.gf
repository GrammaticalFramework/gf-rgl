--# -path=alltenses:.:../abstract:../api

concrete ConstructionRon of Construction = CatRon **
  open SyntaxRon, SymbolicRon, ParadigmsRon,
       LexiconRon, ResRon, Prelude in {

flags coding=utf8 ;

lincat
  Weekday = N ;
  Month = N ;
  Monthday = NP ;
  Year = NP ;

lin
  hungry_VP = mkVP (mkA "flămând") ;
  thirsty_VP = mkVP (mkA "însetat") ;
  tired_VP = mkVP (mkA "obosit") ;
  scared_VP = mkVP (mkA "speriat") ;
  ill_VP = mkVP (mkA "bolnav") ;
  ready_VP = mkVP (mkA "gata") ;

  has_age_VP card = mkVP (dirV2 (mkV "avea"))
                         (SyntaxRon.mkNP <lin Card card : Card> (mkN "an")) ;

  is_right_VP = mkVP (mkA "corect") ;
  is_wrong_VP = mkVP (mkA "greșit") ;

  n_units_AP card cn a =
    mkAP (lin AdA (mkUtt (SyntaxRon.mkNP <lin Card card : Card> (lin CN cn)))) (lin A a) ;

  n_units_of_NP card cn np =
    let unit : NP = SyntaxRon.mkNP <lin Card card : Card> (lin CN cn)
    in heavyNP {
      s = \\c => (unit.s ! c).comp ++ "din" ++ (np.s ! c).comp ;
      a = unit.a ; hasClit = HasRef False ; ss = unit.indForm ++ "din" ++ np.indForm
      } ;

  bottle_of_CN np = mkCN (mkN2 (mkN "sticlă") (mkPrep "de" Ac)) np ;
  cup_of_CN np = mkCN (mkN2 (mkN "ceașcă") (mkPrep "de" Ac)) np ;
  glass_of_CN np = mkCN (mkN2 (mkN "pahar") (mkPrep "de" Ac)) np ;

  weekdayPunctualAdv w = {s = w.s ! Sg ! Indef ! ANomAcc} ;
  weekdayHabitualAdv w = {s = "în fiecare" ++ w.s ! Sg ! Indef ! ANomAcc} ;
  weekdayLastAdv w = {s = w.s ! Sg ! Indef ! ANomAcc ++ "trecută"} ;
  weekdayNextAdv w = {s = w.s ! Sg ! Indef ! ANomAcc ++ "viitoare"} ;

  monthAdv m = {s = "în" ++ m.s ! Sg ! Indef ! ANomAcc} ;
  yearAdv y = {s = "în" ++ (y.s ! No).comp} ;
  dayMonthAdv d m = {s = "la" ++ (d.s ! No).comp ++ m.s ! Sg ! Indef ! ANomAcc} ;
  monthYearAdv m y = {s = "în" ++ m.s ! Sg ! Indef ! ANomAcc ++ (y.s ! No).comp} ;
  dayMonthYearAdv d m y = {
    s = "la" ++ (d.s ! No).comp ++ m.s ! Sg ! Indef ! ANomAcc ++ (y.s ! No).comp
    } ;

  intYear = symb ;
  intMonthday = symb ;

  weekdayN w = w ;
  monthN m = m ;
  weekdayPN w = {s = \\_ => w.s ! Sg ! Indef ! ANomAcc ;
                 g = Fem ; n = Sg ; a = Inanimate} ;
  monthPN m = {s = \\_ => m.s ! Sg ! Indef ! ANomAcc ;
               g = Masc ; n = Sg ; a = Inanimate} ;

  monday_Weekday = mkN "luni" ;
  tuesday_Weekday = mkN "marți" ;
  wednesday_Weekday = mkN "miercuri" ;
  thursday_Weekday = mkN "joi" ;
  friday_Weekday = mkN "vineri" ;
  saturday_Weekday = mkN "sâmbătă" ;
  sunday_Weekday = mkN "duminică" ;

  january_Month = mkN "ianuarie" ;
  february_Month = mkN "februarie" ;
  march_Month = mkN "martie" ;
  april_Month = mkN "aprilie" ;
  may_Month = mkN "mai" ;
  june_Month = mkN "iunie" ;
  july_Month = mkN "iulie" ;
  august_Month = mkN "august" ;
  september_Month = mkN "septembrie" ;
  october_Month = mkN "octombrie" ;
  november_Month = mkN "noiembrie" ;
  december_Month = mkN "decembrie" ;
}
