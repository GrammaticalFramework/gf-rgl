--# -path=.:../abstract:../api

concrete ConstructionDan of Construction = CatDan **
  open SyntaxDan, SymbolicDan, ParadigmsDan, (L = LexiconDan),
       (G = GrammarDan), (C = CommonScand), Prelude in {

flags coding=utf8 ;

lin
  ready_VP = mkVP (mkA "klar") ;

  has_age_VP card =
    mkVP (lin AP (mkAP (lin AdA (mkUtt (mkNP <lin Card card : Card> L.year_N))) L.old_A)) ;

  n_units_AP card cn a =
    mkAP (lin AdA (mkUtt (mkNP <lin Card card : Card> (lin CN cn)))) (lin A a) ;

  n_units_of_NP card cn np =
    G.AdvNP (mkNP <lin Card card : Card> (lin CN cn))
            (SyntaxDan.mkAdv noPrep (lin NP np)) ;

  cup_of_CN np = mkCN (lin N2 (mkN2 (mkN "kop") noPrep)) (lin NP np) ;

lincat
  Weekday = N ;
  Monthday = NP ;
  Month = N ;
  Year = NP ;

lin
  weekdayPunctualAdv w = SyntaxDan.mkAdv on_Prep (mkNP w) ;
  weekdayHabitualAdv w = SyntaxDan.mkAdv on_Prep (mkNP aPl_Det w) ;

  yearAdv y = SyntaxDan.mkAdv (mkPrep "i") y ;
  intYear = symb ;

  weekdayN w = w ;
  monthN m = m ;

  monday_Weekday = mkN "mandag" ;
  tuesday_Weekday = mkN "tirsdag" ;
  wednesday_Weekday = mkN "onsdag" ;
  thursday_Weekday = mkN "torsdag" ;
  friday_Weekday = mkN "fredag" ;
  saturday_Weekday = mkN "lørdag" ;
  sunday_Weekday = mkN "søndag" ;

  january_Month = mkN "januar" ;
  february_Month = mkN "februar" ;
  march_Month = mkN "marts" ;
  april_Month = mkN "april" ;
  may_Month = mkN "maj" ;
  june_Month = mkN "juni" ;
  july_Month = mkN "juli" ;
  august_Month = mkN "august" ;
  september_Month = mkN "september" ;
  october_Month = mkN "oktober" ;
  november_Month = mkN "november" ;
  december_Month = mkN "december" ;
}
