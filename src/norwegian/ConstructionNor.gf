--# -path=.:../abstract:../api

concrete ConstructionNor of Construction = CatNor **
  open SyntaxNor, SymbolicNor, ParadigmsNor, (L = LexiconNor),
       (G = GrammarNor), (C = CommonScand), Prelude in {

flags coding=utf8 ;

lin
  ready_VP = mkVP (mkA "klar") ;

  has_age_VP card =
    mkVP (lin AP (mkAP (lin AdA (mkUtt (mkNP <lin Card card : Card> L.year_N))) L.old_A)) ;

  n_units_AP card cn a =
    mkAP (lin AdA (mkUtt (mkNP <lin Card card : Card> (lin CN cn)))) (lin A a) ;

  n_units_of_NP card cn np =
    G.AdvNP (mkNP <lin Card card : Card> (lin CN cn))
            (SyntaxNor.mkAdv noPrep (lin NP np)) ;

  cup_of_CN np = mkCN (lin N2 (mkN2 (mkN "kopp") noPrep)) (lin NP np) ;

lincat
  Weekday = N ;
  Monthday = NP ;
  Month = N ;
  Year = NP ;

lin
  weekdayPunctualAdv w = SyntaxNor.mkAdv on_Prep (mkNP w) ;
  weekdayHabitualAdv w = SyntaxNor.mkAdv on_Prep (mkNP aPl_Det w) ;

  yearAdv y = SyntaxNor.mkAdv (mkPrep "i") y ;
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
  march_Month = mkN "mars" ;
  april_Month = mkN "april" ;
  may_Month = mkN "mai" ;
  june_Month = mkN "juni" ;
  july_Month = mkN "juli" ;
  august_Month = mkN "august" ;
  september_Month = mkN "september" ;
  october_Month = mkN "oktober" ;
  november_Month = mkN "november" ;
  december_Month = mkN "desember" ;
}
