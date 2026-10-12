--# -path=.:../abstract:../api

concrete ConstructionNno of Construction = CatNno **
  open SyntaxNno, SymbolicNno, ParadigmsNno, (L = LexiconNno),
       (G = GrammarNno), (C = CommonScand), Prelude in {

flags coding=utf8 ;

lin
  ready_VP = mkVP (mkA "klar") ;

  has_age_VP card =
    mkVP (lin AP (mkAP (lin AdA (mkUtt (mkNP <lin Card card : Card> L.year_N))) L.old_A)) ;

  n_units_AP card cn a =
    mkAP (lin AdA (mkUtt (mkNP <lin Card card : Card> (lin CN cn)))) (lin A a) ;

  n_units_of_NP card cn np =
    G.AdvNP (mkNP <lin Card card : Card> (lin CN cn))
            (SyntaxNno.mkAdv noPrep (lin NP np)) ;

  cup_of_CN np = mkCN (lin N2 (mkN2 (mkN "kopp") noPrep)) (lin NP np) ;

lincat
  Weekday = N ;
  Monthday = NP ;
  Month = N ;
  Year = NP ;

lin
  weekdayPunctualAdv w = SyntaxNno.mkAdv on_Prep (mkNP w) ;
  weekdayHabitualAdv w = SyntaxNno.mkAdv on_Prep (mkNP aPl_Det w) ;

  yearAdv y = SyntaxNno.mkAdv (mkPrep "i") y ;
  intYear = symb ;

  weekdayN w = w ;
  monthN m = m ;

  monday_Weekday = mkN "måndag" ;
  tuesday_Weekday = mkN "tysdag" ;
  wednesday_Weekday = mkN "onsdag" ;
  thursday_Weekday = mkN "torsdag" ;
  friday_Weekday = mkN "fredag" ;
  saturday_Weekday = mkN "laurdag" ;
  sunday_Weekday = mkN "sundag" ;

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
