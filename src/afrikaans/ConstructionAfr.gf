--# -path=.:../abstract:../common

concrete ConstructionAfr of Construction = CatAfr **
  open SyntaxAfr, GrammarAfr, ParadigmsAfr, Prelude,
       (N = NounAfr), (L = LexiconAfr) in {

lin
  ready_VP = mkVP (mkA "gereed") ;

  has_age_VP card = mkVP
    (lin AP (mkAP (lin AdA (mkUtt (mkNP <lin Card card : Card> (mkN "jaar"))))
                 (mkA "oud"))) ;

  n_units_AP card cn a = mkAP
    (lin AdA (mkUtt (mkNP <lin Card card : Card> (lin CN cn)))) (lin A a) ;

  n_units_of_NP card cn np = N.AdvNP
    (mkNP <lin Card card : Card> (lin CN cn))
    (SyntaxAfr.mkAdv (mkPrep "van") (lin NP np)) ;

  cup_of_CN np = mkCN (mkCN (mkN "koppie"))
                       (SyntaxAfr.mkAdv (mkPrep "van") (lin NP np)) ;

lincat
  Weekday = N ;
  Month = N ;
  Year = {s : Str} ;
  Monthday = {s : Str} ;

lin
  weekdayPunctualAdv w = SyntaxAfr.mkAdv (mkPrep "op") (mkNP w) ;
  weekdayHabitualAdv w = SyntaxAfr.mkAdv (mkPrep "elke") (mkNP w) ;
  yearAdv y = {s = "in" ++ y.s} ;
  intYear i = {s = i.s} ;

  weekdayN w = w ;
  monthN m = m ;

  monday_Weekday = mkN "Maandag" ;
  tuesday_Weekday = mkN "Dinsdag" ;
  wednesday_Weekday = mkN "Woensdag" ;
  thursday_Weekday = mkN "Donderdag" ;
  friday_Weekday = mkN "Vrydag" ;
  saturday_Weekday = mkN "Saterdag" ;
  sunday_Weekday = mkN "Sondag" ;

  january_Month = mkN "Januarie" ;
  february_Month = mkN "Februarie" ;
  march_Month = mkN "Maart" ;
  april_Month = mkN "April" ;
  may_Month = mkN "Mei" ;
  june_Month = mkN "Junie" ;
  july_Month = mkN "Julie" ;
  august_Month = mkN "Augustus" ;
  september_Month = mkN "September" ;
  october_Month = mkN "Oktober" ;
  november_Month = mkN "November" ;
  december_Month = mkN "Desember" ;

}
