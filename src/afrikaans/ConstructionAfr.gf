--# -path=.:../abstract:../common

concrete ConstructionAfr of Construction = CatAfr **
  open GrammarAfr, ParadigmsAfr, Prelude in {

lin
  ready_VP = UseComp (CompAP (PositA (mkA "gereed"))) ;

  has_age_VP card =
    UseComp (CompAP (AdAP (lin AdA (UttNP (DetCN (DetQuant IndefArt (NumCard card)) (UseN (mkN "jaar")))))
                          (PositA (mkA "oud")))) ;

  n_units_AP card cn a =
    AdAP (lin AdA (UttNP (DetCN (DetQuant IndefArt (NumCard card)) cn)))
         (PositA a) ;

  n_units_of_NP card cn np =
    AdvNP (DetCN (DetQuant IndefArt (NumCard card)) cn)
          (PrepNP (mkPrep "van") np) ;

  cup_of_CN np = AdvCN (UseN (mkN "koppie"))
                       (PrepNP (mkPrep "van") np) ;

lincat
  Weekday = N ;
  Month = N ;
  Year = {s : Str} ;
  Monthday = {s : Str} ;

lin
  weekdayPunctualAdv w = PrepNP (mkPrep "op") (MassNP (UseN w)) ;
  weekdayHabitualAdv w = PrepNP (mkPrep "elke") (MassNP (UseN w)) ;
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
