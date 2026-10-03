--# -path=.:api
concrete ConstructionLat of Construction = CatLat ** 
  open SyntaxLat, SymbolicLat, ParadigmsLat, 
       (L = LexiconLat), (E = ExtraLat), (G = GrammarLat), (I = IrregLat), (R = ResLat), (N = NounLat), Prelude in {

  lincat Timeunit, Hour, Weekday, Month, Monthday, Year, Language = {s : Str} ;

  lin
    ready_VP = R.insertAdj ((mkA "paratus").s ! R.Posit) (R.predV R.esseAux) ;
    has_age_VP card = (R.predV R.esseAux) ** {
      compl = \\a => card.s ! R.Masc ! R.Acc ++ "annos" ++ case a of {
        R.Ag R.Masc _ _ => "natus"; R.Ag R.Fem _ _ => "nata"; _ => "natum"}
      } ;
    n_units_AP card unit adj = {s = \\a => card.s ! R.Masc ! R.Acc ++ unit.s ! R.Pl ! R.Acc ++ adj.s ! R.Posit ! a} ;
    n_units_of_NP card unit np = R.dummyNP (card.s ! R.Masc ! R.Nom ++ unit.s ! R.Pl ! R.Nom ++
      R.combineNounPhrase np ! R.PronNonDrop ! R.APostN ! R.DPreN ! R.Gen) ** {n=R.Pl} ;
    cup_of_CN np = G.PossNP (G.UseN (mkN "poculum")) np ;

    monday_Weekday = ss "dies Lunae";
    tuesday_Weekday = ss "dies Martis";
    wednesday_Weekday = ss "dies Mercurii";
    thursday_Weekday = ss "dies Iovis";
    friday_Weekday = ss "dies Veneris";
    saturday_Weekday = ss "dies Saturni";
    sunday_Weekday = ss "dies Solis";
    january_Month = ss "Ianuarius";
    february_Month = ss "Februarius";
    march_Month = ss "Martius";
    april_Month = ss "Aprilis";
    may_Month = ss "Maius";
    june_Month = ss "Iunius";
    july_Month = ss "Iulius";
    august_Month = ss "Augustus";
    september_Month = ss "September";
    october_Month = ss "October";
    november_Month = ss "November";
    december_Month = ss "December";
    weekdayPunctualAdv d = mkAdv ("die" ++ d.s) ;
    weekdayHabitualAdv d = mkAdv (d.s) ;
    weekdayN d = constN d.s R.Masc ;
    monthN m = constN m.s R.Masc ;
    intYear i = i ;
    yearAdv y = mkAdv ("anno" ++ y.s) ;
}
