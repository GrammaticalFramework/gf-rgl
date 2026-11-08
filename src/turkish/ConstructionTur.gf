--# -path=.:../abstract:../common:../../prelude

concrete ConstructionTur of Construction = CatTur **
  open GrammarTur, ParadigmsTur, ResTur, HarmonyTur, Prelude in {

  lincat
    Weekday = N ;
    Month = N ;
    Monthday = {s : Str} ;
    Year = {s : Str} ;

  lin
    ready_VP = UseComp (CompAP (PositA (mkA "hazır"))) ;

    has_age_VP card = {
      s = \\asp,vf => card.s ! Sg ! Nom ++ "yaşında" ++
                       mkVerbForms olmak_V ! asp ! vf;
      compl = []
    } ;

    n_units_AP card unit adj = {
      s = \\n,c => card.s ! Sg ! Nom ++ unit.s ! Pl ! Nom ++ adj.s ! n ! c;
      h = adj.h
    } ;

    n_units_of_NP card unit np = {
      s = \\c => np.s ! Gen ++ card.s ! Sg ! Nom ++ unit.s ! Pl ! c;
      h = unit.h;
      a = agrP3 Pl
    } ;

    cup_of_CN np = {
      s = \\n,c => np.s ! Gen ++ (mkN "fincan").s ! n ! c;
      gen = \\n,a => np.s ! Gen ++ (mkN "fincan").gen ! n ! a;
      h = (mkN "fincan").h
    } ;

    weekdayPunctualAdv day = {s = day.s ! Sg ! Loc} ;
    weekdayHabitualAdv day = {s = day.s ! Pl ! Loc} ;
    weekdayNextAdv day = {s = "gelecek" ++ day.s ! Sg ! Nom} ;
    weekdayLastAdv day = {s = "geçen" ++ day.s ! Sg ! Nom} ;
    monthAdv month = {s = month.s ! Sg ! Loc} ;
    yearAdv year = {s = year.s ++ "yılında"} ;

    intYear i = {s = i.s} ;
    intMonthday i = {s = i.s} ;

    weekdayN day = day ;
    monthN month = month ;

    monday_Weekday = mkN "Pazartesi" ;
    tuesday_Weekday = mkN "Salı" ;
    wednesday_Weekday = mkN "Çarşamba" ;
    thursday_Weekday = mkN "Perşembe" ;
    friday_Weekday = mkN "Cuma" ;
    saturday_Weekday = mkN "Cumartesi" ;
    sunday_Weekday = mkN "Pazar" ;

    january_Month = mkN "Ocak" ;
    february_Month = mkN "Şubat" ;
    march_Month = mkN "Mart" ;
    april_Month = mkN "Nisan" ;
    may_Month = mkN "Mayıs" ;
    june_Month = mkN "Haziran" ;
    july_Month = mkN "Temmuz" ;
    august_Month = mkN "Ağustos" ;
    september_Month = mkN "Eylül" ;
    october_Month = mkN "Ekim" ;
    november_Month = mkN "Kasım" ;
    december_Month = mkN "Aralık" ;
}
