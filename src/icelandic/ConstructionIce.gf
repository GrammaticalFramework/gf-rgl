--# -path=.:../abstract

concrete ConstructionIce of Construction = CatIce **
  open SyntaxIce, ParadigmsIce, (L = LexiconIce), (G = GrammarIce), (R = ResIce), (S = StructuralIce), Prelude in {

lincat
  Timeunit, Hour, Weekday, Month, Monthday, Year = {s : Str} ;

lin
  hungry_VP = G.UseComp (G.CompAP (G.PositA (mkA "svangur"))) ;
  thirsty_VP = G.UseComp (G.CompAP (G.PositA (mkA "þyrstur"))) ;
  tired_VP = G.UseComp (G.CompAP (G.PositA (mkA "þreyttur"))) ;
  scared_VP = G.UseComp (G.CompAP (G.PositA (mkA "hræddur"))) ;
  ill_VP = G.UseComp (G.CompAP (G.PositA (mkA "veikur"))) ;
  ready_VP = G.UseComp (G.CompAP (G.PositA (mkA "tilbúinn"))) ;

  has_age_VP card = G.UseComp {s = \\a =>
    card.s ! R.Neutr ! R.Nom ++ "ára" ++ (mkA "gamall").s ! R.APosit R.Strong a.n a.g R.Nom
  } ;

  n_units_AP card unit adj = {s = \\n,g,d,c =>
    card.s ! R.Neutr ! R.Nom ++
    unit.s ! R.Pl ! R.Free ! R.Strong ! R.Gen ++
    adj.s ! R.APosit d n g c
  } ;

  n_units_of_NP card unit np = {
    s = \\_ => card.s ! R.Neutr ! R.Nom ++
      unit.s ! R.Pl ! R.Free ! R.Strong ! R.Nom ++ "af" ++ np.s ! R.NCase R.Dat ;
    a = np.a ;
    isPron = False
  } ;

  n_unit_CN card unit cn = {
    s = \\n,sp,d,c => card.s ! R.Neutr ! c ++
      unit.s ! R.Sg ! R.Free ! R.Strong ! R.Gen ++ cn.s ! n ! sp ! d ! c ;
    comp = cn.comp ;
    g = cn.g
  } ;

  bottle_of_CN np = containerCN (mkN "flaska") np ;
  cup_of_CN np = containerCN (mkN "bolli") np ;
  glass_of_CN np = containerCN (mkN "glas") np ;

  timeunitAdv card unit = {s = "í" ++ card.s ! R.Neutr ! R.Nom ++ unit.s} ;
  timeunitRange c1 c2 unit = {s = "í" ++ c1.s ! R.Neutr ! R.Nom ++ "til" ++ c2.s ! R.Neutr ! R.Nom ++ unit.s} ;

  oneHour = ss "eitt" ; twoHour = ss "tvö" ; threeHour = ss "þrjú" ;
  fourHour = ss "fjögur" ; fiveHour = ss "fimm" ; sixHour = ss "sex" ;
  sevenHour = ss "sjö" ; eightHour = ss "átta" ; nineHour = ss "níu" ;
  tenHour = ss "tíu" ; elevenHour = ss "ellefu" ; twelveHour = ss "tólf" ;
  thirteenHour = ss "þrettán" ; fourteenHour = ss "fjórtán" ; fifteenHour = ss "fimmtán" ;
  sixteenHour = ss "sextán" ; seventeenHour = ss "sautján" ; eighteenHour = ss "átján" ;
  nineteenHour = ss "nítján" ; twentyHour = ss "tuttugu" ;
  twentyOneHour = ss "tuttugu og eitt" ; twentyTwoHour = ss "tuttugu og tvö" ;
  twentyThreeHour = ss "tuttugu og þrjú" ; twentyFourHour = ss "tuttugu og fjögur" ;

  timeHour h = {s = "klukkan" ++ h.s} ;
  timeHourMinute h m = {s = "klukkan" ++ h.s ++ m.s ! R.Neutr ! R.Nom} ;

  weekdayPunctualAdv w = {s = "á" ++ w.s} ;
  weekdayHabitualAdv w = {s = "á" ++ w.s ++ "dögum"} ;
  weekdayLastAdv w = {s = "síðasta" ++ w.s} ;
  weekdayNextAdv w = {s = "næsta" ++ w.s} ;
  monthAdv m = {s = "í" ++ m.s} ;
  yearAdv y = {s = "árið" ++ y.s} ;
  dayMonthAdv d m = {s = d.s ++ m.s} ;
  monthYearAdv m y = {s = m.s ++ y.s} ;
  dayMonthYearAdv d m y = {s = d.s ++ m.s ++ y.s} ;
  intYear i = {s = i.s} ;
  intMonthday i = {s = i.s ++ BIND ++ "."} ;

  weekdayN w = nameN w.s R.Masc ;
  monthN m = nameN m.s R.Masc ;
  weekdayPN w = {s = \\_ => w.s ; g = R.Masc} ;
  monthPN m = {s = \\_ => m.s ; g = R.Masc} ;

  second_Timeunit = ss "sekúndu" ; minute_Timeunit = ss "mínútur" ;
  hour_Timeunit = ss "klukkustundir" ; day_Timeunit = ss "daga" ;
  week_Timeunit = ss "vikur" ; month_Timeunit = ss "mánuði" ; year_Timeunit = ss "ár" ;

  monday_Weekday = ss "mánudag" ; tuesday_Weekday = ss "þriðjudag" ;
  wednesday_Weekday = ss "miðvikudag" ; thursday_Weekday = ss "fimmtudag" ;
  friday_Weekday = ss "föstudag" ; saturday_Weekday = ss "laugardag" ;
  sunday_Weekday = ss "sunnudag" ;

  january_Month = ss "janúar" ; february_Month = ss "febrúar" ;
  march_Month = ss "mars" ; april_Month = ss "apríl" ; may_Month = ss "maí" ;
  june_Month = ss "júní" ; july_Month = ss "júlí" ; august_Month = ss "ágúst" ;
  september_Month = ss "september" ; october_Month = ss "október" ;
  november_Month = ss "nóvember" ; december_Month = ss "desember" ;

oper
  nameN : Str -> R.Gender -> N = \s,g -> lin N {
    s = \\_,_,_ => s ;
    g = g
  } ;

  containerCN : N -> NP -> CN = \noun,np -> lin CN {
    s = \\n,sp,d,c => noun.s ! n ! sp ! c ;
    comp = \\_,_ => "af" ++ np.s ! R.NCase R.Dat ;
    g = noun.g
  } ;

}
