concrete ConstructionSqi of Construction = CatSqi **
  open Prelude, ParamX, ResSqi, ParadigmsSqi, GrammarSqi in {

lincat
  Timeunit = N ;
  Hour = {s:Str} ;
  Weekday = N ;
  Month = N ;
  Monthday = {s:Str} ;
  Year = {s:Str} ;
  Language = N ;

oper
  adjVP : A -> VP = \a -> UseComp (CompAP (PositA a)) ;
  appendVP : VP -> Str -> VP = \v,s -> v ** {
    indicative=\\t,n,p,g,c=>v.indicative!t!n!p!g!c++s;
    subjunctive=\\n,p,g,c=>v.subjunctive!n!p!g!c++s;
    imperative=\\n,c=>v.imperative!n!c++s;
    participle=\\a,c=>v.participle!a!c++s
  } ;
  hour : Str -> Hour = \s -> lin Hour {s=s} ;

lin
  hungry_VP=adjVP (mkA "uritur"); thirsty_VP=adjVP (mkA "etur");
  tired_VP=adjVP (mkA "lodhur"); scared_VP=adjVP (mkA "frikësuar");
  ill_VP=adjVP (mkA "sëmurë"); ready_VP=adjVP (mkA "gatshëm");
  has_age_VP c=appendVP (UseV (irregV "kam" "ke" "ka" "kemi" "keni" "kanë" "ki" "kini" "pasur")) (c.s++"vjeç");
  n_units_AP c unit a={s=\\sp,k,g,n=>c.s++unit.s!Indef!Nom!Pl++(PositA a).s!sp!k!g!n};
  n_units_of_NP c unit np={s=\\_=>c.s++unit.s!Indef!Nom!Pl++"nga"++np.s!Ablat;a={gn=GPl;p=P3}};
  n_unit_CN c unit cn={s=\\sp,k,n=>c.s++unit.s!Indef!Nom!Sg++cn.s!sp!k!n;g=cn.g};
  bottle_of_CN np={s=\\sp,k,n=>(mkN "shishe").s!sp!k!n++"me"++np.s!Acc;g=Fem};
  cup_of_CN np={s=\\sp,k,n=>(mkN "filxhan").s!sp!k!n++"me"++np.s!Acc;g=Masc};
  glass_of_CN np={s=\\sp,k,n=>(mkN "gotë").s!sp!k!n++"me"++np.s!Acc;g=Fem};

  timeunitAdv c u={s="për"++c.s++u.s!Indef!Acc!Pl};
  timeunitRange a b u={s="nga"++a.s++"deri në"++b.s++u.s!Indef!Acc!Pl};
  oneHour=hour "1"; twoHour=hour "2"; threeHour=hour "3"; fourHour=hour "4";
  fiveHour=hour "5"; sixHour=hour "6"; sevenHour=hour "7"; eightHour=hour "8";
  nineHour=hour "9"; tenHour=hour "10"; elevenHour=hour "11"; twelveHour=hour "12";
  thirteenHour=hour "13"; fourteenHour=hour "14"; fifteenHour=hour "15"; sixteenHour=hour "16";
  seventeenHour=hour "17"; eighteenHour=hour "18"; nineteenHour=hour "19"; twentyHour=hour "20";
  twentyOneHour=hour "21"; twentyTwoHour=hour "22"; twentyThreeHour=hour "23"; twentyFourHour=hour "24";
  timeHour h={s="në orën"++h.s}; timeHourMinute h m={s="në orën"++h.s++SOFT_BIND++":"++SOFT_BIND++m.s};

  weekdayPunctualAdv w={s="të"++w.s!Def!Acc!Sg};
  weekdayHabitualAdv w={s="të"++w.s!Def!Acc!Pl};
  weekdayLastAdv w={s="të"++w.s!Def!Acc!Sg++"e kaluar"};
  weekdayNextAdv w={s="të"++w.s!Def!Acc!Sg++"e ardhshme"};
  monthAdv m={s="në"++m.s!Indef!Acc!Sg}; yearAdv y={s="në"++y.s};
  dayMonthAdv d m={s="më"++d.s++m.s!Indef!Acc!Sg};
  monthYearAdv m y={s="në"++m.s!Indef!Acc!Sg++y.s};
  dayMonthYearAdv d m y={s="më"++d.s++m.s!Indef!Acc!Sg++y.s};
  intYear i={s=i.s}; intMonthday i={s=i.s};
  weekdayN w=w; monthN m=m;
  weekdayPN w=mkPN (w.s!Indef!Nom!Sg); monthPN m=mkPN (m.s!Indef!Nom!Sg);

  second_Timeunit=mkN "sekondë"; minute_Timeunit=mkN "minutë"; hour_Timeunit=mkN "orë";
  day_Timeunit=mkN "ditë"; week_Timeunit=mkN "javë"; month_Timeunit=mkN "muaj"; year_Timeunit=mkN "vit";
  monday_Weekday=mkN "e hënë"; tuesday_Weekday=mkN "e martë"; wednesday_Weekday=mkN "e mërkurë";
  thursday_Weekday=mkN "e enjte"; friday_Weekday=mkN "e premte"; saturday_Weekday=mkN "e shtunë"; sunday_Weekday=mkN "e diel";
  january_Month=mkN "janar"; february_Month=mkN "shkurt"; march_Month=mkN "mars"; april_Month=mkN "prill";
  may_Month=mkN "maj"; june_Month=mkN "qershor"; july_Month=mkN "korrik"; august_Month=mkN "gusht";
  september_Month=mkN "shtator"; october_Month=mkN "tetor"; november_Month=mkN "nëntor"; december_Month=mkN "dhjetor";
}
