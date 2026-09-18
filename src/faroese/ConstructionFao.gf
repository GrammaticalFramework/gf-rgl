concrete ConstructionFao of Construction = CatFao ** open ResFao, ParadigmsFao in {

lincat
  Timeunit, Hour, Weekday, Month, Monthday, Year, Language = {s : Str} ;

lin
  hungry_VP = beVP "svangur" ;
  thirsty_VP = beVP "tystur" ;
  tired_VP = beVP "troyttur" ;
  scared_VP = beVP "bangin" ;
  ill_VP = beVP "sjúkur" ;
  ready_VP = beVP "klárur" ;
  has_age_VP card = beVP (cardStr card ++ "ára gamal") ;
  have_name_Cl np name = {
    Converb = np.s ! Nom ++ "eitur" ++ name.s ! Nom ;
    Indicative = \\t,pol => np.s ! Nom ++ "eitur" ++ negStr pol ++ name.s ! Nom ;
    Interrogative = \\t,pol => "eitur" ++ np.s ! Nom ++ negStr pol ++ name.s ! Nom ;
    Future = \\pol => np.s ! Nom ++ futureAux ! persNum np.n np.p ++ negStr pol ++ "eita" ++ name.s ! Nom ;
    FutureInterrogative = \\pol => futureAux ! persNum np.n np.p ++ np.s ! Nom ++ negStr pol ++ "eita" ++ name.s ! Nom ;
    Conditional = \\pol => np.s ! Nom ++ conditionalAux ! persNum np.n np.p ++ negStr pol ++ "eita" ++ name.s ! Nom ;
    ConditionalInterrogative = \\pol => conditionalAux ! persNum np.n np.p ++ np.s ! Nom ++ negStr pol ++ "eita" ++ name.s ! Nom ;
    Anterior = \\t,pol => np.s ! Nom ++ perfectAux ! t ! persNum np.n np.p ++ negStr pol ++ "itið" ++ name.s ! Nom ;
    AnteriorInterrogative = \\t,pol => perfectAux ! t ! persNum np.n np.p ++ np.s ! Nom ++ negStr pol ++ "itið" ++ name.s ! Nom ;
    Nonfinite = np.s ! Nom ++ "eita" ++ name.s ! Nom ;
    Participle = \\_ => np.s ! Nom ++ "itið" ++ name.s ! Nom
  } ;
  married_Cl np spouse = {
    Converb = np.s ! Nom ++ "er giftur við" ++ spouse.s ! Dat ;
    Indicative = \\t,pol => np.s ! Nom ++ copula ! t ! persNum np.n np.p ++ negStr pol ++ "giftur við" ++ spouse.s ! Dat ;
    Interrogative = \\t,pol => copula ! t ! persNum np.n np.p ++ np.s ! Nom ++ negStr pol ++ "giftur við" ++ spouse.s ! Dat ;
    Future = \\pol => np.s ! Nom ++ futureAux ! persNum np.n np.p ++ negStr pol ++ "vera giftur við" ++ spouse.s ! Dat ;
    FutureInterrogative = \\pol => futureAux ! persNum np.n np.p ++ np.s ! Nom ++ negStr pol ++ "vera giftur við" ++ spouse.s ! Dat ;
    Conditional = \\pol => np.s ! Nom ++ conditionalAux ! persNum np.n np.p ++ negStr pol ++ "vera giftur við" ++ spouse.s ! Dat ;
    ConditionalInterrogative = \\pol => conditionalAux ! persNum np.n np.p ++ np.s ! Nom ++ negStr pol ++ "vera giftur við" ++ spouse.s ! Dat ;
    Anterior = \\t,pol => np.s ! Nom ++ perfectAux ! t ! persNum np.n np.p ++ negStr pol ++ "verið giftur við" ++ spouse.s ! Dat ;
    AnteriorInterrogative = \\t,pol => perfectAux ! t ! persNum np.n np.p ++ np.s ! Nom ++ negStr pol ++ "verið giftur við" ++ spouse.s ! Dat ;
    Nonfinite = np.s ! Nom ++ "vera giftur við" ++ spouse.s ! Dat ;
    Participle = \\_ => np.s ! Nom ++ "verið giftur við" ++ spouse.s ! Dat
  } ;
  what_name_QCl np = {
    s = \\t,pol => "hvat" ++ "eitur" ++ np.s ! Nom ;
    anterior = \\t,pol => "hvat" ++ perfectAux ! t ! persNum np.n np.p ++ negStr pol ++ "itið" ++ np.s ! Nom ;
    future = \\pol => "hvat" ++ futureAux ! persNum np.n np.p ++ negStr pol ++ "eita" ++ np.s ! Nom ;
    conditional = \\pol => "hvat" ++ conditionalAux ! persNum np.n np.p ++ negStr pol ++ "eita" ++ np.s ! Nom
  } ;
  how_old_QCl np = {
    s = \\t,pol => "hvussu gamal" ++ copula ! t ! persNum np.n np.p ++ negStr pol ++ np.s ! Nom ;
    anterior = \\t,pol => "hvussu gamal" ++ perfectAux ! t ! persNum np.n np.p ++ negStr pol ++ "verið" ++ np.s ! Nom ;
    future = \\pol => "hvussu gamal" ++ futureAux ! persNum np.n np.p ++ negStr pol ++ "vera" ++ np.s ! Nom ;
    conditional = \\pol => "hvussu gamal" ++ conditionalAux ! persNum np.n np.p ++ negStr pol ++ "vera" ++ np.s ! Nom
  } ;
  how_far_QCl np = {
    s = \\t,pol => "hvussu langt burtur" ++ copula ! t ! persNum np.n np.p ++ negStr pol ++ np.s ! Nom ;
    anterior = \\t,pol => "hvussu langt burtur" ++ perfectAux ! t ! persNum np.n np.p ++ negStr pol ++ "verið" ++ np.s ! Nom ;
    future = \\pol => "hvussu langt burtur" ++ futureAux ! persNum np.n np.p ++ negStr pol ++ "vera" ++ np.s ! Nom ;
    conditional = \\pol => "hvussu langt burtur" ++ conditionalAux ! persNum np.n np.p ++ negStr pol ++ "vera" ++ np.s ! Nom
  } ;
  weather_adjCl ap = {
    Converb = "tað er" ++ ap.s ! Strong ! Neuter ! Sg ! Nom ;
    Indicative = \\t,pol => "tað" ++ copula ! t ! PSg P3 ++ negStr pol ++ ap.s ! Strong ! Neuter ! Sg ! Nom ;
    Interrogative = \\t,pol => copula ! t ! PSg P3 ++ "tað" ++ negStr pol ++ ap.s ! Strong ! Neuter ! Sg ! Nom ;
    Future = \\pol => "tað" ++ futureAux ! PSg P3 ++ negStr pol ++ "vera" ++ ap.s ! Strong ! Neuter ! Sg ! Nom ;
    FutureInterrogative = \\pol => futureAux ! PSg P3 ++ "tað" ++ negStr pol ++ "vera" ++ ap.s ! Strong ! Neuter ! Sg ! Nom ;
    Conditional = \\pol => "tað" ++ conditionalAux ! PSg P3 ++ negStr pol ++ "vera" ++ ap.s ! Strong ! Neuter ! Sg ! Nom ;
    ConditionalInterrogative = \\pol => conditionalAux ! PSg P3 ++ "tað" ++ negStr pol ++ "vera" ++ ap.s ! Strong ! Neuter ! Sg ! Nom ;
    Anterior = \\t,pol => "tað" ++ perfectAux ! t ! PSg P3 ++ negStr pol ++ "verið" ++ ap.s ! Strong ! Neuter ! Sg ! Nom ;
    AnteriorInterrogative = \\t,pol => perfectAux ! t ! PSg P3 ++ "tað" ++ negStr pol ++ "verið" ++ ap.s ! Strong ! Neuter ! Sg ! Nom ;
    Nonfinite = "vera" ++ ap.s ! Strong ! Neuter ! Sg ! Nom ;
    Participle = \\_ => "verið" ++ ap.s ! Strong ! Neuter ! Sg ! Nom
  } ;
  is_right_VP = beVP "rættur" ;
  is_wrong_VP = beVP "skeivur" ;
  n_units_AP card cn a = {
    s = \\af,g,n,c => card.s ! cn.g ! Nom ++ cn.s ! Indef ! Pl ! Nom ++ a.s ! af ! g ! n ! c
  } ;
  n_units_of_NP card cn np = mkNP (card.s ! cn.g ! Nom ++ cn.s ! Indef ! Pl ! Nom ++ "av" ++ np.s ! Dat) np.g np.n np.p ;
  n_unit_CN card unit cn = mkCN (card.s ! unit.g ! Nom ++ unit.s ! Indef ! Sg ! Nom ++ cn.s ! Indef ! Sg ! Nom) cn.g ;
  bottle_of_CN np = mkCN ("fløska av" ++ np.s ! Dat) Fem ;
  cup_of_CN np = mkCN ("koppur av" ++ np.s ! Dat) Masc ;
  glass_of_CN np = mkCN ("glas av" ++ np.s ! Dat) Neuter ;
  few_X_short_of_Y np x y = {s = np.s ! Nom ++ "vantar nakrar" ++ x.s ! Indef ! Pl ! Acc ++ "í" ++ y.s ! Def ! Sg ! Acc} ;

  timeunitAdv card unit = {s = "í" ++ cardStr card ++ unit.s} ;
  timeunitRange c1 c2 unit = {s = "í" ++ cardStr c1 ++ "til" ++ cardStr c2 ++ unit.s} ;
  oneHour = {s = "eitt"} ;
  twoHour = {s = "tvey"} ;
  threeHour = {s = "trý"} ;
  fourHour = {s = "fýra"} ;
  fiveHour = {s = "fimm"} ;
  sixHour = {s = "seks"} ;
  sevenHour = {s = "sjey"} ;
  eightHour = {s = "átta"} ;
  nineHour = {s = "níggju"} ;
  tenHour = {s = "tíggju"} ;
  elevenHour = {s = "ellivu"} ;
  twelveHour = {s = "tólv"} ;
  thirteenHour = {s = "trettan"} ;
  fourteenHour = {s = "fjúrtan"} ;
  fifteenHour = {s = "fimtan"} ;
  sixteenHour = {s = "sekstan"} ;
  seventeenHour = {s = "seytjan"} ;
  eighteenHour = {s = "átjan"} ;
  nineteenHour = {s = "nítjan"} ;
  twentyHour = {s = "tjúgu"} ;
  twentyOneHour = {s = "tjúgueitt"} ;
  twentyTwoHour = {s = "tjúgutvey"} ;
  twentyThreeHour = {s = "tjúgutrý"} ;
  twentyFourHour = {s = "tjúgufýra"} ;
  timeHour h = {s = "klokkan" ++ h.s} ;
  timeHourMinute h m = {s = "klokkan" ++ h.s ++ cardStr m} ;
  weekdayPunctualAdv w = {s = w.s} ;
  weekdayHabitualAdv w = {s = w.s} ;
  weekdayLastAdv w = {s = "seinasta" ++ w.s} ;
  weekdayNextAdv w = {s = "næsta" ++ w.s} ;
  monthAdv m = {s = "í" ++ m.s} ;
  yearAdv y = {s = "í" ++ y.s} ;
  dayMonthAdv d m = {s = d.s ++ m.s} ;
  monthYearAdv m y = {s = m.s ++ y.s} ;
  dayMonthYearAdv d m y = {s = d.s ++ m.s ++ y.s} ;
  intYear i = {s = i.s} ;
  intMonthday i = {s = i.s} ;
  InLanguage l = {s = "á" ++ l.s} ;
  weekdayN w = mkCN w.s Masc ;
  monthN m = mkCN m.s Masc ;
  weekdayPN w = {s = w.s} ;
  monthPN m = {s = m.s} ;
  languageNP l = mkNP l.s Neuter Sg P3 ;
  languageCN l = mkCN l.s Neuter ;

  second_Timeunit = {s = "sekund"} ;
  minute_Timeunit = {s = "minutt"} ;
  hour_Timeunit = {s = "tími"} ;
  day_Timeunit = {s = "dagur"} ;
  week_Timeunit = {s = "vika"} ;
  month_Timeunit = {s = "mánaður"} ;
  year_Timeunit = {s = "ár"} ;
  monday_Weekday = {s = "mánadagur"} ;
  tuesday_Weekday = {s = "týsdagur"} ;
  wednesday_Weekday = {s = "mikudagur"} ;
  thursday_Weekday = {s = "hósdagur"} ;
  friday_Weekday = {s = "fríggjadagur"} ;
  saturday_Weekday = {s = "leygardagur"} ;
  sunday_Weekday = {s = "sunnudagur"} ;
  january_Month = {s = "januar"} ;
  february_Month = {s = "februar"} ;
  march_Month = {s = "mars"} ;
  april_Month = {s = "apríl"} ;
  may_Month = {s = "mai"} ;
  june_Month = {s = "juni"} ;
  july_Month = {s = "juli"} ;
  august_Month = {s = "august"} ;
  september_Month = {s = "september"} ;
  october_Month = {s = "oktober"} ;
  november_Month = {s = "november"} ;
  december_Month = {s = "desember"} ;

  afrikaans_Language = {s = "afrikaans"} ;
  amharic_Language = {s = "amhariskt"} ;
  arabic_Language = {s = "arabiskt"} ;
  bulgarian_Language = {s = "bulgarskt"} ;
  catalan_Language = {s = "katalanskt"} ;
  chinese_Language = {s = "kinesiskt"} ;
  danish_Language = {s = "danskt"} ;
  dutch_Language = {s = "hollendskt"} ;
  english_Language = {s = "enskt"} ;
  estonian_Language = {s = "estiskt"} ;
  finnish_Language = {s = "finskt"} ;
  french_Language = {s = "franskt"} ;
  german_Language = {s = "týskt"} ;
  greek_Language = {s = "grikskt"} ;
  hebrew_Language = {s = "hebraiskt"} ;
  hindi_Language = {s = "hindi"} ;
  japanese_Language = {s = "japanskt"} ;
  italian_Language = {s = "italskt"} ;
  latin_Language = {s = "latín"} ;
  latvian_Language = {s = "lettiskt"} ;
  maltese_Language = {s = "maltesiskt"} ;
  nepali_Language = {s = "nepalskt"} ;
  norwegian_Language = {s = "norskt"} ;
  persian_Language = {s = "persiskt"} ;
  polish_Language = {s = "pólskt"} ;
  punjabi_Language = {s = "punjabi"} ;
  romanian_Language = {s = "rumenskt"} ;
  russian_Language = {s = "russiskt"} ;
  sindhi_Language = {s = "sindhi"} ;
  spanish_Language = {s = "spanskt"} ;
  swahili_Language = {s = "swahili"} ;
  swedish_Language = {s = "svenskt"} ;
  thai_Language = {s = "tailendskt"} ;
  turkish_Language = {s = "turkiskt"} ;
  urdu_Language = {s = "urdu"} ;

oper
  beVP : Str -> VerbPhrase = \ap -> {
    Converb = "verið" ++ ap ;
    Imperative = \\n => case n of {Sg => "ver" ; Pl => "verið"} ++ ap ;
    Indicative = \\t,pol,_,p => copula ! t ! p ++ negStr pol ++ ap ;
    Finite = copula ;
    Remainder = \\pol,_,_ => negStr pol ++ ap ;
    Nonfinite = "vera" ++ ap ;
    Participle = \\_ => "verið" ++ ap
  } ;

  cardStr : Card -> Str = \card ->
    card.s ! Neuter ! Nom ;
}
