concrete ConstructionGla of Construction = CatGla ** open ParadigmsGla, ResGla, Prelude in {

lincat
  Timeunit = N ;
  Weekday = N ;
  Monthday = NP ;
  Month = N ;
  Year = NP ;
  Hour = SS ;
  Language = N ;
{-
lin

  timeunitAdv n time =
  let n_card : Card   = n ;
      n_hours_NP : NP = mkNP n_card time ;
  in  SyntaxGla.mkAdv for_Prep n_hours_NP | mkAdv (n_hours_NP.s ! R.npNom) ;

  weekdayPunctualAdv w = ;         -- on Sunday
  weekdayHabitualAdv w = ;         -- on Sundays
  weekdayNextAdv w =      -- next Sunday
  weekdayLastAdv w =      -- last Sunday

  monthAdv m = mkAdv in_Prep (mkNP m) ;
  yearAdv y = mkAdv in_Prep y ;
  dayMonthAdv d m  =  ; -- on 17 Gla
  monthYearAdv m y =  ; -- in Gla 2012
  dayMonthYearAdv d m y =  ; -- on 17 Gla 2013

  intYear = symb ;
  intMonthday = symb ;

lincat Language = N ;

lin InLanguage l = mkAdv ???_Prep (mkNP l) ;

lin
  weekdayN w = w ;
  monthN m = m ;

  weekdayPN w = mkPN w ;
  monthPN m = mkPN m ;

  languageCN l = mkCN l ;
  languageNP l = mkNP l ;


oper mkLanguage : Str -> N = \s -> mkN s ;

----------------------------------------------
---- lexicon of special names

lin second_Timeunit = mkN "second" ;
lin minute_Timeunit = mkN "minute" ;
lin hour_Timeunit = mkN "hour" ;
lin day_Timeunit = mkN "day" ;
lin week_Timeunit = mkN "week" ;
lin month_Timeunit = mkN "month" ;
lin year_Timeunit = mkN "year" ;

lin monday_Weekday = mkN "Monday" ;
lin tuesday_Weekday = mkN "Tuesday" ;
lin wednesday_Weekday = mkN "Wednesday" ;
lin thursday_Weekday = mkN "Thursday" ;
lin friday_Weekday = mkN "Friday" ;
lin saturday_Weekday = mkN "Saturday" ;
lin sunday_Weekday = mkN "Sunday" ;

lin january_Month = mkN "January" ;
lin february_Month = mkN "February" ;
lin march_Month = mkN "March" ;
lin april_Month = mkN "April" ;
lin may_Month = mkN "May" ;
lin june_Month = mkN "June" ;
lin july_Month = mkN "July" ;
lin august_Month = mkN "August" ;
lin september_Month = mkN "September" ;
lin october_Month = mkN "October" ;
lin november_Month = mkN "November" ;
lin december_Month = mkN "December" ;

lin afrikaans_Language = mkLanguage "Afrikaans" ;
lin amharic_Language = mkLanguage "Amharic" ;
lin arabic_Language = mkLanguage "Arabic" ;
lin bulgarian_Language = mkLanguage "Bulgarian" ;
lin catalan_Language = mkLanguage "Catalan" ;
lin chinese_Language = mkLanguage "Chinese" ;
lin danish_Language = mkLanguage "Danish" ;
lin dutch_Language = mkLanguage "Dutch" ;
lin english_Language = mkLanguage "Euslish" ;
lin estonian_Language = mkLanguage "Estonian" ;
lin finnish_Language = mkLanguage "Finnish" ;
lin french_Language = mkLanguage "French" ;
lin german_Language = mkLanguage "German" ;
lin greek_Language = mkLanguage "Greek" ;
lin hebrew_Language = mkLanguage "Hebrew" ;
lin hindi_Language = mkLanguage "Hindi" ;
lin japanese_Language = mkLanguage "Japanese" ;
lin italian_Language = mkLanguage "Italian" ;
lin latin_Language = mkLanguage "Latin" ;
lin latvian_Language = mkLanguage "Latvian" ;
lin maltese_Language = mkLanguage "Maltese" ;
lin nepali_Language = mkLanguage "Nepali" ;
lin norwegian_Language = mkLanguage "Norwegian" ;
lin persian_Language = mkLanguage "Persian" ;
lin polish_Language = mkLanguage "Polish" ;
lin punjabi_Language = mkLanguage "Punjabi" ;
lin romanian_Language = mkLanguage "Romanian" ;
lin russian_Language = mkLanguage "Russian" ;
lin sindhi_Language = mkLanguage "Sindhi" ;
lin spanish_Language = mkLanguage "Spanish" ;
lin swahili_Language = mkLanguage "Swahili" ;
lin swedish_Language = mkLanguage "Swedish" ;
lin thai_Language = mkLanguage "Thai" ;
lin turkish_Language = mkLanguage "Turkish" ;
lin urdu_Language = mkLanguage "Urdu" ;

-}

lin
  hungry_VP = qualityVP "acrach" ;
  thirsty_VP = qualityVP "tartmhor" ;
  tired_VP = qualityVP "sgîth" ;
  scared_VP = qualityVP "fo eagal" ;
  ill_VP = qualityVP "tinn" ;
  ready_VP = qualityVP "deiseil" ;
  is_right_VP = qualityVP "ceart" ;
  is_wrong_VP = qualityVP "ceàrr" ;
  has_age_VP card = qualityVP (card.s ++ "bliadhna a dh'aois") ;

  timeunitAdv card unit = {s = "fad" ++ card.s ++ unit.s ! NOM ! Indef ! Pl} ;
  timeunitRange a b unit = {s = "bho" ++ a.s ++ "gu" ++ b.s ++ unit.s ! NOM ! Indef ! Pl} ;
  weekdayPunctualAdv w = {s = "air" ++ w.s ! NOM ! Indef ! Sg} ;
  weekdayHabitualAdv w = {s = "gach" ++ w.s ! NOM ! Indef ! Sg} ;
  weekdayLastAdv w = {s = w.s ! NOM ! Indef ! Sg ++ "seo chaidh"} ;
  weekdayNextAdv w = {s = w.s ! NOM ! Indef ! Sg ++ "seo tighinn"} ;
  monthAdv m = {s = "anns an" ++ m.s ! NOM ! Indef ! Sg} ;
  yearAdv y = {s = "ann an" ++ linNP y} ;
  dayMonthAdv d m = {s = linNP d ++ m.s ! NOM ! Indef ! Sg} ;
  monthYearAdv m y = {s = m.s ! NOM ! Indef ! Sg ++ linNP y} ;
  dayMonthYearAdv d m y = {s = linNP d ++ m.s ! NOM ! Indef ! Sg ++ linNP y} ;
  intYear i = atomConstrNP i.s ;
  intMonthday i = atomConstrNP i.s ;
  weekdayN w = w ; monthN m = m ;
  weekdayPN w = {s = w.s ! NOM ! Indef ! Sg} ;
  monthPN m = {s = m.s ! NOM ! Indef ! Sg} ;

  second_Timeunit = mkN "diog" ; minute_Timeunit = mkN "mionaid" ;
  hour_Timeunit = mkN "uair" ; day_Timeunit = mkN "latha" ;
  week_Timeunit = mkN "seachdain" ; month_Timeunit = mkN "mìos" ; year_Timeunit = mkN "bliadhna" ;
  monday_Weekday = mkN "Diluain" ; tuesday_Weekday = mkN "Dimàirt" ;
  wednesday_Weekday = mkN "Diciadain" ; thursday_Weekday = mkN "Diardaoin" ;
  friday_Weekday = mkN "Dihaoine" ; saturday_Weekday = mkN "Disathairne" ; sunday_Weekday = mkN "Didòmhnaich" ;
  january_Month = mkN "Am Faoilleach" ; february_Month = mkN "An Gearran" ; march_Month = mkN "Am Màrt" ;
  april_Month = mkN "An Giblean" ; may_Month = mkN "An Cèitean" ; june_Month = mkN "An t-Ògmhios" ;
  july_Month = mkN "An t-Iuchar" ; august_Month = mkN "An Lùnastal" ; september_Month = mkN "An t-Sultain" ;
  october_Month = mkN "An Dàmhair" ; november_Month = mkN "An t-Samhain" ; december_Month = mkN "An Dùbhlachd" ;

  n_units_AP card unit adj = adj ** {
    s = \\f => card.s ++ unit.s ! NOM ! Indef ! Pl ++ adj.s ! f ;
    voc = \\g => card.s ++ unit.voc ! Pl ++ adj.voc ! g
    } ;
  n_units_of_NP card unit np = atomConstrNP (card.s ++ unit.s ! NOM ! Indef ! Pl ++ np.s ! Gen) ;
  n_unit_CN card unit cn = appendConstrCN cn (card.s ++ unit.s ! NOM ! Indef ! Sg) ;
  bottle_of_CN np = containerCN "botal" np ;
  cup_of_CN np = containerCN "cupa" np ;
  glass_of_CN np = containerCN "glainne" np ;

oper
  qualityVP : Str -> VP = \x -> lin VP (extendQuality x) ;
  extendQuality : Str -> ResGla.LinV = \x -> {
    s = "bi" ++ x ; conditional = table {ResGla.Sg => "bhiodh" ++ x ; ResGla.Pl => "bhiodh" ++ x} ;
    imperative = \\_,_ => "bi" ++ x ; future = \\_ => "bidh" ++ x ; past = \\_ => "bha" ++ x ;
    noun = "bhith" ++ x ; participle = "air a bhith" ++ x ;
    copular = True ; complement = x
    } ;
  atomConstrNP : Str -> NP = \x -> lin NP (ResGla.emptyNP ** {s = \\_ => x ; voc = x}) ;
  appendConstrCN : CN -> Str -> CN = \cn,x -> lin CN (cn ** {
    s = \\c,d,n => cn.s ! c ! d ! n ++ x ; voc = \\n => cn.voc ! n ++ x
    }) ;
  containerCN : Str -> NP -> CN = \x,np -> lin CN {
    s = \\_,_,_ => x ++ np.s ! ResGla.Gen ; voc = \\_ => x ++ np.s ! ResGla.Gen ; g = ResGla.Masc
    } ;
}
