concrete ConstructionEus of Construction = CatEus **
  open Prelude, ResEus, ParadigmsEus, (G=GrammarEus) in {
	
lincat
  Timeunit = N ;
  Weekday = N ;
  Monthday = NP ;
  Month = N ;
  Year = NP ;

lin
  weekdayN w = w ;
  monthN m = m ;

  monday_Weekday = mkN "astelehen" ;
  tuesday_Weekday = mkN "astearte" ;
  wednesday_Weekday = mkN "asteazken" ;
  thursday_Weekday = mkN "ostegun" ;
  friday_Weekday = mkN "ostiral" ;
  saturday_Weekday = mkN "larunbat" ;
  sunday_Weekday = mkN "igande" ;

  january_Month = mkN "urtarril" ;
  february_Month = mkN "otsail" ;
  march_Month = mkN "martxo" ;
  april_Month = mkN "apiril" ;
  may_Month = mkN "maiatz" ;
  june_Month = mkN "ekain" ;
  july_Month = mkN "uztail" ;
  august_Month = mkN "abuztu" ;
  september_Month = mkN "irail" ;
  october_Month = mkN "urri" ;
  november_Month = mkN "azaro" ;
  december_Month = mkN "abendu" ;

  weekdayPunctualAdv w = {s = w.s ++ artDef ! Sg ! Ine ! w.ph} ;
  weekdayHabitualAdv w = {s = w.s ++ artDef ! Pl ! Ine ! w.ph} ;
  weekdayNextAdv w = {s = "hurrengo" ++ w.s ++ artDef ! Sg ! Abs ! w.ph} ;
  weekdayLastAdv w = {s = "joan den" ++ w.s ++ artDef ! Sg ! Abs ! w.ph} ;
  monthAdv m = {s = m.s ++ artDef ! Sg ! Ine ! m.ph} ;
  yearAdv y = {s = glue y.stem "an"} ;
  intYear i = lin Year (invariantNP i.s) ;
  intMonthday i = lin Monthday (invariantNP i.s) ;

  ready_VP = G.UseComp (G.CompAP (G.PositA (mkA "prest"))) ;
  has_age_VP card =
    (useV {prc = \\_ => [] ; nstem = "izate" ; val = Du Ukan}) ** {
      dobj = {s = \\_ => card.s ++ "urte" ; agr = Hauek ; isDef = True}
      } ;

  cup_of_CN np = G.PartNP (useN (mkNoun "kikara")) np ;

  n_units_AP card unit a = {
    s = \\_ => card.s ++ linCNIndef unit ++ a.s ! AF Posit ;
    ph = a.ph ; typ = Bare
    } ;

  n_units_of_NP card unit np = lin NP (np ** {
    s = \\c => card.s ++ linCNIndef unit ++ np.s ! c ;
    stem = card.s ++ linCNIndef unit ++ np.stem ; agr = Hauek
    }) ;

oper
  invariantNP : Str -> NP = \s -> lin NP {
    s = \\_ => s ; stem = s ; agr = Hau ; anim = Inan ; isDef = True
    } ;
{-
lin

  timeunitAdv n time = 
  let n_card : Card   = n ;
      n_hours_NP : NP = mkNP n_card time ;
  in  SyntaxEus.mkAdv for_Prep n_hours_NP | mkAdv (n_hours_NP.s ! R.npNom) ;

  weekdayPunctualAdv w = ;         -- on Sunday
  weekdayHabitualAdv w = ;         -- on Sundays
  weekdayNextAdv w =      -- next Sunday
  weekdayLastAdv w =      -- last Sunday

  monthAdv m = mkAdv in_Prep (mkNP m) ;
  yearAdv y = mkAdv in_Prep y ;
  dayMonthAdv d m  =  ; -- on 17 May
  monthYearAdv m y =  ; -- in May 2012
  dayMonthYearAdv d m y =  ; -- on 17 May 2013

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

—}
}
-}
}
