concrete ConstructionAra of Construction = CatAra ** open 
  ParadigmsAra,
  SyntaxAra,
  SymbolicAra,
  StructuralAra,
  (R=ResAra),
  (L=LexiconAra) in {
	
lincat
  Timeunit = N ;
  Weekday = N ;
  Monthday = NP ;
  Month = N ;
  Year = NP ;

lin

  timeunitAdv n time = 
  let n_card : Card   = n ;
      n_hours_NP : NP = mkNP n_card time ;
  in  SyntaxAra.mkAdv during_Prep n_hours_NP | ParadigmsAra.mkAdv (n_hours_NP.s ! R.Nom) ;

  -- random guesses
  weekdayPunctualAdv w = SyntaxAra.mkAdv on_Prep (mkNP w) ;         -- on Sunday
  weekdayHabitualAdv w = SyntaxAra.mkAdv on_Prep (mkNP w) ;         -- on Sundays
  weekdayNextAdv w = SyntaxAra.mkAdv on_Prep (mkNP w) ;      -- next Sunday
  weekdayLastAdv w = SyntaxAra.mkAdv on_Prep (mkNP w) ;     -- last Sunday

  monthAdv m = SyntaxAra.mkAdv in_Prep (mkNP m) ;
  yearAdv y = SyntaxAra.mkAdv in_Prep y ;

  -- dummy
  dayMonthAdv d m  = SyntaxAra.mkAdv on_Prep (mkNP d) ; -- on 17 May
  monthYearAdv m y = SyntaxAra.mkAdv on_Prep (mkNP m) ; -- in May 2012
  dayMonthYearAdv d m y = SyntaxAra.mkAdv on_Prep y ; -- on 17 May 2013

  intYear = symb ;
  intMonthday = symb ;

--  n_units_AP

  -- : NP -> NP -> Cl
  have_name_Cl pe nm = 
    let subjPron : Pron = case pe.a.isPron of {
          True  => pe ;
          False => case (R.pgn2gn pe.a.pgn).g of {
                     R.Fem  => she_Pron ;
                     R.Masc => he_Pron }
        } ;

        myName : NP = mkNP (mkDet subjPron) L.name_N ;
     in mkCl myName nm ; --TODO: now it only works for pronouns, drops the NP

--  what_name_QCl = 

--  how_old_QCl

--  hungry_VP =
--  thirsty_VP =

lincat Language = N ;

lin InLanguage l = mkAdv in_Prep (mkNP l) ;

lin
  weekdayN w = w ;
  monthN m = m ;

  weekdayPN w = mkPN w ;
  monthPN m = mkPN m ;

  languageCN l = mkCN l ;
  languageNP l = mkNP l ;


oper mkLanguage : Str -> N = mkN ;

----------------------------------------------
---- lexicon of special names

-- TODO in arabic
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

-- lin afrikaans_Language = mkLanguage "Afrikaans" ;
-- lin amharic_Language = mkLanguage "Amharic" ;
lin arabic_Language = mkLanguage "عَرَبِيَّة" ;
-- lin bulgarian_Language = mkLanguage "Bulgarian" ;
-- lin catalan_Language = mkLanguage "Catalan" ;
-- lin chinese_Language = mkLanguage "Chinese" ;
-- lin danish_Language = mkLanguage "Danish" ;
-- lin dutch_Language = mkLanguage "Dutch" ;
lin english_Language = mkLanguage "إنْجلِيزيْة" ;
-- lin estonian_Language = mkLanguage "Estonian" ;
lin finnish_Language = mkLanguage "فِنْلَنْدِيّة" ;
-- lin french_Language = mkLanguage "French" ;
-- lin german_Language = mkLanguage "German" ;
-- lin greek_Language = mkLanguage "Greek" ;
-- lin hebrew_Language = mkLanguage "Hebrew" ;
-- lin hindi_Language = mkLanguage "Hindi" ;
-- lin japanese_Language = mkLanguage "Japanese" ;
-- lin italian_Language = mkLanguage "Italian" ;
-- lin latin_Language = mkLanguage "Latin" ;
-- lin latvian_Language = mkLanguage "Latvian" ;
-- lin maltese_Language = mkLanguage "Maltese" ;
-- lin nepali_Language = mkLanguage "Nepali" ;
-- lin norwegian_Language = mkLanguage "Norwegian" ;
lin persian_Language = mkLanguage "فَارِسيّة" ;
-- lin polish_Language = mkLanguage "Polish" ;
-- lin punjabi_Language = mkLanguage "Punjabi" ;
-- lin romanian_Language = mkLanguage "Romanian" ;
-- lin russian_Language = mkLanguage "Russian" ;
-- lin sindhi_Language = mkLanguage "Sindhi" ;
-- lin spanish_Language = mkLanguage "Spanish" ;
-- lin swahili_Language = mkLanguage "Swahili" ;
lin swedish_Language = mkLanguage "سُويدِيّة" ;
-- lin thai_Language = mkLanguage "Thai" ;
-- lin turkish_Language = mkLanguage "Turkish" ;
-- lin urdu_Language = mkLanguage "Urdu" ;

}
