concrete ConstructionAra of Construction = CatAra ** open
  Prelude,
  ParadigmsAra,
  SyntaxAra,
  SymbolicAra,
  StructuralAra,
  (E=ExtendAra),
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

  weekdayPunctualAdv w = ParadigmsAra.mkAdv ((mkNP w).s ! R.Nom) ;  -- on Sunday
--  weekdayPunctualAdv w = SyntaxAra.mkAdv noPrep (mkNP w) ;  -- on Sunday
  -- TODO
  weekdayHabitualAdv,                    -- on Sundays
  weekdayNextAdv,                        -- next Sunday
  weekdayLastAdv = weekdayPunctualAdv ;  -- last Sunday

  monthAdv january =
    let january_CN : CN = mkCN month_Timeunit (mkNP (mkPN january)) ;
        january_NP : NP = R.emptyNP **
           {s = \\c => R.cn2str january_CN R.Sg R.Const c ;
            a = {pgn = R.Per3 january_CN.g R.Sg ; isPron = False}} ;
     in SyntaxAra.mkAdv R.biPrep january_NP ;

  yearAdv y = SyntaxAra.mkAdv in_Prep y ;

  -- dummy
  dayMonthAdv d m  = SyntaxAra.mkAdv on_Prep (mkNP d) ; -- on 17 May
  monthYearAdv m y = SyntaxAra.mkAdv on_Prep (mkNP m) ; -- in May 2012
  dayMonthYearAdv d m y = SyntaxAra.mkAdv on_Prep y ; -- on 17 May 2013

  intYear = symb ;
  intMonthday = symb ;

  -- : Card -> CN -> A -> AP
  n_units_AP card cn a =
    let ap = mkAP a in ap ** {
      s = \\s,g,n,d,c =>
           ap.s ! s ! g ! n ! d ! c
        ++ (mkAdv R.biPrep (mkNP amount_N)).s
        ++ (mkNP card cn).s ! R.Bare ---- ? /IL
      } ;

oper

  amount_N : N = mkN "مِقْدَار" "مَقَادِير‎" masc nohum ;

  -- hack used in the name constructions
  toNP : Bool -> NP -> NP = \b -> if_then_else NP b R.emptyNP ;

lin
  -- : NP -> NP -> Cl
  have_name_Cl np nm =
    let subjPron : Pron = R.np2pron np ;
        me : NP = toNP np.a.isPron np ;
        myName : NP = E.ApposNP me (mkNP (mkDet subjPron) L.name_N) ;
     in mkCl myName nm ;

  -- : NP -> QCl
  what_name_QCl np =
    let subjPron : Pron = R.np2pron np ;
        me : R.NP = toNP np.a.isPron np ;
        myName : NP = E.ApposNP me (mkNP (mkDet subjPron) L.name_N) ;
        what_IP : R.IP = R.mkIP "مَا هُوَ" R.Sg ;
     in mkQCl what_IP myName ;

  how_old_QCl np =
    let subjPron : Pron = R.np2pron np ;
        me : R.NP = toNP np.a.isPron np ;
        age_N = mkN "عُمر" ;
        myAge : NP = E.ApposNP me (mkNP (mkDet subjPron) L.name_N) ;
     in mkQCl what_IP myAge ;

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
---- lexicon of snpcial names

-- TODO in arabic
lin second_Timeunit = mkN "second" ;
lin minute_Timeunit = mkN "minute" ;
lin hour_Timeunit = mkN "hour" ;
lin day_Timeunit = mkN "يَوْم" ;
lin week_Timeunit = mkN "week" ;
lin month_Timeunit = mkN "شَهْر" "أَشْهُر" masc nohum ;
lin year_Timeunit = mkN "year" ;

lin monday_Weekday = mkN day_Timeunit (mkN "إثْنَيْن") ;
lin tuesday_Weekday = mkN day_Timeunit (mkN "ثُلَاثَاء") ;
lin wednesday_Weekday = mkN day_Timeunit (mkN "أَرْبَعَاء") ;
lin thursday_Weekday = mkN day_Timeunit (mkN "خَمِيس") ;
lin friday_Weekday = mkN day_Timeunit (mkN "جُمْعَة") ;
lin saturday_Weekday = mkN day_Timeunit (mkN "سَبْت") ;
lin sunday_Weekday = mkN day_Timeunit (mkN "أَحَد") ;

lin january_Month = mkN (mkN "كَانُون") (mkAP (mkOrd (mkNumeral n2_Unit))) ;
lin february_Month = mkN "شُبَاط" ;
lin march_Month = mkN "آذَار" ;
lin april_Month = mkN "نَيْسَان" ;
lin may_Month = mkN "أَيَّار" ;
lin june_Month = mkN "حَزِيرَان" ;
lin july_Month = mkN "تَمُّوز" ;
lin august_Month = mkN "آب" ;
lin september_Month = mkN "أَيْلُول" ;
lin october_Month = mkN (mkN "تِشْرِين") (mkAP (mkOrd (mkNumeral n1_Unit))) ;
lin november_Month = mkN (mkN "تِشْرِين") (mkAP (mkOrd (mkNumeral n2_Unit))) ;
lin december_Month = mkN (mkN "كَانُون") (mkAP (mkOrd (mkNumeral n1_Unit))) ;

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
