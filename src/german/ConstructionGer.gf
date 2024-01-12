--# -path=.:../abstract

concrete ConstructionGer of Construction = CatGer **
  open SyntaxGer, SymbolicGer, (P = ParadigmsGer),
       (L = LexiconGer), (E = ExtendGer), (G = GrammarGer), (I = IrregGer), (R = ResGer), (N = NounGer), Prelude in {
flags coding=utf8 ;

oper
  mkPrep : Str -> P.Case -> Prep = P.mkPrep ;
  mkV2 : V -> V2 = P.mkV2 ;
  accPrep = P.accPrep ;
  datPrep = P.datPrep ;
  anDat_Prep = P.anDat_Prep ;
  inDat_Prep = P.inDat_Prep ;

  dative = P.dative ;
  accusative = P.accusative ;
  feminine = P.feminine ;
  neuter = P.neuter ;
  regV = P.regV ;
  invarA = P.invarA ;

lin
  hungry_VP = mkVP (P.mkA "hungrig") ;
  thirsty_VP = mkVP (P.mkA "durstig") ;
  tired_VP = mkVP (P.mkA "müde") ;
  scared_VP = mkVP have_V2 (mkNP (P.mkN "Angst" "Ängste" feminine)) ;
  ill_VP = mkVP (P.mkA "krank") ;
  ready_VP = mkVP (P.mkA "bereit") ;

  has_age_VP card = mkVP (lin AP (mkAP (lin AdA (mkUtt (mkNP <lin Card card : Card> L.year_N))) L.old_A)) ;

  have_name_Cl x y = mkCl (lin NP x) (mkV2 I.heißen_V) (lin NP y) ;
  married_Cl x y = ----mkCl (lin NP x) L.married_A2 (lin NP y) | 
                   mkCl (mkNP and_Conj (lin NP x) (lin NP y)) (P.mkA "verheiratet") ;

  what_name_QCl x = mkQCl how_IAdv (mkCl (lin NP x) I.heißen_V) ;
----  how_old_QCl x = mkQCl (E.ICompAP (mkAP L.old_A)) (lin NP x) ; ---- compilation slow
  how_old_QCl x = mkQCl (E.IAdvAdv (P.mkAdv "alt")) (mkCl (lin NP x) G.UseCopula) ; ----
  how_far_QCl x = mkQCl (E.IAdvAdv L.far_Adv) (mkCl (mkVP (SyntaxGer.mkAdv to_Prep (lin NP x)))) ;

-- some more things
  weather_adjCl ap = mkCl (mkVP (lin AP ap)) ;
   
  is_right_VP = mkVP have_V2 (mkNP (P.mkN "Recht")) ;
  is_wrong_VP = mkVP have_V2 (mkNP (P.mkN "Unrecht")) ;

  n_units_AP card cn a = mkAP (lin AdA (mkUtt (mkNP <lin Card card : Card> (lin CN cn)))) (lin A a) ;
  n_unit_CN card unit cn = mkCN (invarA (mkUtt (mkNP <lin Card card : Card> (lin CN unit))).s) cn ;

  bottle_of_CN np = N.ApposCN (mkCN (P.mkN "Flasche")) np ;
  cup_of_CN np    = N.ApposCN (mkCN (P.mkN "Tasse"))   np ;
  glass_of_CN np  = N.ApposCN (mkCN (P.mkN "Glas" "Gläser" neuter)) np ;

  few_X_short_of_Y np x y =  -- np.dat fehlen (wenige x).nom an y
    let
      xs : NP = (mkNP G.few_Det x) ;
      ys : NP = (mkNP G.IndefArt y) ;
      fehlen_V3 : V3 = P.mkV3 (regV "fehlen") datPrep (mkPrep "an" dative) ;
      vp : VP = mkVP (mkVPSlash fehlen_V3 np) ys
    in
      mkS (mkCl xs vp) ;

-- spatial deixis and motion verbs
  where_go_QCl np = mkQCl (lin IAdv (ss "wohin")) (mkCl np (mkVP L.go_V)) ;
  where_come_from_QCl np =  mkQCl (lin IAdv (ss "woher")) (mkCl np (mkVP L.come_V)) ;
  
  go_here_VP = mkVP (mkVP L.go_V) (ParadigmsGer.mkAdv "her") ;
  come_here_VP = mkVP (mkVP L.come_V) (ParadigmsGer.mkAdv "her") ;
  come_from_here_VP = mkVP (mkVP L.come_V) (ParadigmsGer.mkAdv "von hier") ;

  go_there_VP = SyntaxGer.mkVP (SyntaxGer.mkVP L.go_V) (ParadigmsGer.mkAdv "hin") ;
  come_there_VP = mkVP (mkVP L.come_V) (ParadigmsGer.mkAdv "hin") ;
  come_from_there_VP = mkVP (mkVP L.come_V) (ParadigmsGer.mkAdv "von dort") ;

lincat
  Timeunit = N ;
  Hour = {short:Str ; long:Str ; adv:Adv} ;
  Weekday = N ;
  Monthday = NP ;
  Month = N ;
  Year = NP ;

-- timeunitAdv   : Card -> Timeunit -> Adv ; -- (for) three hours
-- timeunitRange : Card -> Card -> Timeunit -> Adv ; -- (cats live) ten to twenty years
lin
    timeunitAdv n time =
      let n_hours_NP : NP = mkNP n time
      in  SyntaxGer.mkAdv (for_Prep | accPrep) n_hours_NP ;

    timeunitRange l u time =
      {s = l.s ! R.AMod (R.gennum R.Masc l.n) R.Nom ++ "bis" ++ u.s ! R.AMod (R.gennum R.Masc u.n) R.Nom ++ time.s ! R.Pl ! R.Nom} ;

  oper
    mkHour : Str -> Str -> Str -> Hour
    = \n,m,daytime ->
      let numeral : Str -> Str = \k -> (SyntaxGer.mkUtt (SyntaxGer.mkCard k)).s
      in lin Hour {short = numeral n ; long = numeral m ; adv = P.mkAdv daytime} ;

lin
    oneHour         = mkHour "1" "1" "nachts" ;
    twoHour         = mkHour "2" "2" "nachts" ;
    threeHour       = mkHour "3" "3" "nachts" ;
    fourHour        = mkHour "4" "4" "morgens" ;
    fiveHour        = mkHour "5" "5" "morgens" ;
    sixHour         = mkHour "6" "6" "morgens" ;
    sevenHour       = mkHour "7" "7" "morgens" ;
    eightHour       = mkHour "8" "8" "vormittags" ;
    nineHour        = mkHour "9" "9" "vormittags" ;
    tenHour         = mkHour "10" "10" "vormittags" ;
    elevenHour      = mkHour "11" "11" "vormittags" ;
    twelveHour      = mkHour "12" "12" "mittags" ;
    thirteenHour    = mkHour "13" "1" "mittags" ;
    fourteenHour    = mkHour "14" "2" "mittags" ;
    fifteenHour     = mkHour "15" "3" "nachmittags" ;
    sixteenHour     = mkHour "16" "4" "nachmittags" ;
    seventeenHour   = mkHour "17" "5" "nachmittags" ;
    eighteenHour    = mkHour "18" "6" "nachmittags" ;
    nineteenHour    = mkHour "19" "7" "abends" ;
    twentyHour      = mkHour "20" "8" "abends" ;
    twentyOneHour   = mkHour "21" "9" "abends" ;
    twentyTwoHour   = mkHour "22" "10" "abends" ;
    twentyThreeHour = mkHour "23" "11" "abends" ;
    twentyFourHour  = mkHour "24" "12" "nachts" ;

  -- timeHour : Hour -> Adv  -- at three a.m./p.m.
                             -- um drei Uhr nachts/nachmittags

  timeHour h = let ada : AdA = lin AdA {s = "um" ++ h.long ++ "Uhr"}
               in SyntaxGer.mkAdv ada h.adv ;

  -- timeHourMinute : Hour -> Card -> Adv ; -- at six forty a.m./p.m.
                                            -- um sechs/achtzehn Uhr vierzig
  timeHourMinute h card =
    let min : Str = (SyntaxGer.mkUtt card).s
    in P.mkAdv ("um" ++ h.short ++ "Uhr" ++ min) ;

{- -- Remark (HL 7/2023):
-- To avoid massive overgeneration, we'd better replace Card here by
  cat
    Minute ;
  fun
    timeHourMinute : Hour -> Minute -> Adv ; -- at six forty a.m./p.m.
    min_1 : Minute ;
    min_2 : Minute ; -- ... min_60 : Minute ;
    lastMinute : Minute ;

  oper
    Min : PType = Predef.Ints 3 ; -- short for 60
    minutes : Min => Str = table {0 => "0" ; 1 => "1" ; 2 => "2" ; 3 => "3"} ;
    mkMinute : Min -> Minute = \j -> lin Minute {s = minutes ! j ; i = j} ;

  lincat
    Minute = { s : Str ; i : Min } ;
  lin
    min_1 = mkMinute 1 ;
    min_2 = mkMinute 2 ;
    lastMinute = mkMinute 3 ;
    timeHourMinute h m = P.mkAdv ("um" ++ h.short ++ ":" ++ m.s ++ "Uhr") ;

-- But this definition of timeHourMinute causes a compiler error! In
--   timeHourMinute = \h,m -> mkAdv (... ++ m.s ++ ..)
-- the argument m is not really restricted to Min, but an unbounded Int, so
--   m.s = {s = minutes ! j ; i = j}.s = (table (Ints 3) [...]) ! j
-- cannot be reduced in Compute.ConcreteNew, as *the compiler does not enforce*
-- 0 =< j =< 3.
-}

lin
  weekdayPunctualAdv w = SyntaxGer.mkAdv anDat_Prep (mkNP the_Det w) ;         -- am Montag
  weekdayHabitualAdv w = SyntaxGer.mkAdv (mkPrep "" accusative) (mkNP every_Det w) ;            ---- jeden Montag
  weekdayLastAdv w = SyntaxGer.mkAdv (mkPrep "am letzten" dative) (mkNP w)  ; -- letzten Montag ----
  weekdayNextAdv w = SyntaxGer.mkAdv (mkPrep "am nächsten" dative) (mkNP w)  ; -- nächsten Montag ----

  monthAdv m = SyntaxGer.mkAdv inDat_Prep (mkNP the_Det m) ;
  yearAdv y = SyntaxGer.mkAdv (mkPrep "im Jahr" dative) y ; ----
  dayMonthAdv d m = ParadigmsGer.mkAdv ("am" ++ d.s ! True ! dative ++ BIND ++ "." ++ m.s ! R.Sg ! R.Nom) ; -- am 17. Mai
  monthYearAdv m y = SyntaxGer.mkAdv inDat_Prep (mkNP the_Det (mkCN m y)) ; -- im Mai 2012
  dayMonthYearAdv d m y = ParadigmsGer.mkAdv ("am" ++ d.s ! True ! dative ++ BIND ++ "." ++ m.s ! R.Sg ! R.Nom ++ y.s ! True ! accusative) ; -- am 17. Mai 2013

  intYear = symb ;
  intMonthday = symb ;

lincat Language = N ;

lin InLanguage l = SyntaxGer.mkAdv on_Prep (mkNP l) ;

lin
  weekdayN w = w ;
  monthN m = m ;

  weekdayPN w = P.mkPN w ;
  monthPN m = P.mkPN m ;

  languageNP l = mkNP l ;
  languageCN l = mkCN l ;

oper mkLanguage : Str -> N = \s -> P.mkN s neuter ; ---- produces Neuter

----------------------------------------------
---- lexicon of special names

lin second_Timeunit = P.mkN "Sekunde" ;
lin minute_Timeunit = P.mkN "Minute" ;
lin hour_Timeunit = P.mkN "Stunde" ;
lin day_Timeunit = P.mkN "Tag" ;
lin week_Timeunit = P.mkN "Woche" ;
lin month_Timeunit = P.mkN "Monat";
lin year_Timeunit = P.mkN "Jahr" "Jahre" neuter ;

lin monday_Weekday = P.mkN "Montag" ;
lin tuesday_Weekday = P.mkN "Dienstag" ;
lin wednesday_Weekday = P.mkN "Mittwoch" ;
lin thursday_Weekday = P.mkN "Donnerstag" ;
lin friday_Weekday = P.mkN "Freitag" ;
lin saturday_Weekday = P.mkN "Samstag" ;
lin sunday_Weekday = P.mkN "Sonntag" ;

lin january_Month = P.mkN "Januar" ;
lin february_Month = P.mkN "Februar" ;
lin march_Month = P.mkN "März" ;
lin april_Month = P.mkN "April" ;
lin may_Month = P.mkN "Mai" ;
lin june_Month = P.mkN "Juni" ;
lin july_Month = P.mkN "Juli" ;
lin august_Month = P.mkN "August" ;
lin september_Month = P.mkN "September" ;
lin october_Month = P.mkN "Oktober" ;
lin november_Month = P.mkN "November" ;
lin december_Month = P.mkN "Dezember" ;

lin afrikaans_Language = mkLanguage "Afrikaans" ;
lin amharic_Language = mkLanguage "Amharisch" ;
lin arabic_Language = mkLanguage "Arabisch" ;
lin bulgarian_Language = mkLanguage "Bulgarisch" ;
lin catalan_Language = mkLanguage "Katalanish" ;
lin chinese_Language = mkLanguage "Chinesisch" ;
lin danish_Language = mkLanguage "Dänisch" ;
lin dutch_Language = mkLanguage "Holländisch" ;
lin english_Language = mkLanguage "Englisch" ;
lin estonian_Language = mkLanguage "Estnisch" ;
lin finnish_Language = mkLanguage "Finnisch" ;
lin french_Language = mkLanguage "Französisch" ;
lin german_Language = mkLanguage "Deutsch" ;
lin greek_Language = mkLanguage "Griechisch" ;
lin hebrew_Language = mkLanguage "Hebräisch" ;
lin hindi_Language = mkLanguage "Hindi" ;
lin japanese_Language = mkLanguage "Japanisch" ;
lin italian_Language = mkLanguage "Italienisch" ;
lin latin_Language = mkLanguage "Latein" ;
lin latvian_Language = mkLanguage "Lettisch" ;
lin maltese_Language = mkLanguage "Maltesisch" ;
lin nepali_Language = mkLanguage "Nepali" ;
lin norwegian_Language = mkLanguage "Norwegisch" ;
lin persian_Language = mkLanguage "Persisch" ;
lin polish_Language = mkLanguage "Polnisch" ;
lin punjabi_Language = mkLanguage "Punjabi" ;
lin romanian_Language = mkLanguage "Rumänisch" ;
lin russian_Language = mkLanguage "Russisch" ;
lin sindhi_Language = mkLanguage "Sindhi" ;
lin spanish_Language = mkLanguage "Spanisch" ;
lin swahili_Language = mkLanguage "Swahili" ;
lin swedish_Language = mkLanguage "Schwedisch" ;
lin thai_Language = mkLanguage "Thai" ;
lin turkish_Language = mkLanguage "Türkisch" ;
lin urdu_Language = mkLanguage "Urdu" ;
  
}
