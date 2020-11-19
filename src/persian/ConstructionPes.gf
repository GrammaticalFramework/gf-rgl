--# -path=.:abstract:prelude:api:common
concrete ConstructionPes of Construction = CatPes ** open
  Prelude,
  ParadigmsPes,
  (P=ParadigmsPes),
  SyntaxPes,
  (S=SyntaxPes),
  SymbolicPes,
  StructuralPes,
  (E=ExtendPes),
  (R=ResPes),
  (L=LexiconPes) in {

lincat
  Timeunit = N ;
  Weekday = N ;
  Monthday = NP ;
  Month = N ;
  Year = NP ;
  Language = N ;


lin
  weekdayN w = w ;
  monthN m = m ;

  -- weekdayPN w = mkPN w ;
  -- monthPN m = mkPN m ;

  timeunitAdv n time =
  let n_card : Card   = n ;
      n_hours_NP : NP = mkNP n_card time ;
  in  S.mkAdv during_Prep n_hours_NP ; ---- /IL

  weekdayPunctualAdv w = lin Adv (S.mkUtt (mkNP w)) ;  -- on Sunday
  -- TODO
  weekdayHabitualAdv,                    -- on Sundays
  weekdayNextAdv,                        -- next Sunday
  weekdayLastAdv = weekdayPunctualAdv ;  -- last Sunday

  monthAdv january =
    let january_NP : NP = mkNP january ;
     in S.mkAdv in_Prep january_NP ;

  yearAdv y = S.mkAdv in_Prep y ;

  intYear = symb ;
  intMonthday = symb ;

  languageCN l = mkCN l ;
  languageNP l = mkNP l ;

  InLanguage l = S.mkAdv in_Prep (mkNP l) ;

  english_Language = mkLanguage "انگلیسی" ;
  finnish_Language = mkLanguage "فنلاند" ;
  swedish_Language = mkLanguage "سوئدی" ;
  arabicLanguage = mkLanguage "عربی" ;
  finnishLanguage = mkLanguage "فنلاندی" ;
  frenchLanguage = mkLanguage "فرانسوی" ;
  persianLanguage = mkLanguage "فارسی" ;
  romanianLanguage = mkLanguage "رومانیایی" ;
  polishLanguage = mkLanguage "لهستانی" ;
  bulgarianLanguage = mkLanguage "بلغاری" ;
  somaliLanguage = mkLanguage "سومالیایی" ;
  turkishLanguage = mkLanguage "ترکی" ;
  kurdishLanguage = mkLanguage "کردی" ;

  -- : Card -> CN -> A -> AP
  n_units_AP card cn a =
    let ap = mkAP a in ap ** {
      s = \\ez =>
           ap.s ! ez
        ++ (mkUtt (mkNP card cn)).s ---- just guessing /IL
      } ;

  hungry_VP = mkVP (mkA "گرسنه") ;
  thirsty_VP = mkVP (mkA "تشنه") ;
  have_name_Cl p n = mkCl (mkNP (E.GenNP p) L.name_N) n ;
  what_name_QCl p = mkQCl what_IAdv (mkNP (E.GenNP p) L.name_N) ;

  how_old_QCl p = mkQCl howMuchAge_IAdv (mkNP (mkNP (E.GenNP p)) (P.mkAdv "سال")) ;

oper
  howMuchAge_IAdv = lin IAdv {s = "چند"} ;
  what_IAdv = lin IAdv {s = "چه چیزی"} ;
  mkLanguage : Str -> N = mkN ;
}
