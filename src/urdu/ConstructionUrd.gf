--# -path=.:../abstract

concrete ConstructionUrd of Construction = CatUrd ** 
  open SyntaxUrd, SymbolicUrd, ParadigmsUrd, CommonHindustani,
       (L = LexiconUrd), (E = ExtraUrd), (G = GrammarUrd), (R = ResUrd), (S = StructuralUrd), Prelude in {
flags coding=utf8 ;


lin
  hungry_VP = mkVP (mkA "بھوکا") ;
  thirsty_VP = mkVP (mkA "پیاسا") ;
  ill_VP = mkVP (mkA "بیمار") ;
  ready_VP = mkVP (mkA "تیار") ;

  has_age_VP card = mkVP (mkNP <card : Card> (mkCN (mmodN L.year_N))) ;

  have_name_Cl x y = mkCl (mkNP (E.GenNP x) L.name_N) (lin NP y) ;
  married_Cl x y = mkCl (lin NP x) L.married_A2 (lin NP y) ;

  what_name_QCl x = mkQCl (mkIComp whatSg_IP) (mkNP (E.GenNP x) L.name_N) ;
  how_old_QCl x = mkQCl (lin IAdv {s = "کتنی"}) (mkNP (E.GenNP x) (mkN "عمر" feminine)) ;
  how_far_QCl x = mkQCl (E.IAdvAdv (ParadigmsUrd.mkAdv "دور")) (lin NP x) ;

oper
  mmodN : N -> N = \noun -> lin N {s = \\n,c => noun.s ! n ! c ++ "کا" ; g = noun.g} ;

---- oper
----  mmodN : N -> N = \noun -> lin N {s = \\n,c =>noun.s!n!c++"का" ; g =noun.g} ;

-- some more things
----  weather_adjCl ap = mkCl (mkVP (lin AP ap)) ;
   
----  is_right_VP = mkVP (ParadigmsUrd.mkA "right") ;
----  is_wrong_VP = mkVP (ParadigmsUrd.mkA "wrong") ;

lin
  n_units_AP card cn a = {
    s = \\n,g,c,d => card.s ++ cn.s ! Pl ! Dir ++ a.s ! n ! g ! c ! d
    } ;

  n_units_of_NP card cn np = {
    s = \\c => np.s ! NPC Obl ++ "کے" ++ card.s ++ cn.s ! Pl ! constructionCase c ;
    a = agrP3 cn.g card.n
    } ;


----  bottle_of_CN np = mkCN (lin N2 (mkN2 "bottle")) (lin NP np) ;
  cup_of_CN np = {
    s = \\n,c => np.s ! NPC Obl ++ "کا" ++ (mkN "پیالہ").s ! n ! c ;
    g = Masc
    } ;
----  glass_of_CN  np = mkCN (lin N2 (mkN2 "glass"))  (lin NP np) ;

{-
-- spatial deixis and motion verbs

  where_go_QCl np = mkQCl where_IAdv (mkCl np (mkVP L.go_V)) ;
  where_come_from_QCl np =  mkQCl from_where_IAdv (mkCl np (mkVP L.go_V)) ;
  
  go_here_VP = mkVP (mkVP L.go_V) S.here_Adv ;
  come_here_VP = mkVP (mkVP L.come_V) S.here_Adv ;
  come_from_here_VP = mkVP (mkVP L.come_V) (mkAdv "from here") ;

  go_there_VP = mkVP (mkVP L.go_V)  S.there_Adv ;
  come_there_VP = mkVP (mkVP L.come_V) S.there_Adv ;
  come_from_there_VP = mkVP (mkVP L.come_V) (mkAdv "from there") ;

--TODO "where did X come from" instead of "from where did X come"
oper from_where_IAdv : IAdv = lin IAdv (ss "from where") ;

-}


lincat
  Weekday = N ;
  Monthday = NP ;
  Month = N ;
  Year = NP ;

lin
  weekdayPunctualAdv w = {s = \\_ => w.s ! Sg ! Dir ++ "کو"} ;
  weekdayHabitualAdv w = {s = \\_ => "ہر" ++ w.s ! Sg ! Dir ++ "کو"} ;
  weekdayNextAdv w = {s = \\_ => "اگلے" ++ w.s ! Sg ! Dir ++ "کو"} ;
  weekdayLastAdv w = {s = \\_ => "پچھلے" ++ w.s ! Sg ! Dir ++ "کو"} ;

  monthAdv m = {s = \\_ => m.s ! Sg ! Dir ++ "میں"} ;
  yearAdv y = {s = \\_ => y.s ! NPC Dir ++ "میں"} ;
{- ----
  dayMonthAdv d m = ParadigmsUrd.mkAdv ("on" ++ d.s ! R.NPAcc ++ m.s ! R.Sg ! R.Nom) ; -- on 17 May
  monthYearAdv m y = SyntaxUrd.mkAdv in_Prep (mkNP (mkCN m y)) ; -- in May 2012
  dayMonthYearAdv d m y = ParadigmsUrd.mkAdv ("on" ++ d.s ! R.NPAcc ++ m.s ! R.Sg ! R.Nom ++ y.s ! R.NPAcc) ; -- on 17 May 2013
-}
  intYear = symb ;
  intMonthday = symb ;

lincat Language = PN ;

lin InLanguage l = SyntaxUrd.mkAdv in_Prep (mkNP l) ; ----

lin
  weekdayN w = w ;
  monthN m = m ;

----  weekdayPN w = mkPN w ;
----  monthPN m = mkPN m ;

  languagePN l = l ;

oper mkLanguage : Str -> PN = \s -> mkPN s ;

oper constructionCase : NPCase -> Case = \c -> case c of {
  NPC x => x ; NPObj => Obl ; NPErg => Obl
  } ;

----------------------------------------------
---- lexicon of special names

lin monday_Weekday = mkN "پیر" ;
lin tuesday_Weekday = mkN "منگل" ;
lin wednesday_Weekday = mkN "بدھ" ;
lin thursday_Weekday = mkN "جمعرات" ;
lin friday_Weekday = mkN "جمعہ" ;
lin saturday_Weekday = mkN "ہفتہ" ;
lin sunday_Weekday = mkN "اتوار" ;

lin january_Month = mkN "جنوری" ;
lin february_Month = mkN "فروری" ;
lin march_Month = mkN "مارچ" ;
lin april_Month = mkN "اپریل" ;
lin may_Month = mkN "مئی" ;
lin june_Month = mkN "جون" ;
lin july_Month = mkN "جولائی" ;
lin august_Month = mkN "اگست" ;
lin september_Month = mkN "ستمبر" ;
lin october_Month = mkN "اکتوبر" ;
lin november_Month = mkN "نومبر" ;
lin december_Month = mkN "دسمبر" ;

lin afrikaans_Language = mkLanguage "Afrikaans" ;
lin amharic_Language = mkLanguage "Amharic" ;
lin arabic_Language = mkLanguage "Arabic" ;
lin bulgarian_Language = mkLanguage "बल्गेरियाई" ;
lin catalan_Language = mkLanguage "Catalan" ;
lin chinese_Language = mkLanguage "चीनी" ;
lin danish_Language = mkLanguage "Danish" ;
lin dutch_Language = mkLanguage "Dutch" ;
lin english_Language = mkLanguage "अंग्रेज़ी" ;
lin estonian_Language = mkLanguage "Estonian" ;
lin finnish_Language = mkLanguage "Finnish" ;
lin french_Language = mkLanguage "फ्रेंच" ;
lin german_Language = mkLanguage "जर्मन" ;
lin greek_Language = mkLanguage "Greek" ;
lin hebrew_Language = mkLanguage "Hebrew" ;
lin hindi_Language = mkLanguage "हिंदी" ;
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


}
