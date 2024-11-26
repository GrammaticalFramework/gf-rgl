concrete ConstructionRus of Construction = CatRus **
  open Predef, SyntaxRus, SymbolicRus, (P=ParadigmsRus), ResRus, Prelude,
    QuestionRus, SentenceRus, AdverbRus, AdjectiveRus, VerbRus,
    (L=LexiconRus), (T=TenseRus),
    (N=NounRus), IdiomRus, (EX=ExtraRus) in {

lin
  hungry_VP = mkVP (P.mkA "голодный" "" "1*a/c'" PrefShort) ;
  thirsty_VP = mkVP want_VV (mkVP (P.mkV Imperfective "пить" "пью" "пьёт")) ;
  tired_VP = mkVP (P.mkA "усталый" "" "1*a/c'" PreferFull) ;
  scared_VP = mkVP (P.mkV Imperfective "бояться" "боюсь" "боится") ;   -- intran
  ill_VP = mkVP ( P.mkA "больной" "" "1*b" PrefShort) ;
  ready_VP = mkVP L.ready_A ;

  is_right_VP = mkVP (P.mkA "правый" "" "1a'" PrefShort) ;
  is_wrong_VP = mkVP (P.mkA "неправый" "" "1a'" PrefShort) ;

  -- : NP -> NP -> Cl ;     -- x is married to y / x on naimisissa y:n kanssa (Fin)
  married_Cl np1 np2 =
    let married : A = case np1.a of {
      Ag (GSg Fem) _ => P.mkA "замужем" "" "0" ;
      _ => P.mkA "женатый" "" "1a" PrefShort
      } in
      let cc : Prep = case np1.a of {
        Ag (GSg Fem) _ => behind_Prep ;
        _ => on_Prep
      } in {
        subj=np1.s ! Nom ;
        adv=[];
        verb=copulaEll ;   -- ???
        dep=[] ;
        compl=\\_ => (PositA married).short ! np1.a ++ applyPrep cc np2 ;
        a=np1.a ; ---
        } ;

  -- : NP -> QCl ;          -- how far is x / quanto dista x (Ita)
  how_far_QCl np = QuestIComp (CompIAdv (AdvIAdv how_IAdv L.far_Adv)) np ;

  -- : NP -> QCl ;          -- what is x's name / wie heisst x (Ger)
  what_name_QCl np = QuestIAdv how_IAdv (GenericCl (ComplSlash (SlashV2a (P.mkV2 (P.mkV Imperfective "звать" "зову" "зовёт") Acc)) np)) ;

  -- : Card -> VP ;         -- x is y years old / x a y ans (Fre)
  has_age_VP card = {
    adv=\\a => [] ;
    verb=copulaEll ;
    dep=[] ;
    compl=\\p,a => (timeunitAdv card year_Timeunit).s;
    p = Pos
    } ;

  -- : NP -> QCl ;          -- how old is x / quanti anni ha x (Ita)
  how_old_QCl np = {
    subj=how8much_IAdv.s ;
    adv=[] ;
    compl=\\p => np.s ! Dat ++ "лет";
    verb=copulaEll ;
    dep=[] ;
    a=Ag (GSg Neut) P3
    } ;

  -- : AP -> Cl ;           -- it is warm / il fait chaud (Fre)
  weather_adjCl ap = {
    subj=[] ;
    adv=[] ;
    verb=copulaEll ;
    dep=ap.short ! Ag (GSg Neut) P3 ;
    compl=\\p=>[] ;
    a=Ag (GSg Neut) P3
    } ;

  -- : NP -> NP -> Cl ;     -- x's name is y / x s'appelle y (Fre)
  have_name_Cl np np1 = {
    subj=[];
    adv=np.s ! Gen ++ L.name_N.snom;
    verb=copulaEll ;   -- ???
    dep=[] ;
    compl=\\_ => np1.s ! Nom ;   -- ???
    a=Ag (GSg L.name_N.g) P3 ;
    } ;

  -- : NP -> CN ;       -- bottle of beer
  bottle_of_CN np = N.ComplN2 (P.mkN2 (P.mkN "бутылка" Fem Inanimate "3*a") part_Prep) np ;
  -- : NP -> CN ;       -- cup of tea
  cup_of_CN np = N.ComplN2 (P.mkN2 (P.mkN "чашка" Fem Inanimate) part_Prep) np ;
  -- : NP -> CN ;       -- glass of wine
  glass_of_CN np = N.ComplN2 (P.mkN2 (P.mkN "стакан") part_Prep) np ;

-- languages
lincat
  Language = N ;
  Timeunit = N ;
  Hour = Symb ;
  Weekday = N ;
  Monthday = NP ;
  Month = N ;
  Year = NP ;

oper
  mkLanguage = overload {
    mkLanguage : Str -> N = \s -> P.mkN (P.mkA s) Masc Animate ;
    mkLanguage : Str -> Str -> N = \s,zi -> P.mkN s Masc Inanimate zi;
  } ;
  mkHour : Str -> Symb = \s -> lin Symb {s=s} ;
  cardCN : Card -> N -> NP = \card,n -> (N.DetCN ((DetArtCard N.IndefArt card)**{type=NormalDet}) (N.UseN n)) ;

lin
  -- : Card -> Timeunit -> Adv ; -- (for) three hours
  timeunitAdv card time = P.mkAdv ((cardCN (lin Card card) time).s ! Nom) ;

  -- : Card -> Card -> Timeunit -> Adv ; -- (cats live) ten to twenty years
  timeunitRange l u time = P.mkAdv (applyPrep from_Prep (cardCN (lin Card l) (lin N (ellNoun time)))
    ++ applyPrep EX.on_to_Prep (cardCN (lin Card u) time)) ;

  oneHour = mkHour "1" ;
  twoHour = mkHour "2" ;
  threeHour = mkHour "3" ;
  fourHour = mkHour "4" ;
  fiveHour = mkHour "5" ;
  sixHour = mkHour "6" ;
  sevenHour = mkHour "7" ;
  eightHour = mkHour "8" ;
  nineHour = mkHour "9" ;
  tenHour = mkHour "10" ;
  elevenHour = mkHour "11" ;
  twelveHour = mkHour "12" ;
  thirteenHour = mkHour "13" ;
  fourteenHour = mkHour "14" ;
  fifteenHour = mkHour "15" ;
  sixteenHour = mkHour "16" ;
  seventeenHour = mkHour "17" ;
  eighteenHour = mkHour "18" ;
  nineteenHour = mkHour "19" ;
  twentyHour = mkHour "20" ;
  twentyOneHour = mkHour "21" ;
  twentyTwoHour = mkHour "22" ;
  twentyThreeHour = mkHour "23" ;
  twentyFourHour = mkHour "24" ;

  -- : Hour -> Adv ; -- at three a.m./p.m.
  timeHour h = P.mkAdv (in_Prep.s ++ h.s) ;

  -- : Hour -> Card -> Adv ; -- at six forty a.m./p.m.
  timeHourMinute h card = P.mkAdv ((timeHour h).s ++ BIND ++ ":" ++ BIND ++ card.s ! Neut ! Inanimate ! Nom) ;  -- TODO: 00?

  -- : Weekday -> Adv ;  -- on Monday
  weekdayPunctualAdv w = P.mkAdv (in_Prep.s ++ w.sacc) ;         -- on Sunday
  -- : Weekday -> Adv ;  -- on Mondays
  weekdayHabitualAdv w = P.mkAdv (EX.along_Prep.s ++ (w.pdat)) ; -- on Sundays
  -- : Weekday -> Adv ;      -- last Monday
  weekdayLastAdv w = P.mkAdv (EX.to2_Prep.s ++ (PositA (P.mkA "прошлый")).s ! GSg w.g ! Inanimate ! Acc ++ w.sacc) ;
  -- : Weekday -> Adv ;      -- next Monday
  weekdayNextAdv w = P.mkAdv (EX.to2_Prep.s ++ (PositA (P.mkA "следующий")).s ! GSg w.g ! Inanimate ! Acc ++ w.sacc) ;

  -- : Month -> Adv ;                        -- in June
  monthAdv month = P.mkAdv ("в" ++ month.sloc) ;
  -- : Year -> Adv ;                         -- in 1976
  yearAdv year = P.mkAdv ("в" ++ year.s ! Loc) ;

  -- : Monthday -> Month -> Adv ;            -- on 17 May
  dayMonthAdv monthday month = P.mkAdv (monthday.s ! Gen ++ month.sgen) ;
  -- : Month -> Year -> Adv ;                -- in May 2013
  monthYearAdv month year = P.mkAdv ("в" ++ month.sloc ++ year.s ! Gen ++ "года") ;
  -- : Monthday -> Month -> Year -> Adv ;    -- on 17 May 2013
  dayMonthYearAdv monthday month year = P.mkAdv (monthday.s ! Gen ++ month.sgen ++ year.s ! Gen ++ "года") ;

  -- : Int -> Year ;  -- (year) 1963
  intYear = symb ;
  -- : Int -> Monthday ; -- 31th (March)
  intMonthday = symb ;

  -- : Weekday -> N ; -- (this) Monday
  weekdayN wd = wd ;
  -- : Month -> N ;   -- (this) November
  monthN month = month ;

  -- : Weekday -> PN ; -- Monday (is free)
  weekdayPN wd = {s = (nounFormsNoun wd).s ! Sg; g = Neut; anim = Inanimate; n=Sg} ;
  -- : Month -> PN ;   -- March (is cold)
  monthPN month = {s = (nounFormsNoun month).s ! Sg; g = Neut; anim = Inanimate; n=Sg} ;

  -- : Card -> CN -> A -> AP ;  -- x inches long
  n_units_AP card cn a =
    let ap=adjFormsAdjective a in
    let as_n_units=(how_IAdv.s
      ++ card.s ! Neut ! Inanimate ! Nom
      ++ cn.s ! animNumSizeNum cn.anim Nom card.size ! (numSizeCase Nom card.size)) in {   --? Nom?
    s=\\gn,anim,cas=> ap.s!gn!anim!cas ++ as_n_units ;
    short=\\a=> ap.short ! a ++ as_n_units ;
    preferShort=PreferFull ;
    isPost=True
    } ;
  -- This does not work in Russian naturally
  -- : Card -> CN -> NP -> NP ;  -- x ounces of this flour
  n_units_of_NP card cn np = {
    s = \\cas => card.s ! Neut ! Inanimate ! cas
      ++ cn.s ! animNumSizeNum cn.anim cas card.size ! (numSizeCase cas card.size)
      ++ np.s ! Gen ;
    pron=False ;
    a = Ag (gennum cn.g (numSizeNumber card.size)) P3
    } ;
  -- : Card -> CN -> CN -> CN ;  -- x gallon bottle
  n_unit_CN card cn_unit cn = cn ** {
    s=\\n,cas=> cn.s ! n ! cas
      ++ "на"
      ++ card.s ! Neut ! Inanimate ! Nom
      ++ cn_unit.s ! animNumSizeNum cn_unit.anim Nom card.size ! numSizeCase Nom card.size
    } ;

----------------------------------------------
---- lexicon of special names

lin
  second_Timeunit = P.mkN "секунда" ;
  minute_Timeunit = P.mkN "минута" ;
  hour_Timeunit = P.mkN "час" Masc Inanimate "1c" ;
  day_Timeunit = L.day_N ;
  week_Timeunit = P.mkN "неделя" Fem Inanimate "2a" ;
  month_Timeunit = P.mkN "месяц" Masc Inanimate "5a" ;
  year_Timeunit = L.year_N ;

  monday_Weekday = P.mkN "понедельник" Masc Inanimate ;
  tuesday_Weekday = P.mkN "вторник" Masc Inanimate ;
  wednesday_Weekday = P.mkN "среда" Fem Inanimate ;
  thursday_Weekday = P.mkN "четверг" Masc Inanimate ;
  friday_Weekday = P.mkN "пятница" Fem Inanimate ;
  saturday_Weekday = P.mkN "суббота" Fem Inanimate ;
  sunday_Weekday = P.mkN "воскресенье" Neut Inanimate ;

  january_Month = P.mkN "январь" Masc Inanimate;
  february_Month = P.mkN "февраль" Masc Inanimate;
  march_Month = P.mkN "март" ;
  april_Month = P.mkN "апрель" Masc Inanimate "2a";
  may_Month = P.mkN "май" ;
  june_Month = P.mkN "июнь" Masc Inanimate ;
  july_Month = P.mkN "июль" Masc Inanimate ;
  august_Month = P.mkN "август" ;
  september_Month = P.mkN "сентябрь" Masc Inanimate ;
  october_Month = P.mkN "октябрь" Masc Inanimate ;
  november_Month = P.mkN "ноябрь" Masc Inanimate ;
  december_Month = P.mkN "декабрь" Masc Inanimate ;

-- : Language -> Adv ; -- in English, auf englisch, englanniksi, etc
lin
  InLanguage l = PrepNP on_Prep (mkNP l) ;
  languageCN l = mkCN l ;
  languageNP l = mkNP l ;

lin afrikaans_Language = mkLanguage "африкаанс" "1a" ;
lin amharic_Language = mkLanguage "амхарский" ;
lin arabic_Language = mkLanguage "арабский" ;
lin bulgarian_Language = mkLanguage "болгарский" ;
lin catalan_Language = mkLanguage "каталанский" ;
lin chinese_Language = mkLanguage "китайский" ;
lin danish_Language = mkLanguage "датский" ;
lin dutch_Language = mkLanguage "голландский" ;
lin english_Language = mkLanguage "английский" ;
lin estonian_Language = mkLanguage "эстонский" ;
lin finnish_Language = mkLanguage "финский" ;
lin french_Language = mkLanguage "французский" ;
lin german_Language = mkLanguage "немецкий" ;
lin greek_Language = mkLanguage "греческий" ;
lin hebrew_Language = mkLanguage "еврейский" ;
lin hindi_Language = mkLanguage "хинди" "0" ;
lin japanese_Language = mkLanguage "японский" ;
lin italian_Language = mkLanguage "итальянский" ;
lin latin_Language = mkLanguage "латинский" ;
lin latvian_Language = mkLanguage "латвийский" ;
lin maltese_Language = mkLanguage "мальтийский" ;
lin nepali_Language = mkLanguage "непальский" ;
lin norwegian_Language = mkLanguage "норвежский" ;
lin persian_Language = mkLanguage "персидский" ;
lin polish_Language = mkLanguage "польский" ;
lin punjabi_Language = mkLanguage "панджаби" "0" ;
lin romanian_Language = mkLanguage "румынский" ;
lin russian_Language = mkLanguage "русский" ;
lin sindhi_Language = mkLanguage "синдхи" "0" ;
lin spanish_Language = mkLanguage "испанский" ;
lin swahili_Language = mkLanguage "суахили" "0";
lin swedish_Language = mkLanguage "шведский" ;
lin thai_Language = mkLanguage "тайский" ;
lin turkish_Language = mkLanguage "турецкий" ;
lin urdu_Language = mkLanguage "урду" "0" ;

}
