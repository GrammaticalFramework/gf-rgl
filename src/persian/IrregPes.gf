--# -path=.:../abstract:../common:../prelude

concrete IrregPes of IrregPesAbs = CatPes **
  open ParadigmsPes, (M=MorphoPes) in {

  flags
    optimize=values ;
    coding=utf8 ;

  lin
    -- Reusable light verbs and stems.
    kardan_V = lin V M.doVerb ;
    shodan_V = lin V M.becomeVerb ;
    dashtan_V = lin V M.haveVerb ;
    budan_V = lin V M.beVerb ;

    amadan_V = lin V (M.mkVerb "آمدن" "آی") ;
    avardan_V = mkV "آوردن" ;
    istadan_V = mkV "ایستادن" ;
    afkandan_V = mkV "افکندن" ;
    afrashtan_V = lin V (M.mkVerb "افراشتن" "افراز") ;
    amukhtan_V = lin V (M.mkVerb "آموختن" "آموز") ;
    bastan_V = lin V (M.mkVerb "بستن" "بند") ;
    bordan_V = mkV "بردن" ;
    paziroftan_V = lin V (M.mkVerb "پذیرفتن" "پذیر") ;
    pokhtan_V = lin V (M.mkVerb "پختن" "پز") ;
    pardakhtan_V = lin V (M.mkVerb "پرداختن" "پرداز") ;
    peyvastan_V = lin V (M.mkVerb "پیوستن" "پیوند") ;
    tavanestan_V = lin V (M.mkVerb "توانستن" "توان") ;
    jostan_V = lin V (M.mkVerb "جستن" "جو") ;
    khordan_V = mkV "خوردن" ;
    dadan_V = lin V (M.mkVerb "دادن" "ده") ;
    danestan_V = lin V (M.mkVerb "دانستن" "دان") ;
    didan_V = lin V (M.mkVerb "دیدن" "بین") ;
    randan_V = mkV "راندن" ;
    raftan_V = lin V (M.mkVerb "رفتن" "رو") ;
    rikhtan_V = lin V (M.mkVerb "ریختن" "ریز") ;
    sepordan_V = lin V (M.mkVerb "سپردن" "سپار") ;
    zadan_V = lin V (M.mkVerb "زدن" "زن") ;
    zistan_V = lin V (M.mkVerb "زیستن" "زی") ;
    sakhtan_V = lin V (M.mkVerb "ساختن" "ساز") ;
    shenakhtan_V = lin V (M.mkVerb "شناختن" "شناس") ;
    shenidan_V = lin V (M.mkVerb "شنیدن" "شنو") ;
    shomordan_V = lin V (M.mkVerb "شمردن" "شمار") ;
    shekastan_V = lin V (M.mkVerb "شکستن" "شکن") ;
    gozaashtan_V = lin V (M.mkVerb "گذاشتن" "گذار") ;
    gozashtan_V = lin V (M.mkVerb "گذشتن" "گذر") ;
    gereftan_V = lin V (M.mkVerb "گرفتن" "گیر") ;
    gorikhtan_V = lin V (M.mkVerb "گریختن" "گریز") ;
    gashtan_V = lin V (M.mkVerb "گشتن" "گرد") ;
    goftan_V = lin V (M.mkVerb "گفتن" "گو") ;
    kastan_V = lin V (M.mkVerb "کاستن" "کاه") ;
    koshtan_V = mkV "کشتن" ;
    kandan_V = mkV "کندن" ;
    mordan_V = lin V (M.mkVerb "مردن" "میر") ;
    mandan_V = mkV "ماندن" ;
    neshastan_V = lin V (M.mkVerb "نشستن" "نشین") ;
    navakhtan_V = lin V (M.mkVerb "نواختن" "نواز") ;
    nahadan_V = mkV "نهادن" ;
    neveshtan_V = lin V (M.mkVerb "نوشتن" "نویس") ;
    oftadan_V = mkV "افتادن" ;
    andakhtan_V = lin V (M.mkVerb "انداختن" "انداز") ;
    afzudan_V = lin V (M.mkVerb "افزودن" "افزا") ;
    forukhtan_V = lin V (M.mkVerb "فروختن" "فروش") ;
    afrukhtan_V = lin V (M.mkVerb "افروختن" "افروز") ;
    goshudan_V = lin V (M.mkVerb "گشودن" "گشا") ;
    khastan_V = lin V (M.mkVerb "خواستن" "خواه") ;
    khabidan_V = mkV "خوابیدن" ;
    yaftan_V = lin V (M.mkVerb "یافتن" "یاب") ;
    robudan_V = lin V (M.mkVerb "ربودن" "ربای") ;

  oper
    -- Preserve malformed or non-inflecting legacy dictionary entries without
    -- weakening the productive regV paradigm with a catch-all case.
    invarV : Str -> V = \s -> lin V (M.invarV s) ;
}
