concrete LexiconSlo of Lexicon =
  CatSlo
  **
  open
    ResSlo, ParadigmsSlo
  in {


  lin
    boy_N = mkN "chlapec" ;
    man_N = mkN "muž" ;
    teacher_N = mkN "učiteľ" ;
    horse_N = mkN "kôň" ;
    father_N = mkN "otec" ;
    husband_N = mkN "manžel" ;

    castle_N = mkN "hrad" ;
    forest_N = mkN "les" ;
    machine_N = mkN "stroj" ;

    woman_N = mkN "žena" ;
    school_N = mkN "učilište" ;
    skirt_N = mkN "sukňa";
    street_N = mkN "ulica" ;
    rose_N = mkN "ruže" ;
    song_N = mkN "pieseň" ;
    bed_N = mkN "posteľ" ;
{-
    door_N = mkN "dveře" ;
    bone_N = declKOST "kost" ;
    village_N = declKOST "ves" ; ----

    city_N = declMESTO "město" ;
    apple_N = declMESTO "jablko" ; ----
    sea_N = declMORE "moře" ;
    airport_N = declMORE "letiště" ;
    chicken_N = declKURE "kuře" ;
    house_N = declSTAVENI "stavení" ; --- building, house
    station_N = declSTAVENI "nádraží" ;

    young_A = mkA "mladý" ;
    old_A = mkA "starý" ;
    good_A = mkA "dobrý" ;
    bad_A = mkA "špatný" ;
    beautiful_A = mkA "krásný" ;
    clean_A = mkA "čistý" ;
    dirty_A = mkA "špinavý" ;
-}

--- from Google translate dictionary

    white_A = mkA "biely" ;
    black_A = mkA "čierny" ;
    red_A = mkA "červený" ;
    brown_A = mkA "hnedý" ;
    blue_A = mkA "modrý" ; --- belasý
    green_A = mkA "zelený" ;
    yellow_A = mkA "žltý" ;

    buy_V2 = mkV2 (iii_kupovatVerbForms "kupovať") ;
    love_V2 = mkV2 (iii_kupovatVerbForms "milovať") ;

}

