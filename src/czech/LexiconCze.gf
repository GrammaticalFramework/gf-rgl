concrete LexiconCze of Lexicon =
  CatCze
  **
  open
    ResCze, ParadigmsCze
  in {

  lin
    child_N = (kureN "dítě") ** {
      sgen = "dítěte" ; sdat,sloc = "dítěti" ; sins = "dítětem" ;
      pnom,pacc = "děti" ; pgen = "dětí" ; pdat = "dětem" ; ploc = "dětech" ; pins = "dětmi" ; gPl = Fem
      } ;
    year_N = (hradN "rok") ** {sgen = "roku" ; sloc = "roce" ; pgen = "let" ; pdat = "letům" ; ploc = "letech" ; pins = "lety"} ;
    boy_N = declPAN "kluk" ;
    man_N = declMUZ "muž" ;
    teacher_N = declMUZ "učitel" ;
    horse_N = declMUZ "kůň" ;
    father_N = declMUZ "otec" ;
    husband_N = declPAN "manžel" ;

    castle_N = declHRAD "hrad" ;
    forest_N = declHRAD "les" ;
    machine_N = declSTROJ "stroj" ;

    woman_N = declZENA "žena" ;
    school_N = zenaN "škola" ;
    skirt_N = declRUZE "sukně";
    street_N = declRUZE "ulice" ;
    rose_N = declRUZE "růže" ;
    song_N = declPISEN "píseň" ;
    bed_N = declPISEN "postel" ;
    door_N = declRUZE "dveře" ;
    bone_N = declKOST "kost" ;
    village_N = declKOST "ves" ; ----

    city_N = (mestoN "město") ** {sloc = "městě"} ;
    apple_N = declMESTO "jablko" ; ----
    sea_N = declMORE "moře" ;
    airport_N = declMORE "letiště" ;
    chicken_N = declKURE "kuře" ;
    house_N = declSTAVENI "stavení" ; --- building, house
    station_N = declSTAVENI "nádraží" ;

    young_A = mkA "mladý" "mladší" ;
    old_A = mkA "starý" "starší" ;
    good_A = mkA "dobrý" "lepší" ;
    bad_A = mkA "špatný" "horší" ;
    beautiful_A = mkA "krásný" "krásnější" ;
    clean_A = mkA "čistý" "čistší" ;
    dirty_A = mkA "špinavý" "špinavější" ;
    
    white_A = mkA "bílý" "bělejší" ;
    black_A = mkA "černý" "černější" ;
    red_A = mkA "červený" "červenější" ;
    brown_A = mkA "hnědý" "hnědší" ;
    blue_A = mkA "modrý" "modřejší" ;
    green_A = mkA "zelený" "zelenější" ;
    yellow_A = mkA "žlutý" "žlutější" ;

    buy_V2 = mkV2 (iii_kupovatVerbForms "kupovat") ;
    love_V2 = mkV2 (iii_kupovatVerbForms "milovat") ;

    beer_N = mestoN "pivo" ;
    bread_N = mkN "chléb" "chleba" mascInanimate ;
    fish_N = zenaN "ryba" ;
    milk_N = (mestoN "mléko") ** {sloc = "mléce"} ;
    salt_N = (kostN "sol") ** {snom,sacc = "sůl" ; pdat = "solím" ; ploc = "solích" ; pins = "solemi"} ;
    water_N = zenaN "voda" ;
    wine_N = mestoN "víno" ;
    cold_A = mkA "studený" "studenější" ;
    warm_A = mkA "teplý" "teplejší" ;


}
