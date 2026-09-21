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
    year_N = (hradN "rok") ** {sgen,svoc = "roku" ; sloc = "roce" ; pgen = "let" ; pdat = "letům" ; ploc = "letech" ; pins = "lety"} ;
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

    buy_V2 = mkV2 (kupovatV "kupovat") ;
    love_V2 = mkV2 (kupovatV "milovat") ;

    drink_V2 = mkV2 (krytV "pít") ;
    eat_V2 = mkV2 (mkV "jíst" "jím" "jíš" "jí" "jíme" "jíte" "jedí" "jedl" "jedli" "jez" "jezme" "jezte") ;
    read_V2 = mkV2 (mkV "číst" "čtu" "čteš" "čte" "čteme" "čtete" "čtou" "četl" "četli" "čti" "čtěme" "čtěte") ;
    write_V2 = mkV2 (mkV "psát" "píši" "píšeš" "píše" "píšeme" "píšete" "píší" "psal" "psali" "piš" "pišme" "pište") ;
    wait_V2 = mkV2 (mkV "čekat" "čekám" "čekáš" "čeká" "čekáme" "čekáte" "čekají" "čekal" "čekali" "čekej" "čekejme" "čekejte") (mkPrep "na" accusative) ;
    play_V = mkV "hrát" "hraji" "hraješ" "hraje" "hrajeme" "hrajete" "hrají" "hrál" "hráli" "hraj" "hrajme" "hrajte" ;
    run_V = mkV "běžet" "běžím" "běžíš" "běží" "běžíme" "běžíte" "běží" "běžel" "běželi" "běž" "běžme" "běžte" ;
    sit_V = mkV "sedět" "sedím" "sedíš" "sedí" "sedíme" "sedíte" "sedí" "seděl" "seděli" "seď" "seďme" "seďte" ;
    sleep_V = mkV "spát" "spím" "spíš" "spí" "spíme" "spíte" "spí" "spal" "spali" "spi" "spěme" "spěte" ;
    swim_V = mkV "plavat" "plavu" "plaveš" "plave" "plaveme" "plavete" "plavou" "plaval" "plavali" "plav" "plavme" "plavte" ;
    walk_V = mkV "chodit" "chodím" "chodíš" "chodí" "chodíme" "chodíte" "chodí" "chodil" "chodili" "choď" "choďme" "choďte" ;
    go_V = mkV "jít" "jdu" "jdeš" "jde" "jdeme" "jdete" "jdou" "šel" "šli" "jdi" "jděme" "jděte" ;
    know_V2 = mkV2 (mkV "znát" "znám" "znáš" "zná" "známe" "znáte" "znají" "znal" "znali" "znej" "znejme" "znejte") ;
    know_VS = mkVS knowV ;
    know_VQ = mkVQ knowV ;
    today_Adv = mkAdv "dnes" ;

    beer_N = mestoN "pivo" ;
    bread_N = mkN "chléb" "chleba" mascInanimate ;
    fish_N = zenaN "ryba" ;
    milk_N = (mestoN "mléko") ** {sloc = "mléce" ; ploc = "mlékách"} ;
    salt_N = (kostN "sol") ** {snom,sacc = "sůl" ; pdat = "solím" ; ploc = "solích" ; pins = "solemi"} ;
    water_N = zenaN "voda" ;
    wine_N = mestoN "víno" ;
    cold_A = mkA "studený" "studenější" ;
    warm_A = mkA "teplý" "teplejší" ;

oper
    knowV : V = mkV "vědět" "vím" "víš" "ví" "víme" "víte" "vědí" "věděl" "věděli" "věz" "vězme" "vězte" ;

}
