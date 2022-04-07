--# -path=.:../prelude:../abstract:../common

concrete LexiconCgg of Lexicon = CatCgg ** 
  open ParadigmsCgg, ResCgg, Prelude in {

lin

  --NOTE: Those commented out are not in the abstract
  --burn_V  = mkV "sya" ;
  --die_V   = mkV "fa" ;
  --fly_V   = mkV "guruka" ;
  
  bird_N   = mkN "ekinyonyi" KI_BI ;
  boat_N   = mkN "eryato" RI_MA ;
  book_N   = mkN "ekitabo" KI_BI ;
  boy_N    = mkN "omwojo" "abojo" MU_BA ;
  bread_N  = mkN "omugati" MU_MI;
  car_N    = mkN "emootoka" N_N ;
  cat_N    = mkN "enjangu" N_N ;
  chair_N  = mkN "entebbe" N_N ;
  child_N  = mkN "omwana" MU_BA ;
  city_N   = mkN "ekibúga" KI_BI; --orurêmbo pl endêmbo
  cloud_N  = mkN "ekikyu" KI_BI ;
  computer_N = mkN "kanyabwêngye" ZERO_ZERO ; --kanyabwêngye, embiikabwengye, kompyuta
  cow_N   = mkN "ente" N_N ;
  dog_N   = mkN "embwa" N_N ;
  person_N = mkN "omuntu"  MU_BA ;
  eye_N  = mkN "eriisho" RI_MA;
  fire_N = mkN "omuriro" MU_MI ;
  fish_N = mkN "eky'ényanja" KI_BI ;
  flower_N = mkN "ekimuri" KI_BI ;
  friend_N = mkN "omunywâni" MU_MI ; -- omunywâni, omunyamukago, omugyenzi
  girl_N   = mkN "omwishiki" MU_BA ;
  --shoe_N   = mkN "ekaito" N_N ;
  --table_N  = mkN "emeza" N_N ;
  --airplane_N = mkN "enyonyi" N_N ; -- mkN "endégye" N_N;
  animal_N = mkN "enyamaishwa" N_N ;
  apple_N = mkN "apple"  ZERO_ZERO ;
  baby_N = mkN "omwana" MU_BA ;
  beer_N = mkN "amarwa" ZERO_MA ;
  bike_N = mkN "egaari"  N_N ;
  bird_N = mkN "ekinyonyi" KI_BI  ;
  blood_N = mkN "eshágama"  N_ZERO ;
  grammar_N = mkN "enyómbeka y'órurími" "enyómbeka z'endími" ZERO_ZERO ; -- two words representing one word
  horse_N = mkN "embaráàsi" N_N ;
  house_N = mkN "enju" N_N ;
  language_N = mkN "orurími" "endími" RU_N ;
  meat_N = mkN "enyama" N_N;
  man_N = mkN "omushaija" MU_BA ;
  milk_N = mkN "amate" ZERO_MA ;
  music_N = mkN "music" ZERO_ZERO ; -- I have not found the translation
  rule_N = mkN "ekiragiro" KI_BI;
  river_N = mkN "omugyera" MU_MI ; --omurîndi,
  sea_N = mkN "enyanja" N_N ;
  ship_N = mkN "ekyombo" KI_BI ; -- eméèri [NC_n_n] 
  star_N = mkN "enyonyóòzi" N_N  ;
  train_N = mkN "egaari y'omwika" N_N ; -- plural would be egáàri z'omwika
  tree_N = mkN "omuti" MU_MI ;
  water_N = mkN "amáìzi" ZERO_MA ;
  wine_N = mkN "víìnyo" ZERO_ZERO ;
  woman_N = mkN "omwishiki" MU_BA ;
  reason_N = mkN "enshoonga" N_N;
  sheep_N = mkN "entaama"  N_N;
  hat_N    = mkN "enkofira"   N_N;
  --Proper Nouns
  john_PN = mkPN "Yohana" (AgP3 Sg MU_BA) False;
  paris_PN = mkPN "Paris" (AgP3 Sg N_N) True; --Noun class for places???
 
  --Adjectives
  bad_A    = mkAdjective "bi" Post False False False; --False means the adjective is a stem and comes after the complete noun
  big_A = mkAdjective "hango" Post False False False;
  black_A = mkAdjective "kwirangura" Post False False False;
  blue_A = mkAdjective "buuru" Post True True False ;
  clean_A = mkAdjective "yonjo" Post False False False ; --: A ;
  cold_A = mkAdjective "kufuka" Post False False False ; --: A ;
  correct_A = mkAdjective "hikire" Post False False False; --: A ;
  good_A =mkAdjective "rungi" Post False False False; --: A ;
  heavy_A = mkAdjective "kuremeera" Post False False False; --: A ; --notice ri as a verb is
  hot_A = mkAdjective "kwosya" Post False False False; -- rikutagata -- problematic words like hot we need a new set of clitics
  new_A = mkAdjective "sya" Post False False False; --: A ;
  old_A = mkAdjective "kúru" Post False False False; --: A ;
  ready_A = mkAdjective "eteekateekire" Post False False False; --: A ;
  red_A = mkAdjective "kutukura" Post False False False; --: A ;
  small_A = mkAdjective "kye" Post False False False;
  warm_A = mkAdjective "kutagata" Post False False False;--: A ;
  white_A = mkAdjective "rikwera" Post False False False;--: A ;
  yellow_A = mkAdjective "kinekye" Post True True False;--: A ; or yero, or kyenju
  young_A = mkAdjective "to" Post False False False;--: A ;
  green_A =mkAdjective "kijubwe" Post False True False;
  thin_A = mkAdjective "kye" Post False False False;

  --ditransitive verbs
  bite_V2 = mkV2 "rum";
  break_V2 = mkV2 "hen" "da" "zire"; --: V2 ;
  buy_V2   = mkV2 "gur" ;  --: V2 ;
  close_V2 = mkV2 "king";
  count_V2 = mkV2 "ba" "ra" "zire";
  cut_V2 = mkV2 "sha" "ra" "zire";
  do_V2 = mkV2 "ko" "ra" "zire";
  drink_V2 = mkV2 "nyw";
  eat_V2 = mkV2 "ry";
  fear_V2 = mkV2 "tiin";
  find_V2 = mkV2 "bon" ; --: V2 ; -- many words; kureeba, kubóna,kushanga, kumamya,kujumbura
  kill_V2 = mkV2 "it"; --: V2 ;
  love_V2 = mkV2 "kûnd" "da" "zire"; --: V2 ;
  read_V2 = mkV2 "shom";--: V2 ;
  see_V2 = mkV2 "reeb"; --: V2 ;
  teach_V2 = mkV2 "shomes" ; --: V2 ; or kwegyesa
  understand_V2 = mkV2 "étegyerez"; --: V2 ;
  wait_V2 = mkV2 "tegyerez"; --: V2 ;
 
  -- ditransitive verbs
  add_V3  = mkV3 "gáìt";
  give_V3 = mkV3 "héére" "za" "ize";
  sell_V3 = mkV3 "gu" "za" "rize";
  send_V3 = mkV3 "tum" ;
  talk_V3 = mkV3 "gamb";
  

-- Intransitive verbs
  come_V = mkV "ij";
  die_V  = mkV "f";
  go_V = mkV "gyen" "da" "zire"; --: V ; -- Many words: kuza, kuraba,kutoora, kugyenda=go away, kushuma=go down
  jump_V = mkV "guruk" ;
  play_V = mkV "záàn"; --: V ;
  live_V = mkV "tuur" ; --manyF: kutuura i.e. live somewhere, stay = kuráàra
  run_V = mkV "íruk"; -- : V ;
  sleep_V = mkV "gwejegyer" ; --: V ;--Kugwejegyera, kubyama
  swim_V = mkV "og"; --: V ;
  travel_V = mkV "gyen" "da" "zire" ;--: V ;
  walk_V = mkV "ribá" "ta" "si"; --: V ; or kuribata Runynakore it is different

  -- A verb whose complement is a sentence
  fear_VS = mkVS (lin V (mkV "tin")); --: VS ;
  hope_VS = mkVS (lin V (mkV "siga")); --: VS ; -- a bit complicated because what we normally use is nyine amatsiko i.e usin the noun. The verb should be kwesiga but this seems borrowed from Luganda
  know_VS = mkVS (lin V (mkV "manya")); --: VS ;
  say_VS  = mkVS (lin V (mkV "gi" "ra" "zire"));--: VS ;

  -- verbs whose complements are questions
  know_VQ = mkVQ (lin V (mkV "many")); --: VQ ;
  --wonder_VQ : VQ ;
  
  -- Verb whose complement is an adjective
  become_VA  = mkVA (lin V mkBecome);--: VA ;
  

  --Verbs that have a noun Phrase complement and a verb phrase complement (V2V)
  --beg_V2V : V2V ;
  --Adverbs
  now_Adv = mkAdv "hati" AgrNo;
  far_Adv = mkAdv "hare" AgrNo;
  
  today_Adv = mkAdv "erizooba" AgrNo;

  father_N2 = mkN2 (mkN "tata" ZERO_BAA) (lin Prep (mkPrep [] [] True)) ;

  distance_N3 = mkN3 (mkN "orugyendo" ZERO_BU) (lin Prep (mkPrep "kurunga" "" False)) (lin Prep (mkPrep "mpáka" "" False)); --could orugyendo work in its place?
  
  alas_Interj ={s="ryakareeba"; }; --: Interj ;
  switch8off_V2 = mkV2 "raza" "za" "riize";
  television_N  = mkN "TV" N_N;
  doctor_N = mkN "omushaho"  MU_BA;
  clever_A =mkAdjective "amagyezi" Post False True False;
  laugh_V = mkV "shek";
  beautiful_A = mkAdjective "rungi" Post  False False False;
  airplane_N = mkN "endegye" N_N;
  year_N = mkN "omwaka" "emyaka" MU_MI;

-- New Lexicon
    {-
        Perhaps create an adverb form whose
        surface realisation comes from verb tense e.g nagyenzire implies I have alredy gone.
    -}
    already_Adv = mkAdv "" AgrNo; -- Already is realized as a verb form
    --answer_V2S = mkV2S (mkV "garu" "kamu" "kiremu") noPrep; 
    apartment_N = mkN "apatimenti" "apatimenti" N_N;
    art_N = mkN "aati" "aati" N_N;
    ashes_N = mkN "eiju" "eiju" N_N;
    --ask_V2Q = mkV2Q (mkV2 "bunza" "za" "rize") noPrep;
    back_N = mkN "omugongo" MU_MI;
    bank_N = mkN "banka" "banka" N_N;
    bark_N = mkN  "ask for the Rukiga equivalent of bark" N_N; --TODO find actual word
    beg_V2V = mkV2V (mkV "shab" "a" "ire") noPrep toP ;
    belly_N = mkN "eibondo" "amabondo" I_MA ;
    blow_V  = mkV "hag" "a" "ire" ;
    bone_N  = mkN "eigufa" "amagufa" I_MA ;
    boot_N  = mkN "buuntu" "buuntu" N_N ;
    boss_N  = mkN "mukama" "bakama" MU_BA ;
    breast_N = mkN "eibere" "amabere" I_MA ;
    breathe_V = mkV "is" "a" "ire";
    broad_A = mkAdjective  "TODO : Ask for the right word for broad" Post False False False;
    --Note: younger brother. mukuru and others
    --brother_N2 = mkN2 (mkN "murumuna" "barumuna" MU_BA) (mkPrep "" ""  True) ; 
    newspaper_N = mkN "eihurire" I_MA;
    night_N = mkN "ekiro" N_ZERO;
    nose_N  = mkN "enyindo" N_N;
    --number_N = mkN "enamba" N_N -- TODO look out for the correct word
    oil_N = mkN "amajuta" N_N;
    open_V2 = mkV2 "yingura" "ura" "wire";
    brown_A = mkAdjective "TODO: Get the right word for brown" Post False  True False;
    burn_V = mkV "batur" "a" "ire";
    butter_N = mkN "amajuta g'ente" "amajuta g'ente" N_N;
    camera_N = mkN "kamera" "kamera" ZERO_ZERO;
    cap_N = mkN "TODO-enkofiira" "enkofiira" N_N;
    carpet_N = mkN "kapeti" "kapeti" ZERO_ZERO;
    ceiling_N = mkN "TO-DO:silingi" "TO-DO:silingi" N_N;
    cheese_N = mkN "TO-DO" "TO-DO" N_N;
    church_N = mkN "ekelezia" "amakelezia" N_N;
    coat_N = mkN "kabuti" "kabuti" ZERO_ZERO;
    country_N = mkN "eihanga" "amahanga" I_MA;
    cousin_N = mkN "munyanyako" "banyanyako" MU_BA;
    day_N = mkN "eizooba" "amazoba" I_MA;
    dig_V = mkV "hiing" "a" "ire";
    dirty_A = mkAdjective "rofa" Post False False False;
    door_N = mkN "orwigi" "enyigi" RU_N;
    dry_A = mkAdjective "yoma" Post False False False;
    dull_A = mkAdjective "TODO: find out word for dull" Post False False False;
    dust_N = mkN "omucuucu" "omucuucu" ZERO_ZERO;
    ear_N = mkN "okutu" "amatu" KU_MA;
    earth_N = mkN "ensi" "ensi" N_N;
    easy_A2V = mkA2V "yaguhi" Post False False False;
    egg_N = mkN "eihuri" "amahuri" I_MA;
    empty_A = mkAdjective "rimu busha" Post False True False;
    enemy_N = mkN "TODO : cofirm omurabe" "TODO : cofirm abarabe" MU_BA;
    factory_N = mkN "TODO : cofirm fakatore" "TODO : cofirm fakatore" N_N;
    fall_V = mkV "gw" "a" "ire";
    fat_N  = mkN "ebishaju" "ebishaju" ZERO_ZERO;
    feather_N = mkN "ekyoya" "ebyoya" KI_BI;
    fight_V2 = mkV2 "rwan" "a" "ire";
    fingernail_N = mkN "ekyara ky'engaro" "ebyara by'engaro" KI_BI;
    --float_V
    --floor_N 
    flow_V = mkV "TODO: confirm himintuk" "a" "ire";
    fly_V = mkV "guruk" "a" "ire";
    fog_N = mkN "TODO word for fog" "TODO word for fog" N_N;
    foot_N = mkN "ekigyere" "ebigyere" KI_BI;
    forest_N = mkN "eihamba" "amahamba" I_MA;
    forget_V2 = mkV2 "yebw" "a" "ire";
    freeze_V = mkV "kwat" "a" "sire";
    fridge_N = mkN "firigi" "firigi" ZERO_ZERO; 
    fruit_N = mkN "ekijuma" "ebijuma" KI_BI;
    full_A = mkAdjective "injwire" Post False True False;
    --fun_AV = mkAdjective
    garden_N = mkN "omusiri" "emisiri" MU_MI;
    glove_N = mkN "gilavu" "gilavu" ZERO_ZERO;
    gold_N = mkN "TODO:feza" "TODO:feza" ZERO_ZERO;
    grass_N = mkN "akanyaasi" "obunyaasi" KA_BU;
    guts_N = mkN "orubondo" "amabondo" RU_MA;
    hair_N = mkN "eishokye" "eishokye" N_N;
    hand_N = mkN "omukono" "emikono" MU_MI;
    -- harbour_N
    hate_V2 = mkV2 "kwag" "a" "ire";
    head_N = mkN "omutwe" MU_MI;
    hear_V2 = mkV2 "hurir" "a" "e";
    heart_N = mkN "omutima" "emitima" MU_MI;
    hill_N = mkN "orushozi" "enshozi" N_N;
    hit_V2 = mkV2 "kangaa" "ta" "sire";
    hold_V2 = mkV2 "kwa" "ta" "sire";
    horn_N = mkN "eihembe" "amahembe" I_MA;
    hunt_V2 = mkV2 "hiig" "a" "ire";
    husband_N = mkN "iba" "biba" ZERO_ZERO;
    -- ice_N
    important_A = mkAdjective "omugasho" Post False True False;
    --industry_N = 
    iron_N = mkN "ekyoma ebirikurunga omu butare" "ebyoma ebirikurunga omu butare" ZERO_ZERO;
    king_N = mkN "omugabe" MU_BA;
    knee_N = mkN "okuju" "amaju" KU_MA;
    know_V2 = mkV2 "many" "a" "ire";
    lake_N = mkN "enyanja" "enyanja" N_N;
    lamp_N = mkN "entara" "entara" N_N;
    leaf_N = mkN "ibabi" "amababi" I_MA;
    learn_V2 = mkV2 "yeg" "a" "ire";
    leather_N = mkN "oruhu" "empu" RU_N; --I think plural should be oruhu again
    leave_V2 = mkV2 "rug" "a" "ire";
    left_Ord = mkOrd "bumosho";
    leg_N = mkN "okuguru" "amaguru" KU_MA;
    lie_V = mkV "beih" "a" "ire";
    like_V2 = mkV2 "kun" "da"  "zire";
    listen_V2 = mkV2 "huri" "iriza" "riize";
    liver_N = mkN "ekine" "ebine" KI_BI;
    long_A = mkAdjective "raingwa" Post False False False;
    --lose_V2 = mkV2 ""
    louse_N = mkN "omura" "emira" MU_MI;
    love_N = mkN "rukundo" "rukundo" ZERO_ZERO; 
    -- married_A2
    moon_N = mkN "okwezi" "emwezi" KU_MA;
    --mother_N2 = mkN2 (mkN "mama" ZERO_BAA) (lin Prep (mkPrep [] [] True)) ;
    --mountain_N = mkN 
    mouth_N = mkN "omunwa" "eminwa" MU_MI;
    name_N = mkN "eiziina" "amaziina" I_MA;
    narrow_A = mkAdjective "kye" Post False False False;
    near_A = mkAdjective "hihi" Pre False True False; -- the preposition is "na"
    neck_N = mkN "ebisya" "ebisya" ZERO_ZERO;
    -- paint_V2A
    paper_N = mkN "orupapura" "empapura" RU_N;
    peace_N = mkN "obusingye" "obusingye" ZERO_BU;
    pen_N = mkN "akacumu" "obucumu" KA_BU;
    planet_N = mkN "ensi" "ensi" N_N;
    plastic_N = mkN "pulasitika" "pulasitika" ZERO_ZERO;
    play_V2 = mkV2 "zaan" "a" "ire";
    policeman_N = mkN "omupolisi" "abapolisi" MU_BA;
    priest_N = mkN "omuhongyerezi" "abahongyerezi" MU_BA;
    -- probable_AS
    pull_V2 = mkV2 "kuru" "ra" "ire";
    push_V2 = mkV2 "sindik" "a" "ire";
    put_V2 = mkV2 "t" "a" "ire";
    queen_N = mkN "Kwini" "baakwini" ZERO_BAA;
    question_N = mkN "ekibuzo" "ebibuzo" KI_BI;
    radio_N = mkN "radiyo" "radiyo" ZERO_ZERO;
    rain_N = mkN "enjura" "enjura" ZERO_N;
    -- rain_V0
    religion_N = mkN "endiini" "endiini" N_N;
    restaurant_N = mkN "hooteeri" "hooteeri" ZERO_ZERO;
    right_Ord = mkOrd "buryo";
    road_N = mkN "orugundo" "engundo" RU_N;
    -- rock_N
    --roof_N = mkN ""
    root_N = mkN "omuzi" "emizi" MU_MI;
    rope_N = mkN "omuguha" "emiguha" MU_MI;
    rotten_A = mkAdjective "njuzire" Post False False False;
    -- round_A
    rub_V2 = mkV2 "harabur" "a" "ire";
    -- rubber_N
    salt_N = mkN "omwonyo" "emyonyo" MU_MI;
    sand_N = mkN "omushenyi" MU_MI;
    school_N = mkN "eishomero"  I_MA;
    science_N = mkN "sayansi" "sayansi" ZERO_ZERO;
    scratch_V2 = mkV2 "ha" "ra" "ire";
    seed_N = mkN "ensingo" "ensigo" N_N; 
    seek_V2 = mkV2 "kyenuuz" "a" "ire";
    sew_V = mkV "ruk" "a" "ire"; 
    sharp_A = mkAdjective "shazire" Post False False False;
    shirt_N = mkN "esaati" "esaati" ZERO_ZERO; --TODO: confirm this entry
    shoe_N = mkN "enkaito" "enkaito" N_N; --TODO : confirm the existence of this entry
    shop_N = mkN "eduuka" "eduuka" ZERO_ZERO;
    short_A = mkAdjective "gufu" Post False False False;
    --silver_N
    sing_V = mkV "yeshogor" "ora" "wire"; --TODO : confirm this word and the conjugation
    sister_N = mkN "munyanyazi" "banyanyanzi" MU_BA;
    sit_V = mkV "shutam" "a" "ire";
    --skin_N = mkN "omubiri" "emibiri" MU_MI; This is wrong
    sky_N = mkN "eiguru" "eiguru" ZERO_ZERO;
    smell_V = mkV "kag" "a" "ire";
    smoke_N = mkN "omwika" "emyiika" MU_MI;
    --smooth_A
    snake_N = mkN "enjoka" "enjoka" N_N;
    -- snow_N
    sock_N = mkN "sitokisi" "sitokisi" ZERO_ZERO;
    song_N = mkN "ekyeshongoro" "ebyeshongoro" KI_BI;
    speak_V2 = mkV2 "gamba";
    spit_V = mkV "cwer" "a" "ire";
    split_V2 = mkV2 "gangabura";
    squeeze_V2 =mkV2 "imat" "a" "si";
    stab_V2 = mkV2 "cumi" "ta" "sire"; --edit for Runyankore
    stand_V = mkV "yemerera" "ra" "ire";
    -- steel_N
    stick_N = mkN "omunyafu" "eminyafu" MU_MI;
    stone_N = mkN "eibare" "amabara" I_MA;
    stop_V = mkV "komya" "ya" "ize";
    stove_N = mkN "sitoovu" "sitoovu" ZERO_ZERO;
    straight_A = mkAdjective "gorwire" Post False False False;
    student_N = mkN "omwana w'eishomero" "abaana b'eishomero" MU_BA;
    stupid_A = mkAdjective "himbagire" Post False False False;
    suck_V2 = mkV2 "komaguza" "za" "ize";
    sun_N = mkN "omushana" "omushana" MU_ZERO;
    swell_V = mkV "bimba";
    switch8on_V2 = mkV2 "ya" "sya" "kise";
    table_N = mkN "emeeza" "emeeza" N_N;
    tail_N = mkN "omukira" "emikira" MU_MI;
    teacher_N = mkN "omushomesa" "abashomesa" MU_BA;
    -- thick_A
    --think_V = mkV 
    throw_V2 = mkV2 "naga";
    tie_V2 = mkV2 "siba";
    tongue_N = mkN "orurimi" RU_N;
    tooth_N = mkN "erino" "amino" RI_MA;
    turn_V = mkV "hinduka";
    ugly_A = mkAdjective "bi" Post False False False; --I think I should add another boolean for copulative version
    uncertain_A = mkAdjective "manyirwe" Post False False True; -- we need to investigate the place of negative adjectives i.e those that come from positive words
    university_N = mkN "yunivasite" "yunivasite" ZERO_ZERO;
    village_N = mkN "ekyalo" "ebyalo" KI_BI;
    vomit_V = mkV "tanaka"; 
    war_N = mkN "orutaro" "entaro" RU_N;
    wash_V2 = mkV2 "yozya" "zya" "yogize";
    watch_V2 = mkV2 "reeba";
    wet_A = mkAdjective "jubire" Post False False False;
    wide_A = mkAdjective "hango" Post False False False;
    wife_N = mkN "omukyara" "abakyara" MU_BA;
    win_V2 = mkV2 "singa";
    wind_N = mkN "omuyaga" "emiyaga" MU_MI;
    window_N = mkN "eidirisa" "eidirisa" N_N;
    wing_N = mkN "eipapa" "amapapa" I_MA;
    wipe_V2 = mkV2 "sim" "ura" "wire";
    --wonder_VQ = mkV2Q (mkV2 "tanga" "ara" "ire") noPrep;
    wood_N = mkN "ekiti" "ebiti" KI_BI;
    --worm_N
    write_V2 = mkV2 "handika";



-- End of New Lexicon
oper
  aboutP = mkPrep  "TODO : find rukiga equivalent of about" "" False ;
  atP = mkPrep  "TODO : find rukiga equivalent of at" "" False ;
  forP = mkPrep  "TODO : find rukiga equivalent of for"  "" False;
  fromP = mkPrep "kurunga" "" False;
  inP = mkPrep "omu" "omuri" False;
  onP = mkPrep "aha" "ahari" False;
  toP = mkPrep ("ku" ++ Predef.BIND) [] False;
  --noPrep = mkPrep [] [] False;

}
