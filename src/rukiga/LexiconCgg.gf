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
  --chair_N  = mkN "entebbe" N_N ;
  child_N  = mkN "omwana" MU_BA ;
  city_N   = mkN "ekibúga" KI_BI; --orurêmbo pl endêmbo
  cloud_N  = mkN "ekikyu" KI_BI ;
  computer_N = mkN "kanyabwêngye" ZERO_ZERO ; --kanyabwêngye, embiikabwengye, kompyuta
  cow_N   = mkN "ente" N_N ;
  dog_N   = mkN "embwa" N_N ;
  --person_N = mkN "omuntu" "abantu" MU_BA ;
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
  man_N = mkN "omushaija" MU_BA ;
  milk_N = mkN "amate" ZERO_MA ;
  music_N = mkN "music" ZERO_ZERO ; -- I have not found the translation
  river_N = mkN "omugyera" MU_MI ; --omurîndi,
  sea_N = mkN "enyanja" N_N ;
  ship_N = mkN "ekyombo" KI_BI ; -- eméèri [NC_n_n] 
  star_N = mkN "enyonyóòzi" N_N  ;
  train_N = mkN "egaari y'omwika" N_N ; -- plural would be egáàri z'omwika
  tree_N = mkN "omuti" MU_MI ;
  water_N = mkN "amáìzi" ZERO_MA ;
  wine_N = mkN "víìnyo" ZERO_ZERO ;
  woman_N = mkN "omwishiki" MU_BA ;
  
  --Proper Nouns
  john_PN = mkPN "Yohana" (AgP3 Sg MU_BA) False;
  paris_PN = mkPN "Paris" (AgP3 Sg N_N) True; --Noun class for places???
 
  --Adjectives
  bad_A    = mkAdjective "bi" Post False False; --False means the adjective is a stem and comes after the complete noun
  --beautiful_A = mkAdjective "rungi" False;
  big_A = mkAdjective "hango" Post False False;
  black_A = mkAdjective "kwirangura" Post False False;
  blue_A = mkAdjective "buuru" Post True True ;
  clean_A = mkAdjective "yonjo" Post False False; --: A ;
  cold_A = mkAdjective "rikufuka" Post False False; --: A ;
  good_A =mkAdjective "rungi" Post False False; --: A ;
  heavy_A = mkAdjective "rikuremeera" Post False False; --: A ; --notice ri as a verb is
  hot_A = mkAdjective "rikwotsya" Post False False; -- rikutagata -- problematic words like hot we need a new set of clitics
  new_A = mkAdjective "sya" Post False False; --: A ;
  old_A = mkAdjective "kúru" Post False False; --: A ;
  ready_A = mkAdjective "eteekateekire" Post False False; --: A ;
  red_A = mkAdjective "ríkutukura" Post False False; --: A ;
  small_A = mkAdjective "kye" Post False False;
  warm_A = mkAdjective "rikutagata" Post False False;--: A ;
  white_A = mkAdjective "rikwera" Post False False;--: A ;
  yellow_A = mkAdjective "kinekye" Post True True;--: A ; or yero, or kyenju
  young_A = mkAdjective "to" Post False False;--: A ;
  green_A =mkAdjective "kijubwe" Post False True;

  --ditransitive verbs
  bite_V2 = mkV2 "rum";
  break_V2 = mkV2 "hend"; --: V2 ;
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
  love_V2 = mkV2 "kûnd"; --: V2 ;
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
  go_V = mkV "gyend"; --: V ; -- Many words: kuza, kuraba,kutoora, kugyenda=go away, kushuma=go down
  jump_V = mkV "guruk" ;
  play_V = mkV "záàn"; --: V ;
  live_V = mkV "tuur" ; --manyF: kutuura i.e. live somewhere, stay = kuráàra
  run_V = mkV "íruk"; -- : V ;
  sleep_V = mkV "nyama" ; --: V ;--Kugwejegyera, kubyama
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
  --far_Adv = mkAdv "hare";
  
  --today_Adv = mkAdv "erizooba" AgrNo;

  father_N2 = mkN2 (mkN "tata" ZERO_BAA) (lin Prep (mkPrep [] [] True)) ;

  distance_N3 = mkN3 (mkN "orugyendo" ZERO_BU) (lin Prep (mkPrep "kurunga" "" False)) (lin Prep (mkPrep "mpáka" "" False)); --could orugyendo work in its place?
  
  alas_Interj ={s="ryakareeba"; }; --: Interj ;

oper
  aboutP = mkPrep "about" ;
  atP = mkPrep "at" ;
  forP = mkPrep "for" ;
  fromP = mkPrep "kurunga" "" False;
  inP = mkPrep "omu" "omuri" False;
  onP = mkPrep "aha" "ahari" False;
  toP = mkPrep "aha" [] False;




  {-
  --Old LexiconCgg.gf
  burn_V  = mkV "sya" ;
  die_V   = mkV "fa" ;
  fly_V   = mkV "guruka" ;
  run_V   = mkV "iruka" ;
  sleep_V = mkV "byama" ;
  walk_V  = mkV "tabula" ;
  
  bird_N   = mkN "ekinyonyi" KI_BI ;
  boat_N   = mkN "eryato" RI_MA ;
  book_N   = mkN "ekitabo" KI_BI ;
  boy_N    = mkN "omwojo" "abojo" MU_BA ;
  car_N    = mkN "emootoka" N_N ;
  chair_N  = mkN "entebbe" N_N;
  cloud_N  = mkN "ekikyu" KI_BI ;
  person_N = mkN "omuntu" "abantu" MU_BA ;
  girl_N   = mkN "omwishiki" MU_BA ;
  shoe_N   = mkN "ekaito" N_N ;
  table_N  = mkN "emeza" N_N ;
  bad_A    = mkAdjective "bi" False; --False means the adjective is a stem and comes after the complet noun
  beautiful_A = mkAdjective "rungi" False; 
  far_Adv = mkAdv "hare";
  now_Adv = mkAdv "hati";
  today_Adv = mkAdv "erizooba";
  bite_V2 = mkV2 "ruma";
  break_V2 = mkV2 "henda";
  buy_V2 = mkV2 "gura";
  close_V2 = mkV2 "kinga";
  count_V2 = mkV2 "bara";
  cut_V2 = mkV2 "shara";
  do_V2 = mkV2 "kora";
  drink_V2 = mkV2 "nywa";
  eat_V2 = mkV2 "rya";
  fear_V2 = mkV2 "tiina";

-------------------------Differences Rukiga only--------------------
airplane_N : mkN "endegye" N_N;
-}

}
