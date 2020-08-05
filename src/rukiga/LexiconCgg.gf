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
  --person_N = mkN "omuntu" "abantu" MU_BA ;
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
  bad_A    = mkAdjective "bi" Post False False; --False means the adjective is a stem and comes after the complete noun
  --beautiful_A = mkAdjective "rungi" False;
  big_A = mkAdjective "hango" Post False False;
  black_A = mkAdjective "kwirangura" Post False False;
  blue_A = mkAdjective "buuru" Post True True ;
  clean_A = mkAdjective "yonjo" Post False False; --: A ;
  cold_A = mkAdjective "kufuka" Post False False; --: A ;
  correct_A = mkAdjective "hikire" Post False False; --: A ;
  good_A =mkAdjective "rungi" Post False False; --: A ;
  heavy_A = mkAdjective "kuremeera" Post False False; --: A ; --notice ri as a verb is
  hot_A = mkAdjective "kwosya" Post False False; -- rikutagata -- problematic words like hot we need a new set of clitics
  new_A = mkAdjective "sya" Post False False; --: A ;
  old_A = mkAdjective "kúru" Post False False; --: A ;
  ready_A = mkAdjective "eteekateekire" Post False False; --: A ;
  red_A = mkAdjective "kutukura" Post False False; --: A ;
  small_A = mkAdjective "kye" Post False False;
  warm_A = mkAdjective "kutagata" Post False False;--: A ;
  white_A = mkAdjective "rikwera" Post False False;--: A ;
  yellow_A = mkAdjective "kinekye" Post True True;--: A ; or yero, or kyenju
  young_A = mkAdjective "to" Post False False;--: A ;
  green_A =mkAdjective "kijubwe" Post False True;
  thin_A = mkAdjective "kye" Post False False;

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
  --far_Adv = mkAdv "hare";
  
  today_Adv = mkAdv "erizooba" AgrNo;

  father_N2 = mkN2 (mkN "tata" ZERO_BAA) (lin Prep (mkPrep [] [] True)) ;

  distance_N3 = mkN3 (mkN "orugyendo" ZERO_BU) (lin Prep (mkPrep "kurunga" "" False)) (lin Prep (mkPrep "mpáka" "" False)); --could orugyendo work in its place?
  
  alas_Interj ={s="ryakareeba"; }; --: Interj ;


-- New Lexicon
  switch8off_V2 = mkV2 "raza" "za" "riize";
  television_N  = mkN "TV" N_N;
  doctor_N = mkN "omushaho"  MU_BA;
  clever_A =mkAdjective "amagyezi" Post False True;
  laugh_V = mkV "sheka";

  airplane_N = mkN "endegye" N_N;
{--
  answer_V2S 
  apartment_N
  art_N
  ashes_N
  ask_V2Q
  back_N
  bank_N
  bark_N
  beautiful_A
  beg_V2V
  belly_N
  blow_V
  bone_N
  boot_N
  boss_N
  breast_N
  breathe_V
  broad_A
  brother_N2
  brown_A
  burn_V
  butter_N
  camera_N
  cap_N
  carpet_N
  ceiling_N
  cheese_N
  church_N
  coat_N
  country_N
  cousin_N
  day_N
  dig_V
  dirty_A
  doctor_N
  door_N
  dry_A
  dull_A
  dust_N
  ear_N
  earth_N
  easy_A2V
  egg_N
  empty_A
  enemy_N
  factory_N
  fall_V
  far_Adv
  fat_N
  feather_N
  fight_V2
  fingernail_N
  float_V
  floor_N
  flow_V
  fly_V
  fog_N
  foot_N
  forest_N
  forget_V2
  freeze_V
  fridge_N
  fruit_N
  full_A
  fun_AV
  garden_N
  glove_N
  gold_N
  grass_N
  guts_N
  hair_N
  hand_N
  harbour_N
  hat_N
  hate_V2
  head_N
  hear_V2
  heart_N
  hill_N
  hit_V2
  hold_V2
  horn_N
  hunt_V2
  husband_N
  ice_N
  important_A
  industry_N
  iron_N
  king_N
  knee_N
  know_V2
  lake_N
  lamp_N
  leaf_N
  learn_V2
  leather_N
  leave_V2
  left_Ord
  leg_N
  lie_V
  like_V2
  listen_V2
  liver_N
  long_A
  lose_V2
  louse_N
  love_N
  married_A2
  moon_N
  mother_N2
  mountain_N
  mouth_N
  name_N
  narrow_A
  near_A
  neck_N
  newspaper_N
  night_N
  nose_N
  number_N
  oil_N
  open_V2
  paint_V2A
  paper_N
  peace_N
  pen_N
  person_N
  planet_N
  plastic_N
  play_V2
  policeman_N
  priest_N
  probable_AS
  pull_V2
  push_V2
  put_V2
  queen_N
  question_N
  radio_N
  rain_N
  rain_V0
  religion_N
  restaurant_N
  right_Ord
  road_N
  rock_N
  roof_N
  root_N
  rope_N
  rotten_A
  round_A
  rub_V2
  rubber_N
  salt_N
  sand_N
  school_N
  science_N
  scratch_V2
  seed_N
  seek_V2
  sew_V
  sharp_A
  shirt_N
  shoe_N
  shop_N
  short_A
  silver_N
  sing_V
  sister_N
  sit_V
  skin_N
  sky_N
  smell_V
  smoke_N
  smooth_A
  snake_N
  snow_N
  sock_N
  song_N
  speak_V2
  spit_V
  split_V2
  squeeze_V2
  stab_V2
  stand_V
  steel_N
  stick_N
  stone_N
  stop_V
  stove_N
  straight_A
  student_N
  stupid_A
  suck_V2
  sun_N
  swell_V
  switch8on_V2
  table_N
  tail_N
  teacher_N
  thick_A
  think_V
  throw_V2
  tie_V2
  tongue_N
  tooth_N
  turn_V
  ugly_A
  uncertain_A
  university_N
  village_N
  vomit_V
  war_N
  wash_V2
  watch_V2
  wet_A
  wide_A
  wife_N
  win_V2
  wind_N
  window_N
  wing_N
  wipe_V2
  wonder_VQ
  wood_N
  worm_N
  write_V2
  
--}
year_N = mkN "omwaka" "emyaka" MU_MI;
-- End of New Lexicon
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
