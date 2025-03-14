--# -path=.:../abstract:../common:../prelude 

-- L.Boizou, 2022 <lboizou@gmail.com>

  concrete LexiconLit of Lexicon = CatLit **  
    open Prelude, ParadigmsLit  in { 

    flags  
      optimize =values ; coding =utf8 ; 

lin  
  airplane_N = mkN "lėktuvas" ;
  animal_N = mkN "gyvūnas" ;
  apartment_N = mkN "būtas" ;
  apple_N = mkN "obuolis" ;
  art_N = mkN "menas" ;
  ashes_N = mkN "pelenai" ;
  baby_N = mkN "kūdikis" ;
  back_N = mkN "nugara" ;
  bank_N = mkN "bankas" ;
  bark_N = mkN "žievė" ;
  beer_N = mkN "alus" ;
  belly_N = mkN "pilvas" ;
  bike_N = mkN "dviratis" ;
  bird_N = mkN "paukštis" ;
  blood_N = mkN "kraujas" ;
  boat_N = mkN "valtis" Fem ;
  bone_N = mkN "kaulas" ;
  book_N = mkN "knyga" ;
--  boot_N = mkN "batas" ; -- aulinai
  boot_N = compN (mkRegAdj "aulinis") (mkN "batas") ;
  boss_N = mkN "vadovas" ;
  boy_N = mkN "vaikinas" ; -- berniukas
  bread_N = mkN "duona" ;
  breast_N = mkN "krūtinė" ;
  butter_N = mkN "sviestas" ;
  camera_N = mkN "fotoaparatas" ;
  cap_N = mkN "kepurė" ; -- "kepka"...
  car_N = mkN "mašina" ;
  carpet_N = mkN "kilimas" ;
  cat_N = mkN "katė" ;
  ceiling_N = mkN "lūbos" ;
  chair_N = mkN "kėdė" ;
  cheese_N = mkN "sūris" ;
  child_N = mkN "vaikas" ;
  church_N = mkN "bažnyčia" ;
  city_N = mkN "miestas" ;
  cloud_N = mkN "debesys" ;
  coat_N = mkN "paltas" ;
  computer_N = mkN "komputeris" ;
  country_N = mkN "šalis" Fem ;
  cousin_N = mkN "pusbrolis" ;
  cow_N = mkN "karvė" ;
  day_N = mkN "diena" ;
  doctor_N = mkN "gydytojas" ;
  dog_N = mkN "šuo" ;
  door_N = mkN "durys" Fem ;
  dust_N = mkN "dulkės" ;
  ear_N = mkN "ausis" Fem;
  earth_N = mkN "žemė" ;
  egg_N = mkN "kiaušinis" ;
  enemy_N = mkN "priešas" ;
  eye_N = mkN "akis" Fem ;
  factory_N = mkN "gamykla" ;
  fat_N = mkN "taukai" ;
  feather_N = mkN "plunksna" ;
  fingernail_N = mkN "nagas" ;
  fire_N = mkN "ugnis" Fem ;
  fish_N = mkN "žuvis" Fem ;
  floor_N = mkN "grindys" ;
  flower_N = mkN "gėlė" ;
  fog_N = mkN "migla" ;
  foot_N = mkN "pėda" ; koja
  forest_N = mkN "miškas" ;
  fridge_N = mkN "šaldytuvas" ;
  friend_N = mkN "draugas" ;
  fruit_N = mkN "vaisius" ;
  garden_N = mkN "sodas" ; -- daržas
  girl_N = mkN "mergina"; -- dukra
  glove_N = mkN "pirštinė";
  gold_N = mkN "auksas" ;
  grammar_N = mkN "gramatika" ;
  grass_N = mkN "žolė" ;
  guts_N = mkN "viduriai" ;
  hair_N = mkN "plaukas" ;
  hand_N = mkN "ranka" ;
  harbour_N = mkN "uostas" ;
  hat_N = mkN "kepurė" ;
  head_N = mkN "galva" ;
  heart_N = mkN "širdis" Fem ;
  hill_N = mkN "kalva" ;
  horn_N = mkN "ragas" ;
  horse_N = mkN "arklys" ;
  house_N = mkN "namas" ;
  husband_N = mkN "vyras" ;
  ice_N = mkN "ledas" ;
  industry_N = mkN "pramonė" ;
  iron_N = mkN "geležis" ;
  king_N = mkN "karalius" ;
  knee_N = mkN "kelis" ;
  lake_N = mkN "ežeras" ;
  lamp_N = mkN "lampa" ;
  language_N = mkN "kalba" ;
  leaf_N = mkN "lapas" ;
  leather_N = mkN "oda" ;
  leg_N = mkN "koja" ;
  liver_N = mkN "kepenys" ;
  louse_N = mkN "utelė" ;
  love_N = mkN "meilė" ;
  man_N = mkN "vyras" ;
  meat_N = mkN "mėsa" ;
  milk_N = mkN "pienas" ;
  moon_N = mkN "mėnulis" ;
  mountain_N = mkN "kalnas" ;
  mouth_N = mkN "burna" ;
  music_N = mkN "muzika" ;
  name_N = mkN "vardas" ;
  neck_N = mkN "kaklas" ;
  newspaper_N = mkN "laikraštis" ;
  night_N = mkN "naktis" ;
  nose_N = mkN "nosis" ;
  number_N = mkN "skaičius" ;
  oil_N = mkN "aliejus" ;
  paper_N = mkN "popierius" ;
  peace_N = mkN "taika" ;
  pen_N = mkN "rašiklis" ;
  person_N = mkN "asmuo" ; -- žmogus
  planet_N = mkN "planeta" ;
  plastic_N = mkN "plastikas" ;
  policeman_N = mkN "policininkas" ;
  priest_N = mkN "kunigas" ;
  queen_N = mkN "karalienė" ;
  question_N = mkN "klausimas" ;
  radio_N = mkN "radijas" ;
  rain_N = mkN "lietus" ;
  reason_N = mkN "priežastis" Fem ;
  religion_N = mkN "religija" ; -- tikyba
  restaurant_N = mkN "restoranas" ;
  river_N = mkN "upė" ;
  road_N = mkN "kelias" ;
  rock_N = mkN "uola" ;
  roof_N = mkN "stogas" ;
  root_N = mkN "šaknis" Fem ;
  rope_N = mkN "virvė" ;
  rubber_N = mkN "guma" ; --trintukas
  rule_N = mkN "taisyklė" ;
  salt_N = mkN "druska" ;
  sand_N = mkN "smėlis" ;
  school_N = mkN "mokykla" ;
  science_N = mkN "mokslas" ;
  sea_N = mkN "jūra" ;
  seed_N = mkN "sėkla" ;
  sheep_N = mkN "avis" Fem ;
  ship_N = mkN "laivas" ;
  shirt_N = mkN "marškiniai" ;
  shoe_N = mkN "batas" ;
  shop_N = mkN "parduotuvė" ;
  silver_N = mkN "sidabras" ;
  sister_N = mkN "sesuo" Fem ;
  skin_N = mkN "oda" ;
  sky_N = mkN "dangus" ;
  smoke_N = mkN "dūmai" ;
  snake_N = mkN "gyvatė" ;
  snow_N = mkN "sniegas" ;
  sock_N = mkN "kojinė" ;
  song_N = mkN "daina" ;
  star_N = mkN "žvaigždė" ;
  steel_N = mkN "plienas" ;
  stick_N = mkN "lazdas" ;
  stone_N = mkN "akmuo" ;
  stove_N = mkN "krosnis" Fem ;
  student_N = mkN "studentas" ; -- studentė
  sun_N = mkN "saulė" ;
  table_N = mkN "stalas" ;
  tail_N = mkN "uodega" ;
  teacher_N = mkN "mokytojas" ; -- mokytoja
  television_N = mkN "televizija" ; -- televizorius
  tongue_N = mkN "liežuvis" ;
  tooth_N = mkN "dantis" Fem ;
  train_N = mkN "traukinys" ;
  tree_N = mkN "medis" ;
  university_N = mkN "universitetas" ;
  village_N = mkN "kaimas" ;
  war_N = mkN "karas" ;
  water_N = mkN "vanduo" ;
  wife_N = mkN "žmona" ;
  wind_N = mkN "vėjas" ;
  window_N = mkN "langas" ;
  wine_N = mkN "vynas" ;
  wing_N = mkN "sparnas" ;
  woman_N = mkN "moteris" Fem ;
  wood_N = mkN "mediena" ; --
  worm_N = mkN "kirmėlė" ; -- kirminas
  year_N = mkN "metai" ;

  distance_N3 = mkN3 (mkN "atstumas") (mkPrep "nuo" Gen) (mkPrep "iki" Gen);
  mother_N2 = mkN2 (mkN "motina");   
  brother_N2 = mkN2 (mkN "brolis" ); 
  father_N2 = mkN2 (mkN "tėvas"); 

--  paris_PN = mkN "Paryžius" ; -- Sg
--  john_PN = mkN "Jonas" ; -- Sg
  
  bad_A = mkRegAdj "blogas"; 
  beautiful_A = mkRegAdj "gražus"; 
  big_A = mkRegAdj "didelis" "didesnis" "didžiausias"; 
  black_A = mkRegAdj "juodas";
  blue_A = mkRegAdj "mėlynas"; -- or žydras 
  broad_A = mkRegAdj "platus"; 
  brown_A = mkRegAdj "rudas"; 
  correct_A = mkRegAdj "teisingas";  
  clean_A = mkRegAdj "švarus";  
  clever_A = mkRegAdj "protingas"; 
  cold_A = mkRegAdj "šaltas"; 
  dirty_A = mkRegAdj "purvinas"; 
  dry_A = mkRegAdj "sausas"; 
  dull_A = mkRegAdj "bukas"; 
  far_Adv = { s = "toli"; advType = AdjT }; 
  full_A = mkRegAdj "pilnas" ; -- 2-place adj
  good_A = mkRegAdj "geras";  
  green_A = mkRegAdj "žalias"; 
  hot_A = mkRegAdj "karštas"; 
  important_A = mkRegAdj "svarbus"; 
  long_A = mkRegAdj "ilgas";
  narrow_A =  mkRegAdj "siauras";  
  new_A =  mkRegAdj "naujas"; 
  old_A = mkRegAdj "senas"; 
  red_A =  mkRegAdj "raudonas"; 
  short_A = mkRegAdj "trumpas"; 
  small_A = mkRegAdj "mažas"; 
  stupid_A = mkRegAdj "kvailas"; 
  thick_A = mkRegAdj "storas"; 
  thin_A = mkRegAdj "plonas"; 
  today_Adv = { s = "šiandien"; advType = PronT }; -- technically it is not pronominal, but neutral position before the verb
  ugly_A = mkRegAdj "negražus"; -- or baisus 
  warm_A = mkRegAdj "šiltas"; 
  white_A = mkRegAdj "baltas";  
  yellow_A = mkRegAdj "geltonas"; 
  young_A = mkRegAdj "jaunas"; 
  now_Adv = { s = "dabar"; advType = PronT }; -- technically it is not pronominal, but neutral position before the verb
  already_Adv = { s = "jau"; advType = PronT };
  fun_AV = mkRegAdj "linksmas"; 
-- --   easy_A2V = mkRegAdj2 (mkRegAdj "łatwy") "dla" Gen ; 
  empty_A = mkRegAdj "tuščias";
--  married_A2 = addComplToAdj ( mkCompAdj "zaślubiony" ) "" Dat;
  probable_AS = mkRegAdj "galimas"; 
--  ready_A = mkCompAdj "gatavas";
--  uncertain_A = mkCompAdj "netikras";
  heavy_A = mkRegAdj "sunkus"; 
  near_A = mkRegAdj "artimas"; 
--  rotten_A = mkCompAdj "zepsuty"; 
  round_A = mkRegAdj "apvalus";
  sharp_A = mkRegAdj "aštrus"; 
  smooth_A = mkRegAdj "švelnus"; 
  straight_A = mkRegAdj "tiesus"; 
  wet_A = mkRegAdj "šlapias";
  wide_A = mkRegAdj "platus"; 

-- Laipsnuojami? guessAdj ne devrait pas apparaitre ici
--  right_Ord = { s = mkAtable (guessAdjModel "dešinys") }; 
--  left_Ord = { s = mkAtable (guessAdjModel "kairys") };  
  
--  rain_V0  = mkItVerb (mkMonoVerb "padać" conj98 Imperfective);
--  wonder_VQ = mkItVerb (mkReflVerb (mkV "zastanawiać" conj98 "zastanowić" conj77a));
--  fear_VS = mkReflVerb (mkMonoVerb "bać" conjbac Imperfective);
--  hope_VS = mkItVerb (mkComplicatedVerb (mkMonoVerb "mieć" conj100 Imperfective) "nadzieję");
--  know_VQ = mkMonoVerb "wiedzieć" conj103 Imperfective;
--  know_VS = dirV2 (mkV "žinoti" "žino" "žinojo)";
  say_VS = mkV "sakyti" "sako" "sakė"; 
--  become_VA =  (mkReflVerb (mkV "stawać" conj57 "stać" conj3)) ** {c={c=Nom;s="";adv=False}};
  answer_V2S = mkV2 (mkV "atsakyti" "atsako" "atsakė") "" Dat;
  ask_V2Q = mkV2 (mkV "klausti" "klausia" "klausė") "" Gen;
--  --   beg_V2V = mkV2 (mkV "prosić" conj83 Imperfective) "" "o" Acc Acc; -- no such verb in Polish; beg is V2S
--  paint_V2A = (mkV1 "dažyti" "dažo" "dažė") ** ({c={s="";c=Ins;adv=True}; c2={s="";c=AccC}});

  add_V3 = mkV3 (mkV "pridėti" "prideda" "pridėjo") "" Acc "prie" Gen; -- į?
  sell_V3 = dirV3 (mkV "parduoti" "parduoda" "pardavė");
  send_V3 = dirV3 (mkV "siųsti" "siunčia" "siuntė");
  talk_V3 = mkV3 (mkV "kalbėti" "kalba" "kalbėjo") "su" Ins "apie" Acc; 
  give_V3 = dirV3 (mkV "duoti" "duoda" "davė");
                        
  fear_V2 = mkV2 (mkV "bijoti" "bijo" "bijojo") "" Gen;
  hit_V2 = dirV2 (mkV "smogti" "smogia" "smogė"); 
  cut_V2 = dirV2 (mkV "pjauti" "pjauna" "pjovė"); -- Is it the best translation?
  pull_V2 = dirV2 (mkV "traukti" "traukia" "traukė"); 
  wait_V2 = mkV2 (mkV "laukti" "laukia" "laukė") "" Gen; 
  read_V2 = dirV2 (mkV "skaityti" "skaito" "skaitė"); 
  scratch_V2 = dirV2 (mkV "krapštyti" "krapšto" "krapštė");
  split_V2 = dirV2 (mkV "dalinti" "dalina" "dalino"); -- Is it the best translation?
  stab_V2 = dirV2 (mkV "durti" "duria" "dūrė");  -- Is it the best translation?
  play_V2 = dirV2 (mkV "žaisti" "žaidžia" "žaidė"); 
  bite_V2 = dirV2 (mkV "kąsti" "kanda" "kando"); 
  lose_V2 = mkV2 (mkV "prarasti" "praranda" "prardo") "" Gen; 
  eat_V2 = dirV2 (mkV "valgyti" "valgo" "valgė"); 
  put_V2 = dirV2 (mkV "dėti" "deda" "dėjo"); 
  love_V2 = dirV2 (mkV "mylėti" "myli" "mylėjo"); 
  buy_V2 = dirV2 (mkV "pirkti" "perka" "pirko"); 
  count_V2 = dirV2 (mkV "skaičiuoti" "skaičiuoja" "skaičiavo");
  like_V2 = dirV2 (mkV "mėgti" "mėgsta" "mėgo"); 
  break_V2 = dirV2 (mkV "laužti" "laužia" "laužė"); 
  wash_V2 = dirV2 (mkV "valyti" "valo" "valė"); -- plauti
  hate_V2 = mkV2 (mkV "nekęsti" "nekenčia" "nekentė") "" Gen;
  watch_V2 = mkV2 (mkV "žiūrėti" "žiūri" "žiūrėjo") "į" Acc; -- Is it the best option with compl.? 
  leave_V2 = dirV2 (mkV "palikti" "palieka" "paliko"); -- išvykti 
  open_V2 = dirV2 (mkV "atidaryti" "atidaro" "atidarė"); 
  push_V2 = dirV2 (mkV "stumti" "stumia" "stūmė"); 
  drink_V2 = dirV2 (mkV "gerti" "geria" "gėrė"); 
  write_V2 = dirV2 (mkV "rašyti" "rašo" "rašė"); 
  hunt_V2 = dirV2 (mkV "medžioti" "medžioja" "medžiojo"); 
  do_V2 = dirV2 (mkV "veikti" "veikia" "veikė");
  speak_V2 = mkV2 (mkV "kalbėti" "kalba" "kalbėjo") "su" Ins;
  understand_V2 = dirV2 (mkV "suprasti" "supranta" "suprato"); 
  throw_V2 = dirV2 (mkV "mesti" "meta" "metė"); 
  listen_V2 = dirV2 (mkV "klausyti" "klausia" "klausė"); 
  hear_V2 = dirV2 (mkV "girdėti" "girdi" "girdėjo"); 
  suck_V2 = dirV2 (mkV "čiulpti" "čiulpia" "čiulpė");     
  seek_V2 = dirV2 (mkV "ieškoti" "ieško" "ieškojo"); 
  wipe_V2 = dirV2 (mkV "šluostyti" "šluosto" "šluostė");
  squeeze_V2 = dirV2 (mkV "spausti" "spaudžia" "spaudė"); 
  rub_V2 = dirV2 (mkV "trinti" "trina" "trynė"); 
  hold_V2 = dirV2 (mkV "laikyti" "laiko" "laikė");
  learn_V2 = mkV2 (mkV "mokytis" "mokosi" "mokėjosi") "" Gen; 
  teach_V2 = mkV2 (mkV "mokyti" "moko" "mokėjo") "" Gen; 
  fight_V2 = mkV2 (mkV "kovoti" "kovoja" "kovojo") "su" Ins;
  tie_V2 = dirV2 (mkV "rišti" "riša" "rišo"); 
  see_V2 = dirV2 (mkV "matyti" "mato" "matė"); 
  know_V2 = dirV2 (mkV "žinoti" "žino" "žinojo"); 
  switch8on_V2 = dirV2 (mkV "įjungti" "įjungia" "įjungė"); 
  win_V2 = dirV2 (mkV "laimėti" "laimi" "laimėjo"); 
  switch8off_V2 = dirV2 (mkV "išjungti" "išjungia" "išjungė"); 
  kill_V2 = dirV2 (mkV "žudyti" "žudo" "žudė"); 
  close_V2 = dirV2 (mkV "uždaryti" "uždaro" "uždarė");
  forget_V2 = dirV2 (mkV "užmiršti" "užmiršta" "užmiršo"); 
  find_V2 = dirV2 (mkV "rasti" "randa" "rado"); 
  
  run_V = mkV1 "bėgti" "bėga" "bėgo"; 
  smell_V = mkV1 "kvepėti" "kvepia" "kvepė"; 
  blow_V = mkV1 "pusti" "pučia" "putė"; 
  float_V = mkV1 "plūduriuoti" "plūduriuoja" "plūduriavo"; -- plaukti
  play_V = mkV1 "žaisti" "žaidžia" "žaidė";
  go_V = mkV1 "vykti" "vyksta" "vyko"; -- to avoid opp. važiuoti/eiti 
  lie_V = mkV1 "meluoti" "meluoja" "melavo"; 
  dig_V = mkV1 "kasti" "kasa" "kasė"; --
  fly_V = mkV1 "skristi" "skrenda" "skrido";
  think_V = mkV1 "manyti" "mano" "manė"; -- galvoti
  turn_V = mkV1 "suktis" "sukasi" "sukosi";
  breathe_V = mkV1 "kvėpuoti" "kvėpuoja" "kvėpavo"; 
  burn_V = mkV1 "degti" "dega" "degė";
  spit_V = mkV1 "pjauti" "spiauna" "spjovė"; 
  flow_V = mkV1 "tekėti" "teka" "tekėjo"; 
  swim_V = mkV1 "plaukti" "plaukia" "plaukė"; 
  travel_V = mkV1 "keliauti" "keliauja" "keliavo"; 
  come_V = mkV1 "atvykti" "atvyksta" "atvyko";
  swell_V = mkV1 "išsipūsti" "išsipučia" "išsipūtė"; 
  vomit_V = mkV1 "vemti" "vemia" "vėmė"; 
  sit_V = mkV1 "atsisėti" "atisėda" "atsisėdo";
  jump_V = mkV1 "šokti" "šoka" "šoko"; 
  walk_V = mkV1 "eiti" "eina" "ėjo"; 
  sleep_V = mkV1 "miegoti" "miega" "miegojo";
  fall_V = mkV1 "krįsti" "krenta" "krito"; 
  laugh_V = mkV1 "juoktis" "juokasi" "juokėsi"; 
  sing_V = mkV1 "dainuoti" "dainuoja" "dainavo"; 
  stand_V = mkV1 "stovėti" "stoja" "stojo"; 
  sew_V = mkV1 "siūti" "siuva" "siuvo"; 
  die_V = mkV1 "mirti" "miršta" "mirė"; 
  freeze_V = mkV1 "salti" "šąla" "šalo"; 
  stop_V = mkV1 "nustoti" "nustoja" "nustojo"; 
  live_V = mkV1 "gyventi" "gyvena" "gyveno";

} ;
