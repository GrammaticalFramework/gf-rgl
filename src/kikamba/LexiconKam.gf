--# -path=.:prelude

concrete LexiconKam of Lexicon = CatKam **
  open ParadigmsKam,StructuralKam, Prelude in {

flags
  optimize=values ;

lin
 add_V3=regV "ongela" ;
already_Adv= mkAdv "tenange" ;
animal_N=regN "nyamu" n_n ;
answer_V2S=regV "sungia" ;
ashes_N=regN "muu" mu_mi ;
ask_V2Q=regV "kulya" ;
back_N=regN "muongo" mu_mi;
bark_N=regN "ikonde" i_ma ;
become_VA=regV "ithiwa" ;
beg_V2V=regV "voya" ;
belly_N=regN "ivu" i_ma ;
bite_V2=regV "uma" ;
blood_N=regN "nthakame" n_n ;
blow_V=regV "uutana" ;
bone_N=regN "ivindi" i_ma ;
break_V2=regV "vunzya" ;
breast_N=regN "nondo" n_n;
breathe_V=regV "veva" ;
burn_V=regV "vya" ;
close_V2=regV "vinga" ;
cloud_N=regN "ithweo" i_ma ;
come_V=regV "uka" ;
count_V2=regV "tala" ;
cut_V2=regV "tema" ;
day_N=regN "muthenya" mu_mi;
die_V=regV "kw'a" ;
dig_V=regV "inza" ;
do_V2=regV "ika" ;
drink_V2=regV "nyw'a" ;
dull_A=regA "tuku" ; --- check the actual translation
dust_N=regN "kitoo" ki_i;
ear_N=regN "kutu"  ku_ma; 
earth_N=regN "nthi" n_n;
--easy_A2V=regV "vuthu" ;
eat_V2=regV "ya" ;
egg_N=regN "itumbi" i_ma;
fall_V=regV "valuka" ;
far_Adv=mkAdv"kuasa" ;
fat_N=regN "mauta" i_ma ;
fear_V2=regV "kia" ;
fear_VS=regV "kia" ;
feather_N=regN "iwia" i_ma ;
fight_V2=regV "ukita" ;
find_V2=regV "ona" ;
fingernail_N=regN "waa" u_n; 
fire_N=regN "mwaki" mu_mi;
float_V=regV "lela" ;
flow_V=regV "nyunyuluka" ;
flower_N=regN "ilaa" i_ma;
fly_V=regV "uluka" ;
fog_N=regN "muumbi" mu_mi;
foot_N=regN "kuu" ku_ma ; 
forest_N=regN "kitheka" ki_i ;
forget_V2=regV "ulwa" ;
freeze_V=regV "thitha" ;
--fun_AV=regV "uthekan'yo" ;
give_V3=regV "nengane" ;
go_V=regV "thi" ; 
grass_N=regN "nyeki" n_n ;
guts_N=regN "muluku" mu_mi;
hair_N=regN "nzwii" n_n;
hand_N=iregN "kw'oko" "moko" ku_ma;
hate_V2=regV "mena" ;
head_N=regN "mutw'e" mu_mi ;
hear_V2=regV "iw'a" ;
heart_N=regN "ngoo" n_n;
hit_V2=regV "kima" ;
hold_V2=regV "kwata" ;
hope_VS=regV "ikwatye" ; 
horn_N=iregN "uvya" "mbvya" u_n;
hunt_V2=regV "syima" ;
husband_N=regN "muume" mu_a ;
ice_N=iregN "ia" "maia" i_ma;
industry_N= regN "kiwanda" ki_i ; 
jump_V=regV "thaanyaku" ;
kill_V2=regV "uua" ;
knee_N=regN "iu" i_ma;
know_V2=regV "manya" ;
know_VQ=regV "manya" ;
know_VS=regV "manya" ;
laugh_V=regV "theka" ;
leaf_N=regN "itu" i_ma;
learn_V2=regV "soma" ;
leather_N=regN "kithuma" ki_i;
leave_V2=regV "tia" ;
lie_V=regV "kenga" ;
like_V2=regV "enda" ;
listen_V2=regV "ithukisye" ; 
live_V=regV "ithiwa thayu" ;
liver_N=regN "itema" i_ma;
lose_V2=regV "asya" ;
louse_N=regN "ndaa" n_n;
love_N=regN "wende" u_ma;
love_V2=regV "enda" ;
meat_N=regN "nyama" n_n ;
milk_N=regN "iia" i_ma;
moon_N=regN "mwei" mu_mi;
mountain_N=regN "kiima" ki_i ;
mouth_N=regN "munuka" mu_mi;
music_N=regN "wini" u_ma ; 
name_N=regN "isyitwa" i_ma;
neck_N=regN "ngingo" n_n ;
newspaper_N=regN "ikaseti" i_ma ; 
night_N=regN "utuku" u_ma;
nose_N=regN "inyuu" i_ma;
now_Adv=mkAdv "yuyu" ;
number_N=regN "utalo" u_ma ;
oil_N=regN "iuta" i_ma ; 
open_V2=regV "vingua" ;
paint_V2A=regV "vaka langi" ;
paper_N=regN "ikalatasi" i_ma ;
peace_N=regN "muuo" mu_mi;
pen_N=regN "kiandiki" ki_i;
planet_N=regN "pulaneti"  n_n;
plastic_N=regN "pulasitiki" n_n;
play_V=regV "thauka" ;
play_V2=regV "thauka" ;
policeman_N=regN "musikali wa volisi" mu_a ;
priest_N=regN "muthembi" mu_a ;
--probable_AS=regV "" ;
pull_V2=regV "kusya" ;
push_V2=regV "sukuma" ;
put_V2=regV "ikia" ;
queen_N=regN "kiveti kya musumbi" mu_a; 
radio_N=regN "rendio" n_n;
rain_N=regN "mbua" n_n ;
rain_V0=regV "ua" ;
read_V2=regV "soma" ;
religion_N=regN "ndini" n_n ;
restaurant_N=regN "uteli" n_n ;
river_N=regN "usi" u_n ;
road_N=regN "nzia" n_n ;
rock_N=regN "ivia" i_ma;
roof_N=regN "kiala" ki_i ;
root_N=regN "mui" mu_mi ;
rope_N=regN "mukwa" mu_mi;
rub_V2=regV "kiitha" ;
rubber_N=regN "kivuti" ki_i ;
run_V=regV "sema" ;
salt_N=regN "munyu" mu_mi;
sand_N=regN "kithangathi" ki_i;
say_VS=regV "asya" ;
school_N=regN "sukulu" n_n;
science_N=regN "sayasi" n_n ;
scratch_V2=regV "thua" ;
sea_N=regN "ukanga" u_ma;
see_V2=regV "ona" ;
seed_N=regN "mbeu" n_n ;
seek_V2=regV "mantha" ;
sell_V3=regV "thoosya" ;
send_V3=regV "tuma" ;
sew_V=regV "tuma" ;
sheep_N=regN "ilondu" i_ma;
ship_N=regN "ngalawa" n_n ;
shoe_N=regN "kiatu" ki_i;
shop_N=regN "nduka" n_n ;
silver_N=iregN "vetha" "vetha" u_n; 
sing_V=regV "ina" ;
sister_N=regN "mwiitu-a-inya" mu_a;
sit_V=regV "ikala" ;
skin_N=regN "kikonde" i_ma;
sky_N=regN "yayaya" i_ma ;
sleep_V=regV "koma" ;
smell_V=regV "nyunga" ;
smoke_N=regN "syuki" i_ma;
snake_N=regN "nzoka" n_n ;
snow_N=iregN "ia""maia" i_ma;
sock_N=regN "sokisi" n_n;
song_N=iregN "wathi" "mbathi" u_n; 
speak_V2=regV "neena" ;
spit_V=regV "tw'ila" ;
split_V2=regV "tilany'a" ;
squeeze_V2=regV "mitua" ;
stab_V2=regV "tonya" ;
stand_V=regV "ungama" ;
star_N=regN "ndata" n_n;
steel_N=regN "kiaa" ki_i;
stick_N=regN "munzyu" mu_mi ;
stone_N=regN "ivia" i_ma;
stop_V=regV "ungama" ;
stove_N=iregN "iko" "maiko" i_ma;
student_N=regN "musomi" mu_a ;
suck_V2=regV "mumunya" ;
sun_N=regN "syua" i_ma;
swell_V=regV "imba" ;
swim_V=regV "thambia" ;
switch8off_V2=regV "vosya" ;
switch8on_V2=regV "kwatya" ;
table_N=regN "mesa" n_n ;
tail_N=regN "mwithe" mu_mi ;
talk_V3=regV "neena" ;
teacher_N=regN "mumanyisya" mu_a ;
television_N=regN "televesini" n_n ;
think_V=regV "suania" ;
tie_V2=regV "ova" ;
tongue_N=regN "uimi" u_ma ; 
train_N=iregN "ngali ya mwaki" "ngali sya mwaki" n_n ;
travel_V=regV "thi" ;
turn_V=regV "alyula" ;
understand_V2=regV "elewa" ;
university_N=regN "nyunivasiti"  n_n;
village_N=regN "ndua" n_n ;
vomit_V=regV "tavika" ;
wait_V2=regV "eteela" ;
walk_V=regV "tembea" ;
war_N=regN "kau" u_ma; 
wash_V2=regV"thamba" ;
watch_V2=regV "syaiisya" ;
water_N=regN "kiwu" ki_i;
wide_A=regA "aamu" ;
win_V2=regV "shinda" ;
wind_N=regN "kiseve" ki_i;
window_N=regN "mbuthi" n_n;
wine_N=regN "mbinyu" n_n ;
wing_N=regN "uthwau" u_ma;
leg_N = regN "kuu" ku_ma;
wipe_V2=regV "vangula" ;
wonder_VQ=regV "senga" ;
wood_N=iregN "uku" "nguku" u_n; 
write_V2=regV "andika" ;
year_N=regN "mwaka" mu_mi;
  hill_N =regN "kiima" ki_i;
  king_N= regN "musumbi" mu_a;
  brother_N2 = mkN2 (mkN "mwana inya" mu_a) ;
  father_N2 = mkN2 (mkN "nau" "nau"mu_a) mkPrepof ;
   mother_N2 = mkN2 (mkN "mwaitu" mu_a) mkPrepof;
  person_N = regN "mundu" mu_a ;
  woman_N =regN "kiveti" ki_i ;
  house_N = regN "nyumba" n_n  ; 
   tree_N = regN  "muti" mu_mi ;
  big_A = regA "nene" ;
  beautiful_A = regA "nake"   ;
  black_A =  regA "iu"  ;
  
  broad_A = regA "amu"   ;
  brown_A = cregA " kaki"  ;
  clean_A =  regA "theu"  ;
  clever_A = regA "ui"   ;
  cold_A =  regA "thithu" ;
  correct_A = regA "seo"   ;
  dry_A = regA "nyau"  ;
  full_A = regA "usuu"  ;
  good_A =  regA "seo"  ;
   heavy_A =  regA "ito"  ;
  important_A =   regA "vata" ;
  long_A = regA "asa" ;
  narrow_A =  regA "theke"  ;
  near_A = regA "kuvi" ;
  new_A =  regA "sau" ;
  old_A = regA "kuu" ;
  red_A =  regA "tune"  ;
  rotten_A =  regA "oou" ;
  round_A =  regA "thyululu" ;
  sharp_A =  regA "ui" ;
  short_A =  regA "kuvi";
    smooth_A = regA "mbinyu"  ;
  straight_A =  regA "laini" ;
  stupid_A = regA "tumanu"   ;
  thick_A = regA "amu"  ;
  thin_A =  regA "theke"  ;
  ugly_A = regA "thuku"  ;
  warm_A = regA "utea" ;
  wet_A =  regA "iu" ;
  white_A =  regA "eu"  ;
  yellow_A = cregA  "nyelo"  ;
  young_A =  regA "nini";
  
    small_A = regA "nini" ;
    shirt_N= iregN "sati" "sati" n_n;
  cow_N = regN "ngombe"  n_n  ;
  doctor_N = regN "muiiti"  mu_a  ;
  dog_N = regN "ngiti"  n_n ;
  door_N = regN " muomo"  mu_mi  ;
  enemy_N = regN "muthiny'a"  mu_a ;
  fish_N =  regN "ikuyu"  i_ma ;
  friend_N =  regN "munyanya"  mu_a ;
  garden_N = regN "muunda"  mu_mi  ;
 girl_N = regN "mwiitu"  mu_a  ;
lamp_N = iregN "taa" "taa"  n_n ;
 man_N = regN " muume"    mu_a  ;
eye_N = regN "iitho"  i_ma ;
tooth_N = regN "ieo"  i_ma  ;
wife_N =  regN "muka"  mu_a ;
left_Ord = mkOrd "kwoko kwa aka" ;
  right_Ord = mkOrd "lungalu" |mkOrd "ailu" ;
  grammar_N = regN "ngulama" n_n ; 
  language_N = regN "kithiomo" ki_i ; 
  rule_N = regN "mwiao" mu_mi ; 
  married_A2 = mkA2 (regA "twae") "ni" ;
  airplane_N = regN "ndeke" n_n;
  alas_Interj = ss "asi" ;
  answer_V2S = regV "sungia"  ;
  apartment_N = regN "nyumba" n_n; 
  apple_N = regN "avuu" i_ma ;
  art_N = regN "ithaa" i_ma ;
  ask_V2Q = regV "kulya" ;
  baby_N = regN "kana" ka_tu;
  bad_A = regA "thuku"  ;
  bank_N = regN "vengi" n_n ;
  beer_N = regN "nzovi" n_n ;
   big_A = regA "nene" ;
  bike_N = regN "kisululu" ki_i ;
  bird_N = regN "nyunyi" n_n;
  black_A = regA "iu" ;
  blue_A = cregA "waiyu"  ;
  boat_N = regN "italu" i_ma;
  book_N = regN "ivuku" i_ma ;
  boot_N = regN "mbuti" n_n ; 
  boss_N = regN "munene" mu_a ;
  boy_N = regN "kivisi" ki_i ;
  bread_N = regN "mukate" mu_mi ;
  butter_N = regN "kimoesyo" ki_i ;
  buy_V2 = regV "ua" ;
  camera_N = regN "kamela" n_n;
  cap_N = regN "kakovila" ka_tu ;
  car_N = regN "mutokaa" mu_mi ;
  carpet_N = regN "mukeka" mu_mi ;
  cat_N = regN "mbaka" n_n ;
  ceiling_N = regN "kisumba" ki_i ;
  chair_N = regN "kivila" ki_i ;
  cheese_N = regN "iuta" i_ma ;
  child_N = iregN "mwana" "syana" mu_a ;
  church_N = regN "ikanisa" i_ma;
  city_N = regN "musyi" mu_mi; 
  clean_A = regA "theu" ;
  clever_A = regA "ui"  ;
   coat_N = regN "ikoti" i_ma;
    computer_N = regN "kombyuta" n_n ;
  country_N = regN "nthi"  n_n;
  cousin_N = regN "mwendw'asa" mu_a | regN "mwana a mwendya" mu_a ;
  dirty_A = iregA "kiko" "kiko" ;
   dog_N = regN "ngiti" n_n;
  empty_A = regA "thei" ;
  factory_N = regN "kithii" ki_i ;
  fish_N = regN "ikuyu" i_ma ;
  floor_N = iregN "nginyo" "nginyo" n_n ; 
   fridge_N = regN "frinji" n_n ;
  friend_N = regN "munyanya"mu_a ;
  fruit_N = regN "itunda" i_ma ; 
  --fun_AV = mkAV (regA "fun") ;
  garden_N = regN "muunda" mu_mi;
  girl_N = regN "mwiitu" mu_a ;
  glove_N = regN "ngloovu" n_n;
  gold_N = regN "thaavu" n_n;
  good_A = regA "seo" ;
    green_A = cregA "matu" ;
  harbour_N = regN "kilindi" ki_i ;
   hat_N = regN "ngovia" n_n;
    hill_N = regN "kiima" ki_i;
   horse_N = regN "mbalasi" n_n ;
  hot_A = regA "latiku" ;
  house_N = regN "nyumba" n_n;
  important_A = regA "vata" ;--check
   iron_N = regN "kyuma" ki_i ;
   lake_N = regN "ia" i_ma;
   teach_V2 = regV "somethya";
   throw_V2 = regV "isya";


-- added 4/6/2007
    paris_PN = regPN   "Paris" va_ku ; -- this is alreay name of place avoid kwa
    john_PN = regPN   "Yoana" mu_a ;  
    question_N = regN "Ikulyo" i_ma ; 
    ready_A = regA "ready" ;
    reason_N = regN "kitumi" ki_i ; 
    today_Adv = mkAdv "umuthi" ;
    uncertain_A = regA "uncertain" ;
    distance_N3 = mkN3 (mkN "muendo"  mu_mi ) fromP toP  ;

oper
  aboutP = mkPrep "undu wa" ;
  atP = mkPrep "vala ve" ;
  forP = mkPrep "for" ;
  fromP = mkPrep "kuma" ;
  inP = mkPrep "thini" ;
  onP = mkPrep "iulu" ;
  toP = mkPrep "kuvika" ;
 -- ofp = possess_Prep.sp ;
} ;