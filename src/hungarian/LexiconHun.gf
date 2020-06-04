concrete LexiconHun of Lexicon = CatHun **
  open ParadigmsHun, ParamHun, Prelude in {

----
-- A

-- lin add_V3 = mkV3 ""  ;
lin airplane_N = mkN "repülőgép" "repülőgépet" ;
-- lin alas_Interj = mkInterj "" ;
-- lin already_Adv = mkA "" ;
lin animal_N = mkN "állat" "állatot" "állatok" "állata" ;
-- lin answer_V2S = mkV2S "válasz" ;
lin apartment_N = mkN "lakás" "lakást" ;
lin apple_N = mkN "alma" ;
lin art_N = mkN "művészet" "művészetet" ;
-- lin ashes_N = mkN "" ;
-- lin ask_V2Q = mkV2 "" ;

----
-- B

lin baby_N = mkN "bébi" "bébit" ;
-- lin back_N = mkN "" ;
lin bad_A = mkA "rossz" ;
lin bank_N = mkN "bank" "bankot" ;
-- lin bark_N = mkN "" ;
lin beautiful_A = mkA "szép" ;
-- lin become_VA = mkVA "" ;
lin beer_N = mkN "sör" "sört" ;
-- lin beg_V2V = mkV2 "" ;
lin belly_N = mkN "has" "hasat" ;
lin big_A = mkA "nagy" ;
lin bike_N = mkN "bicikli" "biciklit";
lin bird_N = mkN "madár" "madarat";
-- lin bite_V2 = mkV2 "" ;
lin black_A = mkA "fekete" ;
lin blood_N = mkN "vér" "vért";
-- lin blow_V = mkV "" ;
lin blue_A = mkA "kék" ;
lin boat_N = mkN "hajó" "hajót" "hajók" "hajója" ;
-- lin bone_N = mkN "" ;
-- lin boot_N = mkN "" ;
-- lin boss_N = mkN "" ;
lin book_N = mkN "könyv" "könyvet" ;
lin boy_N = mkN "fiú" ;
lin bread_N = mkN "kenyér" "kenyeret";
lin break_V2 = mkV2 "szünet" ;
lin breast_N = mkN "mell" "mellet" ;
-- lin breathe_V = mkV "" ;
-- lin broad_A = mkA "" ;
-- lin brother_N2 = mkN "öccsém" ; (possessive form?)
lin brown_A = mkA "barna" ;
-- lin burn_V = mkV "" ;
lin butter_N = mkN "vaj" "vajat" ;
-- lin buy_V2 = mkV2 "" ;

----
-- C

lin camera_N = mkN "fényképezőgép" "fényképezőgépet";
-- lin cap_N = mkN "" ;
lin car_N = mkN "autó" "autót" ;
lin carpet_N = mkN "szőnyeg" "szőnyeget" ;
lin cat_N = mkN "macska" "macskát";
lin ceiling_N = roof_N ;
lin chair_N = mkN "szék" "széket";
lin cheese_N = mkN "sajt" "sajtot" ;
lin child_N = mkN "gyerek" "gyereket" ;
lin church_N = mkN "templom" "templomot" ;
lin city_N = mkN "város" "várost" ;
lin clean_A = mkA "tiszta" ;
lin clever_A = mkA "okos" ;
lin close_V2 = mkV2 "közel" ;
lin cloud_N = mkN "felhő" "felhőt" ;
lin coat_N = mkN "kabát" "kabátot" ;
lin cold_A = mkA "hideg" ;
-- lin come_V = mkV "" ;
lin computer_N = mkN "számítógép" "számítógépet" ;
lin correct_A = mkA "igaz" ;
-- lin count_V2 = mkV2 "" ;
lin country_N = mkN "ország" "országot" "országok" "országa" ;
lin cousin_N = mkN "unoka" (mkN "testvér" "testvért") ; --short "unoka"
lin cow_N = mkN "tehén" ;
-- lin cut_V2 = mkV2 "" ;
--
-- ----
-- -- D
--
lin day_N = mkN "nap" "napot" ;
-- lin die_V = mkV "" ;
-- lin dig_V = mkV "" ;
-- lin dirty_A = mkA "" ;
-- lin distance_N3 = mkN "" ;
-- lin do_V2 = mkV2 do_V ;
lin doctor_N = mkN "orvos" "orvost";
lin dog_N = mkN "kutya" "kutyát";
lin door_N = mkN "ajtó" "ajtót" ;
-- lin drink_V2 = mkV2 "" ;
-- lin dry_A = mkA "" ;
-- lin dull_A = mkA "" ;
-- lin dust_N = mkN "" ;

----
-- E

lin ear_N = mkN "fül" "fület";
lin earth_N = mkN "föld" "földet";
-- lin eat_V2 = mkV2 "" ;
lin egg_N = mkN "tojás" "tojást" ;
lin empty_A = mkA "üres" ;
lin enemy_N = mkN "ellenség" "ellenséget" ;
lin eye_N = mkN "szem" "szemet";

----
-- F

lin factory_N = mkN "gyár" "gyárat" ;
-- lin fall_V = mkV "" ;
-- lin far_Adv = mkA "" ;
lin fat_N = mkN "kövér" "kövéret";
lin father_N2 = mkN2 (mkN "apa" "apát") ;
-- lin fear_V2 = mkV2 "" ;
-- lin fear_VS = mkVS "" ;
lin feather_N = mkN "madártoll" "madártollat";
-- lin fight_V2 = mkV2 "" ;
-- lin find_V2 = mkV2 "" ;
lin fingernail_N = mkN "köröm" "körmöt";
lin fire_N = mkN "tűz" "tüzet" ;
lin fish_N = mkN "hal" "halat" ;
-- lin float_V = mkV "" ;
lin floor_N = mkN "padló" "padlót" ;
-- lin flow_V = mkV "" ;
lin flower_N = mkN "virág" "virágot" "virágok" "virága" ;
lin fly_V = mkV "repül" ;
lin fog_N = mkN "köd" "ködöt" "ködön" "ködhöz" "ködök"
                "ködöm" "köde" "ködünk" "ködei" ;
lin foot_N = leg_N ; --same as leg, to specify "lábfej"
lin forest_N = mkN "erdő" "erdőt" ;
-- lin forget_V2 = mkV2 "" ;
-- lin freeze_V = mkV "" ;
lin fridge_N = mkN "hűtő" "hűtőt" ;
lin friend_N = mkN "barát" "barátot" ;
lin fruit_N = mkN "gyümölcs" "gyümölcsöt" "gyümölcsök" "gyümölcse" ; --TODO: plural PossPl2 fails "gyümölcseitek" instead of "gyümölcseitök", wovel harmony changing?
lin full_A = mkA "tele" ;
-- --lin fun_AV

----
-- G

lin garden_N = mkN "kert" "kertet" ;
lin girl_N = mkN "lány" "lányt";
-- lin give_V3 = mkV3 "" ;
lin glove_N = mkN "kesztyű" "kesztyűt" ;
-- lin go_V = mkV "" ;
lin gold_N = mkN "arany" "aranyat" ;
lin good_A = mkA "jó" ;
lin grammar_N = mkN "nyelvtan" "nyelvtant";
lin grass_N = mkN "fű" "füvet";
lin green_A = mkA "zöld" ;

----
-- H

lin hair_N = mkN "haj" "hajat" ;
lin hand_N = mkN "kéz" "kezet" ;
-- lin harbour_N = mkN "" ;
lin hat_N = mkN "kalap" "kalapot" ;
-- lin hate_V2 = mkV2 "" ;
lin head_N = mkN "fej" "fejet";
-- lin hear_V2 = mkV2 "" ;
lin heart_N = mkN "szív" "szívet";
lin heavy_A = mkA "nehéz" ;
-- lin hill_N = mkN "" ;
-- lin hit_V2 = mkV2 "" ;
-- lin hold_V2 = mkV2 "" ;
-- lin hope_VS = mkV "" ;
-- lin horn_N = mkN "" ;
lin horse_N = mkN "ló" "lovat";
lin hot_A = mkA "forró" ;
lin house_N = mkN "ház" "házat" ;
-- lin hunt_V2 = mkV2 "" ;
lin husband_N = mkN "férj" "férjet";

--------
-- I - K

lin ice_N = mkN "jég" "jeget" ;
lin industry_N = mkN "ipar" "ipart" ;
lin iron_N = mkN "vas" "vasat" ;
-- lin john_PN = mkPN "" ;
-- lin jump_V = mkV "" ;
-- lin kill_V2 = mkV2 "" ;
lin king_N = mkN "király" "királyt" ;
lin knee_N = mkN "térd" "térdet";
-- lin know_V2 = mkV2 "" ;
-- lin know_VQ = mkVQ "" ;
-- lin know_VS = mkV "" ;


----
-- L

lin lake_N = mkN "tó" "tavat" ;
lin lamp_N = mkN "lámpa" "lámpát";
lin language_N = mkN "nyelv" "nyelvet";
-- lin laugh_V = mkV "" ;
lin leaf_N = mkN "levél" "levelet";
-- lin learn_V2 = mkV2 "" ;
lin leather_N = mkN "bőr" "bőrt";
-- lin leave_V2 = mkV2 "" ;
lin leg_N = mkN "láb" "lábat";
-- lin lie_V = mkV "" ;
-- lin like_V2 = mkV2 "" ;
-- lin listen_V2 = mkV2 "" ;
-- lin live_V = mkV "";
-- lin liver_N = mkN "" ;
-- lin long_A = mkA "" ;
-- lin lose_V2 = mkV2 "" ;
-- lin louse_N = mkN "" ;
lin love_N = mkN "szerelem" "szerelmet";
-- lin love_V2 = mkV2 "" ;

----
-- M

lin man_N = mkN "férfi" "ak" harmA ; -- force plural allomorph and a-harmony
lin married_A2 = mkA2 "házas" Ins ;
lin meat_N = mkN "hús" "húst";
lin milk_N = mkN "tej" "tejet" ;
lin moon_N = mkN "hold" "holdat" ;
lin mother_N2 = mkN2 (mkN "anya" "anyát") ;
lin mountain_N = mkN "hegy" "hegyet";
lin mouth_N = mkN "száj" "szájat" ;
lin music_N = mkN "zene" "zenét";

----
-- N

lin name_N = mkN "név" "nevet" ;
lin narrow_A = mkA "keskeny" "keskenyet"; --also "szűk"
lin near_A = mkA "közel" ;
lin neck_N = mkN "nyak" "nyakat";
lin new_A = mkA "új" ;
lin newspaper_N = mkN "újság" "újságot" ;
lin night_N = mkN "éjszaka" "éjszakát"; --also shortened to "éj" ("este" more for evening)
lin nose_N = mkN "orr" "orrot" ;
lin now_Adv = mkAdv "most" ;
lin number_N = mkN "szám" "számot" ;
--
-- --------
-- -- O - P
--
--
lin oil_N = mkN "olaj" "olajat" ; -- TODO olajok or olajak?
lin old_A = mkA "öreg" ; --also "idős"
-- lin open_V2 = mkV2 "" ;
-- lin paint_V2A = mkV2A "" ;
lin paper_N = mkN "papír" "papírt" "papírok" "papírja" ;
-- lin paris_PN = mkPN "Paris" ;
lin peace_N = mkN "béke" "békét";
lin pen_N = mkN "toll" "tollat" ;
lin person_N = mkN "ember" "embert";
lin planet_N = mkN "bolygó" "bolygót" ;
lin plastic_N = mkN "műanyag" "műanyagot" "műanyagok" "műanyaga" ;
-- lin play_V = mkV "" ;
lin policeman_N = mkN "rendőr" "rendőrt"; --the police "rendőrség"
lin priest_N = mkN "pap" "papot" "papok" "papja" ;
-- lin pull_V2 = mkV2 "" ;
-- lin push_V2 = mkV2 "" ;
-- lin put_V2 = mkV2 "" ;
--
-- --------
-- -- Q - R
--
lin queen_N = mkN "kírálynő" "kírálynőt" ;
lin question_N = mkN "kérdés" "kérdést" ;
lin radio_N = mkN "rádió" "rádiót" ;
lin rain_N = mkN "eső" "esőt" ;
-- lin rain_V0 = mkV "" ;
-- lin read_V2 = mkV2 "" ;
lin ready_A = mkA "kész" ;
lin reason_N = mkN "ok" "okot" "okok" "oka" ;
lin red_A = mkA "piros" ;
lin religion_N = mkN "vallás" "vallást";
lin restaurant_N = mkN "étterem" "éttermet";
lin river_N = mkN "folyó" "folyót" ;
lin road_N = mkN "út" "utat" ;
lin rock_N = mkN "szikla" "sziklát";
lin roof_N = mkN "plafon" "plafont" "plafonok" "plafonja" ;
lin root_N = mkN "gyökér" "gyökeret";
lin rope_N = mkN "kötél" "kötelet";
-- lin rotten_A = mkA "" ;
-- lin round_A = mkA "" ;
-- lin rub_V2 = mkV2 "" ;
lin rubber_N = mkN "gumi" "gumit";
lin rule_N = mkN "szabály" "szabályt" ;
-- lin run_V = mkV "" ;

----
-- S

lin salt_N = mkN "só" "sót" ;
lin sand_N = mkN "homok" "homokot" "homokok" "homokja" ;
-- lin say_VS = mkVS "" ;
lin school_N = mkN "iskola" "iskolát";
lin science_N = mkN "tudomány" "tudományt" ;
-- lin scratch_V2 = mkV2 "" ;
lin sea_N = mkN "tenger" "tengert";
lin see_V2 = mkV2 "lát" ;
lin seed_N = mkN "mag" "magot" "magok" "magja" ;
-- lin seek_V2 = mkV2 "" ;
-- lin sell_V3 = mkV3 "" ;
-- lin send_V3 = mkV3 "" ;
-- lin sew_V = mkV "" ;
-- lin sharp_A = mkA "" ;
-- lin sheep_N = mkN "" fem ;
-- lin ship_N = mkN "" ;
lin shirt_N = mkN "ing" "inget"; --shirt like t-shirt or the more formal?
lin shoe_N = mkN "cipő" "cipőt" ;
lin shop_N = mkN "üzlet" "üzletet";
lin short_A = mkA "rövid" ; --in short text, if human length then "alacsony"
lin silver_N = mkN "ezüst" "ezüstöt" "ezüstön" "ezüsthöz" "ezüstök"
                   "ezüstöm" "ezüstje" "ezüstünk" "ezüstjei" ;
-- lin sing_V = mkV "" ;
-- lin sister_N = mkN "" ;
-- lin sit_V = mkV "" ;
lin skin_N = mkN "bőr" "bőrt";
lin sky_N = mkN "ég" "eget" ;
-- lin sleep_V = mkV "" ;
lin small_A = mkA "kicsi" "kicsit";
-- lin smell_V = mkV "" ;
lin smoke_N = mkN "füst" "füstet";
lin smooth_A = mkA "sima" ;
lin snake_N = mkN "kígyó" "kígyót" ;
lin snow_N = mkN "hó" "havat" ;
lin sock_N = mkN "zokni" "zoknit";
lin song_N = mkN "dal" "dalt" ;
-- lin speak_V2 = mkV2 "" ;
-- lin spit_V = mkV "" ;
-- lin split_V2 = mkV2 "" ;
-- lin squeeze_V2 = mkV2 "" ;
-- lin stab_V2 = mkV2 "" ;
-- lin stand_V = mkV "" ;
lin star_N = mkN "csillag" "csillagot" "csillagok" "csillaga" ;
lin steel_N = mkN "acél" "ok" harmA ;
lin stick_N = mkN "rúd" "rudat" ;
lin stone_N = mkN "kő" "követ" ;
-- lin stop_V =  mkV "" ;
-- lin stove_N = mkN "" ;
lin straight_A = mkA "egyenes";
lin student_N = mkN "diák" "diákot" ;
lin stupid_A = mkA "buta" ; --also "hülye"
-- lin suck_V2 = mkV2 "" ;
lin sun_N = mkN "nap" "napot"; --same as day
-- lin swell_V = mkV "" ;
-- lin swim_V = mkV "" ;

----
-- T


lin table_N = mkN "asztal" "asztalt";
lin tail_N = mkN "farok" "farkot";
lin talk_V3 = mkV3 "beszél" ;
-- lin teach_V2 = mkV2 "" ;
lin teacher_N = mkN "tanár" "tanárt" ;
lin television_N = mkN "tévé" "tévét" ; --also "televízió" but not used
lin thick_A = mkA "vastag" ;
lin thin_A = mkA "vekony" ;
-- lin think_V = mkV "" ;
-- lin throw_V2 = mkV2 "" ;
-- lin tie_V2 = mkV2 "" ;
lin today_Adv = mkAdv "ma" ;
lin tongue_N = mkN "nyelv" "nyelvet"; --same as language
lin tooth_N = mkN "fog" "fogat" ;
lin train_N = mkN "vonat" "vonatot" "vonatok" "vonata" ;
-- lin travel_V = mkV "" ;
lin tree_N = mkN "fa" "fát";
-- lin turn_V = mkV "" ;

--------
-- U - V

lin ugly_A = mkA "csúf" ;
-- lin uncertain_A = mkA "" ;
-- lin understand_V2 = mkV2 "" ;
lin university_N = mkN "egyetem" "egyetemet";
lin village_N = mkN "falu" "falut" "falvak";
-- lin vomit_V = mkV2 "" ;

--------
-- W - Y

-- lin wait_V2 = mkV2 "" ;
-- lin walk_V = mkV "" ;
lin war_N = mkN "háború" "háborút" ;
lin warm_A = mkA "meleg" ;
-- lin wash_V2 = mkV2 "" ;
-- lin watch_V2 = mkV2 "" ;
lin water_N = mkN "víz" "vizet" ;
lin wet_A = mkA "nedves" ;
lin white_A = mkA "fehér" ;
lin wide_A = mkA "széles" ;
lin wife_N = mkN "feleség" "feleséget" ;
-- lin win_V2 = mkV2 "" ;
lin wind_N = mkN "szél" "szelet" ;
lin window_N = mkN "ablak" "ablakot" "ablakok" "ablaka" ;
lin wine_N = mkN "bor" "bort";
lin wing_N = mkN "szárny" "szárnyat";
-- lin wipe_V2 = mkV2 "" ;
lin woman_N = mkN "nő" "nőt" ;
-- lin wonder_VQ = mkVQ "" ;
lin wood_N = mkN "fa" "fát"; --same as tree
lin worm_N = mkN "féreg" "férget"; --also "kukac"
lin write_V2 = mkV2 (mkV "írok" "írsz" "ír" "írunk" "írtok" "írnak" "írni") ;
lin year_N = mkN "év" "évet";
lin yellow_A = mkA "sárga" ;
lin young_A = mkA "fiatal" ;

}
