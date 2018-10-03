--# -path=.:prelude

concrete LexiconGus of Lexicon = CatGus **
  open ParadigmsGus,StructuralGus, Prelude in {

flags
  optimize=values ;

lin
person_N = regN "omonto" omo_aba;
  woman_N = regN "omosubati" omo_aba;
  house_N = regN "enyomba" e_ci ;
  tree_N = regN "omote" omo_eme;
  big_A =  regA "nene"  ;
  beautiful_A =  regA "muya"  ;
  black_A =  regA "mwamu";
  blue_A = regA "buluu" ;
  broad_A =  regA "gare"  ;
  brown_A =  regA "maraba"  ;
  clean_A =  iregA "safi" "safi";
  clever_A =  regA "ng'aini"  ;
  cold_A =  regA "kundu";
  correct_A =   regA "keene"  ;
  dirty_A =  regA "chabu" ;
  dry_A = regA "kamoku"  ;
  full_A =  regA "ichire"  ;
  good_A =  regA "ya"  ;
  green_A =  regA "machani" ;
  heavy_A =  regA "rito" ;
  hot_A = regA "morero"  ;
  important_A =    regA "eng'encho" ;
  long_A =  regA "tambe"  ;
  narrow_A =  regA "nyerere"  ;
  near_A = regA "ang'e" ;
  new_A = regA "nyia" ;
  old_A =  regA "koro"  ;
  red_A =   regA "bariri"  ;
  rotten_A =   regA "ng'undo"  ;
  round_A =    regA "viringo"  ;
  sharp_A =   regA "oge"  ;
  short_A =   regA "eng'e"  ;
  small_A =   regA "ke"  ;
  smooth_A =  regA "terere"  ;
  straight_A = regA "ronge"  ;
  stupid_A =  regA "riri"  ;
  thick_A = regA "netu" ;
  thin_A =   regA "reu"  ;
  ugly_A = regA "be";
  warm_A =  regA "berera"  ;
  wet_A =  regA "nyiu" ;
  white_A = regA "rabu"  ;
  yellow_A =  regA "yaye"  ;
  young_A =   regA "ke"  ;
  certain_A=regA "maena";
  cow_N = regN "eng'ombe"  e_ci ;
  doctor_N = regN "omorwaria" omo_aba ;
  dog_N =  regN "esese" e_ci;
  door_N =  regN "omorangwa" omo_eme  ;
  enemy_N =  regN "omobisa" omo_aba;
  fish_N =   regN "enswe" e_ci;
  friend_N =   regN "omosani" omo_aba;
  garden_N = regN "omogondo" omo_eme;
  girl_N = regN "omoiseke" omo_aba  ;
  lamp_N =  regN "etaya" e_ci;
  man_N = regN "omosacha" omo_aba  ;
  eye_N = iregN "eriso" "amaiso" eri_ama  ;
  tooth_N = regN "ero" eri_ama ;
  wife_N =   regN "omokungu" omo_aba;

oper
  aboutP = mkPrep "juu ya" ;
  atP = mkPrep "vala ve" ;
  forP = mkPrep "for" ;
  fromP = mkPrep "kutoka" ;
  inP = mkPrep "ndani" ;
  onP = mkPrep "juu" ;
  toP = mkPrep "hadi" ;
 
} ;
