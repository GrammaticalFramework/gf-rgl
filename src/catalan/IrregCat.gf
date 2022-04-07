--# -path=.:../romance:../abstract:../common:prelude
-- machine-generated GF file from Andersson & Söderberg's MSc work
concrete IrregCat of IrregCatAbs = CatCat **
open CommonRomance, ParadigmsCat, Prelude, BeschCat in {
flags optimize=values ;
  coding=utf8 ;

lin haver_V = verbV (haver_59 "haver" (True|False)) ; 
lin estar_V = verbV (estar_54 "estar") ;
lin ser_V = verbV (ser_52 "ser" (True|False)) ;
lin callar_V = verbV (cantar_15 "callar") ;
lin caure_V = verbV (caure_18 "caure") ;
lin cloure_V = verbV (cloure_19 "cloure") ;
lin complaure_V = verbV (complaure_24 "complaure") ; 
lin contradir_V = verbV (dir_41 "contradir") ;
lin cosir_V = verbV (cosir_31 "cosir") ;
lin dir_V = verbV (dir_41 "dir") ;
lin doldre_V = verbV (doldre_42 "doldre" (True|False)) ;
lin escopir_V = verbV (escopir_49 "escopir") ; 
lin fondre_V = verbV (fondre_57 "fendre") ;
lin jeure_V = verbV (jeure_62 "jeure" (True|False)) ;
lin omplir_V = verbV (omplir_80 "omplir") ;
lin tossir_V = verbV (tossir_31b "tossir") ;
lin venir_V = verbV (venir_117 "venir" (True|False)) ;

}