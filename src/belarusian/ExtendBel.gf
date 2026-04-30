--# -path=.:../abstract:../common:prelude
concrete ExtendBel of Extend = CatBel ** open ResBel, ParadigmsBel in {

lin
  iFem_Pron      = mkPron "я" "мяне" "мне"  "мяне" "мне" "мной" Fem Sg P1 ;
  youFem_Pron    = mkPron "ты" "табе" "табе" "табе" "табе" "табой" Fem Sg P2 ;
  weFem_Pron     = mkPron "мы" "нас" "нам" "нас" "наc" "намі" Fem Pl P1 ;
  youPlFem_Pron  = mkPron "вы" "вас" "вам" "вас" "вас" "вамі" Fem Pl P2 ;
  theyFem_Pron   = mkPron "вони" "їх" "їм" "їх" "них" "ними" Fem Pl P3 ;  
  youPolFem_Pron = mkPron "вы" "вас" "вам" "вас" "вас" "вамі" Fem Pl P2 ;
  youPolPl_Pron  = mkPron "вы" "вас" "вам" "вас" "вас" "вамі" Fem Pl P2 ;
  youPolPlFem_Pron = mkPron "вы" "вас" "вам" "вас" "вас" "вамі" Fem Pl P2 ;

}

