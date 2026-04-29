--# -path=.:../abstract:../common:prelude
concrete ExtendUkr of Extend = CatUkr ** open ResUkr, ParadigmsUkr in {

lin
  iFem_Pron      = mkPron "я" "мене" "мені" "мене" "мені" "мною" Fem Sg P1 ;
  youFem_Pron    = mkPron "ти" "тeбе" "тобі" "мене" "тобі" "тобою" Fem Sg P2 ;  
  weFem_Pron     = mkPron "ми" "нас" "нам" "нас" "наc" "нами" Fem Pl P1 ;
  youPlFem_Pron  = mkPron "ви" "вас" "вам" "вас" "вас" "вами" Fem Pl P2 ;  
  theyFem_Pron   = mkPron "вони" "їх" "їм" "їх" "них" "ними" Fem Pl P3 ;  
  youPolFem_Pron = mkPron "ви" "вас" "вам" "вас" "вас" "вами" Fem Pl P2 ;
  youPolPl_Pron  = mkPron "ви" "вас" "вам" "вас" "вас" "вами" Masc Pl P2 ;
  youPolPlFem_Pron = mkPron "ви" "вас" "вам" "вас" "вас" "вами" Fem Pl P2 ;

}

