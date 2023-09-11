--# -path=.:../romance:../abstract:../common:prelude

concrete CatPor of Cat = CommonX - 
  [SC,Temp,TTAnt,Tense,TPres,TPast,TFut,TCond,Pol,MU] ** CatRomance with
  (ResRomance = ResPor) ;
