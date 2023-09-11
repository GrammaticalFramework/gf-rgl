--# -path=.:../romance:../abstract:../common:prelude

concrete CatSpa of Cat = CommonX - 
  [SC,Temp,TTAnt,Tense,TPres,TPast,TFut,TCond,Pol,MU] ** CatRomance with
  (ResRomance = ResSpa) ;
