--# -path=.:../romance:../abstract:../common:prelude

concrete CatIta of Cat = CommonX - [SC,Temp,TTAnt,Tense,TPres,TPast,TFut,TCond,Pol,MU] ** CatRomance with
  (ResRomance = ResIta) ;
