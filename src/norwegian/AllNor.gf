--# -path=.:../scandinavian:../abstract:../common:../prelude:../api

concrete AllNor of AllNorAbs = 
  LangNor,
  IrregNor - [fly_V],
  ExtendNor 
  ** {} ;
