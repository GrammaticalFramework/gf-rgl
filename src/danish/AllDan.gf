--# -path=.:../scandinavian:../abstract:../common:../prelude:../api

concrete AllDan of AllDanAbs = 
  LangDan,
  IrregDan - [fly_V],
  ExtendDan 
  ** {} ;
