--# -path=.:../abstract:../common:../api

concrete AllSco of AllScoAbs =
  LangSco,
  IrregSco - [burn_V, freeze_V],
  ExtendSco
  **
    {} ;
