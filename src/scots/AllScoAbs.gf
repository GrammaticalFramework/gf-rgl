--# -path=.:../abstract:../common:prelude

abstract AllScoAbs =
  Lang,
  IrregScoAbs - [burn_V, freeze_V],
  Extend
  ** {} ;
