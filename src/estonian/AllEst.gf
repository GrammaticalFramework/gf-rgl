--# -path=.:../abstract:../common:../prelude:../api

concrete AllEst of AllEstAbs =
  LangEst, --  - [SlashV2VNP,SlashVV, TFut], ---- to speed up linking; to remove spurious parses
  ExtendEst -- - [ProDrop, ProDropPoss, S_OSV, S_VSO, S_ASV] -- to exclude spurious parses
  **
  open ExtraEst --- to force compilation since this module is used in many places
  in
    {} ;
