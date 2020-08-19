--# -path=.:../abstract:../common:../api:../prelude

concrete AllRus of AllRusAbs = LangRus, ExtraRus,
  DictRus,
  ExtendRus ** {flags optimize=all ; coding=utf8;}