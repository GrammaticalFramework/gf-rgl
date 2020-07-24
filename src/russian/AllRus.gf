--# -path=.:../abstract:../common:../api

concrete AllRus of AllRusAbs = LangRus, ExtraRus ** open ExtendRus in {flags coding=utf8;}