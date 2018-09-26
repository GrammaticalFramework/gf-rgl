--# -path=.:../common:../abstract

concrete ExtendAra of Extend =
  CatAra ** ExtendFunctor - [
    GenNP, SlashBareV2S, PredAPVP, GenModNP, ExistsNP,
    StrandRelSlash, ExistPluralCN, ExistMassCN, ExistCN, EmptyRelSlash, DetNPMasc, DetNPFem,
    ComplBareVS, ComplDirectVS, ComplDirectVQ
]
  with (Grammar=GrammarAra)
  ** open

    Prelude,
    ResAra

  in {

  lin
    GenNP np = { s = \\_,_,_,_ => np.s ! Gen ; d = Const ; isNum, isPron = False } ;
} ;
