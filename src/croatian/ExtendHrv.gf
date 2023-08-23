concrete ExtendHrv of Extend = CatHrv ** 
  ExtendFunctor - [
    --- ReflPossPron
    CardCNCard
    ---- constant not found (yet)
    ,youPolFem_Pron
    ,UttVPShort
    ,UttAccIP
    ,UttDatIP
    ,SubjRelNP
    ,StrandRelSlash
    ,StrandQuestSlash
    ,SlashBareV2S
    ,PredIAdvVP
    ,PredAPVP
    ,ExistsNP
    ,ExistS
    ,ExistPluralCN
    ,ExistNPQS
    ,ExistMassCN
    ,ExistIPQS
    ,ExistCN
    ,EmptyRelSlash
    ,DetNPMasc
    ,DetNPFem
    ,ComplBareVS
    ,CompIQuant
    ,CompBareCN
    ,PiedPipingQuestSlash
    ,PiedPipingRelSlash
    ]
  with (Grammar = GrammarHrv)
    **
open
  ResHrv
in {

---lin ReflPossPron = justDemPronFormsAdjective reflPossessivePron ;

lin CardCNCard card cn = {
  s = \\g,c => card.s ! g ! c ++ numSizeForm cn.s card.size c ;
  size = NS_20_
  } ;

}
