--# -path=.:../common:../abstract

concrete ExtendTMP of Extend = CatTMP
  ** ExtendFunctor - [
    VPS           -- finite VP's with tense and polarity
    , ListVPS
    , VPI
    , ListVPI -- infinitive VP's (TODO: with anteriority and polarity)
    , MkVPS
    , PredVPS

    -- excluded because RGL funs needed for them not implemented yet
    , SlashBareV2S
    , PredAPVP
    , ComplBareVS
    , AdvIsNP, AdvIsNPAP
    , CompBareCN
    , CompIQuant
    , ComplSlashPartLast
    , ComplDirectVQ
    , ComplDirectVS
    , DetNPFem, DetNPMasc
    , ExistCN, ExistMassCN, ExistPluralCN, ExistsNP
    , ExistIPQS, ExistNPQS, ExistS
    , PredIAdvVP
    , PrepCN
    , ReflPossPron
    , UttVP, UttVPShort, UttAccNP, UttDatNP, UttAccIP, UttDatIP
    , EmptyRelSlash, StrandQuestSlash, StrandRelSlash
    , SubjRelNP
    , UseComp_ser, UseComp_estar
    , iFem_Pron, weFem_Pron, youFem_Pron, youPlFem_Pron, youPolFem_Pron, youPolPlFem_Pron, youPolPl_Pron, theyFem_Pron, theyNeutr_Pron
    , GenModNP

  ] with (Grammar=GrammarTMP) ;
