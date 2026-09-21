concrete ExtendCze of Extend = CatCze ** 
  ExtendFunctor - [
    ReflPossPron, ProDrop,
    iFem_Pron, youFem_Pron, weFem_Pron, youPlFem_Pron,
    theyFem_Pron, theyNeutr_Pron, youPolFem_Pron, youPolPlFem_Pron
    ---- constant not found (yet)
    ,UttVPShort
    ,UttAccIP
    ,UttDatIP
    ,SubjRelNP
    ,StrandRelSlash
    ,StrandQuestSlash
    ,SlashBareV2S
    ,PredIAdvVP
    ,PredAPVP
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
    ,TPastSimple
    ]
  with (Grammar = GrammarCze)
    **
open
  ResCze, Prelude, (S = SyntaxCze), (P = ParadigmsCze)
in {

lin
  -- Retain full forms for objects, coordination and NP modifiers.
  ProDrop pron = pron ** {isDrop = True} ;

  iFem_Pron = P.genderPron Fem S.i_Pron ;
  youFem_Pron = P.genderPron Fem S.youSg_Pron ;
  weFem_Pron = P.genderPron Fem S.we_Pron ;
  youPlFem_Pron = P.genderPron Fem S.youPl_Pron ;
  theyFem_Pron = P.genderPron Fem S.they_Pron ;
  theyNeutr_Pron = P.genderPron Neutr S.they_Pron ;
  youPolFem_Pron = P.genderPron Fem S.youPol_Pron ;
  youPolPlFem_Pron = P.genderPron Fem S.youPl_Pron ;

  ReflPossPron = justDemPronFormsAdjective reflPossessivePron ;


}
