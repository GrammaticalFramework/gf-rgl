--# -path=../common:../abstract

concrete ExtendRus of Extend =
  CatRus ** ExtendFunctor - [
    iFem_Pron,
    theyFem_Pron, weFem_Pron, youFem_Pron, youPlFem_Pron, youPolFem_Pron, youPolPlFem_Pron, youPolPl_Pron,
    VPS, ListVPS, VPI, ListVPI, VPS2, ListVPS2, VPI2, ListVPI2, RNP, RNPList,
    UseComp, RelNP, UseComp_estar, SubjRelNP, PredAPVP, ImpersCl, UseComp, CompAP, EmbedVP, ExistNP, UseQCl,
    QuestCl, ExistNP, UseQCl, ExistIP, AdvVP, AdvVP, AdvVP, UseComp, CompAP, ExistS, ExistNPQS, ExistIPQS,
    ComplDirectVS, ComplDirectVQ, AdvIsNPAP, AdAdV, AdjAsCN, AdjAsNP,
    -- ApposNP,
    AdvIsNP,
    BaseVPS, ConsVPS, BaseVPI, ConsVPI, BaseVPS2, ConsVPS2, BaseVPI2, ConsVPI2,
    MkVPS, ConjVPS, PredVPS, MkVPI, ConjVPI, ComplVPIVV,
    MkVPS2, ConjVPS2, ComplVPS2, MkVPI2, ConjVPI2, ComplVPI2,
    Base_nr_RNP, Base_rn_RNP, Base_rr_RNP, ByVP, CompBareCN,
    CompIQuant, CompQS, CompS, CompVP, ComplBareVS, ComplGenVV, ComplSlashPartLast, ComplVPSVV, CompoundAP,
    CompoundN, ConjRNP, ConjVPS, ConsVPS, Cons_nr_RNP, Cons_rr_RNP, DetNPMasc, DetNPFem, EmbedPresPart, EmptyRelSlash,
    ExistsNP, ExistCN, ExistMassCN, ExistPluralCN, ProDrop,
    FocusAP, FocusAdV, FocusAdv, FocusObj, GenIP, GenModIP, GenModNP, GenNP, GenRP,
    GerundAdv, GerundCN, GerundNP, IAdvAdv, ICompAP, InOrderToVP, MkVPS, NominalizeVPSlashNP,
    PassAgentVPSlash, PassVPSlash, ProgrVPSlash, PastPartAP, PastPartAgentAP, PositAdVAdj, PredVPS, PredVPSVV, PredetRNP, PrepCN,
    EmbedSSlash, PresPartAP, PurposeVP, ReflPoss, ReflPron, ReflRNP, SlashBareV2S, SlashV2V, StrandQuestSlash, StrandRelSlash,
    -- PredIAdvVP,
    UncontractedNeg, UttAccIP, UttAccNP, UttAdV, UttDatIP, UttDatNP, UttVPShort, WithoutVP, BaseVPS2, ConsVPS2, ConjVPS2, ComplVPS2, MkVPS2
   ]
  with (Grammar=GrammarRus)
  ** open Prelude, ResRus, ParadigmsRus, (E=Extend) in {

lincat
  RNP     = {s : Agr => Str} ;
  RNPList = {s1,s2 : Agr => Str} ;

lin
  -- : NP -> NP -> NP ;        -- Mr Macron, the president of France,
  ApposNP np np2 = {
    s=\\cas=> np.s ! cas ++ embedInCommas (np2.s ! cas) ;
    a=np.a
    } ;

  -- : IAdv -> VP -> QCl ; -- how to walk?
  PredIAdvVP iadv vp = QuestIAdv iadv (GenericCl vp) ; -- DEFAULT how does one walk
} ;
