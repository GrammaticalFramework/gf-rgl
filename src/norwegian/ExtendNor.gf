--# -path=.:../scandinavian:../abstract:../common:prelude
concrete ExtendNor of Extend = CatNor **
  ExtendFunctor -
  [
    GenNP, GenModNP, ComplBareVS, CompBareCN,
    ApposNP, DetNPMasc, DetNPFem,
    StrandRelSlash, EmptyRelSlash, StrandQuestSlash,
    PassVPSlash, PassAgentVPSlash, UttVPShort, ByVP, InOrderToVP,
    MkVPI, BaseVPI, ConsVPI, ConjVPI, ComplVPIVV,
    MkVPS, BaseVPS, ConsVPS, ConjVPS, PredVPS, RelVPS,
    MkVPS2, ConjVPS2, ComplVPS2, ReflVPS2, MkVPI2, ConjVPI2, ComplVPI2,
    ICompAP,ProDrop,EmbedSSlash,
    AdAdV, PositAdVAdj, GerundCN, GerundNP, GerundAdv, PresPartAP, PastPartAP, PastPartAgentAP,
    RNP, RNPList, ReflRNP, ReflPron, ReflPoss, PredetRNP, ConjRNP,
    Base_rr_RNP, Base_nr_RNP, Base_rn_RNP, Cons_rr_RNP, Cons_nr_RNP, ReflPossPron,
    CompoundN, CompoundAP, AdvIsNP,
    A2VPSlash, N2VPSlash,
    CardCNCard,
    GenRP
  ]
  with (Grammar = GrammarNor)
    ** {

  flags coding=utf8 ;

}
