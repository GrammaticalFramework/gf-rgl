--# -path=.:../common:../abstract

concrete ExtendTel of Extend =
  CatTel ** ExtendFunctor - [
    VPS, ListVPS, BaseVPS, ConsVPS, ConjVPS, MkVPS, PredVPS,
    CompBareCN, CompIQuant, CompoundN, DetNPMasc, DetNPFem, EmptyRelSlash,
    ExistCN, ExistIPQS, ExistMassCN, ExistNPQS, ExistPluralCN, ExistS,
    ExistsNP, GenModNP, PiedPipingQuestSlash, PiedPipingRelSlash,
    GerundAdv, GerundCN, PassAgentVPSlash, PassVPSlash,
    PastPartAP, PastPartAgentAP, PresPartAP,
    PossPronRNP, ReflPoss,
    PredAPVP, PredIAdvVP, PrepCN, SlashBareV2S, StrandQuestSlash,
    StrandRelSlash, SubjRelNP, SubjunctRelCN, UttAccIP, UttDatIP,
    theyFem_Pron, theyNeutr_Pron, youFem_Pron, youPlFem_Pron,
    youPolFem_Pron, youPolPlFem_Pron, youPolPl_Pron,
    ReflPron, PositAdVAdj
  ]
  with (Grammar = GrammarTel) ** open ResTel, Prelude in {

  lincat
    VPS = {s : Agr => Str} ;
    [VPS] = {s1,s2 : Agr => Str} ;

  lin
    UseDAP dap = {s = \\c => dap.s ! Neutr ! npcase2case c ; a = agrP3 Neutr dap.n} ;
    UseDAPMasc dap = {s = \\c => dap.s ! Masc ! npcase2case c ; a = agrP3 Masc dap.n} ;
    UseDAPFem dap = {s = \\c => dap.s ! Fem ! npcase2case c ; a = agrP3 Fem dap.n} ;

    ReflPron = {
      s = \\c => case c of {NPC Dir => "తనను" ; _ => "తన"} ;
      a = defaultAgr ;
      lock_NP = <>
      } ;

    AdvRNP np prep rnp = {
      s = \\c => np.s ! c ++ rnp.s ! NPC Obl ++ prep.s ;
      a = np.a ;
      lock_NP = <>
      } ;

    AdvRVP vp prep rnp = insertAdv (rnp.s ! NPC Obl ++ prep.s) vp ;
    AdvRAP ap prep rnp = {
      s = \\g,n,c => ap.s ! g ! n ! c ++ rnp.s ! NPC Obl ++ prep.s
      } ;

    PositAdVAdj a = {s = a.s ! Masc ! Sg ! Dir} ;

    MkVPS temp pol vp = {
      s = \\agr => let f = vp.s ! pol.p !
                              VPTense (tenseVPH temp.t temp.a) agr
                    in vp.obj.s ++ vp.comp ! agr ++ f.neg ++ f.inf ++ f.fin
      } ;

    BaseVPS first second = {s1 = first.s ; s2 = second.s} ;
    ConsVPS first rest = {
      s1 = \\agr => first.s ! agr ++ "," ++ rest.s1 ! agr ;
      s2 = rest.s2
      } ;
    ConjVPS conj vps = {
      s = \\agr => vps.s1 ! agr ++ conj.s2 ++ vps.s2 ! agr
      } ;
    PredVPS np vps = {s = np.s ! NPC Dir ++ vps.s ! np.a} ;

    CompoundN modifier head = {
      s = \\n,c => modifier.s ! Sg ! Dir ++ head.s ! n ! c ;
      g = head.g
      } ;

    GenModNP num np cn = {
      s = \\c => np.s ! NPC Obl ++ cn.s ! num.n ! npcase2case c ;
      a = agrP3 cn.g num.n
      } ;

    PresPartAP vp = {
      s = \\_,_,_ => let f = vp.s ! Pos ! VPStem in
        vp.obj.s ++ vp.comp ! defaultAgr ++ f.inf ++ f.fin
      } ;

    PastPartAP vps = {
      s = \\_,_,_ => let f = vps.s ! Pos ! VPStem in
        vps.obj.s ++ vps.comp ! defaultAgr ++ f.inf ++ f.fin
      } ;

    PastPartAgentAP vps np = {
      s = \\_,_,_ => let f = vps.s ! Pos ! VPStem in
        np.s ! NPC Obl ++ vps.obj.s ++ vps.comp ! defaultAgr ++ f.inf ++ f.fin
      } ;

    GerundCN vp = {
      s = \\_,_ => let f = vp.s ! Pos ! VPInf in
        vp.obj.s ++ vp.comp ! defaultAgr ++ f.inf ++ f.fin ;
      g = Neutr
      } ;

    GerundAdv vp = {
      s = let f = vp.s ! Pos ! VPInf in
        vp.obj.s ++ vp.comp ! defaultAgr ++ f.inf ++ f.fin
      } ;

    ReflPoss num cn = {
      s = \\c => cn.s ! num.n ! npcase2case c ;
      a = agrP3 cn.g num.n ;
      lock_NP = <>
      } ;

    PossPronRNP pron num cn rnp = {
      s = \\c => pron.s ! PPoss ++ cn.s ! num.n ! Dir ++ rnp.s ! c ;
      a = agrP3 cn.g num.n
      } ;

    PassVPSlash vps = {
      s = vps.s ;
      obj = vps.obj ;
      subj = VIntrans ;
      comp = vps.comp
      } ;

    PassAgentVPSlash vps agent = {
      s = vps.s ;
      obj = vps.obj ;
      subj = VIntrans ;
      comp = \\agr => agent.s ! NPC Obl ++ vps.comp ! agr
      } ;
}
