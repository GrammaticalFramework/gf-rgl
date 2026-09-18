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
    VPI = {s : Str} ;
    [VPI] = {s1,s2 : Str} ;
    [Comp] = {s1,s2 : Agr => Str} ;
    [Imp] = {s1,s2 : Polarity => Number => Str} ;

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
      s = \\c => rnp.s ! NPC Obl ++ prep.s ++ np.s ! c ;
      a = np.a ;
      lock_NP = <>
      } ;

    AdvRVP vp prep rnp = insertAdv (rnp.s ! NPC Obl ++ prep.s) vp ;
    AdvRAP ap prep rnp = {
      s = \\g,n,c => rnp.s ! NPC Obl ++ prep.s ++ ap.s ! g ! n ! c
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

    MkVPI vp = {s = let f = vp.s ! Pos ! VPInf in
      vp.obj.s ++ vp.comp ! defaultAgr ++ f.neg ++ f.inf ++ f.fin} ;
    BaseVPI first second = {s1 = first.s ; s2 = second.s} ;
    ConsVPI first rest = {s1 = first.s ++ "," ++ rest.s1 ; s2 = rest.s2} ;
    ConjVPI conj vpi = {s = vpi.s1 ++ conj.s2 ++ vpi.s2} ;
    ComplVPIVV vv vpi = predV vv ** {comp = \\_ => vpi.s} ;

    BaseComp first second = {s1 = first.s ; s2 = second.s} ;
    ConsComp first rest = {
      s1 = \\agr => first.s ! agr ++ "," ++ rest.s1 ! agr ;
      s2 = rest.s2
      } ;
    ConjComp conj comps = {
      s = \\agr => comps.s1 ! agr ++ conj.s2 ++ comps.s2 ! agr
      } ;

    BaseImp first second = {s1 = first.s ; s2 = second.s} ;
    ConsImp first rest = {
      s1 = \\pol,num => first.s ! pol ! num ++ "," ++ rest.s1 ! pol ! num ;
      s2 = rest.s2
      } ;
    ConjImp conj imps = {
      s = \\pol,num => imps.s1 ! pol ! num ++ conj.s2 ++ imps.s2 ! pol ! num
      } ;

    CompoundN modifier head = {
      s = \\n,c => modifier.s ! Sg ! Dir ++ head.s ! n ! c ;
      g = head.g
      } ;

    CompoundAP noun adjective = {
      s = \\g,n,c => noun.s ! Sg ! Dir ++ adjective.s ! g ! n ! c
      } ;

    GenModNP num np cn = {
      s = \\c => np.s ! NPC Obl ++ cn.s ! num.n ! npcase2case c ;
      a = agrP3 cn.g num.n
      } ;

    PresPartAP vp = {
      s = \\g,n,_ => let f = vp.s ! Pos ! VPPresPart in
        vp.obj.s ++ vp.comp ! Ag g n P3 ++ f.neg ++ f.inf ++ f.fin
      } ;

    PastPartAP vps = {
      s = \\g,n,_ => let f = vps.passive ! Pos ! VPPastPart in
        vps.obj.s ++ vps.comp ! Ag g n P3 ++ f.neg ++ f.inf ++ f.fin
      } ;

    PastPartAgentAP vps np = {
      s = \\g,n,_ => let f = vps.passive ! Pos ! VPPastPart in
        np.s ! NPC Obl ++ "చేత" ++ vps.obj.s ++ vps.comp ! Ag g n P3 ++
        f.neg ++ f.inf ++ f.fin
      } ;

    GerundCN vp = {
      s = \\_,_ => let f = vp.s ! Pos ! VPInf in
        vp.obj.s ++ vp.comp ! defaultAgr ++ f.inf ++ f.fin ;
      g = Neutr
      } ;

    GerundNP vp = {
      s = \\_ => let f = vp.s ! Pos ! VPInf in
        vp.obj.s ++ vp.comp ! defaultAgr ++ f.neg ++ f.inf ++ f.fin ;
      a = defaultAgr
      } ;

    GerundAdv vp = {
      s = let f = vp.s ! Pos ! VPInf in
        vp.obj.s ++ vp.comp ! defaultAgr ++ f.inf ++ f.fin
      } ;

    ByVP vp = {
      s = let f = vp.s ! Pos ! VPInf in
        vp.obj.s ++ vp.comp ! defaultAgr ++ f.neg ++ f.inf ++ f.fin ++ "ద్వారా"
      } ;

    ApposNP first second = {
      s = \\c => first.s ! c ++ "," ++ second.s ! c ;
      a = first.a
      } ;

    ReflPoss num cn = {
      s = \\c => "తన" ++ cn.s ! num.n ! npcase2case c ;
      a = agrP3 cn.g num.n ;
      lock_NP = <>
      } ;

    PossPronRNP pron num cn rnp = {
      s = \\c => pron.s ! PPoss ++ cn.s ! num.n ! Dir ++ rnp.s ! c ;
      a = agrP3 cn.g num.n
      } ;

    PassVPSlash vps = {
      s = vps.passive ;
      passive = vps.passive ;
      obj = vps.obj ;
      subj = VIntrans ;
      comp = vps.comp
      } ;

    PassAgentVPSlash vps agent = {
      s = vps.passive ;
      passive = vps.passive ;
      obj = vps.obj ;
      subj = VIntrans ;
      comp = \\agr => agent.s ! NPC Obl ++ "చేత" ++ vps.comp ! agr
      } ;

    ProgrVPSlash vps = vps ;
}
