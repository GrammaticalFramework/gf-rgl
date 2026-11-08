--# -path=.:../common:../abstract

concrete ExtendUrd of Extend =
  CatUrd ** ExtendFunctor -
  [
    ComplDirectVQ, ComplDirectVS,
    BaseVPS, ConsVPS, MkVPS, ConjVPS, PredVPS,
    BaseVPI, ConsVPI, MkVPI, ConjVPI, ComplVPIVV,
    BaseComp, ConsComp, ConjComp, BaseImp, ConsImp, ConjImp,
    PresPartAP, PastPartAP, PastPartAgentAP,
    PassVPSlash, PassAgentVPSlash, ProgrVPSlash,
    ReflRNP, ReflPron, ReflPoss, PredetRNP,
    AdvRNP, AdvRVP, AdvRAP, ReflA2RNP, PossPronRNP,
    CompoundN, CompoundAP, GerundCN, GerundNP, GerundAdv,
    WithoutVP, ByVP, InOrderToVP, ApposNP, PositAdVAdj,
    UseDAP, UseDAPMasc, UseDAPFem
  ]
  with
    (Grammar = GrammarUrd) **
  open Prelude, CommonHindustani, ResUrd, ParadigmsUrd, Coordination,
       (G = GrammarUrd) in {
{-
-} --# notpresent

lincat
  VPS = {s : Agr => Str ; c : NPCase} ;
  [VPS] = {s1,s2 : Agr => Str ; c : NPCase} ;
  VPI = {s : Agr => Str} ;
  [VPI] = {s1,s2 : Agr => Str} ;
  [Comp] = {s1,s2 : Agr => Str} ;
  [Imp] = {s1,s2 : CPolarity => ImpForm => Str} ;

lin
  MkVPS temp pol vp = {
    s = \\a => (mkSClause [] a vp).s ! tenseOf temp ! pol.p ! ODir ;
    c = case <temp.t,temp.a,vp.subj> of {
      <Past,Simul,VTrans> => NPErg ;
      <Past,Simul,VTransPost> => NPErg ;
      _ => NPC Dir
      }
    } ;
  BaseVPS x y = twoTable Agr x y ** {c = x.c} ;
  ConsVPS x xs = consrTable Agr comma x xs ** {c = xs.c} ;
  ConjVPS conj xs = conjunctDistrTable Agr conj xs ** {c = xs.c} ;
  PredVPS np vps = {s = np.s ! vps.c ++ vps.s ! np.a} ;

  MkVPI vp = {s = \\a => infVP False vp a} ;
  BaseVPI = twoTable Agr ;
  ConsVPI = consrTable Agr comma ;
  ConjVPI = conjunctDistrTable Agr ;
  ComplVPIVV vv vpi = insertObj (\\a => vpi.s ! a) (predV vv) ;

  BaseComp = twoTable Agr ;
  ConsComp = consrTable Agr comma ;
  ConjComp = conjunctDistrTable Agr ;
  BaseImp = twoTable2 CPolarity ImpForm ;
  ConsImp = consrTable2 CPolarity ImpForm comma ;
  ConjImp = conjunctDistrTable2 CPolarity ImpForm ;

  PresPartAP vp = partAP vp ;
  PastPartAP vp = pastAP vp [] ;
  PastPartAgentAP vp np = pastAP vp (np.s ! NPC Obl ++ "کے ذریعے") ;
  PassVPSlash vp = passiveVP vp ;
  PassAgentVPSlash vp np = insertAdV (np.s ! NPC Obl ++ "کے ذریعے") (passiveVP vp) ;
  ProgrVPSlash vp = predProg vp ** {c2 = vp.c2} ;

  ReflRNP vp rnp = insertObject rnp vp ;
  ReflPron = {s = \\_ => "خود" ; a = defaultAgr ; lock_NP = <>} ;
  ReflPoss num cn = {
    s = \\c => reflPoss ! num.n ! cn.g ! npcase2case c ++ cn.s ! num.n ! npcase2case c ;
    a = agrP3 cn.g num.n ; lock_NP = <>
    } ;
  PredetRNP pred np = {s = \\c => pred.s ++ np.s ! c ; a = np.a ; lock_NP = <>} ;
  AdvRNP np prep rnp = {
    s = \\c => rnp.s ! NPC Obl ++ prep.s ! giveGender rnp.a ++ np.s ! c ;
    a = np.a ; lock_NP = <>
    } ;
  AdvRVP vp prep rnp = insertAdV (rnp.s ! NPC Obl ++ prep.s ! giveGender rnp.a) vp ;
  AdvRAP ap prep rnp = {s = \\n,g,c,d => rnp.s ! NPC Obl ++ prep.s ! giveGender rnp.a ++ ap.s ! n ! g ! c ! d} ;
  ReflA2RNP a rnp = {s = \\n,g,c,d => rnp.s ! NPC Obl ++ a.c2 ++ a.s ! n ! g ! c ! d} ;
  PossPronRNP pron num cn rnp = {
    s = \\c => pron.ps ! num.n ! cn.g ++ cn.s ! num.n ! npcase2case c ++
                 rnp.s ! NPC Obl ++ genPost ! num.n ! cn.g ;
    a = agrP3 cn.g num.n
    } ;

  CompoundN modifier head = {s = \\n,c => modifier.s ! Sg ! Dir ++ head.s ! n ! c ; g = head.g} ;
  CompoundAP noun adj = {s = \\n,g,c,d => noun.s ! Sg ! Dir ++ adj.s ! n ! g ! c ! d} ;
  GerundCN vp = {
    s = \\_,c => vp.obj.s ++ vp.ad ++ vp.comp ! defaultAgr ++ vp.cvp ++
                    case c of {Dir => vp.inf ; _ => (vp.s ! VPInf).fin} ;
    g = Masc
    } ;
  GerundNP vp = {s = \\_ => infVP False vp defaultAgr ; a = defaultAgr} ;
  GerundAdv vp = {s = \\_ => vp.obj.s ++ vp.ad ++ vp.comp ! defaultAgr ++ vp.cvp ++ conjunctive vp} ;
  WithoutVP vp = {s = \\_ => infVP False vp defaultAgr ++ "کے بغیر"} ;
  ByVP vp = {s = \\_ => vp.obj.s ++ vp.ad ++ vp.comp ! defaultAgr ++ vp.cvp ++ conjunctive vp} ;
  InOrderToVP vp = {s = \\_ => infVP False vp defaultAgr ++ "کے لیے"} ;
  ApposNP x y = {s = \\c => x.s ! c ++ comma ++ y.s ! c ; a = x.a} ;
  PositAdVAdj a = {s = a.s ! Sg ! Masc ! Dir ! Posit} ;
  UseDAP dap = dapNP Masc dap ;
  UseDAPMasc dap = dapNP Masc dap ;
  UseDAPFem dap = dapNP Fem dap ;

oper
  tenseOf : Temp -> VPHTense = \temp -> case <temp.t,temp.a> of {
    <Pres,Simul> => VPGenPres ; <Pres,Anter> => VPPerfPres ;
    <Past,Simul> => VPImpPast ; <Past,Anter> => VPPerfPast ;
    <Fut,Simul> => VPFut ; <Fut,Anter> => VPPerfFut ;
    <Cond,Simul> => VPFut ; <Cond,Anter> => VPSubj
    } ;
  npcase2case : NPCase -> Case = \c -> case c of {NPC x => x ; NPObj => Obl ; NPErg => Obl} ;
  reflPoss : Number => Gender => Case => Str = table {
    Sg => table {Masc => table {Dir => "اپنا" ; _ => "اپنے"} ; Fem => table {_ => "اپنی"}} ;
    Pl => table {Masc => table {_ => "اپنے"} ; Fem => table {_ => "اپنی"}}
    } ;
  genPost : Number => Gender => Str = table {
    Sg => table {Masc => "کا" ; Fem => "کی"} ;
    Pl => table {Masc => "کے" ; Fem => "کی"}
    } ;
  partAP : VPH -> AP = \vp -> {
    s = \\n,g,_,_ => let a = agrP3 g n in vp.obj.s ++ vp.ad ++ vp.comp ! a ++ vp.cvp ++
                    (vp.s ! VPTense VPPres a).inf ++ hwa a ;
    lock_AP = <>
    } ;
  conjunctive : VPH -> Str = \vp -> (vp.s ! VPStem).inf ++ "کے" ;
  pastAP : VPHSlash -> Str -> AP = \vp,agent -> {
    s = \\n,g,_,_ => vp.obj.s ++ vp.ad ++ vp.comp ! (agrP3 g n) ++ agent ++ vp.cvp ++
                    (vp.s ! VPTense VPPast (agrP3 g n)).inf ;
    lock_AP = <>
    } ;
  passiveVP : VPHSlash -> VPH = \vp ->
    let aux = predV (mkV "جانا") in {
      s = table {
        VPTense tense agr => {
          inf = (vp.s ! VPTense VPPast agr).inf ++ (aux.s ! VPTense tense agr).inf ;
          fin = (aux.s ! VPTense tense agr).fin
          } ;
        form => {
          inf = (vp.s ! VPTense VPPast defaultAgr).inf ++ (aux.s ! form).inf ;
          fin = (aux.s ! form).fin
          }
        } ;
      obj = vp.obj ; subj = VIntrans ; inf = vp.inf ++ "جانا" ;
      ad = vp.ad ; embComp = vp.embComp ; prog = vp.prog ; comp = vp.comp ; cvp = vp.cvp
      } ;
  dapNP : Gender -> DAP -> CommonHindustani.NP = \g,dap -> {
    s = \\c => dap.s ! dap.n ! g ! npcase2case c ; a = agrP3 g dap.n
    } ;

{-  --# notpresent
-} 

}
