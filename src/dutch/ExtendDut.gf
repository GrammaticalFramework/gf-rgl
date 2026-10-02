--# -path=alltenses:../common:../abstract

concrete ExtendDut of Extend =
  CatDut ** ExtendFunctor
   - [PastPartAP,PastPartAgentAP,PresPartAP,ProgrVPSlash,ICompAP,IAdvAdv,
      VPS,
      BaseVPS, ConsVPS,
      MkVPS, ConjVPS, PredVPS,
      VPI,BaseVPI,ConsVPI,MkVPI,ConjVPI,ComplVPIVV,
      PassVPSlash, PassAgentVPSlash,
      RNP,RNPList,ReflRNP,ReflPron,ReflPoss,PredetRNP,
      AdvRNP,AdvRVP,AdvRAP,ReflA2RNP,PossPronRNP,
      ConjRNP,Base_rr_RNP,Base_nr_RNP,Base_rn_RNP,Cons_rr_RNP,Cons_nr_RNP,
      CompoundN,CompoundAP,GerundCN,GerundNP,GerundAdv,ByVP,InOrderToVP,
      ApposNP,PositAdVAdj,
      BaseComp,ConsComp,ConjComp,BaseImp,ConsImp,ConjImp
     ]
  with
    (Grammar = GrammarDut) **
  open
    GrammarDut,
    ResDut,
    Coordination,
    Prelude,
    ParadigmsDut in {

lin --# notpresent
  PastPartAP vp = { --# notpresent
    s = \\agr,af => let aForm = case vp.isHeavy of { --# notpresent
                          True  => APred ; --# notpresent
                          False => af } ; --# notpresent
                     in (infClause [] agr vp aForm).s ! Past ! Anter ! Pos ! Sub ; --# notpresent
    isPre = notB vp.isHeavy ; --# notpresent
   } ; --# notpresent

lin
  PresPartAP vp = {
    s = \\agr,af =>
      (infClause [] agr vp (case vp.isHeavy of {True => APred ; False => af})).s !
        Pres ! Simul ! Pos ! Sub ;
    isPre = notB vp.isHeavy
    } ;

  PastPartAgentAP vp np = {
    s = \\agr,af =>
      (infClause [] agr vp APred).s ! Past ! Anter ! Pos ! Sub ++
      "door" ++ np.s ! NPAcc ;
    isPre = False
    } ;

  ProgrVPSlash vp =
    let vpi = infVP True vp in
    insertAdv ("aan het" ++ vpi.inf ++ vpi.ext)
      (insertObj vpi.obj (compV zijn_V)) ** {c2 = vp.c2} ;

lincat
  VPS   = {s : Order => Agr => Str} ;
  [VPS] = {s1,s2 : Order => Agr => Str} ;
  VPI = {s : Bool => Agr => Str} ;
  [VPI] = {s1,s2 : Bool => Agr => Str} ;

lin
  BaseVPS = twoTable2 Order Agr ;
  ConsVPS = consrTable2 Order Agr comma ;

  PredVPS np vpi = 
    let
      subj = np.s ! NPNom ;
      agr  = np.a ;
    in {
      s = \\o => 
        let verb = vpi.s ! o ! agr 
        in case o of {
          Main => subj ++ verb ;
          Inv  => verb ++ subj ;   ---- älskar henne och sover jag
          Sub  => subj ++ verb 
          }
      } ;

  MkVPS tm p vp = {
    s = \\o,agr =>
      let
        ord   = case o of {
          Sub => True ;  -- glue prefix to verb
          _ => False
          } ;
        subj = [] ;
        t = tm.t ;
        a = tm.a ;
        b = p.p ;
        vform = vForm t agr.g agr.n agr.p o ;
        auxv = (auxVerb vp.s.aux).s ;
        vperf = vp.s.s ! VPerf APred ;
        verb : Str * Str = case <t,a> of {
          <Fut|Cond,Simul>  => <zullen_V.s ! vform, vp.s.s ! VInf> ; --# notpresent
          <Fut|Cond,Anter>  => <zullen_V.s ! vform, vperf ++ auxv ! VInf> ; --# notpresent
          <_,       Anter>  => <auxv ! vform,       vperf> ; --# notpresent
          <_,       Simul>  => <vp.s.s ! vform,     []>
          } ;
        fin   = verb.p1 ;
        neg   = vp.a1 ! b ;
        obj0  = vp.n0 ! agr ;
        obj   = vp.n2 ! agr ;
        compl = obj0 ++ neg ++ obj ++ vp.a2 ++ vp.s.prefix ;
        inf   = 
          case <vp.isAux, vp.inf.p2, a> of {                  --# notpresent
            <True,True,Anter> => vp.s.s ! VInf ++ vp.inf.p1 ; --# notpresent
            _ =>                                              --# notpresent
               vp.inf.p1 ++ verb.p2
            }                                                 --# notpresent
            ;
        extra = vp.ext ;
        inffin = 
          case <a,vp.isAux> of {                              --# notpresent
            <Anter,True> => fin ++ inf ; -- double inf   --# notpresent
            _ =>                                              --# notpresent
            inf ++ fin              --- or just auxiliary vp
          }                                                   --# notpresent
      in
      tm.s ++ p.s ++
      case o of {
        Main => subj ++ fin ++ compl ++ inf ++ extra ;
        Inv  => fin ++ subj ++ compl ++ inf ++ extra ;
        Sub  => subj ++ compl ++ inffin ++ extra
        }
  } ;

lin
  ConjVPS = conjunctDistrTable2 Order Agr ;

  BaseVPI = twoTable2 Bool Agr ;
  ConsVPI = consrTable2 Bool Agr comma ;
  MkVPI vp = {s = \\bare => useInfVP bare vp} ;
  ConjVPI = conjunctDistrTable2 Bool Agr ;
  ComplVPIVV vv vpi =
    insertObj (\\agr => vpi.s ! vv.isAux ! agr)
      (predVGen vv.isAux AfterObjs vv) ;

  ICompAP ap = {s = \\agr => "hoe" ++ ap.s ! agr ! APred} ; 

  IAdvAdv adv = {s = "hoe" ++ adv.s} ;

lin PassVPSlash vps = 
      insertInf (vps.s.s ! VPerf APred) (predV ResDut.worden_V) ;
    PassAgentVPSlash vps np = 
      insertAdv (appPrep (mkPrep "door") np) (insertInf (vps.s.s ! VPerf APred) (predV ResDut.worden_V)) ;

lin
  UseDAP dap = dap ** {
    s = \\_ => dap.sp ! Neutr ;
    a = agrP3 dap.n ;
    isPron = False
    } ;

  UseDAPMasc, UseDAPFem = \dap -> dap ** {
    s = \\_ => dap.sp ! Utr ;
    a = agrP3 dap.n ;
    isPron = False
    } ;

lin CompoundN n1 n2 = {
    s  = \\n => n1.s ! NF Sg Nom  ++ BIND ++ n2.s ! n ;
    g = n2.g
    } ;

lin CompoundAP noun adj = {
      s = \\agr,af => noun.s ! NF Sg Nom ++ BIND ++ adj.s ! Posit ! af ;
      isPre = True
    } ;

lincat
  RNP = {s : Agr => Str ; isPron : Bool} ;
  RNPList = {s1,s2 : Agr => Str} ;

lin
  ReflRNP vps rnp =
    insertObjNP (andB rnp.isPron (notB vps.c2.p2)) vps.negPos
      (\\agr => appPrep vps.c2.p1 (npLite (\\_ => rnp.s ! agr))) vps ;

  ReflPron = {s = reflPron ; isPron = True} ;

  ReflPoss num cn = {
    s = \\agr => reflPossDet agr num.n cn.g ++ num.s ++
                  cn.s ! Strong ! NF num.n Nom ;
    isPron = False
    } ;

  PredetRNP pred rnp = {
    s = \\agr => pred.s ! agr.n ! agr.g ++ rnp.s ! agr ;
    isPron = False
    } ;

  AdvRNP np prep rnp = {
    s = \\agr => np.s ! NPAcc ++
                  appPrep prep (npLite (\\_ => rnp.s ! agr)) ;
    isPron = False
    } ;

  AdvRVP vp prep rnp =
    insertObj (\\agr => appPrep prep (npLite (\\_ => rnp.s ! agr))) vp ;

  AdvRAP ap prep rnp = {
    s = \\agr,af => ap.s ! agr ! af ++
                     appPrep prep (npLite (\\_ => rnp.s ! agr)) ;
    isPre = False
    } ;

  ReflA2RNP adj rnp = {
    s = \\agr,af => adj.s ! Posit ! af ++
                     appPrep adj.c2 (npLite (\\_ => rnp.s ! agr)) ;
    isPre = False
    } ;

  PossPronRNP pron num cn rnp = heavyNP {
    s = \\_ => pron.stressed.poss ++ num.s ++
                cn.s ! Strong ! NF num.n Nom ++ "van" ++ rnp.s ! pron.a ;
    a = agrP3 num.n
    } ;

  ConjRNP conj rnps = conjunctDistrTable Agr conj rnps ** {isPron = False} ;
  Base_rr_RNP = twoTable Agr ;
  Base_nr_RNP np rnp = twoTable Agr {s = \\_ => np.s ! NPAcc} rnp ;
  Base_rn_RNP rnp np = twoTable Agr rnp {s = \\_ => np.s ! NPAcc} ;
  Cons_rr_RNP = consrTable Agr comma ;
  Cons_nr_RNP np rnps = consrTable Agr comma {s = \\_ => np.s ! NPAcc} rnps ;

lin
  GerundCN vp = {
    s = \\_,_ => useInfVP True vp ! agrP3 Sg ;
    g = Neutr
    } ;

  GerundNP vp = heavyNP {
    s = \\_ => useInfVP True vp ! agrP3 Sg ;
    a = agrP3 Sg
    } ;

  GerundAdv vp = {
    s = (infClause [] (agrP3 Sg) vp APred).s ! Pres ! Simul ! Pos ! Sub
    } ;

  ByVP vp = {s = "door" ++ useInfVP True vp ! agrP3 Sg} ;
  InOrderToVP vp = {s = "om" ++ useInfVP False vp ! agrP3 Sg} ;

  ApposNP np1 np2 = heavyNP {
    s = \\c => np1.s ! c ++ bindComma ++ np2.s ! c ;
    a = np1.a
    } ;

  PositAdVAdj adj = {s = adj.s ! Posit ! APred} ;

lincat
  [Comp] = {s1,s2 : Agr => Str} ;
  [Imp] = {s1,s2 : Polarity => ImpForm => Str} ;

lin
  BaseComp = twoTable Agr ;
  ConsComp = consrTable Agr comma ;
  ConjComp = conjunctDistrTable Agr ;
  BaseImp = twoTable2 Polarity ImpForm ;
  ConsImp = consrTable2 Polarity ImpForm comma ;
  ConjImp = conjunctDistrTable2 Polarity ImpForm ;

oper
  reflPossDet : Agr -> Number -> Gender -> Str = \agr,n,g ->
    case <agr.n,agr.p,n,g> of {
      <Sg,P1,_,_> => "mijn" ;
      <Sg,P2,_,_> => "jouw" ;
      <Sg,P3,_,_> => "zijn" ;
      <Pl,P1,Sg,Neutr> => "ons" ;
      <Pl,P1,_,_> => "onze" ;
      <Pl,P2,_,_> => "jullie" ;
      <Pl,P3,_,_> => "hun"
      } ;

}
