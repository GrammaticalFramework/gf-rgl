--# -path=.:../common:../abstract

concrete ExtendLav of Extend =
  CatLav ** ExtendFunctor -
  [
    iFem_Pron, weFem_Pron, youFem_Pron, youPolFem_Pron, youPlFem_Pron,
    theyFem_Pron,
    ComplDirectVQ, ComplDirectVS,
    VPS, BaseVPS, ConsVPS, MkVPS, ConjVPS, PredVPS,
    VPI, BaseVPI, ConsVPI, MkVPI, ConjVPI, ComplVPIVV,
    ConjComp, BaseComp, ConsComp, ConjImp, BaseImp, ConsImp,
    PresPartAP, PastPartAP, PastPartAgentAP, PassVPSlash,
    PassAgentVPSlash, ProgrVPSlash,
    RNP, RNPList, ReflRNP, ReflPron, ReflPoss, PredetRNP,
    AdvRNP, AdvRVP, AdvRAP, PossPronRNP, ConjRNP,
    Base_rr_RNP, Base_nr_RNP, Base_rn_RNP, Cons_rr_RNP, Cons_nr_RNP,
    CompoundN, CompoundAP, GerundCN, GerundNP, GerundAdv, ByVP,
    InOrderToVP, ApposNP, PositAdVAdj, UseDAP, UseDAPMasc, UseDAPFem
   ]
  with
    (Grammar = GrammarLav) **
  open
    ResLav,
    ParadigmsPronounsLav,
    ParadigmsLav,
    StructuralLav,
    VerbLav,
    Coordination,
    Prelude in {

lincat
  VPS = {s : Agreement => Str} ;
  [VPS] = {s1, s2 : Agreement => Str} ;
  VPI = {s : Agreement => Str} ;
  [VPI] = {s1, s2 : Agreement => Str} ;
  [Comp] = {s1, s2 : Agreement => Str} ;
  [Imp] = {s1, s2 : Polarity => Number => Str} ;

  RNP = {s : Agreement => Case => Str ; isPron : Bool} ;
  RNPList = {s1, s2 : Agreement => Case => Str} ;

lin iFem_Pron = mkPronoun_I Fem ;
    weFem_Pron = mkPronoun_We Fem ;

    youFem_Pron = mkPronoun_You_Sg Fem ;
    youPolFem_Pron = mkPronoun_You_Pol Fem ;
    youPlFem_Pron = mkPronoun_You_Pl Fem ;

    theyFem_Pron = mkPronoun_They Fem ;

    UseDAP dap = {
      s = dap.s ! Masc ; agr = AgrP3 dap.num Masc ; pol = dap.pol ;
      isRel = False ; isPron = True
    } ;
    UseDAPMasc = UseDAP ;
    UseDAPFem dap = {
      s = dap.s ! Fem ; agr = AgrP3 dap.num Fem ; pol = dap.pol ;
      isRel = False ; isPron = True
    } ;

    MkVPS temp pol vp = {
      s = \\agr => temp.s ++ pol.s ++
        buildVerb vp.v (Ind temp.a temp.t) pol.p agr Pos vp.rightPol ++
        vp.compl ! agr
    } ;
    BaseVPS x y = {s1 = x.s ; s2 = y.s} ;
    ConsVPS x xs = {
      s1 = \\agr => x.s ! agr ++ "," ++ xs.s1 ! agr ; s2 = xs.s2
    } ;
    ConjVPS conj xs = {
      s = \\agr => conj.s1 ++ xs.s1 ! agr ++ conj.s2 ++ xs.s2 ! agr
    } ;
    PredVPS np vps = {s = np.s ! Nom ++ closeRelCl np.isRel ++ vps.s ! np.agr} ;

    MkVPI vp = {s = \\agr => buildVP vp Pos VInf agr} ;
    BaseVPI x y = {s1 = x.s ; s2 = y.s} ;
    ConsVPI x xs = {
      s1 = \\agr => x.s ! agr ++ "," ++ xs.s1 ! agr ; s2 = xs.s2
    } ;
    ConjVPI conj xs = {
      s = \\agr => conj.s1 ++ xs.s1 ! agr ++ conj.s2 ++ xs.s2 ! agr
    } ;
    ComplVPIVV vv vpi = {
      v = vv ; compl = vpi.s ; voice = Act ; leftVal = vv.leftVal ;
      rightAgr = AgrP3 Sg Masc ; rightPol = Pos ; objPron = False
    } ;

    BaseComp x y = {s1 = x.s ; s2 = y.s} ;
    ConsComp x xs = {
      s1 = \\agr => x.s ! agr ++ "," ++ xs.s1 ! agr ; s2 = xs.s2
    } ;
    ConjComp conj xs = {
      s = \\agr => conj.s1 ++ xs.s1 ! agr ++ conj.s2 ++ xs.s2 ! agr
    } ;
    BaseImp x y = {s1 = x.s ; s2 = y.s} ;
    ConsImp x xs = {
      s1 = \\pol,num => x.s ! pol ! num ++ "," ++ xs.s1 ! pol ! num ;
      s2 = xs.s2
    } ;
    ConjImp conj xs = {
      s = \\pol,num => conj.s1 ++ xs.s1 ! pol ! num ++
                       conj.s2 ++ xs.s2 ! pol ! num
    } ;

    PresPartAP vp = {
      s = \\_,g,n,c => vp.v.s ! Pos ! (VPart Act g n c) ++
                        vp.compl ! AgrP3 n g
    } ;
    PastPartAP vp = {
      s = \\_,g,n,c => vp.v.s ! Pos ! (VPart Pass g n c) ++
                        vp.compl ! AgrP3 n g
    } ;
    PastPartAgentAP vp np = {
      s = \\_,g,n,c => vp.v.s ! Pos ! (VPart Pass g n c) ++
                        vp.compl ! AgrP3 n g ++ "no" ++ np.s ! Gen
    } ;
    PassVPSlash vp = {
      v = vp.v ; compl = vp.compl ; voice = Pass ;
      leftVal = vp.rightVal.c ! Sg ; rightAgr = vp.rightAgr ;
      rightPol = vp.rightPol ; objPron = False
    } ;
    PassAgentVPSlash vp np =
      insertObjReg (\\_ => "no" ++ np.s ! Gen) False (PassVPSlash vp) ;
    ProgrVPSlash vp = vp ;

    ReflPron = {s = \\_,c => reflPron ! c ; isPron = True} ;
    ReflPoss num cn = {
      s = \\agr,c => (mkPronoun_Gend "savs").s ! (fromAgr agr).gend ! num.num ! c ++
                       cn.s ! Def ! num.num ! c ;
      isPron = False
    } ;
    PredetRNP pred rnp = {
      s = \\agr,c => pred.s ! (fromAgr agr).gend ++ rnp.s ! agr ! c ;
      isPron = False
    } ;
    AdvRNP np prep rnp = {
      s = \\agr,c => np.s ! c ++ prep.s ++
                       rnp.s ! agr ! (prep.c ! (fromAgr agr).num) ;
      isPron = False
    } ;
    AdvRVP vp prep rnp = insertObjReg
      (\\agr => prep.s ++ rnp.s ! agr ! (prep.c ! (fromAgr agr).num)) False vp ;
    AdvRAP ap prep rnp = {
      s = \\d,g,n,c => ap.s ! d ! g ! n ! c ++ prep.s ++
                        rnp.s ! AgrP3 n g ! (prep.c ! n)
    } ;
    ReflRNP vp rnp = insertObjPre
      (\\agr => vp.rightVal.s ++
                rnp.s ! agr ! (vp.rightVal.c ! (fromAgr agr).num)) vp ;
    PossPronRNP pron num cn rnp = {
      s = \\c => pron.poss ! (fromAgr pron.agr).gend ! num.num ! c ++
                   cn.s ! Def ! num.num ! c ++ rnp.s ! pron.agr ! Gen ;
      agr = AgrP3 num.num cn.gend ; pol = Pos ; isRel = False ; isPron = False
    } ;
    Base_rr_RNP x y = {s1 = x.s ; s2 = y.s} ;
    Base_nr_RNP x y = {s1 = \\_,c => x.s ! c ; s2 = y.s} ;
    Base_rn_RNP x y = {s1 = x.s ; s2 = \\_,c => y.s ! c} ;
    Cons_rr_RNP x xs = {
      s1 = \\agr,c => x.s ! agr ! c ++ "," ++ xs.s1 ! agr ! c ; s2 = xs.s2
    } ;
    Cons_nr_RNP x xs = {
      s1 = \\agr,c => x.s ! c ++ "," ++ xs.s1 ! agr ! c ; s2 = xs.s2
    } ;
    ConjRNP conj xs = {
      s = \\agr,c => conj.s1 ++ xs.s1 ! agr ! c ++ conj.s2 ++ xs.s2 ! agr ! c ;
      isPron = False
    } ;

    CompoundN modifier head = {
      s = \\n,c => modifier.s ! Sg ! Gen ++ head.s ! n ! c ;
      gend = head.gend
    } ;
    CompoundAP noun adj = {
      s = \\d,g,n,c => noun.s ! Sg ! Gen ++ adj.s ! (AAdj Posit d g n c)
    } ;
    GerundCN vp = {
      s = \\_,_,_ => buildVP vp Pos VInf (AgrP3 Sg Masc) ;
      gend = Fem ; isRel = False
    } ;
    GerundNP vp = {
      s = \\_ => buildVP vp Pos VInf (AgrP3 Sg Masc) ;
      agr = AgrP3 Sg Fem ; pol = Pos ; isRel = False ; isPron = False
    } ;
    GerundAdv vp = {s = buildVP vp Pos VInf (AgrP3 Sg Masc) ; isPron = False} ;
    ByVP vp = {s = buildVP vp Pos VInf (AgrP3 Sg Masc) ; isPron = False} ;
    InOrderToVP vp = {
      s = "lai" ++ buildVP vp Pos VInf (AgrP3 Sg Masc) ; isPron = False
    } ;
    ApposNP x y = {
      s = \\c => x.s ! c ++ "," ++ y.s ! c ; agr = x.agr ; pol = x.pol ;
      isRel = y.isRel ; isPron = False
    } ;
    PositAdVAdj a = {s = a.s ! (AAdv Posit)} ;

}
