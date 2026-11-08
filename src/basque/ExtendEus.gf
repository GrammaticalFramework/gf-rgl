--# -path=.:../common:../abstract

concrete ExtendEus of Extend =
  CatEus ** ExtendFunctor - [
    VPS, ListVPS, BaseVPS, ConsVPS, ConjVPS, MkVPS, PredVPS,
    VPI, ListVPI, BaseVPI, ConsVPI, ConjVPI, MkVPI, ComplVPIVV,
    ListComp, BaseComp, ConsComp, ConjComp,
    ListImp, BaseImp, ConsImp, ConjImp,
    GenNP, GenModNP, ICompAP,
    PresPartAP, PastPartAP, PastPartAgentAP,
    PassVPSlash, PassAgentVPSlash, ProgrVPSlash,
    ReflPron, ReflPoss, AdvRNP, AdvRVP, AdvRAP, PossPronRNP,
    CompoundN, CompoundAP, GerundCN, GerundNP, GerundAdv,
    ByVP, InOrderToVP, ApposNP, PositAdVAdj,
    UseDAP, UseDAPMasc, UseDAPFem, CompVP, ExistIPQS
    ]
  with (Grammar=GrammarEus)
  ** open Prelude, ResEus, AditzTrinkoak, Coordination, (G=GrammarEus) in {

  lincat
    VPS = {s : Agr => Str ; sc : Case} ;
    [VPS] = {s1,s2 : Agr => Str ; sc : Case} ;
    VPI = {s : Agr => Str} ;
    [VPI] = {s1,s2 : Agr => Str} ;
    [Comp] = {s1,s2 : Agr => Str ; copula : SyntVerb1} ;
    [Imp] = {s1,s2 : Str} ;

  lin
    MkVPS temp pol vp = {
      s = \\agr => linS ((mkClause False
                    (empty_NP ** {agr = agr}) vp).s
                    ! temp.t ! temp.a ! pol.p ! Stat) ;
      sc = subjCase vp.val
      } ;
    BaseVPS x y = {s1 = x.s ; s2 = y.s ; sc = x.sc} ;
    ConsVPS x xs = {
      s1 = \\agr => x.s ! agr ++ SOFT_BIND ++ "," ++ xs.s1 ! agr ;
      s2 = xs.s2 ; sc = x.sc
      } ;
    ConjVPS conj xs = {
      s = \\agr => conj.s1 ++ xs.s1 ! agr ++ conj.s2 ++ xs.s2 ! agr ;
      sc = xs.sc
      } ;
    PredVPS np vps = sentenceFromStr (np.s ! vps.sc ++ vps.s ! np.agr) ;

    MkVPI vp = {s = linVPPrc vp} ;
    BaseVPI x y = {s1 = x.s ; s2 = y.s} ;
    ConsVPI x xs = {
      s1 = \\agr => x.s ! agr ++ SOFT_BIND ++ "," ++ xs.s1 ! agr ;
      s2 = xs.s2
      } ;
    ConjVPI conj xs = {
      s = \\agr => conj.s1 ++ xs.s1 ! agr ++ conj.s2 ++ xs.s2 ! agr
      } ;
    ComplVPIVV vv vpi = insertComp vpi.s (useV vv) ;

    BaseComp x y = {s1 = x.s ; s2 = y.s ; copula = x.copula} ;
    ConsComp x xs = {
      s1 = \\agr => x.s ! agr ++ SOFT_BIND ++ "," ++ xs.s1 ! agr ;
      s2 = xs.s2 ; copula = xs.copula
      } ;
    ConjComp conj xs = {
      s = \\agr => conj.s1 ++ xs.s1 ! agr ++ conj.s2 ++ xs.s2 ! agr ;
      copula = xs.copula
      } ;

    BaseImp x y = {s1 = x.s ; s2 = y.s} ;
    ConsImp x xs = {s1 = x.s ++ SOFT_BIND ++ "," ++ xs.s1 ; s2 = xs.s2} ;
    ConjImp conj xs = {s = conj.s1 ++ xs.s1 ++ conj.s2 ++ xs.s2} ;

    GenNP np = -- NP -> Quant ; -- this man's
      { s = artDef ;
	indep, isDef = True ;
	pref = np.s ! Gen } ;

    GenModNP num np cn = G.DetCN (G.DetQuant G.DefArt num) (G.PossNP cn np) ;


    ICompAP ap = -- AP -> IComp ; -- "how old"
      { s = "nola" ++ ap.s ! Hau } ; --TODO agreement -- change type of IComp

    PresPartAP vp = relativeAP vp ;
    PastPartAP vps = relativeAP (passVPSlash vps) ;
    PastPartAgentAP vps agent = relativeAP (passAgentVPSlash vps agent) ;

    PassVPSlash = passVPSlash ;
    PassAgentVPSlash = passAgentVPSlash ;
    ProgrVPSlash vps = vps ** {prc = \\_ => vps.prc ! Pres ++ "ari"} ;

    ReflPron = reflexiveNP ;
    ReflPoss num cn = lin RNP (G.DetCN (reflexiveDet num) cn) ;
    AdvRNP np prep rnp = lin RNP (np ** {
      s = \\c => np.s ! c ++ applyPost prep rnp ;
      stem = np.stem ++ applyPost prep rnp
      }) ;
    AdvRVP vp prep rnp = insertAdv (ss (applyPost prep rnp)) vp ;
    AdvRAP ap prep rnp = ap ** {
      s = \\agr => ap.s ! agr ++ applyPost prep rnp
      } ;
    PossPronRNP pron num cn rnp =
      G.DetCN (G.DetQuant (G.PossPron pron) num)
        (cn ** {s = \\agr => rnp.s ! Gen ++ cn.s ! agr}) ;

    CompoundN modifier head = head ** {s = modifier.s ++ head.s} ;
    CompoundAP noun adjective = adjective ** {
      s = \\agr => noun.s ++ adjective.s ! AF Posit ;
      typ = Bare
      } ;

    GerundCN vp = {
      s = \\_ => gerundStr vp ; comp = [] ; ph = FinalVow ; anim = Inan ;
      heavyMod = \\_ => []
      } ;
    GerundNP vp = G.MassNP (GerundCN vp) ;
    GerundAdv vp = {s = (GerundNP vp).s ! Gen ++ "bidez"} ;
    ByVP = GerundAdv ;
    InOrderToVP vp = {s = vpObjects vp ++ glue vp.nstem "ra"} ;

    ApposNP np app = np ** {
      s = \\c => np.s ! c ++ SOFT_BIND ++ "," ++ app.s ! Abs ++ SOFT_BIND ++ "," ;
      stem = np.stem ++ SOFT_BIND ++ "," ++ app.stem ++ SOFT_BIND ++ ","
      } ;

    PositAdVAdj a = {s = a.s ! AAdv} ;

    UseDAP dap = G.DetNP dap ;
    UseDAPMasc = UseDAP ;
    UseDAPFem = UseDAP ;

    CompVP ant pol vp = {
      s = \\agr => case pol.p of {
        Pos => vpObjects vp ++ vp.prc ! Past ;
        Neg => vpObjects vp ++ "ez" ++ vp.prc ! Past
        } ;
      copula = Izan
      } ;

    ExistIPQS temp pol ip =
      G.UseQCl temp pol (G.QuestCl (G.ExistNP (lin NP ip))) ;

  oper
    sentenceFromStr : Str -> {s : Sentence} = \str -> {
      s = {beforeAux = str ; aux = mkVForms [] ; afterAux = []}
      } ;

    passVPSlash : ResEus.VPSlash -> ResEus.VerbPhrase = \vps -> vps ** {
      val = Da Izan ;
      dobj = {s = \\_ => [] ; agr = Hau ; isDef = True}
      } ;

    passAgentVPSlash : ResEus.VPSlash -> NounPhrase -> ResEus.VerbPhrase = \vps,agent ->
      insertAdv (ss (agent.s ! Ins)) (passVPSlash vps) ;

    relativeAP : ResEus.VerbPhrase -> AdjPhrase = \vp ->
      let rc = rclFromVP (BIND ++ "n") vp
      in {s = \\agr => rc.s ! Pres ! Simul ! Pos ! agr ;
          ph = FinalCons ; typ = Ko} ;

    reflexiveDet : Num -> Determiner = \num -> {
      s = artDef ! num.n ; indep = True ; nbr = num.n ;
      pref = "bere" ; isDef = True
      } ;

    reflexiveNP : RNP = lin RNP
      (G.DetCN (reflexiveDet G.NumSg) (useN (mkNoun "buru"))) ;

    vpObjects : ResEus.VerbPhrase -> Str = \vp ->
      vp.adv ++ vp.iobj.s ++ vp.dobj.s ! Pos ++ vp.comp ! Hau ;

    gerundStr : ResEus.VerbPhrase -> Str = \vp -> vpObjects vp ++ vp.nstem ;
 } ;
