--# -path=alltenses:../common:../abstract

concrete ExtendGer of Extend =
  CatGer ** ExtendFunctor
   - [
    InOrderToVP,
    VPS, ListVPS, VPI, ListVPI,
    MkVPS, BaseVPS, ConsVPS, ConjVPS, PredVPS, 
    MkVPI, BaseVPI, ConsVPI, ConjVPI, ComplVPIVV,
    CardCNCard, PassVPSlash, PassAgentVPSlash, CompoundN
    ]
  with
    (Grammar = GrammarGer) **
  open
    GrammarGer,
    ResGer,
    Coordination,
    Prelude,
    ParadigmsGer in {

  lincat
    VPI   = {s : Bool => Str} ;
    [VPI] = {s1,s2 : Bool => Str} ;
    VPS   = {s : Order => Agr => Str} ;
    [VPS] = {s1,s2 : Order => Agr => Str} ;

lin

  InOrderToVP vp = {s = "um" ++ useInfVP False vp} ;

    BaseVPI = twoTable Bool ;
    ConsVPI = consrTable Bool comma ;

    MkVPI vp = {s = \\b => useInfVP b vp} ;
    ConjVPI = conjunctDistrTable Bool ;

    ComplVPIVV v vpi = 
      insertInf {inpl = <\\_ => [], (vpi.s ! v.isAux)> ; extr = \\_ => []}  -- HL 3/22
        (predVGen v.isAux v) ;

    BaseVPS = twoTable2 Order Agr ;
    ConsVPS = consrTable2 Order Agr comma ;

    PredVPS np vpi = 
      let
        subj = np.s ! NPC Nom ++ bigNP np ;
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

    MkVPS tm p vp = 
      let vps = useVP vp in {
        s = \\o,agr => 
         let 
          ord   = case o of {
            Sub => True ;  -- glue prefix to verb
            _ => False
            } ;
          b = p.p ;
          a = tm.a ;
          t = tm.t ;
          m = tm.m ;
          subj  = [] ;
          verb  = vps.s  ! ord ! agr ! VPFinite m t a ;
          haben = verb.inf2 ;
          neg   = tm.s ++ p.s ++ vp.a1 ++ negation ! b ; -- HL 8/19 ++ vp.a1 ! b ;
          -- obj1  = (vp.nn ! agr).p1 ;
          -- obj   = (vp.nn ! agr).p2 ; 
          -- compl = obj1 ++ neg ++ obj ++ vp.a2 ; -- from EG 15/5
          obj1  = (vp.nn ! agr).p1 ++ (vp.nn ! agr).p2 ; -- refl ++ pronouns ++ light nps
          obj2  = (vp.nn ! agr).p3 ;                     -- pp-objects and heavy nps
          obj3  = (vp.nn ! agr).p4 ++ vp.adj ++ vp.a2 ;  -- pred.AP|CN|Adv, via useComp HL 6/2019
          compl = obj1 ++ neg ++ obj2 ++ obj3 ;
          infObjs = (vp.inf.inpl.p1)!agr ;               -- adapted to new VP.inf, HL 3/2022
          infPred = vp.inf.inpl.p2 ;
          infCompl : Str = case <t,a,vp.isAux> of {
             <Fut|Cond,Anter,True> => [] ;                                --# notpresent
             _ => infObjs ++ infPred } ;
          pred : {inf, infComplfin : Str} = case <t,a,vp.isAux> of {
             <Fut|Cond,Anter,True>  =>                                    --# notpresent
               {inf    = infObjs ++ haben ++ infPred ++ verb.inf ;        --# notpresent Duden 318
                infComplfin = -- es ++ wird ++ haben ++ tun ++ wollen     --# notpresent
                   infObjs ++ verb.fin ++ haben ++ infPred ++ verb.inf} ; --# notpresent
             <_,Anter,True> =>                                            --# notpresent
               {inf    = verb.inf ++ haben ;                              --# notpresent
                infComplfin = -- es ++ wird/hat/hatte ++ tun ++ wollen    --# notpresent
                   infObjs ++ verb.fin ++ infPred ++ verb.inf ++ haben} ; --# notpresent
              <Pres,_,_> =>
               {inf    = verb.inf ++ haben ;
                infComplfin = -- es zu tun ++ [] ++ [] ++ versucht
                   infCompl ++ verb.inf ++ haben ++ verb.fin}
                                                                         ; --# notpresent
              _ =>                                                         --# notpresent
               {inf    = verb.inf ++ haben ;                               --# notpresent
                infComplfin = -- es zu tun ++ versucht ++ [] ++ hat        --# notpresent
                              infCompl ++ verb.inf ++ haben ++ verb.fin}   --# notpresent
              } ;
           extra = vp.inf.extr!agr ++ vp.ext ;
        in
        case o of {
	  Main => subj ++ verb.fin ++ compl ++ infCompl ++ pred.inf ++ extra ;
	  Inv  => verb.fin ++ subj ++ compl ++ infCompl ++ pred.inf ++ extra ;
	  Subj =>             subj ++ compl ++   pred.infComplfin   ++ extra
        }
    } ;

    ConjVPS = conjunctDistrTable2 Order Agr ;
    
    UseDAP det = {
      s = \\c => det.sp ! Neutr ! c ;
      a = agrP3 det.n ;
      w = case det.isDef of { True => WLight ; _ => WHeavy } ;
      rc, ext = []
      } ;

    UseDAPMasc det = {
      s = \\c => det.sp ! Masc ! c ;
      a = agrP3 det.n ;
      w = WLight ;
      rc, ext = []
      } ;

    UseDAPFem det = {
      s = \\c => det.sp ! Fem ! c ;
      a = agrP3 det.n ;
      w = WLight ;
      rc, ext = []
      } ;

lin
  CardCNCard card cn = {
    s = \\g,c =>
      (Grammar.DetCN (Grammar.DetQuant Grammar.IndefArt (Grammar.NumCard card)) cn).s ! NPC c ;
    n = Pl
    } ;

lin GivenName = \n -> { s = n.s; g = sex2gender n.g; n = Sg } ;
lin MaleSurname = \n -> { s = n.s ! Male ; g = Masc; n = Sg } ;
lin FemaleSurname = \n -> { s = n.s ! Female ; g = Fem; n = Sg } ;
lin PlSurname = \n -> { s = n.s ! Male ; g = Masc; n = Pl } ;
lin FullName gn sn = {
       s = \\c => gn.s ! Nom ++ sn.s ! gn.g ! c ;
       g = sex2gender gn.g ;
       n = Sg
    } ;

lin PassVPSlash vp = 
      insertObj (\\_ => (PastPartAP vp).s ! APred) (predV werdenPass) **
          { c1 = subjPrep vp.c2 } ;
    -- this also gives "mit dir wird gerechnet" ;
    -- the alternative linearisation ("es wird mit dir gerechnet") is not implemented

lin PassAgentVPSlash vp np = ---- "von" here, "durch" in StructuralGer
      insertObj (\\_ => (PastPartAgentAP (lin VPSlash vp) (lin NP np)).s ! APred) (predV werdenPass) ;

lin CompoundN a x =
       let s = a.co in
       lin N {
          s  = \\n,c => s ++ Predef.BIND ++ x.uncap.s ! n ! c ; 
          co = s ++ Predef.BIND ++ x.uncap.co ;
          uncap = {
            s  = \\n,c => a.uncap.co  ++ Predef.BIND ++ x.uncap.s ! n ! c ; 
            co = a.uncap.co  ++ Predef.BIND ++ x.uncap.co ;
            } ;
          g = x.g
          } ;

}
