--# -path=alltenses:../common:../abstract

concrete ExtendGer of Extend =
  CatGer ** ExtendFunctor
   - [
    InOrderToVP,
    VPS, ListVPS, VPI, ListVPI,
    MkVPS, BaseVPS, ConsVPS, ConjVPS, PredVPS, 
    MkVPI, BaseVPI, ConsVPI, ConjVPI, ComplVPIVV
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
--        insertInf (vpi.s ! v.isAux) (
      insertInf {s=(vpi.s ! v.isAux);isAux=v.isAux;ctrl=SubjC} ( -- HL ??
            predVGen v.isAux v) ; ----

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
          neg   = tm.s ++ p.s ++ vp.a1 ++ negation ! b ; -- HL 8/19 ++ vp.a1 ! b ;
          -- obj1  = (vp.nn ! agr).p1 ;
          -- obj   = (vp.nn ! agr).p2 ; 
          -- compl = obj1 ++ neg ++ obj ++ vp.a2 ; -- from EG 15/5
          obj1  = (vp.nn ! agr).p1 ++ (vp.nn ! agr).p2 ; -- refl ++ pronouns ++ nonpronouns
          obj2  = (vp.nn ! agr).p3 ;                     -- pp-objects
          obj3  = (vp.nn ! agr).p4 ++ vp.adj ++ vp.a2 ;  -- pred.AP|CN|Adv, via useComp HL 6/2019
          compl = obj1 ++ neg ++ obj2 ++ obj3 ;
          inf   = vp.inf.s ++ verb.inf ++ verb.inf2 ;
          extra = vp.ext ;
          infE : Str =                              -- HL 30/6/2019
            case <t,a,vp.isAux> of {
              <Fut|Cond,Simul,True> => inf ;                           --# notpresent
              <Fut|Cond,Anter,True> -- Duden 318: kommen wollen haben => haben kommen wollen --# notpresent
                => verb.inf2 ++ vp.inf.s ++ verb.inf ;                   --# notpresent
              <_,Anter,True> => inf ;                                  --# notpresent
              _ => verb.inf ++ verb.inf2 ++ vp.inf.s } ;
          inffin : Str =
            case <t,a,vp.isAux> of {
	           <Fut|Cond,Anter,True>  -- ... wird|würde haben kommen wollen --# notpresent
                     => verb.fin ++ verb.inf2 ++ vp.inf.s ++ verb.inf ;  --# notpresent
	           <_,Anter,True>                                      --# notpresent
                     => verb.fin ++ inf ;            -- double inf     --# notpresent
                   _ => inf ++ verb.fin              --- or just auxiliary vp
            } ;
        in
        case o of {
	    Main => subj ++ verb.fin ++ compl ++ vp.infExt ++ infE ++ extra ;
	    Inv  => verb.fin ++ subj ++ compl ++ vp.infExt ++ infE ++ extra ;
	    Sub  => subj ++ compl ++ vp.infExt ++ inffin ++ extra
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

}
