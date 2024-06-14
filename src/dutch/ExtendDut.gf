--# -path=alltenses:../common:../abstract

concrete ExtendDut of Extend =
  CatDut ** ExtendFunctor
   - [PastPartAP,ICompAP,IAdvAdv,
      VPS,
      BaseVPS, ConsVPS,
      MkVPS, ConjVPS, PredVPS,
      PassVPSlash, PassAgentVPSlash,
      CompoundN
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

lincat
  VPS   = {s : Order => Agr => Str} ;
  [VPS] = {s1,s2 : Order => Agr => Str} ;

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

}
