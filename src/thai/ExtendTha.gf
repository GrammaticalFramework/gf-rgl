--# -path=.:../abstract:../common

concrete ExtendTha of Extend = CatTha ** open
  Prelude, ResTha, StringsTha, Coordination, (G = GrammarTha)
in {

  lincat
    VPS, VPI = SS ;
    [VPS], [VPI] = {s1, s2 : Str} ;
    VPS2, VPI2 = SS ** {c2 : Str} ;
    [VPS2], [VPI2] = {s1, s2, c2 : Str} ;
    RNP = NP ;
    RNPList = {s1, s2 : Str} ;
    [Comp] = {s1, s2 : Str} ;
    [Imp] = {s1, s2 : Polarity => Str} ;
    X = SS ;

  lin
    GenModNP num np cn =
      G.DetCN (G.DetQuant G.DefArt num)
        (cn ** {s = thbind cn.s khoog_s np.s}) ;
    CompBareCN = G.CompCN ;
    StrandQuestSlash = G.QuestSlash ;
    StrandRelSlash = G.RelSlash ;
    EmptyRelSlash slash = {s = \\p => thbind (slash.s ! p) slash.c2} ;

    MkVPS t p vp = ss (thbind t.s p.s (vp.s ! p.p) vp.e) ;
    BaseVPS = twoSS ;
    ConsVPS = consrSS [] ;
    ConjVPS = conjunctDistrSS ;
    PredVPS np vps = ss (thbind np.s vps.s) ;

    MkVPI vp = ss (infVP vp) ;
    BaseVPI = twoSS ;
    ConsVPI = consrSS [] ;
    ConjVPI = conjunctDistrSS ;
    ComplVPIVV vv vpi = G.ComplVV vv (lin VP {s = \\_ => vpi.s ; e = []}) ;

    BaseComp x y = {s1 = x.s ! Pos ; s2 = y.s ! Pos} ;
    ConsComp x xs = {s1 = thbind (x.s ! Pos) xs.s1 ; s2 = xs.s2} ;
    ConjComp c xs = {s = \\p => thbind c.s1 xs.s1 c.s2 xs.s2} ;

    BaseImp x y = {s1 = x.s ; s2 = y.s} ;
    ConsImp x xs = {s1 = \\p => thbind (x.s ! p) (xs.s1 ! p) ; s2 = xs.s2} ;
    ConjImp c xs = {s = \\p => thbind c.s1 (xs.s1 ! p) c.s2 (xs.s2 ! p)} ;

    PassVPSlash vps = {
      s = \\p => thbind (polStr may_s p) thuuk_s (vps.s ! Pos) ;
      e = vps.e
      } ;
    PassAgentVPSlash vps np = insertExtra (thbind "โดย" np.s) (PassVPSlash vps) ;
    PresPartAP vp = mkAdj (thbind thii_s (infVP vp)) ;
    PastPartAP vps = mkAdj (thbind thii_s thuuk_s (infVP vps)) ;
    PastPartAgentAP vps np =
      mkAdj (thbind thii_s thuuk_s (infVP vps) "โดย" np.s) ;
    ProgrVPSlash vps = vps ** {
      s = \\p => thbind (thword kam_s lag2_s) (vps.s ! p)
      } ;

    ReflPron = mkNP reflPron ;
    ReflPoss num cn = mkNP (thbind cn.s khoog_s reflPron) ;
    PredetRNP pred rnp = mkNP (thbind pred.s1 rnp.s pred.s2) ;
    ReflRNP slash rnp = insertObj rnp slash ;
    AdvRNP np prep rnp = mkNP (thbind np.s prep.s rnp.s) ;
    AdvRVP vp prep rnp = insertObj (ss (thbind prep.s rnp.s)) vp ;
    AdvRAP ap prep rnp = mkAdj (thbind ap.s prep.s rnp.s) ;
    ReflA2RNP a rnp = mkAdj (thbind a.s a.c2 rnp.s) ;
    PossPronRNP pron num cn rnp =
      -- Keep both possessive relations distinct.  In particular, do not
      -- coordinate the possessed noun with its complement ("his
      -- abandonment and his wife ...").
      mkNP (thbind cn.s khoog_s pron.s "ที่มีต่อ" rnp.s) ;

    Base_rr_RNP x y = twoSS x y ;
    Base_nr_RNP x y = twoSS x y ;
    Base_rn_RNP x y = twoSS x y ;
    Cons_rr_RNP x xs = consrSS [] x xs ;
    Cons_nr_RNP x xs = consrSS [] x xs ;
    ConjRNP c xs = mkNP (thbind c.s1 xs.s1 c.s2 xs.s2) ;

    CompoundN modifier head = {s = thbind head.s modifier.s ; c = head.c} ;
    CompoundAP n a = mkAdj (thbind a.s n.s) ;
    GerundCN vp = {s = thbind "การ" (infVP vp) ; c = []} ;
    GerundNP vp = mkNP (thbind "การ" (infVP vp)) ;
    GerundAdv vp = ss (thbind "โดย" (infVP vp)) ;
    WithoutVP vp = ss (thbind "โดยไม่" (infVP vp)) ;
    ByVP vp = ss (thbind "โดย" (infVP vp)) ;
    InOrderToVP vp = ss (thbind "เพื่อ" (infVP vp)) ;
    ApposNP np1 np2 = mkNP (thbind np1.s np2.s) ;
    PositAdVAdj a = ss a.s ;

    UseDAP dap = mkNP (thbind dap.adj dap.s1 dap.s2) ;
    UseDAPMasc = UseDAP ;
    UseDAPFem = UseDAP ;

    ComplBareVS vs s = insertObj (mkNP s.s) (predV vs) ;
    ComplSlashPartLast = G.ComplSlash ;
    UttVPShort vp = ss (infVP vp) ;
    TPastSimple = G.TPast ;

    UttAdV adv = adv ;
}
