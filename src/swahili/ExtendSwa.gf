--# -path=.:../abstract:../common:../bantu:../prelude

concrete ExtendSwa of Extend =
  CatSwa ** ExtendFunctor - [
    VPS, ListVPS, VPI, ListVPI,
    RNP, RNPList, ListComp, ListImp,
    ReflPron, ReflPoss, PredetRNP, AdvRNP, AdvRVP, AdvRAP,
    ReflA2RNP, ReflRNP, PossPronRNP,
    Base_rr_RNP, Base_nr_RNP, Base_rn_RNP, Cons_rr_RNP, Cons_nr_RNP, ConjRNP,
    MkVPS, BaseVPS, ConsVPS, ConjVPS, PredVPS,
    MkVPI, BaseVPI, ConsVPI, ConjVPI, ComplVPIVV,
    CompoundN, GenModNP, PresPartAP, PastPartAP, PastPartAgentAP,
    PassVPSlash, PassAgentVPSlash, GerundCN, GerundAdv, UttVPShort,
    ComplSlashPartLast, UseDAP, UseDAPMasc, UseDAPFem, PositAdVAdj,
    ComplBareVS, EmptyRelSlash, ProgrVPSlash, CompoundAP, GerundNP, ByVP,
    ApposNP, BaseComp, ConsComp, ConjComp, BaseImp, ConsImp, ConjImp,
    TPastSimple
  ] with (Grammar = GrammarSwa) **
  open Prelude, ResSwa, GrammarSwa in {

  lincat
    VPS = {s : Agre => Str} ;
    [VPS] = {s1,s2 : Agre => Str} ;
    VPI = {s : Agre => Str} ;
    [VPI] = {s1,s2 : Agre => Str} ;
    RNP = {s : Agr => Str ; isPron : Bool} ;
    RNPList = {s1,s2 : Agr => Str} ;
    [Comp] = {s1,s2 : Agr => Str} ;
    [Imp] = {s1,s2 : Polarity => Number => Str} ;

  lin
    TPastSimple = GrammarSwa.TPast ;

    ReflPron = {s = reflPron ; isPron = True} ;
    ReflPoss num cn = {s = \\a =>
      cn.s ! num.n ! Nom ++ cn.s2 ! num.n ++
      possessivePron a num.n cn.g ++ selfPron a ;
      isPron = False} ;
    PredetRNP p r = {s = \\a => r.s ! a ++ p.s ! getGender a ; isPron = False} ;
    AdvRNP np p r = {s = \\a => np.s ! NPAcc ++ p.s ! getNumber a ! getGender a ++ r.s ! a ; isPron = False} ;
    AdvRVP vp p r = insertObj (\\a => p.s ! a.n ! a.g ++ r.s ! (Ag a.g a.n a.p)) vp ;
    AdvRAP ap p r = {s = \\g,n => ap.s ! g ! n ++ p.s ! n ! g ++ r.s ! (Ag g n P3)} ;
    ReflA2RNP a r = {s = \\g,n => a.s ! AAdj g n ++ a.c2 ++ r.s ! (Ag g n P3)} ;
    ReflRNP vp r = insertObj (\\a => vp.c2 ++ r.s ! (Ag a.g a.n a.p)) vp ;
    PossPronRNP pr num cn r = {s = \\_ =>
      cn.s ! num.n ! Nom ++ pr.s ! Poss num.n cn.g ++
      possess_Prep.s ! num.n ! cn.g ++ r.s ! pr.a ;
      a = Ag cn.g num.n P3} ;
    Base_rr_RNP a b = {s1 = a.s ; s2 = b.s} ;
    Base_nr_RNP a b = {s1 = \\_ => a.s ! NPAcc ; s2 = b.s} ;
    Base_rn_RNP a b = {s1 = a.s ; s2 = \\_ => b.s ! NPAcc} ;
    Cons_rr_RNP a xs = {s1 = \\g => a.s ! g ++ "," ++ xs.s1 ! g ; s2 = xs.s2} ;
    Cons_nr_RNP a xs = {s1 = \\g => a.s ! NPAcc ++ "," ++ xs.s1 ! g ; s2 = xs.s2} ;
    ConjRNP c xs = {s = \\a => c.s1 ++ xs.s1 ! a ++ c.s2 ++ xs.s2 ! a ; isPron = False} ;

    MkVPS t pol vp = {s = \\a => t.s ++ pol.s ++ vp.s ! t.t ! t.a ! pol.p ! a ++ vp.obj ! a} ;
    BaseVPS a b = {s1 = a.s ; s2 = b.s} ;
    ConsVPS a xs = {s1 = \\g => a.s ! g ++ "," ++ xs.s1 ! g ; s2 = xs.s2} ;
    ConjVPS c xs = {s = \\a => c.s1 ++ xs.s1 ! a ++ c.s2 ++ xs.s2 ! a} ;
    PredVPS np v = {s = np.s ! npNom ++ v.s ! agrFeatures np.a} ;
    MkVPI vp = {s = \\a => infVP Pos a vp} ;
    BaseVPI a b = {s1 = a.s ; s2 = b.s} ;
    ConsVPI a xs = {s1 = \\g => a.s ! g ++ "," ++ xs.s1 ! g ; s2 = xs.s2} ;
    ConjVPI c xs = {s = \\a => c.s1 ++ xs.s1 ! a ++ c.s2 ++ xs.s2 ! a} ;
    ComplVPIVV v x = insertObj (\\a => x.s ! a) (predV v) ;

    CompoundN a b = {s = \\n,c => b.s ! n ! c ++ possess_Prep.s ! n ! b.g ++ a.s ! Sg ! Nom ; g = b.g} ;
    GenModNP num np cn = {s = \\c => cn.s ! num.n ! npcase2case c ++ possess_Prep.s ! num.n ! cn.g ++ np.s ! NPoss ; a = Ag cn.g num.n P3} ;
    PresPartAP vp = {s = \\g,n => ambaPron g n ++ vp.s ! Pres ! Simul ! Pos ! agre g n P3 ++ vp.obj ! agre g n P3} ;
    PastPartAP vp = let pass = passiveVP vp in
      {s = \\g,n => ambaPron g n ++ pass.s ! Past ! Simul ! Pos ! agre g n P3 ++ pass.obj ! agre g n P3} ;
    PastPartAgentAP vp np = let pass = passiveVP vp in
      {s = \\g,n => ambaPron g n ++ pass.s ! Past ! Simul ! Pos ! agre g n P3 ++ pass.obj ! agre g n P3 ++ "na" ++ np.s ! NPAcc} ;
    PassVPSlash vp = passiveVP vp ;
    PassAgentVPSlash vp np = insertObj (\\_ => "na" ++ np.s ! NPAcc) (passiveVP vp) ;
    GerundCN vp = {s = \\_,_ => infVP Pos (agre G11 Sg P3) vp ; s2 = \\_ => [] ; g = G11} ;
    GerundAdv vp = {s = infVP Pos (agre G11 Sg P3) vp} ;
    UttVPShort vp = {s = vp.imp ! Pos ! Sg ++ vp.obj ! agre G1 Sg P2} ;
    ComplSlashPartLast vp np = insertObj (\\_ => np.s ! NPAcc ++ vp.c2) vp ;
    UseDAP d = {s = \\_ => d.s ! Obj G4 ; a = Ag G4 d.n P3} ;
    UseDAPMasc d = {s = \\_ => d.s ! Obj G1 ; a = Ag G1 d.n P3} ;
    UseDAPFem d = {s = \\_ => d.s ! Obj G1 ; a = Ag G1 d.n P3} ;
    PositAdVAdj a = {s = a.s ! AAdj G1 Sg} ;
    ComplBareVS v s = insertObj (\\_ => s.s) (predV v) ;
    EmptyRelSlash cl = {s = \\t,a,b,_ => cl.s ! t ! a ! b ++ cl.c2 ; c = NPAcc} ;
    ProgrVPSlash vp = vp ;
    CompoundAP n a = {s = \\g,num => n.s ! Sg ! Nom ++ a.s ! AAdj g num} ;
    GerundNP vp = {s = \\_ => infVP Pos (agre G11 Sg P3) vp ; a = Ag G11 Sg P3} ;
    ByVP vp = {s = "kwa" ++ infVP Pos (agre G11 Sg P3) vp} ;
    ApposNP a b = {s = \\c => a.s ! c ++ SOFT_BIND ++ "," ++ b.s ! c ; a = a.a} ;
    BaseComp a b = {s1 = a.s ; s2 = b.s} ;
    ConsComp a xs = {s1 = \\g => a.s ! g ++ "," ++ xs.s1 ! g ; s2 = xs.s2} ;
    ConjComp c xs = {s = \\a => c.s1 ++ xs.s1 ! a ++ c.s2 ++ xs.s2 ! a} ;
    BaseImp a b = {s1 = a.s ; s2 = b.s} ;
    ConsImp a xs = {s1 = \\p,n => a.s ! p ! n ++ "," ++ xs.s1 ! p ! n ; s2 = xs.s2} ;
    ConjImp c xs = {s = \\p,n => c.s1 ++ xs.s1 ! p ! n ++ c.s2 ++ xs.s2 ! p ! n} ;

  oper
    possessivePron : Agr -> Number -> Gender -> Str = \owner,n,g ->
      case n of {
        Sg => ProunSgprefix g + possessiveStem owner ;
        Pl => ProunPlprefix g + possessiveStem owner
        } ;

    possessiveStem : Agr -> Str = \owner -> case owner of {
      Ag G1 Sg P1 => "angu" ; Ag G1 Sg P2 => "ako" ;
      Ag G1 Sg P3 => "ake" ; Ag G1 Pl P1 => "etu" ;
      Ag G1 Pl P2 => "enu" ; Ag G1 Pl P3 => "ao" ;
      Ag _ _ _ => "ake"
      } ;

    selfPron : Agr -> Str = \owner -> case owner of {
      Ag _ Sg _ => "mwenyewe" ; Ag _ Pl _ => "wenyewe"
      } ;

    ambaPron : Gender -> Number -> Str = \g,n -> case <g,n> of {
      <G1,Sg> => "ambaye" ; <G1,Pl> => "ambao" ;
      <G2,Sg> => "ambao" ; <G2,Pl> => "ambayo" ;
      <G3,Sg> => "ambalo" ; <G3,Pl> => "ambayo" ;
      <G4,Sg> => "ambacho" ; <G4,Pl> => "ambavyo" ;
      <G5,Sg> => "ambayo" ; <G5,Pl> => "ambazo" ;
      <G6,Sg> => "ambao" ; <G6,Pl> => "ambazo" ;
      <G8,Sg> => "ambao" ; <G8,Pl> => "ambayo" ;
      <G9,_> => "ambayo" ; <G10,Sg> => "ambayo" ; <G10,Pl> => "ambazo" ;
      <_,_> => "ambayo"
      } ;
}
