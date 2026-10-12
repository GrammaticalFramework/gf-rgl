--# -path=.:../abstract:../common:../prelude

concrete ExtendIna of Extend = CatIna **
  ExtendFunctor - [
    VPS, BaseVPS, ConsVPS, MkVPS, ConjVPS, PredVPS,
    VPI, BaseVPI, ConsVPI, MkVPI, ConjVPI, ComplVPIVV,
    BaseComp, ConsComp, ConjComp, BaseImp, ConsImp, ConjImp,
    PassVPSlash, PassAgentVPSlash, PresPartAP, PastPartAP,
    PastPartAgentAP, ProgrVPSlash,
    RNP, RNPList, ReflRNP, ReflPron, ReflPoss, PredetRNP,
    AdvRNP, AdvRVP, AdvRAP, ReflA2RNP, PossPronRNP,
    ConjRNP, Base_rr_RNP, Base_nr_RNP, Base_rn_RNP,
    Cons_rr_RNP, Cons_nr_RNP,
    CompoundN, CompoundAP, GerundCN, GerundNP, GerundAdv,
    ByVP, InOrderToVP, ApposNP, PositAdVAdj, UttVPShort,
    ComplSlashPartLast, UseDAP, UseDAPMasc, UseDAPFem
  ]
  with (Grammar = GrammarIna) **
  open GrammarIna, ResIna, Coordination, Prelude in {

  flags coding=utf8 ;

  lincat
    VPS = {s : Agr => Str} ;
    [VPS] = {s1,s2 : Agr => Str} ;
    VPI = {s : Agr => Str} ;
    [VPI] = {s1,s2 : Agr => Str} ;

  lin
    MkVPS temp pol vp = {
      s = \\a => (mkClause [] a vp).s ! variants {True;False}
                                      ! temp.t ! temp.a ! pol.p ! ODir
      } ;
    BaseVPS = twoTable Agr ;
    ConsVPS = consrTable Agr comma ;
    ConjVPS = conjunctDistrTable Agr ;
    PredVPS np vps = {s = np.s ! Nom ++ vps.s ! np.a} ;

    MkVPI vp = {s = \\a => infVPAg vp a} ;
    BaseVPI = twoTable Agr ;
    ConsVPI = consrTable Agr comma ;
    ConjVPI = conjunctDistrTable Agr ;
    ComplVPIVV vv vpi = insertInvarObj (vpi.s ! Sp3) (predV vv) ;

  lincat
    [Comp] = {s1,s2 : Agr => Str} ;
    [Imp] = {s1,s2 : Polarity => Number => Str} ;

  lin
    BaseComp = twoTable Agr ;
    ConsComp = consrTable Agr comma ;
    ConjComp = conjunctDistrTable Agr ;

    BaseImp = twoTable2 Polarity Number ;
    ConsImp = consrTable2 Polarity Number comma ;
    ConjImp = conjunctDistrTable2 Polarity Number ;

    PassVPSlash vps =
      insertInvarObj (partVP vps Sp3) (predV_ esserV) ;
    PassAgentVPSlash vps np =
      insertInvarObj (partVP vps Sp3 ++ "per" ++ np.s ! Acc)
                     (predV_ esserV) ;

    PresPartAP vp = {
      s = \\a => vp.prp ++ vp.clitics ! a ++ vp.rest ! a ;
      isPre = False
      } ;
    PastPartAP vps = {
      s = \\a => partVP vps a ;
      isPre = False
      } ;
    PastPartAgentAP vps np = {
      s = \\a => partVP vps a ++ "per" ++ np.s ! Acc ;
      isPre = False
      } ;
    ProgrVPSlash vps = vps ;

  lincat
    RNP = {s : Agr => Case => Str ; isPronoun : Bool} ;
    RNPList = {s1,s2 : Agr => Case => Str} ;

  lin
    ReflRNP vps rnp = insertReflObj vps.p2 vps.c2 rnp vps ;
    ReflPron = {
      s = \\a,c => casePrep [] c ++ reflPron ! a ;
      isPronoun = True
      } ;
    ReflPoss num cn = {
      s = \\a,c => casePrep [] c ++ reflPoss a ++ cn.s ! num.n ;
      isPronoun = False
      } ;
    PredetRNP pred rnp = {
      s = \\a,c => pred.s ++ rnp.s ! a ! c ;
      isPronoun = False
      } ;
    AdvRNP np prep rnp = {
      s = \\a,c => np.s ! c ++ prep.s ++ rnp.s ! a ! prep.c ;
      isPronoun = False
      } ;
    AdvRVP vp prep rnp = vp ** {
      rest = \\a => vp.rest ! a ++ prep.s ++ rnp.s ! a ! prep.c
      } ;
    AdvRAP ap prep rnp = {
      s = \\a => ap.s ! a ++ prep.s ++ rnp.s ! a ! prep.c ;
      isPre = False
      } ;
    ReflA2RNP a rnp = {
      s = \\agr => a.s ! AAdj Posit ++ a.c2 ++ rnp.s ! agr ! Acc ;
      isPre = False
      } ;
    PossPronRNP pron num cn rnp = {
      s = \\c => casePrep [] c ++ pron.possForm ++ cn.s ! num.n ++
                   rnp.s ! pron.a ! Gen ;
      a = agrP3 num.n ;
      isPronoun = False
      } ;
    ConjRNP conj rnps = conjunctDistrTable2 Agr Case conj rnps ** {
      isPronoun = False
      } ;
    Base_rr_RNP = twoTable2 Agr Case ;
    Base_nr_RNP np rnp = twoTable2 Agr Case
      {s = \\_,c => np.s ! c} rnp ;
    Base_rn_RNP rnp np = twoTable2 Agr Case
      rnp {s = \\_,c => np.s ! c} ;
    Cons_rr_RNP = consrTable2 Agr Case comma ;
    Cons_nr_RNP np rnps = consrTable2 Agr Case comma
      {s = \\_,c => np.s ! c} rnps ;

    CompoundN n1 n2 = {
      s = \\n => n2.s ! n ++ "de" ++ n1.s ! Sg
      } ;
    CompoundAP noun adj = {
      s = \\a => adj.s ! AAdj Posit ++ "de" ++ noun.s ! Sg ;
      isPre = False
      } ;
    GerundCN vp = {s = \\_ => infVPAg vp Sp3} ;
    GerundNP vp = {
      s = \\_ => infVPAg vp Sp3 ;
      a = Sp3 ;
      isPronoun = False
      } ;
    GerundAdv vp = {s = vp.prp ++ vp.clitics ! Sp3 ++ vp.rest ! Sp3} ;
    ByVP vp = {s = "per" ++ infVPAg vp Sp3} ;
    InOrderToVP vp = {s = "pro" ++ infVPAg vp Sp3} ;
    ApposNP np1 np2 = {
      s = \\c => np1.s ! c ++ comma ++ np2.s ! c ;
      a = np1.a ;
      isPronoun = False
      } ;
    PositAdVAdj a = {s = a.s ! AAdv} ;
    UttVPShort vp = {s = infVPAg vp Sp3} ;
    ComplSlashPartLast vps np = insertObj vps.p2 vps.c2 np vps ;

    UseDAP dap = {
      s = dap.s ;
      a = agrP3 dap.n ;
      isPronoun = False
      } ;
    UseDAPMasc = UseDAP ;
    UseDAPFem = UseDAP ;

  oper
    infVPAg : ResIna.VP -> Agr -> Str = \vp,a -> variants {
      vp.clitics ! a ++ vp.inf ++ vp.rest ! a ;
      vp.inf ++ vp.clitics ! a ++ vp.rest ! a
      } ;

    partVP : ResIna.VP -> Agr -> Str = \vp,a ->
      vp.part ++ vp.clitics ! a ++ vp.rest ! a ;

    reflPoss : Agr -> Str = \a -> case <a.n,a.p> of {
      <Sg,P1> => "mi" ; <Sg,P2> => "tu" ; <Sg,P3> => "su" ;
      <Pl,P1> => "nostre" ; <Pl,P2> => "vostre" ; <Pl,P3> => "lor"
      } ;
}
