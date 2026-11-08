concrete ExtendAfr of Extend =
  CatAfr ** ExtendFunctor - [
    VPS, BaseVPS, ConsVPS, MkVPS, ConjVPS, PredVPS,
    VPI, BaseVPI, ConsVPI, MkVPI, ConjVPI, ComplVPIVV,
    PassVPSlash, PassAgentVPSlash,
    PresPartAP, PastPartAP, PastPartAgentAP, ProgrVPSlash,
    RNP, RNPList, ReflRNP, ReflPron, ReflPoss, PredetRNP,
    AdvRNP, AdvRVP, AdvRAP, ReflA2RNP, PossPronRNP,
    ConjRNP, Base_rr_RNP, Base_nr_RNP, Base_rn_RNP,
    Cons_rr_RNP, Cons_nr_RNP,
    GenModNP, ComplBareVS, CompoundN, CompoundAP,
    GerundCN, GerundNP, GerundAdv, ByVP, InOrderToVP,
    ApposNP, UttVPShort, ComplSlashPartLast,
    PositAdVAdj,
    UseDAP, UseDAPMasc, UseDAPFem,
    BaseComp, ConsComp, ConjComp, BaseImp, ConsImp, ConjImp
  ]
  with
    (Grammar = GrammarAfr) **

  open
    ParadigmsAfr, ResAfr, Coordination, Prelude in {

lincat
  VPS = {s : Order => Agr => Str} ;
  [VPS] = {s1,s2 : Order => Agr => Str} ;
  VPI = {s : Agr => Str} ;
  [VPI] = {s1,s2 : Agr => Str} ;

lin BaseVPS = twoTable2 Order Agr ;
    ConsVPS = consrTable2 Order Agr comma ;
    MkVPS tm pol vp = {
      s = \\o,a => (mkClause [] a vp).s ! tm.t ! tm.a ! pol.p ! o
      } ;
    ConjVPS conj vps = conjunctDistrTable2 Order Agr conj vps ;
    PredVPS np vps = {
      s = \\o => case o of {
        Main | Sub => np.s ! NPNom ++ vps.s ! o ! np.a ;
        Inv => vps.s ! Inv ! np.a ++ np.s ! NPNom
        }
      } ;

    BaseVPI = twoTable Agr ;
    ConsVPI = consrTable Agr comma ;
    MkVPI vp = {s = \\a => infVPString vp a} ;
    ConjVPI conj vpis = conjunctDistrTable Agr conj vpis ;
    ComplVPIVV vv vpi = insertInf (vpi.s ! agrP3 Sg) (predVGen vv.isAux vv) ;

lincat
  RNP = {s : Agr => Str ; isPron : Bool} ;
  RNPList = {s1,s2 : Agr => Str} ;

lin ReflRNP vps rnp = insertObjNP rnp.isPron (\\a => appPrep vps.c2 (\\_ => rnp.s ! a)) vps ;
    ReflPron = {s = reflPron ; isPron = True} ;
    ReflPoss num cn = {
      s = \\a => reflPossDet a ++ num.s ++ cn.s ! Strong ! NF num.n Nom ;
      isPron = False
      } ;
    PredetRNP pred rnp = {
      s = \\a => pred.s ! a.n ! a.g ++ rnp.s ! a ;
      isPron = False
      } ;
    AdvRNP np prep rnp = {
      s = \\a => np.s ! NPAcc ++ prep.s ++ rnp.s ! a ;
      isPron = False
      } ;
    AdvRVP vp prep rnp = insertObj (\\a => prep.s ++ rnp.s ! a) vp ;
    AdvRAP ap prep rnp = {
      s = \\af => ap.s ! af ++ prep.s ++ rnp.s ! agrP3 Sg ;
      isPre = False
      } ;
    ReflA2RNP a rnp = {
      s = \\af => a.s ! Posit ! af ++ a.c2 ++ rnp.s ! agrP3 Sg ;
      isPre = False
      } ;
    PossPronRNP pron num cn rnp = heavyNP {
      s = \\c => pron.unstressed.poss ++ num.s ++ cn.s ! Strong ! NF num.n Nom ++
                   "van" ++ rnp.s ! pron.a ;
      a = agrP3 num.n
      } ;
    ConjRNP conj rnps = conjunctDistrTable Agr conj rnps ** {isPron = False} ;
    Base_rr_RNP = twoTable Agr ;
    Base_nr_RNP np rnp = twoTable Agr {s = \\_ => np.s ! NPAcc} rnp ;
    Base_rn_RNP rnp np = twoTable Agr rnp {s = \\_ => np.s ! NPAcc} ;
    Cons_rr_RNP = consrTable Agr comma ;
    Cons_nr_RNP np rnps = consrTable Agr comma {s = \\_ => np.s ! NPAcc} rnps ;

lin GenModNP num np cn = heavyNP {
      s = \\c => np.s ! NPNom ++ "se" ++ cn.s ! Strong ! NF num.n Nom ;
      a = agrP3 num.n
      } ;
    ComplBareVS v s = insertExtrapos (s.s ! Sub) (predV v) ;

    CompoundN n1 n2 = {
      s = \\f => n1.s ! NF Sg Nom ++ BIND ++ n2.s ! f ;
      g = n2.g
      } ;
    CompoundAP noun adj = {
      s = \\af => noun.s ! NF Sg Nom ++ BIND ++ adj.s ! Posit ! af ;
      isPre = True
      } ;

    PresPartAP vp = {
      s = \\_ => "wat" ++ (mkClause [] (agrP3 Sg) vp).s ! Pres ! Simul ! Pos ! Sub ;
      isPre = False
      } ;
    PastPartAP vp = {
      s = \\_ => partVP vp (agrP3 Sg) ;
      isPre = True
      } ;
    PastPartAgentAP vp np = {
      s = \\_ => partVP vp (agrP3 Sg) ++ "door" ++ np.s ! NPAcc ;
      isPre = False
      } ;
    ProgrVPSlash vp = vp ;
    GerundCN vp = {
      s = \\_,_ => infVPString vp (agrP3 Sg) ;
      g = Neutr
      } ;
    GerundNP vp = heavyNP {
      s = \\_ => infVPString vp (agrP3 Sg) ;
      a = agrP3 Sg
      } ;
    GerundAdv vp = {s = "door" ++ infVPString vp (agrP3 Sg)} ;
    ByVP vp = {s = "door" ++ infVPString vp (agrP3 Sg)} ;
    InOrderToVP vp = {s = "om" ++ infVPString vp (agrP3 Sg)} ;

    ApposNP np1 np2 = heavyNP {
      s = \\c => np1.s ! c ++ "," ++ np2.s ! c ;
      a = np1.a
      } ;
    PositAdVAdj a = {s = a.s ! Posit ! APred} ;
    UttVPShort vp = {s = vp.s.s ! VInf ++ vp.n2 ! agrP3 Sg ++ vp.a2} ;
    ComplSlashPartLast vps np = insertObjNP np.isPron
      (\\_ => np.s ! NPAcc ++ vps.c2) (vps ** {c2 = []}) ;

    UseDAP dap = heavyNP {
      s = \\_ => dap.sp ! Neutr ;
      a = agrP3 dap.n
      } ;
    UseDAPMasc = UseDAP ;
    UseDAPFem = UseDAP ;

lincat [Comp] = {s1,s2 : Agr => Str} ;
lin BaseComp = twoTable Agr ;
    ConsComp = consrTable Agr comma ;
    ConjComp = conjunctDistrTable Agr ;

lincat [Imp] = {s1,s2 : Polarity => ImpForm => Str} ;
lin BaseImp = twoTable2 Polarity ImpForm ;
    ConsImp = consrTable2 Polarity ImpForm comma ;
    ConjImp = conjunctDistrTable2 Polarity ImpForm ;

-- KA: guessed from PassV2 in Afrikaans and the equivalents in Dutch
lin PassVPSlash vps = 
      insertInf (vps.s.s ! VPerf) (predV word_V) ;
    PassAgentVPSlash vps np = 
      insertAdv (appPrep "door" np.s) (insertInf (vps.s.s ! VPerf) (predV word_V)) ;

oper
  infVPString : ResAfr.VP -> Agr -> Str = \vp,a ->
    "om" ++ vp.n0 ! a ++ vp.n2 ! a ++ vp.a2 ++ "te" ++
    vp.s.s ! VInf ++ vp.inf.p1 ++ vp.ext ;

  partVP : ResAfr.VP -> Agr -> Str = \vp,a ->
    vp.s.s ! VPerf ++ vp.n0 ! a ++ vp.n2 ! a ++ vp.a2 ++ vp.inf.p1 ++ vp.ext ;

  reflPossDet : Agr -> Str = \a -> case <a.n,a.p> of {
    <Sg,P1> => "my" ; <Sg,P2> => "jou" ; <Sg,P3> => "sy" ;
    <Pl,P1> => "ons" ; <Pl,P2> => "julle" ; <Pl,P3> => "hulle"
    } ;

}
