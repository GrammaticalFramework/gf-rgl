--# -path=.:../common:../abstract

concrete ExtendAra of Extend =
  CatAra ** ExtendFunctor - [
    GenNP, ApposNP, ICompAP, DetNPMasc, DetNPFem,
    EmptyRelSlash, PredAPVP,
    ComplDirectVS, ComplDirectVQ, UttAdV, -- because of Utt
    VPS, MkVPS, PredVPS, BaseVPS, ConsVPS, ConjVPS,
    VPI, MkVPI, BaseVPI, ConsVPI, ConjVPI, ComplVPIVV,
    BaseComp, ConsComp, ConjComp,
    BaseImp, ConsImp, ConjImp,
    EmbedSSlash, AdjAsNP, GerundNP,
    PassVPSlash, ---- bogus implementation, see below
    PassAgentVPSlash, PresPartAP, PastPartAP, PastPartAgentAP, ProgrVPSlash,
    ReflPron, ReflPoss, AdvRNP, AdvRVP, AdvRAP, PossPronRNP,
    CompoundAP, GerundCN, GerundAdv, ByVP, PositAdVAdj,
    CompoundN, UseDAP, UseDAPMasc, UseDAPFem
]
  with (Grammar=GrammarAra)
  ** open

    ResAra,
    (P=ParamX),
    Prelude,
    ParadigmsAra,
    RelativeAra,
    Coordination

  in {

lin
  -- If the NP is a pronoun, just use PossPron
  GenNP np = case np.a.isPron of {
    True  => Grammar.PossPron (pgn2pron np.a.pgn) ;
    False => let g = np.s ! Gen in mkQuant3 g g g Const
  } ;

  -- : NP -> NP -> NP
  ApposNP np1 np2 = np2 ** {s = \\c => np1.s ! c ++ np2.s ! c} ;

  UttAdV adv = {s = \\_ => adv.s} ;

  -- : AP -> IComp ;   -- "how old"
  ICompAP ap = {s = \\gn => "كَمْ" ++ ap.s ! NoHum ! gn.g ! gn.n ! Indef ! Acc} ;

  -- : AP -> VP -> Cl ;   -- it is good to walk
  PredAPVP ap vp =
    let isGood : Str = "مِنَ" ++ ap.s ! Hum ! Masc ! Sg ! Def ! Gen ;
        toWalk : Str = "ال" ++ BIND ++ (uttVP VPGer vp ! Masc) ; -- TODO: Masdar into NForm=>Str?
        goodToWalk_Adv : Adv = lin Adv {s = isGood ++ toWalk} ;
     in PredVP emptyNP (UseComp (CompAdv goodToWalk_Adv)) ;

  -- : Det -> NP
  DetNPMasc det = emptyNP ** {s = det.s ! NoHum ! Masc} ;
  DetNPFem det = emptyNP ** {s = det.s ! NoHum ! Fem} ;

  -- : AP -> NP
  AdjAsNP ap =
  let adjAsN : Noun = {
        s = ap.s ! NoHum ! Masc ;
        s2 = emptyNTable ;
        g = Masc ;
        h = NoHum ;
        isDual = False } ;
   in MassNP (UseN adjAsN) ;

  -- : ClSlash -> RCl  -- he lives in
  EmptyRelSlash = RelSlash (IdRP ** {s = \\_ => []}) ;

  -- : SSlash -> SC
  EmbedSSlash = Grammar.EmbedS ;

  -- : VP -> NP
  GerundNP vp = emptyNP ** {
    s = \\_ => uttVP VPGer vp ! Masc ; -- NB. NP should inflect in case, but there are no cases for masdar in the inflection table of VP. If desired, can add here with BIND. /IL
    } ; -- The Gender param here doesn't make a difference, because the VPGer in VP's inflection table doesn't have gender to start with. So we could equally well choose Fem too.

lincat
  VPS   = {s : PerGenNum => Str} ;  -- finite VP's with tense and polarity
  [VPS] = {s1,s2 : PerGenNum => Str} ;
  VPI   = {s : Str} ;
  [VPI] = {s1,s2 : Str} ;
  [Comp] = {s1,s2 : AAgr => ResAra.Case => Str} ;
  [Imp] = {s1,s2 : P.Polarity => ResAra.Gender => ResAra.Number => Str} ;
lin
  -- : Temp -> Pol -> VP -> VPS ; -- hasn't slept
  MkVPS t p vp = {
    s = \\pgn => let vps =
      wordOrderNoSubj
        Nominal --  Nominal (=SVO) generalises best for ConjVPS.
        vp.obj.a.isPron
        (vStr vp pgn t.t p.p Nominal)
        vp.obj.s -- BIND is taken care of when the VP was made, no need to add one here
        (pred vp pgn t.t p.p)
        vp.s2
    in vps.before ++ vps.after -- word order is SVO, so this is safe for just this case.
    } ;

  -- maybe more robust to use إِيَّا as object carrier if the VPS
  -- consists of several VPSs; like it's done in Slash3V3 /IL
  BaseVPS = twoTable PerGenNum ;
  ConsVPS = consrTable PerGenNum comma ;
  ConjVPS = conjunctDistrTable PerGenNum ;

  PredVPS np vps = {
    s = \\_ => np.s ! Nom ++ vps.s ! np.a.pgn -- first quick version with order always Nominal.
    } ;                                       -- if necessary, change VPS into {s : PerGenNum => Order => {before,after : Str}}

  MkVPI vp = {s = uttVP VPGer vp ! Masc} ;
  BaseVPI x y = {s1 = x.s ; s2 = y.s} ;
  ConsVPI x xs = {s1 = x.s ++ SOFT_BIND ++ "," ++ xs.s1 ; s2 = xs.s2} ;
  ConjVPI conj xs = {s = xs.s1 ++ conj.s2 ++ xs.s2} ;
  ComplVPIVV vv vpi = insertStr (vv.s2 ++ vpi.s) (predV vv) ;

  BaseComp x y = {
    s1 = \\agr,cas => x.s ! agr ! cas ++ x.obj.s ;
    s2 = \\agr,cas => y.s ! agr ! cas ++ y.obj.s
    } ;
  ConsComp x xs = {
    s1 = \\agr,cas => x.s ! agr ! cas ++ x.obj.s ++ SOFT_BIND ++ "," ++ xs.s1 ! agr ! cas ;
    s2 = xs.s2
    } ;
  ConjComp conj xs = {
    s = \\agr,cas => xs.s1 ! agr ! cas ++ conj.s2 ++ xs.s2 ! agr ! cas ;
    obj = emptyObj ;
    isNP = False
    } ;

  BaseImp x y = {s1 = x.s ; s2 = y.s} ;
  ConsImp x xs = {
    s1 = \\p,g,n => x.s ! p ! g ! n ++ SOFT_BIND ++ "," ++ xs.s1 ! p ! g ! n ;
    s2 = xs.s2
    } ;
  ConjImp conj xs = {
    s = \\p,g,n => xs.s1 ! p ! g ! n ++ conj.s2 ++ xs.s2 ! p ! g ! n
    } ;


-- AR 24-02-08
    PassVPSlash vpslash = vpslash ** {
      s = \\pgn,vpf => case vpf of {
        VPPerf   => vpslash.s ! pgn ! VPPassPerf ;
        VPImpf m => vpslash.s ! pgn ! VPPassImpf m ;
        _        => vpslash.s ! pgn ! vpf
        } ;
      obj = emptyObj ;
      c2 = accPrep ;
      agrObj = \\_ => []
      } ;

    PassAgentVPSlash vpslash agent =
      insertStr ("مِنْ قِبَلِ" ++ agent.s ! Gen) (PassVPSlash vpslash) ;

    PresPartAP vp = {
      s = \\h,g,n,_,_ => presentRelative ! h ! g ! n
        ++ vp.s ! Per3 g n ! VPImpf Ind
        ++ vp.obj.s ++ vp.pred.s ! {g = g ; n = n} ! Acc ++ vp.s2
      } ;

    PastPartAP vpslash = {
      s = \\_,_,_,_,_ =>
        vpslash.s ! Per3 Masc ResAra.Sg ! VPPPart
        ++ vpslash.obj.s ++ vpslash.s2
      } ;

    PastPartAgentAP vpslash agent =
      let ap = PastPartAP vpslash in ap ** {
        s = \\h,g,n,d,c => ap.s ! h ! g ! n ! d ! c
                         ++ "مِنْ قِبَلِ" ++ agent.s ! Gen
        } ;

    ProgrVPSlash vpslash = vpslash ;

    ---- very unsure about this as well
    CompoundN a b = b ** {
      s  = \\n, s, c => b.s ! n ! Const ! c ++ a.s ! n ! s ! c ;
      s2 = \\n, s, c => b.s2 ! n ! Const ! c ++ a.s2 ! n ! s ! c
      } ;

lin UseDAP dap = case dap.isEmpty of {
      True => case <dap.d,dap.n> of {  -- if the s field is empty, make up some other determiner
                <Def,One>   => it_Pron ;
                <Def,_>     => they_Pron ;
                <Indef,One> => emptyNP ** {s = someSg_Det.s ! NoHum ! Masc} ;
                _           => emptyNP ** {s = somePl_Det.s ! NoHum ! Masc}
              } ;
      False => emptyNP ** {s = dap.s ! NoHum ! Masc} } ;

lin UseDAPMasc dap = case dap.isEmpty of {
      True => case <dap.d,dap.n> of {  -- if the s field is empty, make up some other determiner
                <Def,One>   => it_Pron ;
                <Def,_>     => theyMasc_Pron ;
                <Indef,One> => emptyNP ** {s = someSg_Det.s ! NoHum ! Masc} ;
                _           => emptyNP ** {s = somePl_Det.s ! NoHum ! Masc}
              } ;
      False => emptyNP ** {s = dap.s ! NoHum ! Masc} } ;

lin UseDAPFem dap = case dap.isEmpty of {
      True => case <dap.d,dap.n> of {  -- if the s field is empty, make up some other determiner
                <Def,One>   => it_Pron ;
                <Def,_>     => theyFem_Pron ;
                <Indef,One> => emptyNP ** {s = someSg_Det.s ! NoHum ! Fem} ;
                _           => emptyNP ** {s = somePl_Det.s ! NoHum ! Fem}
              } ;
      False => emptyNP ** {s = dap.s ! NoHum ! Fem} } ;

lin ReflPoss num cn =
      DetCN (DetQuant (PossPron he_Pron) num) cn ;

    ReflPron = lin NP (indeclNP "نَفْسِهِ" ResAra.Sg) ;

    AdvRNP np prep rnp = AdvNP np (PrepNP prep rnp) ;
    AdvRVP vp prep rnp = AdvVP vp (PrepNP prep rnp) ;
    AdvRAP ap prep rnp = AdvAP ap (PrepNP prep rnp) ;

    PossPronRNP pron num cn rnp =
      DetCN (DetQuant (PossPron pron) num)
        (PossNP cn rnp) ;

    GerundCN vp = useN {
      s = \\_,_,_ => uttVP VPGer vp ! Masc ;
      s2 = emptyNTable ;
      g = Masc ;
      h = NoHum ;
      isDual = False
      } ;
    GerundAdv vp = {s = uttVP VPGer vp ! Masc} ;
    ByVP vp = {s = "بِـ" ++ uttVP VPGer vp ! Masc} ;

    CompoundAP n a =
      let ap = Grammar.PositA a in ap ** {
        s = \\h,g,num,d,c => ap.s ! h ! g ! num ! d ! c
                           ++ n.s ! ResAra.Sg ! Const ! Gen
        } ;

    PositAdVAdj a = {s = a.s ! APosit Masc ResAra.Sg Indef Acc} ;

oper presentRelative : ResAra.Species => ResAra.Gender => ResAra.Number => Str = table {
  NoHum => \\_,_ => "الَّتِي" ;
  Hum => table {
    Masc => table {
      ResAra.Sg => "الَّذِي" ; Dl => "اللَّذَانِ" ; ResAra.Pl => "الَّذِينَ"
      } ;
    Fem => table {
      ResAra.Sg => "الَّتِي" ; Dl => "اللَّتَانِ" ; ResAra.Pl => "اللَّاتِي"
      }
    }
  } ;

}
