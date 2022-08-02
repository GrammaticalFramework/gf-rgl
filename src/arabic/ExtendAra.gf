--# -path=.:../common:../abstract

concrete ExtendAra of Extend =
  CatAra ** ExtendFunctor - [
    GenNP, ApposNP, ICompAP, DetNPMasc, DetNPFem,
    EmptyRelSlash, PredAPVP,
    ComplDirectVS, ComplDirectVQ, -- because of Utt
    VPS, MkVPS, PredVPS, BaseVPS, ConsVPS, ConjVPS,
    EmbedSSlash, AdjAsNP, GerundNP
]
  with (Grammar=GrammarAra)
  ** open

    ResAra,
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

}
