--# -path=.:../common:../abstract

concrete ExtendAra of Extend =
  CatAra ** ExtendFunctor - [
    GenNP, SlashBareV2S, PredAPVP, GenModNP, ExistsNP,
    StrandRelSlash, ExistPluralCN, ExistMassCN, ExistCN, EmptyRelSlash, DetNPMasc, DetNPFem,
    ComplBareVS, ComplDirectVS, ComplDirectVQ,
    ICompAP,
    VPS, MkVPS, PredVPS, BaseVPS, ConsVPS, ConjVPS,
    ApposNP
]
  with (Grammar=GrammarAra)
  ** open

    ParamX,
    ResAra,
    Prelude,
    RelativeAra,
    Coordination

  in {

  lin
    GenNP np = baseQuant ** {s = \\_,_,_,_ => np.s ! Gen ; d = Const} ;

    -- : NP -> NP -> NP
    ApposNP np1 np2 = np2 ** {s = \\c => np1.s ! c ++ np2.s ! c} ;

    -- : AP -> IComp ;   -- "how old"
    ICompAP ap = {s = \\gn => "كَمْ" ++ ap.s ! NoHum ! gn.g ! gn.n ! Indef ! Acc} ;

    -- : ClSlash -> RCl  -- he lives in
    EmptyRelSlash = RelSlash (IdRP ** {s = \\_ => []}) ;

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
