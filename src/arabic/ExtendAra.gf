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
    Coordination

  in {

  lin
    GenNP np = baseQuant ** {s = \\_,_,_,_ => np.s ! Gen ; d = Const} ;

    -- : NP -> NP -> NP
    ApposNP np1 np2 = np2 ** {s = \\c => np1.s ! c ++ np2.s ! c} ;

    -- : AP -> IComp ;   -- "how old"
    ICompAP ap = {s = \\gn => "كَمْ" ++ ap.s ! NoHum ! gn.g ! gn.n ! Indef ! Acc} ;

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
          (case <vp.isPred,vp.obj.a.isPron> of {
                    <False,True> => BIND ++ vp.obj.s ;
                    _            =>         vp.obj.s })
          (pred vp pgn t.t p.p) 
          vp.s2 
      in vps.before ++ vps.after -- word order is SVO, so this is safe for just this case.
      } ;

    BaseVPS = twoTable PerGenNum ;
    ConsVPS = consrTable PerGenNum comma ;
    ConjVPS = conjunctTable PerGenNum ;

    PredVPS np vps = {
      s = \\_ => np.s ! Nom ++ vps.s ! np.a.pgn -- first quick version with order always Nominal.
      } ;                                       -- if necessary, change VPS into {s : PerGenNum => Order => {before,after : Str}}

}
