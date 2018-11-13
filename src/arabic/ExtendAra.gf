--# -path=.:../common:../abstract

concrete ExtendAra of Extend =
  CatAra ** ExtendFunctor - [
    GenNP, SlashBareV2S, PredAPVP, GenModNP, ExistsNP,
    StrandRelSlash, ExistPluralCN, ExistMassCN, ExistCN, EmptyRelSlash, DetNPMasc, DetNPFem,
    ComplBareVS, ComplDirectVS, ComplDirectVQ,
    ICompAP,
    VPS, MkVPS
]
  with (Grammar=GrammarAra)
  ** open

    Prelude,
    ResAra,
    ParamX

  in {

  lin
    GenNP np = {s = \\_,_,_,_ => np.s ! Gen ; d = Const ; isNum,isPron,is1sg = False} ;

    -- : AP -> IComp ;   -- "how old"
    ICompAP ap = {s = \\gn => "كَمْ" ++ ap.s ! NoHum ! gn.g ! gn.n ! Indef ! Acc} ;

  lincat

    VPS = VP ;  -- finite VP's with tense and polarity

  lin
    -- : Temp -> Pol -> VP -> VPS ; -- hasn't slept
    MkVPS t p vp = lin VPS (vp ** {
      s = \\pgn,vf => case <t.t,t.a> of { --- IL guessed tenses
             <(Pres|Fut),Simul> => vp.s ! pgn ! VPImpf Ind ;
             <Cond,_    > => vp.s ! pgn ! VPImpf Cnj ;
             <_   ,_    > => vp.s ! pgn ! VPPerf
           }
      }) ;
}
