concrete RelativeKaz of Relative = CatKaz ** open ResKaz, (P = ParamX) in {
  lin
    IdRP = {s=""} ;
    RelVP rp vp = {s=rp.s ++ vp.infinitive} ;
    RelSlash rp cl = {s=rp.s ++ cl.pres ! P.Pos} ;
}
