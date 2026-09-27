concrete IdiomKaz of Idiom = CatKaz ** open ResKaz, ParadigmsKaz in {
  lin
    ProgrVP vp = vp ;
    ExistNP np = mkClause np (mkV "болу") ;
    ExistNPAdv np adv = mkClause np (prefixVerb adv.s (mkV "болу")) ;
    ExistIP ip = {pres=\\_ => ip.s ++ "бар";past=\\_ => ip.s ++ "болды";
      fut=\\_ => ip.s ++ "болады";cond=\\_ => ip.s ++ "болар еді";anter=\\_ => ip.s ++ "болған"} ;
    ImpersCl vp = mkClause {s=\\_ => [];a=defaultAgr} vp ;
    GenericCl vp = mkClause {s=\\_ => "адам";a=defaultAgr} vp ;
    CleftNP np rs = stringClause (np.s ! Nom ++ rs.s) ;
    CleftAdv adv s = stringClause (adv.s ++ s.s) ;
    ImpPl1 vp = {s=vp.imperative!Pos!Informal!Pl} ;
}
