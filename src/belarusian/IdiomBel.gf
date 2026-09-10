concrete IdiomBel of Idiom = CatBel ** open ResBel, (R = ParamX), ParadigmsBel in {

lin
  ImpersCl vp = {s = \\t,p => vp.s ! t ! p ! defaultAgr} ;
  GenericCl vp = {s = \\t,p => "нехта" ++ vp.s ! t ! p ! defaultAgr} ;
  CleftNP np rs = {s = \\_,_ => "гэта" ++ np.s ! Nom ++ rs.s} ;
  CleftAdv adv s = {s = \\_,_ => "гэта" ++ adv.s ++ s.s} ;

  ExistNP np = {s = \\t,p => copula t p np.a ++ np.s ! Nom} ;
  ExistIP ip = {s = \\t,p => copula t p ip.a ++ ip.s ! Nom} ;
  ExistNPAdv np adv = {s = \\t,p => copula t p np.a ++ np.s ! Nom ++ adv.s} ;
  ExistIPAdv ip adv = {s = \\t,p => copula t p ip.a ++ ip.s ! Nom ++ adv.s} ;

  ProgrVP vp = vp ;
  ImpPl1 vp = {s = "давайце" ++ vp.inf} ;
  ImpP3 np vp = {s = "няхай" ++ np.s ! Nom ++ vp.s ! R.Pres ! R.Pos ! np.a} ;

  SelfAdvVP vp = addAdvVP vp "сам" ;
  SelfAdVVP vp = addAdVVP "сам" vp ;
  SelfNP np = {s = \\c => np.s ! c ++ "сам"; a = np.a} ;

}
