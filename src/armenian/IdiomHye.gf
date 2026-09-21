concrete IdiomHye of Idiom = CatHye ** open ResHye in {
  oper
    bareCl : Str -> Cl = \s -> lin Cl {
      s=s; negative=table {_ => "չ" ++ s}; conditional=\\_,_ => s;
      converb={imperfective=s;futCon1=s;futCon2=s;negative=s;perfective=s;simultaneous=s};
      passive=s;past=\\_,_ => s;participle=\\_ => s;subjunctive=\\_,_ => s
    } ;
  lin
    ImpersCl vp = bareCl vp.s ;
    GenericCl vp = bareCl ("մարդ" ++ vp.s) ;
    CleftNP np rs = bareCl (np.s ! Nom ++ rs.s) ;
    CleftAdv adv s = bareCl (adv.s ++ s.s) ;
    ExistNP np = bareCl (np.s ! Nom ++ "կա") ;
    ExistIP ip = {s=ip.s ++ "կա՞"} ;
    ExistNPAdv np adv = bareCl (adv.s ++ np.s ! Nom ++ "կա") ;
    ExistIPAdv ip adv = {s=adv.s ++ ip.s ++ "կա՞"} ;
    ProgrVP vp = vp ;
    ImpPl1 vp = {s="եկեք" ++ vp.s} ;
    ImpP3 np vp = {s="թող" ++ np.s ! Nom ++ vp.s} ;
    SelfAdvVP vp = vp ; SelfAdVVP vp = vp ; SelfNP np = np ;
}
