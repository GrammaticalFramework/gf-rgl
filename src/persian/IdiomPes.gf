concrete IdiomPes of Idiom = CatPes ** open Prelude,Predef, ResPes in {

  flags optimize=all_subs ;
  flags coding = utf8;

lin

  ImpersCl vp = mkSClause " " (agrP3 Sg) vp ;
  GenericCl vp = mkSClause "آدم" (agrP3 Sg) vp ;

  CleftNP np rs =
	 let cl = mkSClause (np.s ! Bare) (np.a) (predAux auxBe);
	  in
	   {s = \\t,p,o =>  cl.s ! t ! p ! o ++ rs.s ! np.a };

  CleftAdv ad ss = { s = \\t,b,o => ad.s ++ ss.s};

  ExistNP np =
    mkSClause " " (agrP3 (fromAgr np.a).n)
        (insertObj (\\_ => np.s ! Bare) (predAux auxBe)) ;

  ExistIP ip =
    let cl = mkSClause ( ip.s ) (agrP3 ip.n) (predAux auxBe);
	  in {
       s = \\t,p,qf => case qf of {
	      QDir =>   cl.s ! t ! p ! ODir;
          QIndir => cl.s ! t! p ! ODir
		  }
		};


  ProgrVP vp = predProg vp ;

  ImpPl1 vp = {s = "بیایید" ++ vp.s ! VVForm (agrP1 Pl)} ;
	ImpP3 np vp = {s = "بگذارید" ++ np.s!Bare ++ vp.s ! VVForm np.a};


}
