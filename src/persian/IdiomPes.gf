concrete IdiomPes of Idiom = CatPes ** open Prelude,Predef, ResPes in {

  flags optimize=all_subs ;
  flags coding = utf8;

lin

  ImpersCl vp = mkSClause " " (agrP3 Sg) vp ;
  GenericCl vp = mkSClause "آدم" (agrP3 Sg) vp ;

  CleftNP np rs =
	 let cl = mkSClause (np.s ! Bare) (np.a) (predV beVerb);
	  in
	   {s = \\t,p,o =>  cl.s ! t ! p ! o ++ rs.s ! np.a };

  CleftAdv ad ss = { s = \\t,b,o => ad.s ++ ss.s ! Indic};

  ExistNP np =
    mkSClause [] (agrP3 (fromAgr np.a).n)
              (insertComp (\\_ => np.s ! Bare) (predV beVerb)) ;

  ExistIP ip =
    let cl = mkSClause ip.s (agrP3 ip.n) (predV beVerb);
    in {s = \\t,p => cl.s ! t ! p ! ODir};


  ProgrVP vp = predProg vp ;

  ImpPl1 vp = let a = agrP1 Pl in
    {s = "بیایید" ++ showVPH (VSubj Pos a) a vp } ;
	ImpP3 np vp =
    {s = "بگذارید" ++ np.s ! Bare ++ showVPH (VSubj Pos np.a) np.a vp};


}
