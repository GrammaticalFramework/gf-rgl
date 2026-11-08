concrete IdiomIce of Idiom = CatIce ** open Prelude, ResIce in {

	lin
		ImpersCl vp = mkClause "það" vp {g = Neutr ; n = Sg ; p = P3} ;

		GenericCl vp = mkClause "maður" vp {g = Masc ; n = Sg ; p = P3} ;

		CleftNP np rs = let vp = (predV verbBe) in
			mkClause "það" (vp ** {dirObj = \\_ => np.s ! rs.c ++ rs.s ! np.a}) np.a ;

		CleftAdv ad s = let vp = (predV verbBe) in
			mkClause "það" (vp ** {dirObj = \\_ => ad.s ++ "sem" ++ s.s}) {g = Neutr ; n = Sg ; p = P3} ;

		ExistNP np = let vp = (predV verbBe) in
			mkClause "til" (vp ** {dirObj = \\_ => np.s ! NCase Nom}) np.a ;

		ExistIP ip = let 
				vp = (predV verbBe) ;
				cl = mkClause (ip.s ! Masc ! Nom) vp {g = Masc ; n = ip.n ; p = P3}
			in {s = \\ten,ant,pol,_ => cl.s ! ten ! ant ! pol ! ODir ++ "þarna"} ;

		ExistNPAdv np adv = let vp = (predV verbBe) in
			mkClause "til" (vp ** {dirObj = \\_ => np.s ! NCase Nom ++ adv.s}) np.a ;

		ExistIPAdv ip adv = let
				vp = (predV verbBe) ;
				cl = mkClause (ip.s ! Masc ! Nom) vp {g = Masc ; n = ip.n ; p = P3}
			in {s = \\ten,ant,pol,_ => cl.s ! ten ! ant ! pol ! ODir ++ adv.s} ;

		ProgrVP vp = let vvp = predV verbBe in vvp ** {
			dirObj = \\a => "að" ++ infVPPlus vp VPInf Pos a
		} ;

		ImpPl1 vp = {s = infVPPlus vp (VPMood Pres Simul) Pos (gennumperToAgr Masc Pl P1)} ;

		ImpP3 np vp = {s = verbLet.s ! VPres Active Indicative np.a.n np.a.p ++
			np.s ! NCase Acc ++ infVPPlus vp VPInf Pos np.a} ;

		SelfAdvVP vp = vp ** {dirObj = \\a => vp.dirObj ! a ++ reflPron a.p a.n a.g Nom} ;

		SelfAdVVP vp = vp ** {dirObj = \\a => reflPron a.p a.n a.g Nom ++ vp.dirObj ! a} ;

		SelfNP np = {
			s = \\c => reflPron np.a.p np.a.n np.a.g Nom ++ np.s ! c ;
			a = np.a ;
			isPron = False
		} ;
}

