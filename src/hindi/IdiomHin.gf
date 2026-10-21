concrete IdiomHin of Idiom = CatHin ** open Prelude,Predef, ResHin,ParamX,CommonHindustani in {

  flags optimize=all_subs ;
  flags coding = utf8;

  lin
    ImpersCl vp = mkSClause " " (agrP3 Masc Sg) vp ;
    GenericCl vp = mkSClause "कोई" (agrP3 Masc Sg) vp ;

   CleftNP np rs = 
	 let cl = mkSClause (np.s ! NPC rs.c) (np.a) (predAux auxBe);
	  in 
	   {s = \\t,p,o =>  cl.s ! t ! p ! o ++ rs.s ! np.a };
	  
    CleftAdv ad ss = { s = \\t,b,o => ad.s ! Masc ++ ss.s};
        
     ExistNP np = 
      mkSClause "" (agrP3 (fromAgr np.a).g (fromAgr np.a).n)  --removed waha:n
        (insertObj (\\_ => np.s ! NPC Dir) (predAux auxBe)) ;

   ExistIP ip =
     let cl = mkSClause ("वहाँ" ++ ip.s ! Dir) (agrP3 ip.g ip.n) (predAux auxBe); 
	   in {
       s = \\t,p,qf => case qf of { 
	      QDir =>   cl.s ! t ! p ! ODir;
          QIndir => cl.s ! t! p ! ODir
		  }
		};

   ExistNPAdv np adv =
     mkSClause "" np.a
       (insertAdV (adv.s ! giveGender np.a)
       (insertObj (\\_ => np.s ! NPC Dir) (predAux auxBe))) ;

   ExistIPAdv ip adv =
     let cl = mkSClause (ip.s ! Dir) (agrP3 ip.g ip.n)
                (insertAdV (adv.s ! ip.g) (predAux auxBe))
     in {s = \\t,p,_ => cl.s ! t ! p ! ODir} ;

   SelfAdvVP vp = insertAdV "स्वयं" vp ;
   SelfAdVVP vp = insertAdV "स्वयं" vp ;
   SelfNP np = {s = \\c => np.s ! c ++ "स्वयं" ; a = np.a} ;

--    ProgrVP vp = insertObj (\\a => vp.obj.s ++ vp.ad ++ vp.comp ! a ++ (vp.s ! VPStem).inf ++ raha (fromAgr a).g (fromAgr a).n ) (predAux auxBe) ;
      ProgrVP vp =  (predProg vp) ;


--    ImpPl1 vp = {s = "आओ" ++ infVP True vp (agrP1 Masc Pl)} ;
      ImpPl1 vp = {s = "आओ" ++ vp.obj.s ++ (vp.s !  VPTense VPFutr (agrP1 Masc Pl)).inf ++ vp.comp ! (agrP1 Masc Pl)} ;
	ImpP3 np vp = {s = np.s!NPC Dir ++ "को" ++ (vp.s ! VPImp ).inf ++ "दो"};


}

