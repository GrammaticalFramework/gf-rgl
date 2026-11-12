concrete ConjunctionGer of Conjunction = 
  CatGer ** open ResGer, Coordination, Prelude in {

  flags optimize=all_subs ;

  lin

    ConjS conj ss = conjunctDistrTable Order conj ss ;

    ConjAdv conj ss = conjunctDistrSS conj ss ;

    ConjNP conj ss = heavyNP (
    {s = \\_ => (conjunctDistrTable Case conj ss).s ;
     a = let n : Number = (conjNumber conj.n (numberAgr ss.a)) ;
             p : Person = personAgr ss.a ;
             agr : Agr = case <n,p> of {<Pl,q> => AgPl q ;
                                        <Sg,P3> => AgSgP3 Neutr ;
                                        <Sg,P1> => AgSgP1 ;
                                        <Sg,P2> => AgSgP2 }
         in (conjAgr agr ss.a) }) ;

    ConjAP conj ss = conjunctDistrTable AForm conj ss ** {
      s2 = \\c => [] ; -- comparison np of ap = {s:AForm => Str; s2:Case => Str} HL 1/23
      isPre = ss.isPre ; c = ss.c ; ext = ss.ext} ;

    ConjRS conj ss = conjunctDistrTable RelGenNum conj ss ** {
      c = ss.c
      } ;

    ConjCN conj ss = conjunctDistrTable3 Adjf Number Case conj ss ** {
      rc = \\_ => [] ; ext = [] ; adv = [] ; g = ss.g
      } ;

    ConjIAdv conj ss = conjunctDistrSS conj ss ;

    ConjAdV conj ss = conjunctDistrSS conj ss ;

    ConjDet conj daps = {
      s =  \\_,g,c => conj.s1 ++ daps.s1 ! g ! c ++ conj.s2 ++ daps.s2 ! g ! c ;
      sp = \\_,g,c => conj.s1 ++ daps.sp1 ! g ! c ++ conj.s2 ++ daps.sp2 ! g ! c ;
      n = daps.n ; a = daps.a ;
      isDef = True ; hasDefArt = False} ;

-- These fun's are generated from the list cat's.

    BaseS x y = { -- twoTable Order ;
      s1 = x.s ;
      s2 = table {Inv => y.s ! Main ; o => y.s ! o}
      } ;
    ConsS x xs = { -- consrTable Order comma ;
      s1 = \\o => x.s ! Inv ++ comma ++ xs.s1 ! case o of {Inv => Main ; _ => o} ;
      s2 = xs.s2
      } ;

    BaseAdv = twoSS ;
    ConsAdv = consrSS comma ;
    BaseNP x y = {
		s1 = \\c => x.s ! False ! c ++ bigNP x ;
		s2 = \\c => y.s ! False ! c ++ bigNP y ;
		a = conjAgr x.a y.a } ;
    ConsNP xs x = {
		s1 = \\c => xs.s ! False ! c ++ bigNP xs ++ comma ++ x.s1 ! c ;
		s2 = x.s2 ;
		a = conjAgr xs.a x.a } ;
    BaseAP x y = lin AP {
		s1 = bigAP x ;
		s2 = bigAP y ;
		isPre = andB x.isPre y.isPre ;
		c = <[],[]> ;
	  	ext = []} ;
   ConsAP xs x = lin AP {
		s1 = \\a => (bigAP xs) ! a ++ comma ++ x.s1 ! a ;
		s2 = x.s2 ;
		isPre = andB x.isPre xs.isPre ;
		c = <[],[]> ;
	  	ext = []} ;
    BaseRS x y = twoTable RelGenNum x y ** {c = y.c} ;
    ConsRS xs x = consrTable RelGenNum comma xs x ** {c = xs.c} ;
    BaseCN x y = lin CN {
      s1 = bigCN x ;
      s2 = bigCN y ;
      g  = x.g ; --- gender of first CN, used e.g. in articles
      } ; 
    ConsCN x xs = lin CN {
      s1 = \\a,n,c => bigCN x ! a ! n ! c ++ comma ++ xs.s1 ! a ! n ! c ;
      s2 = xs.s2 ;
      g  = x.g ; --- gender of first CN, used e.g. in articles
      } ; 

    BaseDAP x y = {
      s1 = x.s ; sp1 = x.sp ;
      s2 = y.s ; sp2 = y.sp ;
      n = y.n ;
      a = y.a } ;
    ConsDAP x xs = {
      s1 = \\g,c => x.s ! g ! c ++ comma ++ xs.s1 ! g ! c ;
      sp1 = \\g,c => x.sp ! g ! c ++ comma ++ xs.sp1 ! g ! c ;
      s2 = xs.s2 ; sp2 = xs.sp2 ;
      n = xs.n ; a = xs.a} ;

    BaseAdV = twoSS ;
    ConsAdV = consrSS comma ;
    BaseIAdv = twoSS ;
    ConsIAdv = consrSS comma ;

  lincat
    [S] = {s1,s2 : Order => Str} ;
    [Adv] = {s1,s2 : Str} ;
    [NP] = {s1,s2 : Case => Str ; a : Agr} ;
    [AP] = {s1,s2 : AForm => Str ; isPre : Bool; c : Str * Str ; ext : Str} ;
    [RS] = {s1,s2 : RelGenNum => Str ; c : Case} ;
    [CN] = {s1,s2 : Adjf => Number => Case => Str ; g : Gender} ;
    [DAP] = {s1,s2,sp1,sp2 : Gender => Case => Str ; n : Number ; a : Adjf} ;
    [AdV] = {s1,s2 : Str} ;
    [IAdv] = {s1,s2 : Str} ;

  oper
    bigAP : AP -> AForm => Str = \ap ->                                         -- HL 1/23: not always ok:
		\\a => ap.c.p1 ++ ap.s ! a ++ ap.c.p2 ++ ap.ext ++ ap.s2 ! Nom ; -- comparison np in Nom
    bigCN : CN -> Adjf => Number => Case => Str = \cn ->
		\\a,n,c => cn.s ! a ! n ! c ++ cn.adv ++ cn.ext ++ cn.rc ! n ;
		
}
