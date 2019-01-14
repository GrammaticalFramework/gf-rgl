concrete ConjunctionLat of Conjunction = 
  CatLat ** open ResLat, StructuralLat, Coordination, Prelude, ParadigmsLat in {
--
--  flags optimize=all_subs ;
    --

  lin
    -- ConjS    : Conj -> ListS -> S ;       -- he walks and she runs
    -- TO FIX
    -- ConjS conj ss = { s = \\_ => conjunctDistrX conj (ss.l ! conj.c) ; sadv = lin Adv { s = []} ; neg = ss.neg } ;

    -- ConjAdv  : Conj -> ListAdv -> Adv ;   -- here or there
    ConjAdv conj ss = mkAdv (conjunctDistrSS conj (ss.l ! conj.c) ).s ;

    -- ConjNP   : Conj -> ListNP -> NP ;     -- she or we
    ConjNP conj nps =
      {
	s = case conj.c of {
	  And => case nps.isBase of {
            False => (conjunctDistrTable Case conj (nps.l ! And)).s ;
            True => \\c => conj.s1 ++ (nps.l ! And).s1 ! c ++ conj.s2 ++ (nps.l ! And).s2 ! c
	    } ;
	  c => (conjunctDistrTable Case conj (nps.l ! And)).s
	  } ;
	n = case conj.c of { And => Pl ; _ => nps.n } ;
      	g = nps.g ;
      	p = nps.p ;
      	adv = nps.adv ;
      	preap = nps.preap ;
	postap = nps.postap ;
	det = { s = \\_,_ => "" ; sp = \\_,_ => "" ; n = nps.n };
      } ;

    -- ConjAP   : Conj -> ListAP -> AP ;
    ConjAP conj ss = conjunctDistrTable Agr conj (ss.l ! conj.c) ;

    --
--    DConjS = conjunctDistrSS ;
--    DConjAdv = conjunctDistrSS ;
--
--    ConjNP conj ss = conjunctTable Case conj ss ** {
--      a = conjAgr (agrP3 conj.n) ss.a 
--      } ;
--    DConjNP conj ss = conjunctDistrTable Case conj ss ** {
--      a = conjAgr (agrP3 conj.n) ss.a
--      } ;
--    DConjAP conj ss = conjunctDistrTable Agr conj ss ** {
--      isPre = ss.isPre
--      } ;
---}
--
---- These fun's are generated from the list cat's.
    --

    -- BaseS : S -> S -> ListS
    -- TO FIX
    -- BaseS x y = { l = \\c => twoStr (x.s ! PreS) (y.s ! PreS) } ;

    -- ConsS : S -> ListS -> ListS
    -- TO FIX
    -- ConsS x xs = { l = \\_ => consrSS bindComma (ss (x.s ! PreS)) (xs.l ! Comma) };

    -- BaseAdv : Adv -> Adv -> ListAdv
    BaseAdv x y = { l = \\c => twoSS (ss (x.s ! Posit)) (ss (y.s ! Posit)) } ;

    -- ConsAdv : Adv -> ListAdv -> ListAdv
    ConsAdv x xs = { l = \\_ => consrSS bindComma (ss (x.s ! Posit)) (xs.l ! Comma) } ;

    -- BaseNP : NP -> NP -> ListNP ;      -- John, Mary
    BaseNP x y = {
      l = \\c => twoTable Case x y ;
      g = Masc ; -- Just guessing (but maybe sexist bullshit)
      n = matchNumber x.n y.n ;
      p = P3 ;
      adv = x.adv ++ y.adv ;
      preap = lin AP { s = \\a => x.preap.s ! a ++ y.preap.s ! a } ;
      postap = lin AP { s = \\a => x.postap.s ! a ++ y.postap.s ! a } ;
      isBase = True ;
      det = { s = \\g,c => x.det.s ! g ! c ++ y.det.s ! g ! c ; sp = \\g,c => x.det.sp ! g ! c ++ y.det.sp ! g ! c ; n = matchNumber x.get.n y.get.n } ;
	
      } ; 

    -- ConsNP : NP -> ListNP -> ListNP ;  -- John, Mary, Bill
    ConsNP x xs = {
      l = \\_ => consrTable Case bindComma x ( xs.l ! Comma );
      n = matchNumber x.n xs.n ;
      g = xs.g ;
      p = xs.p ;
      adv = x.adv ++ xs.adv ;
      preap = lin AP { s = \\a => x.preap.s ! a ++ xs.preap.s ! a } ;
      postap = lin AP { s = \\a => x.postap.s ! a ++ xs.postap.s ! a } ;
      isBase = False
	-- TODO det
      } ;
    
    -- BaseAP : AP -> AP -> ListAP
    BaseAP x y = { l = \\c => twoTable Agr x y };

    -- ConsAP : AP -> ListAP -> ListAP
    ConsAP x xs =
      { l = \\_ => consrTable Agr and_Conj.s2 x (xs.l ! Comma ) } ;
--
  lincat
    -- [S] = { l : Coordinator => {s1,s2 : Str} } ; -- TO FIX
    [Adv] = { l: Coordinator => {s1,s2 : Str}} ;
    [NP] = {l : Coordinator => {s1,s2 : Case => Str} ; g : Gender ; n : Number ; p : Person ; adv : Str ; preap : AP ; postap : AP ; isBase : Bool } ;
    [AP] = {l : Coordinator => {s1,s2 : Agr => Str } } ;

  oper
    -- Generates a new number value given two number values.
    --   Pl if any of the two is Pl
    --   Sg otherwise
    matchNumber : Number -> Number -> Number = \n1,n2 ->
      case <n1,n2> of {
	<Pl,_> => Pl ;
	<_,Pl> => Pl ;
	<_,_> => Sg
      } ;
--
}
