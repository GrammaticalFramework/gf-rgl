concrete ConjunctionLat of Conjunction = 
  CatLat ** open ResLat, StructuralLat, Coordination, Prelude, ParadigmsLat in {
--
--  flags optimize=all_subs ;
    --

  lin
    -- ConjS    : Conj -> ListS -> S ;       -- he walks and she runs
    -- TO FIX
    -- ConjS conj ss = { s = \\_ => conjunctDistrX conj (ss.l ! conj.c) ; sadv = lin Adv { s = []} ; neg = ss.neg } ;
    ConjS conj ss = {
--      s = \\apos => coord conj.c { init = (ss.s ! conj.c).init ! SPreS ! apos ! CPreV ! SOV ;
      --	last = (ss.s ! conj.c).last ! SPreS ! apos ! CPreV ! SOV} ;
      s = \\apos => conj.s1 ++ (ss.s ! conj.c).init ! SAPreS ! apos ! DPreN ! VReg ! CPreV ! SOV  ++ conj.s2 ++
	(ss.s ! conj.c).last ! SAPreS ! apos ! DPreN ! VReg ! CPreV ! SOV ++ conj.s3 ;
      o = \\_ => [] ;
      v = \\_ => [] ;
      neg = \\_ => [] ;
      compl = [] ;
      p = ss.p ;
      sadv = [] ;
      t = ss.t ;
      det = { s, sp = \\_ => [] } ;
      } ;

    -- ConjAdv  : Conj -> ListAdv -> Adv ;   -- here or there
--    ConjAdv conj ss = mkAdv (conjunctDistrSS conj (ss.l ! conj.c) ).s ;

    -- ConjNP   : Conj -> ListNP -> NP ;     -- she or we
    ConjNP conj nps =
      {
    	-- s = case conj.c of {
    	--   Et => case nps.isBase of {
        --     False => (conjunctDistrTable Case conj (nps.l ! Et)).s ;
        --     True => \\c => conj.s1 ++ (nps.l ! Et).s1 ! c ++ conj.s2 ++ (nps.l ! Et).s2 ! c
    	--     } ;
    	--   c => (conjunctDistrTable Case conj (nps.l ! Et)).s
    	--   } ;
	s = \\pd,ca =>  conj.s1 ++ (nps.s ! conj.c).init ! pd ! APreN ! DPreN ! ca ++ conj.s2 ++ (nps.s ! conj.c).last ! pd ! APreN ! DPreN ! ca ++ conj.s3;
    	n = case conj.c of { Et => Pl ; _ => nps.n } ;
      	g = nps.g ;
      	p = nps.p ;
      	adv = "" ;
      	preap , postap = { s = \\_ => "" };
    	det = { s , sp = \\_ => ""} ;
      } ;

    -- ConjAP   : Conj -> ListAP -> AP ;
--    ConjAP conj ss = conjunctDistrTable Agr conj (ss.l ! conj.c) ;

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

    -- ConjRS : Conj -> ListRS -> RS
    ConjRS conj rss = { s = \\g,n => conj.s1 ++ (rss.s ! conj.c).init ! g ! n ++ conj.s2 ++ (rss.s ! conj.c).last ! g ! n++ conj.s3 };

  ---- These fun's are generated from the list cat's.
    --

    -- BaseS : S -> S -> ListS
   BaseS x y = {
     s = \\c => { init = combineSentence x ; last = combineSentence y } ;
     p = y.p ;
     t = y.t 
     } ;
    
    -- ConsS : S -> ListS -> ListS
    -- TO FIX
    -- ConsS x xs = { l = \\_ => consrSS bindComma (ss (x.s ! PreS)) (xs.l ! Comma) };
    ConsS s ss = {
      s = \\co =>
    	{ init = \\s,a,d,v,c,o => coord co { init = (ss.s ! co).init ! s ! a ! d ! v ! c ! o ; last = (ss.s ! co).last ! s ! a ! d ! v ! c ! o } ;
    	  last = combineSentence s } ;
      p = s.p ;
      t = s.t 
      } ;
    
    -- BaseAdv : Adv -> Adv -> ListAdv
   BaseAdv x y =
     {
       s = \\_ => { init = x.s ! Posit ; last = y.s ! Posit }
     } ;

    -- ConsAdv : Adv -> ListAdv -> ListAdv
   ConsAdv x xs =
     {
       --       s = \\_ => consrSS bindComma (ss (x.s ! Posit)) (xs.l ! Comma)
       s = \\c => { init = coord c (xs.s ! c) ; last = x.s ! Posit }
     } ;

    -- -- BaseNP : NP -> NP -> ListNP ;      -- John, Mary
    BaseNP x y = {
      --      s = \\c => twoTable Case x y ;
      s = \\c => { init = combineNounPhrase x ; last = combineNounPhrase y } ;
      g = Neutr ; -- Trying to avoid trouble by choosing a gender
      n = matchNumber x.n y.n ;
      p = P3 ;
      isBase = True ;
      } ; 

    -- -- ConsNP : NP -> ListNP -> ListNP ;  -- John, Mary, Bill
    ConsNP x xs = {
      --      s = \\_ => consrTable Case bindComma x ( xs.s ! Comma );
      s = \\co => { init = \\pd,ap,dp,ca => coord co { init = (xs.s ! co).init ! pd ! ap ! dp ! ca ; last = (xs.s ! co).last ! pd ! ap ! dp ! ca} ; last = combineNounPhrase x } ;
      n = matchNumber x.n xs.n ;
      g = xs.g ;
      p = xs.p ;
      isBase = False ;
      } ;
    
    -- -- BaseAP : AP -> AP -> ListAP
    -- BaseAP x y = { l = \\c => twoTable Agr x y };

    -- -- ConsAP : AP -> ListAP -> ListAP
    -- ConsAP x xs =
    --   { l = \\_ => consrTable Agr and_Conj.s2 x (xs.l ! Comma ) } ;

    -- BaseRS : RS -> RS -> ListRS ;
    BaseRS rs1 rs2 = { s = \\co => { init = rs1.s ; last = rs2.s }} ;
    
    -- ConsRS : RS -> List RS -> ListRS ;
    ConsRS rs rss = { s = \\co => { init = rs.s ; last = \\g,n =>  coord co { init = (rss.s ! co).init ! g ! n ; last = (rss.s ! co).last ! g ! n } } } ;
    

--
  lincat
    [S] = { s : Coordinator => {init,last : SAdvPos => AdvPos => DetPos => VPos => ComplPos => Order => Str} ; p : Pol ; t : Tense } ; -- TO FIX
    [Adv] = { s: Coordinator => {init,last : Str}} ;
    [NP] = { s : Coordinator => {init,last : PronDropForm => AdvPos => DetPos => Case => Str} ; g : Gender ; n : Number ; p : Person ; isBase : Bool } ;
    [AP] = {s : Coordinator => {init,last : Agr => Str } } ;
    [RS] = { s : Coordinator => { init, last : Gender => Number => Str }} ;
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
    
    coord : Coordinator -> {init : Str ; last : Str} -> Str =
      \c,l ->
      l.init ++
      table {
	Aut => "aut" ;
	Et => "et" ;
	Sed => "sed" ;
	Si => "si" ;
	Vel => "vel" ;
	Comma => bindComma ;
	Colon => ":" ;
	Empty => "" ;
	Missing => nonExist
      } ! c
      ++ l.last ;
--
}
