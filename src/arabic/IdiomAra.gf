concrete IdiomAra of Idiom = CatAra ** open 
  Prelude,
  ResAra,
  ParadigmsAra
 in {


 lin

  -- : VP -> Cl ;        -- it is hot
  ImpersCl vp = 
    let it : ResAra.NP = pron2np (pgn2pron vp.obj.a.pgn) ; -- if no obj, Per3 Masc Sg chosen by default
     in predVP it vp ;

  --  : VP -> Cl ;        -- one sleeps
  GenericCl = predVP (regNP "المَرْء" Sg) ;    

  -- : NP  -> RS -> Cl ; -- it is I who did it
  --CleftNP np rs = 

  -- : Adv -> S -> Cl ; -- it is here she slept
  CleftAdv adv s = 
  	let comp : Comp = {s = \\_,_ => adv.s ++ s.s} in ----
  	predVP he_Pron (kaan comp) ;

   -- : NP -> Cl ;        -- there is a house
  ExistNP np =
    predVP emptyNP (insertObj np (predV copula ** {c2=noPrep})) ; -- dummy /IL

  -- ExistIP   : IP -> QCl ;       -- which houses are there

-- 7/12/2012 generalizations of these

   -- : NP -> Adv -> Cl ;    -- there is a house in Paris
  ExistNPAdv np adv = 
   	predVP emptyNP (insertStr adv.s (insertObj np (predV copula ** {c2=noPrep}))) ;

   -- ExistIPAdv : IP -> Adv -> QCl ;   -- which houses are there in Paris

    -- ProgrVP   : VP -> VP ;        -- be sleeping

    -- ImpPl1    : VP -> Utt ;       -- let's go

    -- ImpP3     : NP -> VP -> Utt ; -- let John walk

-- 3/12/2013 non-reflexive uses of "self"

  -- : VP -> VP ;        -- is at home himself; is himself at home
  SelfAdvVP, 
  SelfAdVVP = \vp -> vp ** {
    s = \\pgn,vf => let pron : ResAra.NP = pgn2pron pgn in
        vp.s ! pgn ! vf ++ refl ! Nom ++ pron.s ! Gen 
    } ;

  -- : NP -> NP ;        -- the president himself (is at home)
  SelfNP np = np ** {
  	s = let pron : ResAra.NP = np2pron np ;
  		 in \\c => np.s ! c ++ refl ! c ++ pron.s ! Gen
    } ;


}

