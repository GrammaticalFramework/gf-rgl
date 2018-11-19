concrete IdiomAra of Idiom = CatAra ** open
  Prelude,
  ResAra,
  VerbAra,
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
    let comp : Comp = CompAdv adv in
    predVP he_Pron (UseComp comp) ;

   -- : NP -> Cl ;        -- there is a house
  ExistNP np =
    predVP (emptyNP ** {s=\\c=>"هُنَاكَ"}) (UseComp (CompNP np)) ; -- IL

  -- ExistIP   : IP -> QCl ;       -- which houses are there

-- 7/12/2012 generalizations of these

  -- : NP -> Adv -> Cl ;    -- there is a house in Paris
  ExistNPAdv np adv =
    predVP (emptyNP ** {s=\\c=>"هُنَاكَ"}) (AdvVP (UseComp (CompNP np)) adv) ; -- IL

   -- ExistIPAdv : IP -> Adv -> QCl ;   -- which houses are there in Paris

    -- ProgrVP   : VP -> VP ;        -- be sleeping

    -- ImpPl1    : VP -> Utt ;       -- let's go

    -- ImpP3     : NP -> VP -> Utt ; -- let John walk

-- 3/12/2013 non-reflexive uses of "self"

  -- : VP -> VP ;        -- is at home himself; is himself at home
  SelfAdvVP,
  SelfAdVVP = \vp -> vp ** {
    s = \\pgn,vf => vp.s ! pgn ! vf ++ reflPron Nom pgn
    } ;

  -- : NP -> NP ;        -- the president himself (is at home)
  SelfNP np = np ** {
    s = \\c => np.s ! c ++ reflPron c (np.a.pgn)
    } ;

}
