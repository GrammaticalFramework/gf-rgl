concrete IdiomAra of Idiom = CatAra ** open
  Prelude,
  ResAra,
  VerbAra,
  ParadigmsAra
 in {


 lin

  -- : VP -> Cl ;        -- it is hot
  -- "it is a girl" becomes "she is a girl";
  -- "it is twins" becomes "they<dual> are<dual> twins".
  ImpersCl vp =            -- if no obj, default Per3 Masc Sg
    let it : ResAra.NP = gn2pron vp.obj.a.gn ;
     in predVP it vp ;

  --  : VP -> Cl ;        -- one sleeps
  GenericCl = predVP (regNP "المَرْء" Sg Def) ;

  -- : NP  -> RS -> Cl ; -- it is I who did it
  --CleftNP np rs =

  -- : Adv -> S -> Cl ; -- it is here she slept
  CleftAdv adv s =
    let comp : Comp = CompAdv (lin Adv {s = adv.s ++ s.s ! Verbal}) ; -- no idea about word order /IL
        pass_V = mkV "مضي" va vi ; -- switch to copula or some other verb if better /IL
     in predVP emptyNP (UseV pass_V ** {vtype=Copula ; pred=comp}) ; -- very hacky /IL

   -- : NP -> Cl ;        -- there is a house
  ExistNP np =
    predVP (indeclNP "هُنَاكَ" Sg) (UseComp (CompNP np)) ; -- IL

  -- : IP -> QCl ;       -- which houses are there
  ExistIP ip = let cl = ExistNP (ip2np ip False) in {
    s = \\t,p,q => cl.s ! t ! p ! Nominal -- IL guessed
    } ;

-- 7/12/2012 generalizations of these

  -- : NP -> Adv -> Cl ;    -- there is a house in Paris
  ExistNPAdv np adv =
    predVP (indeclNP "هُنَاكَ" Sg) (AdvVP (UseComp (CompNP np)) adv) ; -- IL

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
