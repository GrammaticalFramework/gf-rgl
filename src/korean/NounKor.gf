concrete NounKor of Noun = CatKor ** open ResKor, Prelude in {

  flags optimize=all_subs ;

  lin

--2 Noun phrases

-- : Det -> CN -> NP
  DetCN det cn = {s = \\c => det.s ++ cn.s ! c} ;

  -- : PN -> NP ;
--  UsePN pn = pn ** {
--    } ;

  -- : Pron -> NP ;
  -- UsePron pron = pron ;

  -- : Predet -> NP -> NP ; -- only the man
  -- PredetNP predet np =

-- A noun phrase can also be postmodified by the past participle of a
-- verb, by an adverb, or by a relative clause


  -- : NP -> V2  -> NP ;    -- the man seen
  -- PPartNP np v2 = np ** {
  --   s = \\c => v2.s ! ??? ++ np.s ! c } ; ----

  -- : NP -> Adv -> NP ;    -- Paris today ; boys, such as ..
  --AdvNP,ExtAdvNP = \np,adv -> np ** {} ;

  -- : NP -> RS -> NP ;    -- Paris, which is here
  -- RelNP np rs = np ** {
  --   } ;

-- Determiners can form noun phrases directly.

  -- : Det -> NP ;
  -- DetNP det = emptyNP ** {
  --   } ;

  -- MassNP : CN -> NP ;
  -- MassNP cn = useN cn ** {
  --   } ;


--2 Determiners

-- The determiner has a fine-grained structure, in which a 'nucleus'
-- quantifier and an optional numeral can be discerned.

  -- : Quant -> Num -> Det ;
  DetQuant quant num = quant ** {
    n = num.n
    } ;

  -- : Quant -> Num -> Ord -> Det ;  -- these five best
  -- DetQuantOrd quant num ord =
  --   let theseFive = DetQuant quant num in theseFive ** {
  --     } ;

-- Whether the resulting determiner is singular or plural depends on the
-- cardinal.

-- All parts of the determiner can be empty, except $Quant$, which is
-- the "kernel" of a determiner. It is, however, the $Num$ that determines
-- the inherent number.

  NumSg = baseNum ;
  NumPl = baseNum ** {n = Pl} ;

  -- : Card -> Num ;
  -- NumCard card =

  -- : Digits  -> Card ;
  --  NumDigits dig =

  -- : Numeral -> Card ;
  -- NumNumeral num

{-
  -- : AdN -> Card -> Card ;
  AdNum adn card = card ** { s = adn.s ++ card.s } ;

  -- : Digits  -> Ord ;
  OrdDigits digs = digs ** { s = digs.s ! NOrd } ;
-}
  -- : Numeral -> Ord ;
  -- OrdNumeral num = num ** {
  --   s = \\_ => num.ord
  --   } ;

  -- : A       -> Ord ;
  -- OrdSuperl a = {
  --   s = \\af => "제일" ++ a.s ! af ;
  --   n = Sg -- ?? is this meaningful?
  --   } ;

-- One can combine a numeral and a superlative.

  -- : Numeral -> A -> Ord ; -- third largest
  -- OrdNumeralSuperl num a = num ** {  } ;

  -- : Quant
  DefArt = baseQuant ** {sp = \\_ => []} ;

  -- : Quant
  IndefArt = baseQuant ** {sp = \\_ => []} ;

  -- : Pron -> Quant
  -- PossPron pron =
  --   let p = pron.poss ;
  --    in DefArt ** {
  --       } ;

--2 Common nouns

  -- : N -> CN
  -- : N2 -> CN ;
  UseN,UseN2 = ResKor.useN ;

  -- : N2 -> NP -> CN ;
  -- ComplN2 n2 np =

  -- : N3 -> NP -> N2 ;    -- distance from this city (to Paris)
  -- ComplN3 n3 np =


  -- : N3 -> N2 ;          -- distance (from this city)
  -- Use2N3 n3 = lin N2 n3 ** { c2 = n3.c3 } ;

  -- : N3 -> N2 ;          -- distance (to Paris)
  -- Use3N3 n3 = lin N2 n3 ;
  -- : AP -> CN -> CN
  -- AdjCN ap cn = cn ** {
  --   } ;

  -- : CN -> RS  -> CN ;
  -- RelCN cn rs = cn ** {
  --   } ;

{-
  -- : CN -> Adv -> CN ;
  AdvCN cn adv = cn ** {  } ;

-- Nouns can also be modified by embedded sentences and questions.
-- For some nouns this makes little sense, but we leave this for applications
-- to decide. Sentential complements are defined in VerbKor.

  -- : CN -> SC  -> CN ;   -- question where she sleeps
  SentCN cn sc = cn ** { } ;


--2 Apposition

-- This is certainly overgenerating.

  -- : CN -> NP -> CN ;    -- city Paris (, numbers x and y)
  ApposCN cn np = cn ** { s =  } ;
-}

--2 Possessive and partitive constructs

  -- : PossNP  : CN -> NP -> CN ;
  -- PossNP cn np = cn ** {
  --   } ;

  -- : CN -> NP -> CN ;     -- glass of wine / two kilos of red apples
  -- PartNP cn np = cn ** {
  --   } ;

{-

-- This is different from the partitive, as shown by many languages.

  -- : Det -> NP -> NP ;
  CountNP det np = np **
    { } ; -- Nonsense for DefArt or IndefArt

--3 Conjoinable determiners and ones with adjectives

  -- : DAP -> AP -> DAP ;    -- the large (one)
  AdjDAP dap ap = dap ** { } ;

  -- : Det -> DAP ;          -- this (or that)
  DetDAP det = det ;
-}

}
