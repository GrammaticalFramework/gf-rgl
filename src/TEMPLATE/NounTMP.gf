concrete NounTMP of Noun = CatTMP ** open ResTMP, Prelude in {

  flags optimize=all_subs ;

  lin

--2 Noun phrases

-- : Det -> CN -> NP
    DetCN det cn = emptyNP ** {
      s = det.s ++ cn.s ! det.n
      } ;
{-
  -- : PN -> NP ;
  -- Assuming that lincat PN = lincat NP
  UsePN pn = pn ;

  -- : Pron -> NP ;
  -- Assuming that lincat Pron = lincat NP
  UsePron pron = pron ;

  -- : Predet -> NP -> NP ; -- only the man
  PredetNP predet np =

-- A noun phrase can also be postmodified by the past participle of a
-- verb, by an adverb, or by a relative clause

  -- low prio
  -- : NP -> V2  -> NP ;    -- the man seen
  -- PPartNP np v2 = np ** {
  --   s =
  -- } ;

  -- : NP -> Adv -> NP ;    -- Paris today
  AdvNP np adv = np ** {
    s = np.s ++ "," ++ adv.s
  } ;

  -- : NP -> Adv -> NP ;    -- boys, such as ..
  ExtAdvNP np adv = AdvNP np {s = "," ++ adv.s} ;

  -- : NP -> RS -> NP ;    -- Paris, which is here
    RelNP np rs = np ** {

      } ;

-- Determiners can form noun phrases directly.

  -- : Det -> NP ;
    DetNP det = emptyNP ** {
      s = \\_ => linDet det ;
      } ;
-}
  -- MassNP : CN -> NP ;
    MassNP cn = emptyNP ** {
      s = linCN cn
      } ;


--2 Determiners

-- The determiner has a fine-grained structure, in which a 'nucleus'
-- quantifier and an optional numeral can be discerned.

  -- : Quant -> Num -> Det ;
    DetQuant quant num = quant ** {
      s = quant.s ! num.n ++ num.s ;
      n = num.n ;
      } ;

  -- : Quant -> Num -> Ord -> Det ;
    -- DetQuantOrd quant num ord = quant ** {

    -- } ;

-- Whether the resulting determiner is singular or plural depends on the
-- cardinal.

-- All parts of the determiner can be empty, except $Quant$, which is
-- the "kernel" of a determiner. It is, however, the $Num$ that determines
-- the inherent number.

  NumSg = {s = [] ; n = Sg} ;
  NumPl = {s = [] ; n = Pl} ;

{-
  -- : Card -> Num ;    -- two
  NumCard card = card ;

  -- : Digits  -> Card ;
  NumDigits dig = -- probably like OrdDigits, but choose the NCard form

  -- : Numeral -> Card ;
  NumNumeral num = {
    s = num.s ! NCard ;
    n = num.n -- inherits grammatical number (Sg, Pl, …) from the Numeral
    } ;

  -- : AdN -> Card -> Card ;
  AdNum adn card = card ** { s = adn.s ++ card.s } ;

  -- : Digits  -> Ord ;
  OrdDigits digs = digs ** { s = digs.s ! NOrd } ;

  -- : Numeral -> Ord ;
  OrdNumeral num = {
    s = num.s ! NOrd
    } ;

  -- : A       -> Ord ;
  OrdSuperl a = {
    s = "most" ++ a.s ! Superl
    } ;

-- One can combine a numeral and a superlative.

  -- : Numeral -> A -> Ord ; -- third largest
  OrdNumeralSuperl num a = {
    s = num.s ! NOrd ++ a.s ! Superl
  } ;
-}

  -- : Quant
  DefArt = mkQuant "the" "the" ;

  -- : Quant
  IndefArt = mkQuant "a" [] ;

{-
  -- : Pron -> Quant        -- my
  PossPron pron = mkQuant pron.s ** {

    } ;
-}

--2 Common nouns

  -- : N -> CN
  UseN n = n ;

{-
  -- : N2 -> CN ;
  UseN2 n2 =

  -- : N2 -> NP -> CN ;
  ComplN2 n2 np =

  -- : N3 -> NP -> N2 ;    -- distance from this city (to Paris)
  ComplN3 n3 np =

  -- : N3 -> N2 ;          -- distance (from this city)
  Use2N3 n3 = lin N2 n3 ** { c2 = n3.c3 } ;

  -- : N3 -> N2 ;          -- distance (to Paris)
  Use3N3 n3 = lin N2 n3 ;

  -- : AP -> CN -> CN
  AdjCN ap cn =

  -- : CN -> RS -> CN ;
  RelCN cn rs =


  -- : CN -> Adv -> CN ;
  AdvCN cn adv =

-- Nouns can also be modified by embedded sentences and questions.
-- For some nouns this makes little sense, but we leave this for applications
-- to decide. Sentential complements are defined in VerbTMP.

  -- : CN -> SC  -> CN ;   -- question where she sleeps
  SentCN cn sc =

--2 Apposition

-- This is certainly overgenerating.

  -- : CN -> NP -> CN ;    -- city Paris (, numbers x and y)
  ApposCN cn np = cn ** {
    s =
    } ;

--2 Possessive and partitive constructs
-- NB. Below this, the functions are not in the API, so lower prio to implement

  -- : PossNP  : CN -> NP -> CN ;
  -- in English: book of someone; point is that we can add a determiner to the CN,
  -- so it can become "a book of someone" or "the book of someone"
  PossNP cn np =


  -- : Det -> NP -> NP ; -- three of them, some of the boys
  CountNP det np = -- Nonsense for DefArt or IndefArt, but don't worry about that! RGL can contain weird sentences, as long as it contains the non-weird stuff we want


  -- : CN -> NP -> CN ;     -- glass of wine / two kilos of red apples
  PartNP cn np =

--3 Conjoinable determiners and ones with adjectives

  -- : DAP -> AP -> DAP ;    -- the large (one)
  AdjDAP dap ap = dap ** {

    } ;

  -- : Det -> DAP ;          -- this (or that)
  DetDAP det = det ;
-}

}
