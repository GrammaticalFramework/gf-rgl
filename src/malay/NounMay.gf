concrete NounMay of Noun = CatMay ** open ResMay, Prelude in {

  flags optimize=all_subs ;

  lin

--2 Noun phrases

-- : Det -> CN -> NP
    DetCN det cn = emptyNP ** {
      s = \\poss =>
        det.pr
        -- TODO classifier is necessary if numeral comes after noun. See Mintz p. 298.
        -- ++ if_then_Str (isNum det.n) "buah" [] -- TODO store classifier in CN
        ++ case det.poss of {
          Bare => cn.s ! NF (toNum det.n) det.poss ;
          _ => cn.s ! NF (toNum det.n) det.poss -- TODO check if this make sense
        } ++ det.s ++ cn.heavyMod ;
      } ;

  -- : PN -> NP ;
    UsePN pn = MassNP (UseN pn) ;

  -- : Pron -> NP ;
    UsePron pron = pron ** {
      s = \\_ => pron.s ;
      a = IsPron pron.p ;
    };

  -- : Predet -> NP -> NP ; -- only the man
  -- PredetNP predet np =

-- A noun phrase can also be postmodified by the past participle of a
-- verb, by an adverb, or by a relative clause


  -- : NP -> V2  -> NP ;    -- the man seen
  -- PPartNP np v2 = np ** {
  --   s = \\c => v2.s ! ??? ++ np.s ! c } ; ----

  -- : NP -> Adv -> NP ;    -- Paris today ; boys, such as ..
  AdvNP,ExtAdvNP = \np,adv -> np ** {
    s = \\pos => np.s ! pos ++ adv.s
  } ;

  -- : NP -> RS -> NP ;    -- Paris, which is here
    RelNP np rs = np ** {
      s = \\poss => np.s ! poss ++ rs.s ! agr2p np.a
      } ;

-- Determiners can form noun phrases directly.

  -- : Det -> NP ;
    DetNP det = emptyNP ** {
      s = \\_ => linDet det ;
      } ;

  -- MassNP : CN -> NP ;
    MassNP cn = emptyNP ** {
      s = \\poss => cn.s ! NF Sg poss ++ cn.heavyMod
      } ;

--2 Determiners

-- The determiner has a fine-grained structure, in which a 'nucleus'
-- quantifier and an optional numeral can be discerned.

  -- : Quant -> Num -> Det ;
    DetQuant quant num = quant ** {
      pr = num.s ; -- if it's not a number or digit, num.s is empty
      s = quant.s ;
      n = num.n ;
      count = "ke" ++ BIND ++ num.s ++ BIND ++ "-" ++ BIND ++ num.s;
      } ;

  -- : Quant -> Num -> Ord -> Det ;
    DetQuantOrd quant num ord = quant ** {
      pr = num.s ;
      n = num.n ;
      s = ord.s ++ quant.s ;
      count = "" ;
    } ;

-- Whether the resulting determiner is singular or plural depends on the
-- cardinal.

-- All parts of the determiner can be empty, except $Quant$, which is
-- the "kernel" of a determiner. It is, however, the $Num$ that determines
-- the inherent number.

  NumSg = baseNum ;
  NumPl = baseNum ** {n = NoNum Pl} ;

  -- : Card -> Num ;
  NumCard card = card ** {
    n = IsNumber  -- for the purposes of modifying a noun, this is singular
    } ;

  -- : Digits  -> Card ;
  NumDigits dig = {
    s = dig.s ! NCard
    } ;

  NumDecimal dec = {
    s = dec.s ! NCard
    } ;

  -- : Numeral -> Card ;
  NumNumeral num = num ;


  -- : AdN -> Card -> Card ;
  AdNum adn card = card ** { s = adn.s ++ card.s } ;

  -- : Digits  -> Ord ;
  OrdDigits digs = digs ** { s = digs.s ! NOrd } ;

  -- : Numeral -> Ord ;
  OrdNumeral num = {
    s = num.ord
    } ;

  -- : A       -> Ord ;
  OrdSuperl a = {
    s = "ter" ++ BIND ++ a.s
    } ;

-- One can combine a numeral and a superlative.

  -- : Numeral -> A -> Ord ; -- third largest
  OrdNumeralSuperl num a = {
    s = num.ord ++ "ter" ++ BIND ++ a.s
  } ;

  -- : Quant
  DefArt = mkQuant [] ;

  -- : Quant
  IndefArt = mkQuant [] ;

  -- : Pron -> Quant
  PossPron pron = mkQuant pron.s ** {
    poss = Bare ; -- this becomes "kucing dia". for "kucingnya", use PossNP.
    } ;


--2 Common nouns

  -- : N -> CN
  -- : N2 -> CN ;
  UseN,UseN2 = ResMay.useN ;

  -- : N2 -> NP -> CN ;
  ComplN2 n2 np = useN n2 ** {
    s = \\nf =>
      case <n2.c2.prepType, np.a, nf> of {
        <DirObj, IsPron p, NF num _>
          => n2.s ! NF num (Poss p) ++ np.empty ; -- DirObj is reused here to mean possession
        _ => n2.s ! nf ++ applyPrep n2.c2 np
      }
    } ;

  -- : N3 -> NP -> N2 ;    -- distance from this city (to Paris)
  -- ComplN3 n3 np =


  -- : N3 -> N2 ;          -- distance (from this city)
  -- Use2N3 n3 = lin N2 n3 ** { c2 = n3.c3 } ;

  -- : N3 -> N2 ;          -- distance (to Paris)
  -- Use3N3 n3 = lin N2 n3 ;
  -- : AP -> CN -> CN
  AdjCN ap cn = cn ** {
    s = \\nf => cn.s ! nf ++ ap.s
    } ;

  -- : CN -> RS  -> CN ;
  RelCN cn rs = cn ** {
    heavyMod = cn.heavyMod ++ rs.s ! P3
    } ;

  -- : CN -> Adv -> CN ;
  AdvCN cn adv = cn ** {
    heavyMod = cn.heavyMod ++ adv.s
    } ;

{-
  -- : CN -> Adv -> CN ;
  AdvCN cn adv = cn ** {  } ;

-- Nouns can also be modified by embedded sentences and questions.
-- For some nouns this makes little sense, but we leave this for applications
-- to decide. Sentential complements are defined in VerbMay.

  -- : CN -> SC  -> CN ;   -- question where she sleeps
  SentCN cn sc = cn ** { } ;


--2 Apposition

-- This is certainly overgenerating.

  -- : CN -> NP -> CN ;    -- city Paris (, numbers x and y)
  ApposCN cn np = cn ** { s =  } ;
-}

--2 Possessive and partitive constructs

  -- : PossNP  : CN -> NP -> CN ;
  -- this produces "bukunya".
  PossNP cn np = cn ** {
    s = \\nf => case <np.a, nf> of {
      <IsPron p, NF num _>
        => cn.s ! NF num (Poss p) ++ np.empty ;
      _ => cn.s ! nf ++ np.s ! Bare
      }
    } ;


  -- : Det -> NP -> NP ;
  CountNP det np = np **
    {
      s = \\pos => det.count ++ np.s ! pos;
    } ; -- Nonsense for DefArt or IndefArt


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
