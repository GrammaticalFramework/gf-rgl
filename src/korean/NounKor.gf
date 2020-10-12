concrete NounKor of Noun = CatKor ** open ResKor, Prelude in {

  flags optimize=all_subs ;

  lin

--2 Noun phrases

-- : Det -> CN -> NP
  DetCN det cn = cn ** {s =
    \\c =>
      let cns : Str = case det.n of {
                        Pl => cn.s ! Bare ++ BIND ++ plural ! c ;
                        Sg => cn.s ! c } ;
          dets : Str = det.s ! cn.c.origin ;
          detnum : Str = case det.numtype of {
                         IsNum => dets ++ cn.c.s ;
                         IsDig => glue dets cn.c.s ;
                         NoNum => dets } ;
       in case isNum det of {
            True  => cn.rs ++ cns ++ detnum ;
            False => cn.rs ++ detnum ++ cns }
    } ;

  -- : PN -> NP ;
  UsePN pn = pn ;

  -- : Pron -> NP ;
  UsePron pron = pron ** {empty = []};

  -- : Predet -> NP -> NP ; -- only the man
  PredetNP predet np = np ** {s =
    let sakwa = np.s ! Bare ;
        man = predet.s ! np.p ;
        un = table {Vowel => "는"; Consonant => "은"} ! predet.p
    in table { -- TODO: do particles combine?
         Topic => glue (glue sakwa man) un ;
         _     => glue sakwa man
       }
    } ;

-- A noun phrase can also be postmodified by the past participle of a
-- verb, by an adverb, or by a relative clause


  -- : NP -> V2  -> NP ;    -- the man seen
  -- PPartNP np v2 = np ** {
  --   s = \\c => v2.s ! ??? ++ np.s ! c } ; ----

  -- : NP -> Adv -> NP ;    -- Paris today
  AdvNP np adv = np ** {
    s = \\nf => adv.s ++ np.s ! nf
    } ; -- TODO test

   -- : NP -> Adv -> NP ;    -- boys, such as ..
  -- ExtAdvNP np adv = np ** {} ;

  -- : NP -> RS -> NP ;    -- Paris, which is here
  -- RelNP np rs = np ** {
  --   } ;

-- Determiners can form noun phrases directly.

  -- : Det -> NP ;
  DetNP det = det ** {
    s = det.sp ;
    c = baseCounter
    } ;

  -- MassNP : CN -> NP ;
  MassNP cn = cn ** {
    s = \\nf => cn.rs ++ cn.s ! nf
    } ;

--2 Determiners

-- The determiner has a fine-grained structure, in which a 'nucleus'
-- quantifier and an optional numeral can be discerned.

  -- : Quant -> Num -> Det ;
  DetQuant quant num = quant ** num ** {
    s = \\origin => quant.s ++ num.s ! origin ! Attrib
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
  NumCard card = card ;

  -- : Digits  -> Card ;
  NumDigits dig = baseNum ** {
    s = \\_,_ => dig.s ! NCard ;
    n = dig.n ;
    numtype = IsDig
    } ;

  -- : Numeral -> Card ;
  NumNumeral num = num ;

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
  OrdSuperl a = a ** {
    s = \\vf => "가장" ++ a.s ! vf ;
    n = Sg -- ?? is this meaningful?
    } ;

-- One can combine a numeral and a superlative.

  -- : Numeral -> A -> Ord ; -- third largest
  -- OrdNumeralSuperl num a = num ** {  } ;

  -- : Quant
  DefArt,
  IndefArt = mkQuant [] [] ;

  -- : Pron -> Quant
  PossPron pron = pron.poss ;

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
  AdjCN ap cn = cn ** {
    s = \\nf => ap.compar ++ ap.s ! VAttr Pos ++ cn.s ! nf
    } ;

  -- : CN -> RS  -> CN ;
  RelCN cn rs = cn ** {
    rs = cn.rs ++ rs.s ! Subord
    } ;


  -- : CN -> Adv -> CN ;
  AdvCN cn adv = cn ** {
    rs = adv.s ++ cn.rs
    } ;

-- Nouns can also be modified by embedded sentences and questions.
-- For some nouns this makes little sense, but we leave this for applications
-- to decide. Sentential complements are defined in VerbKor.

  -- : CN -> SC  -> CN ;   -- question where she sleeps
  SentCN cn sc = cn ** {
    rs = cn.rs ++ sc.s
    } ;


--2 Apposition

-- This is certainly overgenerating.

  -- : CN -> NP -> CN ;    -- city Paris (, numbers x and y)
  ApposCN cn np = cn ** {
    s = \\nf => np.s ! Bare ++ cn.s ! nf -- TODO which form of NP?
    } ;

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
