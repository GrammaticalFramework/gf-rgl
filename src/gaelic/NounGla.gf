concrete NounGla of Noun = CatGla ** open ResGla, Prelude in {

  flags optimize=all_subs ;

  lin

--2 Noun phrases

-- : Det -> CN -> NP
    DetCN det cn = emptyNP ** {
      art = det.s ! cn.g ;
      s = \\c => case det.dt of {
                   DDef n sp => variants {
                     cn.s ! c ! sp ! n ;
                     cn.s ! NOM ! sp ! n ;
                     cn.s ! NOM ! Indef ! Sg
                     } ;
                   DPoss n _ => variants {
                     cn.s ! c ! Def ! n ;
                     cn.s ! NOM ! Def ! n ;
                     cn.s ! NOM ! Indef ! Sg
                     }
                 } ;
      voc = case det.dt of {
              DDef n sp => cn.voc ! n ;  -- ???????????????? guessed
              DPoss n _ => cn.voc ! n    -- ????????????????
            } ;
      a = NotPron det.dt ;
      } ;

  -- : PN -> NP ;
  -- Assuming that lincat PN = lincat NP
  -- UsePN pn = pn ;

  -- : Pron -> NP ;
  -- Assuming that lincat Pron = lincat NP
  UsePron pron = emptyNP ** pron ** {
    s = pron.s ;
    a = IsPron pron.a
    } ;

  UsePN pn = emptyNP ** {s = \\_ => pn.s ; voc = pn.s} ;

  -- : Predet -> NP -> NP ; -- only the man
  PredetNP predet np = np ** {art = \\c => predet.s ++ np.art ! c} ;

-- A noun phrase can also be postmodified by the past participle of a
-- verb, by an adverb, or by a relative clause

  -- low prio
  -- : NP -> V2  -> NP ;    -- the man seen
  -- PPartNP np v2 = np ** {
  --   s =
  -- } ;

  -- : NP -> Adv -> NP ;    -- Paris today
  AdvNP np adv = np ** {s = \\c => np.s ! c ++ adv.s} ;

  -- : NP -> Adv -> NP ;    -- boys, such as ..
  ExtAdvNP np adv = np ** {s = \\c => np.s ! c ++ "," ++ adv.s} ;

  -- : NP -> RS -> NP ;    -- Paris, which is here
  RelNP np rs = np ** {s = \\c => np.s ! c ++ rs.s} ;

-- Determiners can form noun phrases directly.

  -- : Det -> NP ;
  DetNP det = emptyNP ** {s = \\_ => det.sp ; a = NotPron det.dt} ;
  -- MassNP : CN -> NP ;
    MassNP cn = emptyNP ** {
      s = \\_ => cn.s ! NOM ! Indef ! Sg
      } ;


--2 Determiners

-- The determiner has a fine-grained structure, in which a 'nucleus'
-- quantifier and an optional numeral can be discerned.

  -- : Quant -> Num -> Det ;
    DetQuant quant num = quant ** {
      s = \\g,c => getArt quant num.n g c ++ num.s ;
      s2 = \\g,c => "DUMMY" ; -- "teen" from numbers like seventeen
      dt = case quant.qt of {
              QDef defi => DDef num.n defi ;
              QPoss agr => DPoss num.n agr } ;
      } ;

  DetQuantOrd quant num ord = DetQuant quant num ** {
    s = \\g,c => getArt quant num.n g c ++ num.s ++ ord.s
    } ;

-- Whether the resulting determiner is singular or plural depends on the
-- cardinal.

-- All parts of the determiner can be empty, except $Quant$, which is
-- the "kernel" of a determiner. It is, however, the $Num$ that determines
-- the inherent number.

  NumSg = {s = [] ; n = Sg} ;
  NumPl = {s = [] ; n = Pl} ;

  -- : Card -> Num ;    -- two
  NumCard card = card ;

  -- : Digits  -> Card ;
  NumDigits dig = {s = dig.s ! NCard ; n = dig.n} ;

  -- : Numeral -> Card ;
  NumNumeral num = {
    s = num.s ! NCard ;
    n = num.n -- inherits grammatical number (Sg, Pl, …) from the Numeral
      } ;

  NumDecimal dec = {s = dec.s ; n = Pl} ;

  QuantityNP dec mu = emptyNP ** {
    s = \\_ => case mu.isPre of {True => mu.s ++ dec.s ; False => dec.s ++ mu.s} ;
    voc = dec.s ++ mu.s ; a = NotPron (DDef Pl Indef)
    } ;

  -- : AdN -> Card -> Card ;
  AdNum adn card = card ** {s = adn.s ++ card.s} ;

  -- : Digits  -> Ord ;
  OrdDigits digs = {s = digs.s ! NOrd} ;

  -- : Numeral -> Ord ;
  OrdNumeral num = {
    s = num.s ! NOrd
    } ;

  -- : A       -> Ord ;
  OrdSuperl a = {s = "as" ++ a.compar} ;

-- One can combine a numeral and a superlative.

  -- : Numeral -> A -> Ord ; -- third largest
  OrdNumeralSuperl num a = {
    s = num.s ! NOrd ++ a.compar
  } ;

  -- : Quant
  DefArt = ResGla.defArt ;

  -- : Quant
  IndefArt = {
    s = \\_ => [] ;
    sp = [] ;
    qt = QDef Indef ;
    } ;

  -- : Pron -> Quant        -- my
  PossPron pron = {
    s = \\_ => pron.poss ;
    sp = pron.poss ;
    qt = QPoss pron.a ;
    } ;

--2 Common nouns

  -- : N -> CN
  UseN n = n ;

  -- : N2 -> CN ;
  UseN2 n2 = n2 ;

  -- : N2 -> NP -> CN ;
  ComplN2 n2 np = appendCN n2 (prepNP n2.c2 np) ;

  -- : N3 -> NP -> N2 ;    -- distance from this city (to Paris)
  ComplN3 n3 np = appendCN n3 (prepNP n3.c2 np) ** {c2 = n3.c3} ;

  -- : N3 -> N2 ;          -- distance (from this city)
  Use2N3 n3 = n3 ** {c2 = n3.c3} ;

  -- : N3 -> N2 ;          -- distance (to Paris)
  Use3N3 n3 = n3 ;
  -- : AP -> CN -> CN
  AdjCN ap cn = {
    -- A large part of the imported morphology has only the citation form
    -- of an adjective.  Using that total form here is preferable to making
    -- the complete NP disappear when an inflected cell is absent.
    s = \\c,sp,n => cn.s ! c ! sp ! n ++ ap.s ! ASg NOM Masc ;
    voc = \\n => cn.voc ! n ++ ap.s ! ASg NOM Masc ;
    g = cn.g
  } ;
  -- : CN -> RS -> CN ;
  RelCN cn rs = appendCN cn rs.s ;


  -- : CN -> Adv -> CN ;
  AdvCN cn adv = appendCN cn adv.s ;

-- Nouns can also be modified by embedded sentences and questions.
-- For some nouns this makes little sense, but we leave this for applications
-- to decide. Sentential complements are defined in VerbGla.

  -- : CN -> SC  -> CN ;   -- question where she sleeps
  SentCN cn sc = appendCN cn sc.s ;

--2 Apposition

-- This is certainly overgenerating.

  -- : CN -> NP -> CN ;    -- city Paris (, numbers x and y)
  ApposCN cn np = appendCN cn (linNP np) ;

--2 Possessive and partitive constructs
-- NB. Below this, the functions are not in the API, so lower prio to implement

  -- : PossNP  : CN -> NP -> CN ;
  -- in English: book of someone; point is that we can add a determiner to the CN,
  -- so it can become "a book of someone" or "the book of someone"
  PossNP cn np = appendCN cn (np.art ! Gen ++ np.s ! Gen) ;


  -- : Det -> NP -> NP ; -- three of them, some of the boys
  CountNP det np = np ** {art = \\c => det.s ! Masc ! c ++ np.art ! c} ;


  -- : CN -> NP -> CN ;     -- glass of wine / two kilos of red apples
  PartNP cn np = appendCN cn (np.art ! Gen ++ np.s ! Gen) ;

--3 Conjoinable determiners and ones with adjectives

  -- : DAP -> AP -> DAP ;    -- the large (one)
  AdjDAP dap ap = dap ** {
    s = \\g,c => dap.s ! g ! c ++ ap.s ! ASg c g ;
    s2 = \\g,c => dap.s2 ! g ! c ++ ap.s ! ASg c g
    } ;

  -- : Det -> DAP ;          -- this (or that)
  DetDAP det = det ;

oper
  appendCN : LinN -> Str -> LinN = \cn,x -> cn ** {
    s = \\c,d,n => cn.s ! c ! d ! n ++ x ;
    voc = \\n => cn.voc ! n ++ x
    } ;

}
