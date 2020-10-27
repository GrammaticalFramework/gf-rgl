concrete NounHun of Noun = CatHun ** open
  ResHun, Prelude, Coordination in {

  flags optimize=all_subs ;

  lin

--2 Noun phrases

-- : Det -> CN -> NP
  DetCN det cn = emptyNP ** cn ** det ** {
    s = \\p,c =>
      let possessed : Str = caseFromPossStem cn det c ;
          standalone : Str = caseFromStem glue cn c det.n ;
      in case det.caseagr of {
           True  => det.s ! c ;
           False => det.s ! Nom
         } ++ case <p,det.dt> of {
                <_, DetPoss _>
                   => possessed ;
                <NoPoss, _>
                  => standalone ;
                <Poss per rnum, _>
                  => let pron : Pronoun = pronTable ! <per,rnum> ; -- Possessor's number
                         dnum : CatHun.Num = case det.n of { -- Possessed's number
                                  Sg => NumSg ; Pl => NumPl } ;
                      in caseFromPossStem cn (DetQuant (PossPron pron) dnum) c
         } ++ cn.compl ! det.n ! c ;
    agr = <P3,det.n> ;
    objdef = dt2objdef det.dt ;
    } ;

  -- : PN -> NP ;
  UsePN pn = pn ;

  -- : Pron -> NP ;
  UsePron pron = pron ** {
    s = \\_ => pron.s ;
    postmod = [] ;
  } ;

  -- : Predet -> NP -> NP ; -- only the man
  PredetNP predet np = np ** {
    s = \\p,c => predet.s ++ np.s ! p ! c ;
    } ;

-- A noun phrase can also be postmodified by the past participle of a
-- verb, by an adverb, or by a relative clause

  -- : NP -> V2  -> NP ;    -- the man seen
  -- PPartNP np v2 = np ** {
  --  s = \\c => v2.s ! ??? ++ np.s ! c } ; ----

  -- : NP -> Adv -> NP ;    -- Paris today
  AdvNP np adv = np ** {
    s = \\p,c => case adv.isPre of {
          True => adv.s ++ np.s ! p ! c ;
          False => np.s ! p ! c ++ adv.s } ;
    } ;

  -- : NP -> Adv -> NP ;    -- boys, such as ..
  ExtAdvNP np adv = np ** {
    s = \\p,c => np.s ! p ! c ++ bindComma ++ adv.s ;
    } ;

  -- : NP -> RS -> NP ;    -- Paris, which is here
  RelNP np rs = np ** {
    s = \\p,c => np.s ! p ! c ++ bindComma ++ rs.s ! np.agr.p2 ! c ;
    } ;

-- Determiners can form noun phrases directly.

  -- : Det -> NP ;
  DetNP det = emptyNP ** det ** {
    s = \\p => det.sp ;
    objdef = dt2objdef det.dt ;
    agr = <P3,det.n> ;
    } ;

  -- : CN -> NP ;
  MassNP cn = emptyNP ** cn ** {
    s = \\p,c => case p of {
          NoPoss => caseFromStem glue cn c Sg ;
          Poss per rnum =>
            let pron : Pronoun = pronTable ! <per,rnum> ; -- Possessor's number
                dnum : CatHun.Num = NumSg ; -- because this is MassNP, no Det
             in caseFromPossStem cn (DetQuant (PossPron pron) dnum) c
           } ++ cn.compl ! Sg ! c ;
    agr = <P3,Sg> ;
    } ;

--2 Determiners

-- The determiner has a fine-grained structure, in which a 'nucleus'
-- quantifier and an optional numeral can be discerned.

  -- : Quant -> Num -> Det ;
  DetQuant quant num = let n = num2number num.n in
    quant ** num ** {
      s = \\c => case <isNum num,isIndefArt quant> of {
                   <True,True> => [] ; -- don't output "a 2 cars"
                   _           => quant.s ! n ! c }
              ++ num.s ! Attrib ;      -- TODO: add inflection table in numbers
      sp = \\c => case <isNum num,isIndefArt quant> of {
                   <True,True> => [] ;
                   _           => quant.sp ! n ! c }
              ++ num.s ! Indep ;
      n = n ;
      dt = qt2dt quant.qt ;
      } ;

  -- : Quant -> Num -> Ord -> Det ;  -- these five best
  DetQuantOrd quant num ord =
    let theseFive = DetQuant quant num ;
        n = num2number num.n ;
     in theseFive ** {
      s = \\c => theseFive.s ! c ++ ord.s ! n ! Nom ;
      sp = \\c => theseFive.sp ! c ++ ord.s ! n ! Nom ;
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
    n = IsNum -- Numerals take noun in Sg: e.g. öt város, literally 'five city'
    } ;

  -- : Digits  -> Card ;
  NumDigits dig = dig ** {
    s = \\place => dig.s ! NCard ;
    } ;

  -- : Numeral -> Card ;
  NumNumeral num = num ;

{-
  -- : AdN -> Card -> Card ;
  AdNum adn card = card ** { s = adn.s ++ card.s } ;

  -- : Digits  -> Ord ;
  OrdDigits digs = digs ** { s = digs.s ! NOrd } ;

  -- : Numeral -> Ord ;
  OrdNumeral num = num ** {
    s = \\_ => num.ord
    } ;
-}
  -- : A       -> Ord ;
  OrdSuperl a = {
    s = \\n,c =>
      let adj : Noun = (a ** {s = a.s ! Superl}) in
      caseFromStem glue adj c n ;
    n = Sg -- ?? is this meaningful?
    } ;

-- One can combine a numeral and a superlative.

  -- : Numeral -> A -> Ord ; -- third largest
  -- OrdNumeralSuperl num a = num ** {  } ;

  -- : Quant
  DefArt = mkQuant "a" "a" ** {
    s,
    sp = \\_,_ => pre {"a" ; "az" / v } ;
    qt = DefQuant ;
    } ;

  -- : Quant
  IndefArt = mkQuant "egy" [] ** {
    s = \\n,_ => case n of {Sg => "egy" ; Pl => []} ;
    sp = \\n,_ => case n of {Sg => "egy" ; Pl => "sok"} ;
    qt = IndefArticle ;
    } ;

  -- : Pron -> Quant
  PossPron pron = pron ** {
    s,sp = \\_ => pron.s ;
    qt = QuantPoss (agr2pstem pron.agr) ;
    caseagr = False ;
    } ;

--2 Common nouns

  -- : N -> CN
  -- : N2 -> CN ;
  UseN,UseN2 = \n -> n ** {
    compl = \\_,_ => [] ;
    postmod = [] ;
    } ;

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
    s = \\nc => ap.s ! Sg ! Nom ++ cn.s ! nc ;
    compl = \\n,c => ap.compl ! n ++ cn.compl ! n ! c ;
    } ;

  -- : CN -> RS  -> CN ;
  RelCN cn rs = cn ** {
    compl = \\n,c => cn.compl ! n ! c ++ rs.s ! n ! c
    } ;

  -- : CN -> Adv -> CN ;
  AdvCN cn adv = case adv.isPre of {
    True => AdjCN (invarAP adv.s) cn ;
    False => cn ** {postmod = cn.postmod ++ adv.s}
  } ;

-- Nouns can also be modified by embedded sentences and questions.
-- For some nouns this makes little sense, but we leave this for applications
-- to decide. Sentential complements are defined in VerbHun.

  -- : CN -> SC  -> CN ;   -- question where she sleeps
  -- SentCN cn sc = cn ** { } ;

--2 Apposition

-- This is certainly overgenerating.

  -- : CN -> NP -> CN ;    -- city Paris (, numbers x and y)
  ApposCN cn np = cn ** {
    compl = \\n,c => cn.compl ! n ! c ++ np.s ! NoPoss ! Nom
    } ;

--2 Possessive and partitive constructs

  -- : PossNP  : CN -> NP -> CN ;
  -- PossNP cn np = cn ** {
  --  compl = \\n,c => cn.compl ! n ! c ++ np.s ! Poss P3 n ! c -- TODO check
  --  } ;

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
