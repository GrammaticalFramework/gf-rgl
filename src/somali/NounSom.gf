concrete NounSom of Noun = CatSom ** open ResSom, Prelude in {

  flags optimize=all_subs ;

  lin

--2 Noun phrases

-- The three main types of noun phrases are
-- - common nouns with determiners
-- - proper names
-- - pronouns

  -- : Det -> CN -> NP
  DetCN det cn = useN cn ** {
    s = \\c =>
         let nfc : {nf : NForm ; c : Case} =
           case <c,cn.hasMod,det.d> of {
              <Nom,True, Indef Sg> => {nf=Indef Sg ; c=Abs} ;
              <Nom,False,Indef Sg> => {nf=IndefNom ; c=Nom} ; -- special form for fem. nouns
              <Nom,True,Def x vA>   => {nf=Def x vA ; c=Abs} ;
              <Nom,False,Def x vA>  => {nf=Def x vU ; c=Nom} ;
              _                    => {nf=det.d ; c=c}
           } ;
          in cn.s ! nfc.nf
          ++ det.s ! nfc.c
          ++ cn.mod ! getNum (getAgr det.d Masc) ! c ;
    a = getAgr det.d cn.g
    } ;

  -- : PN -> NP ;
  UsePN pn = pn ** {
    s = \\c => pn.s ;
    isPron = False ;
    sp = pn.s } ;

  -- : Pron -> NP ;
  UsePron pron = lin NP pron ;


  -- : Predet -> NP -> NP ; -- only the man
  PredetNP predet np = np ** {
    s = \\c => predet.s ++ np.s ! c ---- ?
    } ;


-- A noun phrase can also be postmodified by the past participle of a
-- verb, by an adverb, or by a relative clause


  -- : NP -> V2  -> NP ;    -- the man seen
  -- PPartNP np v2 = np ** {
  --   s = \\c => v2.s ! ??? ++ np.s ! c } ; ----

  -- : NP -> Adv -> NP ;    -- Paris today ; boys, such as ..
  --AdvNP,ExtAdvNP = \np,adv -> np ** {} ; --adverbs are complicated

  -- : NP -> RS  -> NP ;    -- Paris, which is here
  -- RelNP np rs = np ** { s = \\c => rs.s ! np.agr ++ np.s ! c } ;

-- Determiners can form noun phrases directly.

  -- : Det -> NP ;
  DetNP det = {
    s = det.sp ! Masc ; ---- Any way to decide for gender here?
    a = getAgr det.d Masc ;
    isPron = False ; sp = []
    } ;

  -- MassNP : CN -> NP ;
  MassNP cn = useN cn ** {
    s = table { Nom => cn.s ! IndefNom ++ cn.mod ! Sg ! Nom ;
                Abs => cn.s ! Indef Sg ++ cn.mod ! Sg ! Abs }
    } ;


--2 Determiners

-- The determiner has a fine-grained structure, in which a 'nucleus'
-- quantifier and an optional numeral can be discerned.

  -- : Quant -> Num -> Det ;
  -- DetQuant quant num = quant **
  --   { } ;

  -- : Quant -> Num -> Ord -> Det ;  -- these five best
  -- DetQuantOrd quant num ord =
  --   let theseFive = DetQuant quant num
  --   in theseFive ** { s = \\c,ph => theseFive.s ! c ! ph ++ ord.s } ; --TODO: dummy implementation

-- Whether the resulting determiner is singular or plural depends on the
-- cardinal.

-- All parts of the determiner can be empty, except $Quant$, which is
-- the "kernel" of a determiner. It is, however, the $Num$ that determines
-- the inherent number.
{-
  NumSg = { s = [] ; n = Sg ; isNum = False } ;
  NumPl = { s = [] ; n = Pl ; isNum = False } ;

  -- : Card -> Num ;
  NumCard card = (card ** { isNum = True }) ;

  -- : Digits  -> Card ;
  NumDigits dig = { s = dig.s ! NCard ; n = dig.n } ;

  -- : Numeral -> Card ;
  NumNumeral num = num ;

  -- : AdN -> Card -> Card ;
  AdNum adn card = card ** { s = adn.s ++ card.s } ;

  -- : Digits  -> Ord ;
  OrdDigits digs = digs ** { s = digs.s ! NOrd } ;

  -- : Numeral -> Ord ;
  OrdNumeral num = num ;

  -- : A       -> Ord ;
  OrdSuperl a = {  } ; -- why force Sg?

-- One can combine a numeral and a superlative.

  -- : Numeral -> A -> Ord ; -- third largest
  OrdNumeralSuperl num a = num ** {  } ;

  -- : Quant
  DefArt = {  } ;
  -- : Quant
  IndefArt = { s     = artDef ;
               indep = False ;
               pref  = [] ;
               isDef = False } ; --has suffix, but turns into partitive in negative!

  -- : Pron -> Quant
  PossPron pron = { s     = artDef ;
                    indep = True ;
                    pref  = pron.s ! Gen ;
                    isDef = True } ;
-}
--2 Common nouns

  -- : N -> CN
  -- : N2 -> CN ;
  UseN,UseN2 = ResSom.useN ;

{-
  -- : N2 -> NP -> CN ;    -- mother of the king
  ComplN2 n2 np =
    let compl = applyPost n2.compl1 np ;
    in useN n2 ** { s = \\agr => compl ++ n2.s } ;

  -- : N3 -> NP -> N2 ;    -- distance from this city (to Paris)
  ComplN3 n3 np =
    let compl = applyPost n3.c3 np ;
    in n3 ** {s = compl ++ n3.s } ;
-}

  -- : N3 -> N2 ;          -- distance (from this city)
  Use2N3 n3 = lin N2 n3 ** { c2 = n3.c3 } ;

  -- : N3 -> N2 ;          -- distance (to Paris)
  Use3N3 n3 = lin N2 n3 ;
  -- : AP -> CN -> CN
  AdjCN ap cn = cn ** {
    s = table { IndefNom => cn.s ! Indef Sg ; -- When an adjective is added, noun loses case marker.
                x        => cn.s ! x } ;
    mod = \\n,c => cn.mod ! n ! Abs -- If there was something before, it is now in Abs
                ++ ap.s ! AF n c ;
    hasMod = True
    } ;

{-
  -- : CN -> RS  -> CN ;
  RelCN cn rs = cn ** {  } ;


  -- : CN -> Adv -> CN ;
  AdvCN cn adv = cn ** {  } ;

-- Nouns can also be modified by embedded sentences and questions.
-- For some nouns this makes little sense, but we leave this for applications
-- to decide. Sentential complements are defined in VerbSom.

  -- : CN -> SC  -> CN ;   -- question where she sleeps
  SentCN cn sc = cn ** { } ;


--2 Apposition

-- This is certainly overgenerating.

  -- : CN -> NP -> CN ;    -- city Paris (, numbers x and y)
  ApposCN cn np = cn ** { s =  } ;


--2 Possessive and partitive constructs

  -- : PossNP  : CN -> NP -> CN ;
  PossNP cn np = cn ** {  } ;

  -- : CN -> NP -> CN ;     -- glass of wine / two kilos of red apples
  PartNP cn np = cn ** {  } ;



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
