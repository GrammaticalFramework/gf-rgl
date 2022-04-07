concrete NounSom of Noun = CatSom ** open ResSom, Prelude in {

  flags optimize=all_subs ;

  lin

--2 Noun phrases

-- : Det -> CN -> NP
  DetCN det cn = useN cn ** {
    s = sTable ;
    st = det.st ;
    a = getAgr det.n (gender cn) } where {
      sTable : Case => Str = \\c =>
         let nfc : {nf : NForm ; c : Case} =
             case <det.numtype,c,cn.modtype,det.st,det.n> of {
                -- Numbers
                <Basic|Compound,_,_,_,_> => {nf=Numerative ; c=c} ;

                -- special form for fem. nouns
                <_,Nom,NoMod|OtherMod,Indefinite,Sg> => {nf=NomSg ; c=c} ;

                -- If cn has modifier, Nom ending attaches to the modifier
                <_,Nom,AMod,_,_> => {nf=Def det.n ; c=Abs} ;

                -- a Det with st=Indefinite uses Indef forms
                <_,_,_,Indefinite,n>  => {nf=Indef n ; c=c} ;

                -- All other determiners use the definite stem
                _ => {nf=Def det.n ; c=c}
             } ;
          art = gda2da cn.gda ! det.n ;
          num = case isNum det.numtype of {True => Sg ; _ => det.n} ;
          dt : {pref,s : Str} =
            case <nfc.nf,cn.isPoss,andB det.isPoss cn.shortPoss> of {
              -- Det is a cardinal number. The number is the head of the NP,
              -- and CN becomes its modifier. If CN has modifiers of its own,
              -- we insert the conjunction "oo" between the number and the CN.
              <Numerative,_,_> =>
                 let oo = case det.numtype of {Compound => "oo" ; _ => []}
                  in {s = [] ; pref = det.s ! art ! nfc.c ++ oo} ;

              -- CN has undergone ComplN2 and is already quantified
              <_,True,_> => {pref = [] ; s = det.sp ! gender cn ! nfc.c} ;

              -- CN is e.g. a kinship term and takes short possessive
              <_,_,True> => {pref = [] ; s = BIND ++ det.shortPoss ! art} ;

              -- Default case
              _ => {pref = [] ; s = det.s ! art ! nfc.c}
            } ;
        in dt.pref -- if det is numeral
        ++ cn.s ! nfc.nf
        ++ dt.s -- non-numeral det
        ++ cn.mod ! det.st ! num ! c
    } ;

  -- : PN -> NP ;
  UsePN pn = pn ** {
    s = \\c => pn.s ;
    isPron = False ;
    st = Definite ;
    empty = [] ;
    } ;

  -- : Pron -> NP ;
  UsePron pron = pron ** {st = Definite} ;

  -- : Predet -> NP -> NP ; -- only the man
  PredetNP predet np =
    let qnt = PossPron (pronTable ! np.a) ;
        det = qnt.shortPoss ! predet.da ;
         predetS : Str = case predet.isPoss of {
          True => glue predet.s det ;
          False => predet.s
        } ;
     in np ** {
          s = \\c =>
            case <np.isPron,predet.isPoss> of {
              <True,True> => np.empty ++ predetS ;
              _ => np.s ! c ++ predetS} ;
          isPron = False ; -- NP it loses its pronoun status when Predet is added
        } ;

-- A noun phrase can also be postmodified by the past participle of a
-- verb, by an adverb, or by a relative clause


  -- : NP -> V2  -> NP ;    -- the man seen
  -- PPartNP np v2 = np ** {
  --   s = \\c => v2.s ! ??? ++ np.s ! c } ; ----

  -- : NP -> Adv -> NP ;    -- Paris today ; boys, such as ..
  --AdvNP,ExtAdvNP = \np,adv -> np ** {} ; --adverbs are complicated

  -- : NP -> RS -> NP ;    -- Paris, which is here
  {- NB. technically, if the RS has undergone ConjRS, it could contain both
     restrictive and appositive relative clauses. Quote Saeed p.215-216:
       "When multiple relative clauses occur, this formal distinction is
        maintained, since in the only context both can occur, on nouns with
        determiners, restrictives are joined by ee while appositives employ oo."
     In practice, we don't care--it's impossible to know on the RGL level
     which RS are restrictive and which appositive, as it is semantic.
   -}
  RelNP np rs = np ** {
    s = \\c => objpron np ! c ++ "oo" ++ rs.s ! Indefinite ! npgennum np ! c ;
    isPron = False ;
    } ;

-- Determiners can form noun phrases directly.

  -- : Det -> NP ;
  DetNP det = emptyNP ** {
    s = det.sp ! Masc ; ---- Any way to decide for gender here?
    a = getAgr det.n Masc ;
    isPron = False ;
    } ;

  -- MassNP : CN -> NP ;
  MassNP cn = useN cn ** {
    s = table { Nom => cn.s ! NomSg ++ cn.mod ! Indefinite ! Sg ! Nom ;
                c   => cn.s ! Indef Sg ++ cn.mod ! Indefinite ! Sg ! c }
    } ;


--2 Determiners

-- The determiner has a fine-grained structure, in which a 'nucleus'
-- quantifier and an optional numeral can be discerned.

  -- : Quant -> Num -> Det ;
  DetQuant quant num = let indep = Hal in quant ** {
    s = \\da,c =>
            case isNum num.numtype of {
               True => num.s ! indep ++ quant.s ! num.da ! c ++ num.thousand ;
               False => num.s ! indep ++ quant.s ! da ! c ++ num.thousand } ;

    sp = \\g,c => case <num.n,g> of {
          <Sg,Masc> => num.s ! indep ++ quant.sp ! SgMasc ! c ++ num.thousand ;
          <Sg,Fem> => num.s ! indep ++ quant.sp ! SgFem ! c ++ num.thousand ;
          -- Independent form uses plural morpheme, not gender-flipped allomorph
          <Pl,_> => num.s ! indep ++ quant.sp ! PlInv ! c ++ num.thousand } ;
    numtype = num.numtype ;
    n = num.n ;
    shortPoss = \\da => quant.shortPoss ! da ++ num.s ! indep
    } ;

  -- : Quant -> Num -> Ord -> Det ;  -- these five best
  DetQuantOrd quant num ord =
    let theseFive = DetQuant quant num in theseFive ** {
      s = \\g,c  => theseFive.s ! g ! c  ++ ord.s ! AF num.n c ;
      sp = \\g,c => theseFive.sp ! g ! c ++ ord.s ! AF num.n c ;
      shortPoss = \\da => theseFive.shortPoss ! da ++ ord.s ! AF num.n Abs
      } ;

-- Whether the resulting determiner is singular or plural depends on the
-- cardinal.

-- All parts of the determiner can be empty, except $Quant$, which is
-- the "kernel" of a determiner. It is, however, the $Num$ that determines
-- the inherent number.

  NumSg = baseNum ;
  NumPl = baseNum ** {n = Pl} ;

  -- : Card -> Num ;
  NumCard card = card ** {
    numtype = case card.hasThousand of {
                True => Compound ;
                False => Basic }
    } ;

  -- : Digits  -> Card ;
--  NumDigits dig = { s = dig.s ! NCard ; n = dig.n } ;

  -- : Numeral -> Card ;
  NumNumeral num = num ; -- ** {s = num.s ! NCard};

{-
  -- : AdN -> Card -> Card ;
  AdNum adn card = card ** { s = adn.s ++ card.s } ;

  -- : Digits  -> Ord ;
  OrdDigits digs = digs ** { s = digs.s ! NOrd } ;
-}
  -- : Numeral -> Ord ;
  OrdNumeral num = num ** {
    s = \\_ => num.ord
    } ;

  -- : A       -> Ord ;
  OrdSuperl a = {
    s = \\af => "ugu" ++ a.s ! af ;
    n = Sg -- ?? is this meaningful?
    } ;

-- One can combine a numeral and a superlative.

  -- : Numeral -> A -> Ord ; -- third largest
  -- OrdNumeralSuperl num a = num ** {  } ;

  -- : Quant
  DefArt = defQuant "a" "kan" "tan" "kuwan" False ;

  -- : Quant
  IndefArt = indefQuant ; -- TODO sp forms

  -- : Pron -> Quant
  PossPron pron =
    let p = pron.poss ;
     in DefArt ** {
          shortPoss = p.short ;
          isPoss = True ;
          s = \\da,c => let casevow = case c of {Nom => "u" ; Abs => "a"}
                         in BIND ++ p.s ! da ++ BIND ++ casevow ;

          sp = \\gn,c => let prefix = case gn of {SgFem => "t" ; _ => "k"} ;
                             casevow = case c of {Nom => "u" ; Abs => "a"}
                          in prefix ++ BIND ++ p.sp ! gn ++ BIND ++ casevow ;
        } ;

--2 Common nouns

  -- : N -> CN
  -- : N2 -> CN ;
  UseN,UseN2 = ResSom.useN ;

  -- : N2 -> NP -> CN ;    -- Sahra hooyadeed
  ComplN2 n2 np = genModCN (useN n2) np ;

{-
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
    s = table { -- Add oo after Numerative only if this is CN's first modifier.
          Numerative => cn.s ! Numerative
                     ++ andConj Indefinite (notMod cn.modtype) ;
          NomSg => cn.s ! Indef Sg ; -- Add adj -> noun loses case marker
          nf => cn.s ! nf } ;
    mod = \\st,n,c =>
            cn.mod ! st ! n ! Abs -- If there was something before, it is now in Abs
         ++ andConj st cn.modtype -- If the sentence is already modified, any new modifier needs to be introduced with conjunction
         ++ ap.s ! AF n c ;
    modtype = AMod
    } ;

  -- : CN -> RS  -> CN ;
  RelCN cn rs = cn ** {
    s = table {
          Numerative => cn.s ! Numerative ++ andConj Indefinite (notMod cn.modtype) ;
          nf => cn.s ! nf } ;
    mod = \\st,n,c => --what to do with subject case if there's both adj and RS?
            cn.mod ! st ! n ! Abs
         ++ andConj st cn.modtype
         ++ rs.s ! st ! gennum cn Sg ! c ; -- gennum cn Sg, because plural form is only for 1st person plural
    modtype = AMod
    } ;

{-
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
-}

--2 Possessive and partitive constructs

  -- : PossNP  : CN -> NP -> CN ;
  PossNP cn np = cn ** { -- guriga Axmed, not Axmed gurigiisa
    mod = \\st,n,c => cn.mod ! st ! n ! c ++ objpron np ! Abs
    } ;

  -- : CN -> NP -> CN ;     -- glass of wine / two kilos of red apples
  PartNP cn np = cn ** {
    s = table {
          Numerative => cn.s ! Numerative ++ andConj Indefinite (notMod cn.modtype) ;
          nf => cn.s ! nf } ;
    mod = \\st,n,c =>
            cn.mod ! st ! n ! c
          ++ andConj st cn.modtype  -- If the sentence is already modified, any new modifier needs to be introduced with conjunction
          ++ objpron np ! Abs
          ++ "ah" ;
    modtype = case cn.modtype of {
      AMod => AMod ;
      _ => OtherMod }
    } ;

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

oper
  andConj : State -> ModType -> Str = \st,mod ->
    case <st,mod> of {
      <Indefinite,AMod|OtherMod> => "oo" ;
      <Definite,AMod|OtherMod>   => "ee" ;
      _  => []
    } ;

  genModCN : CN -> NP -> CN = \cn,np -> cn ** {
    s = \\nf =>
      let num = case nf of {
                      Def n  => n ;
                      Indef n => n ;
                      _ => Sg } ;
          art = gda2da cn.gda ! num ;
          qnt = PossPron (pronTable ! np.a) ;
          det = case cn.shortPoss of {
                  True => qnt.shortPoss ! art ;
                  _ => qnt.s ! sg cn.gda ! Abs } ;
          noun = case np.isPron of {
                   True  => (pronTable ! np.a).sp ! Abs ; -- long subject pronoun
                   False => np.s ! Abs }
       in noun ++ cn.s ! Def num ++ BIND ++ det ;
     isPoss = True} ;

}
