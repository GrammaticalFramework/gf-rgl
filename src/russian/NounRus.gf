concrete NounRus of Noun = CatRus ** open ResRus, ParamRus, Coordination, Prelude in {
flags coding=utf8 ; optimize=all ;

lin

---------------
-- Noun phrases

  -- : Det -> CN -> NP ;   -- the man
  DetCN det cn =
    let n = numSizeNumber det.size in {
      s=case det.type of {
        _ => \\cas => det.s ! cn.g ! cn.anim ! cas
          ++ cn.s ! animNumSizeNum cn.anim cas det.size ! numSizeCase cas det.size
        } ;
      pron=False ;
      a=Ag (gennum cn.g (forceMaybeNum cn.mayben n)) P3
      } ;

  -- : PN -> NP ;          -- John
  UsePN pn = {
    s=pn.s ;
    pron=False;
    a=Ag (gennum pn.g pn.n) P3
    } ;   -- Does NP need animacy?

  -- : Pron -> NP ;
  UsePron pron = lin NP (pronFormsPronoun pron) ;

  -- : Predet -> NP -> NP ; -- only the man
  PredetNP predet np = np ** {s=\\cas => predet.s ! (agrGenNum np.a) ! Inanimate ! cas ++ np.s ! numSizeCase cas predet.size} ;

  -- : NP -> V2 -> NP ;    -- the man seen
  PPartNP np v2 = np ** {
    s = \\cas => np.s ! cas ++ (shortPastPassPart v2 (agrGenNum np.a))
    } ;

  -- : NP -> Adv -> NP ;    -- Paris today
  AdvNP np adv = np ** {s=\\cas=>np.s ! cas ++ adv.s} ;

  -- : NP -> Adv -> NP ;    -- boys, such as ..
  ExtAdvNP np adv = np ** {s=\\cas=>np.s ! cas ++ embedInCommas adv.s} ;
  -- : NP -> RS -> NP ;    -- Paris, which is here
  RelNP np rs = np ** {s=\\cas=>np.s ! cas ++ embedInCommas (rs.s ! agrGenNum np.a ! Inanimate !cas)} ;

  -- : Det -> NP ;        -- these five
  DetNP det =
    let g = Neut in {
      s=case det.type of {
        EmptyIndef => \\cas => a_Det.s ! g ! Inanimate ! cas ++ det.s ! g ! Inanimate ! cas ;
        EmptyDef => \\cas => the_Det.s ! g ! Inanimate ! cas ++ det.s ! g ! Inanimate ! cas ;
        _ => \\cas => det.s ! g ! Inanimate ! cas
        } ;
      pron=False ;
      a=Ag (gennum g (numSizeNumber det.size)) P3
      } ;

  -- : CN -> NP ;           -- (beer)
  MassNP cn =
    let n=forceMaybeNum cn.mayben Sg in {
      s = \\cas => cn.s ! Sg ! cas ;
      pron=False ;
      a = Ag (gennum cn.g n) P3
      } ;

  -- : N2 -> NP -> CN ;    -- mother of the king - мать короля
  ComplN2 n2 np = {
    s=\\n,cas=> (nounFormsNoun n2).s ! n ! cas ++ n2.c2.s ++ np.s ! n2.c2.c ;
    g=n2.g ;
    mayben=n2.mayben ;
    anim=n2.anim ;
    rel=n2.rel;
    rt=n2.rt;
    } ;
  -- : N3 -> NP -> N2 ;    -- distance from this city (to Paris)
  ComplN3 n3 np = let n3_noun = nounFormsNoun n3 in nounToNounForm {
    s=\\n,g=>n3_noun.s ! n ! g ++ n3.c2.s ++ np.s ! n3.c2.c ;
    g=n3.g ;
    mayben=n3.mayben ;
    anim=n3.anim ;
    rel=n3.rel;
    rt=n3.rt ;
  } ** {c2=n3.c3; rt = n3.rt} ;

--------------
-- Determiners
  -- : Numeral -> Card  ;  -- fifty-one
  NumNumeral n = n ;
  -- : Card -> Num
  NumCard card = card ;
  -- : Digits -> Card ;  -- 51
  NumDigits n = {s = \\_,_,_ => n.s ; size = n.size } ;

  NumDecimal n = {s = \\_,_,_ => n.s ; size = n.size } ;

  -- : Quant -> Num -> Det ;  -- these five
  DetQuant quant num = {
    s=\\g,anim,cas => quant.s ! (gennum g (numSizeNumber num.size)) ! anim ! cas ++ num.s ! g ! anim ! cas ;
    type=quant.type ;
    g=quant.g ;
    c=quant.c ;
    size=num.size
    } ;

  -- : Quant -> Num -> Ord -> Det ;  -- these five best
  DetQuantOrd quant num ord = {
    s=\\g,a,cas => num.s ! g ! a ! cas
      ++ quant.s ! (gennum g (numSizeNumber num.size)) ! a ! cas
      ++ (adjFormsAdjective ord).s ! gennum g (animNumSizeNum Inanimate cas num.size) ! Inanimate ! numSizeCase cas num.size ;
    type=quant.type ;
    g=quant.g ;
    c=quant.c ;
    size=num.size
    } ;

  -- : Num  -- mark as singular
  NumSg = {s = \\_,_,_ => [] ; size = Num1 } ;
  -- : Num  -- mark as plural
  NumPl = {s = \\_,_,_ => [] ; size = NumAll } ;

  -- Digits -> Ord ;  -- 51st
  OrdDigits d = ith_forms d.s ;

  -- : Numeral -> Ord ;  -- fifty-first
  OrdNumeral numeral = numeral.o ** {
    sm=numeral.s ! Masc ! Inanimate ! Nom; -- these are not correct, but needed to prevent parsing problems
    sf=numeral.s ! Fem ! Inanimate ! Nom;
    sn=numeral.s ! Neut ! Inanimate ! Nom;
    sp=numeral.s ! Neut ! Inanimate ! Gen ;
    comp=numeral.s ! Neut ! Inanimate ! Gen ;
    p=False ;
    preferShort=PreferFull
  } ;

  -- : A -> Ord ;
  OrdSuperl a = long_superlative a ;

  -- : Numeral -> A -> Ord ; -- third largest
  OrdNumeralSuperl num a = ord_long_superlative num.o a  ;

  -- : Pron -> Quant ;    -- my (house)
  PossPron pron = {
    s=mkPronTable pron.poss ;
    type=NormalDet ;
    short=\\a=>[] ;
    c=Nom ;
    preferShort=PreferFull
    } ;

  -- : AdN -> Card -> Card
  AdNum adn card = card ** {
    s=\\g,a,cas => adn.s ++ card.s ! g ! a ! cas
    } ;

---------------
-- Common nouns

  -- : AP -> CN -> CN ;   -- big house - большой дом
  AdjCN ap cn = cn ** {
    s = \\n,cas => preOrPost (notB ap.isPost) (ap.s ! (gennum cn.g (forceMaybeNum cn.mayben n)) ! cn.anim ! cas) (cn.s ! n ! cas)
    } ;

  -- : N -> CN
  UseN n = nounFormsNoun n ;

  -- : N2 -> CN ;
  UseN2 n = nounFormsNoun n ;

  -- : N3 -> N2 ; -- distance (from this city)
  Use2N3 n3 = lin N2 n3 ** { compl1 = n3.compl2 } ;

  -- : N3 -> N2 ; -- distance (to Paris)
  Use3N3 n3 = lin N2 n3 ;


  -- : CN -> RS -> CN ;   -- house that John bought
  RelCN cn rs = cn ** {
    s = \\n,c => cn.s ! n ! c ++ embedInCommas (rs.s ! gennum cn.g (forceMaybeNum cn.mayben n) ! cn.anim ! c)
    } ;

  -- : CN -> SC -> CN ;   -- question where she sleeps
  SentCN cn sc = cn ** {
    s = \\n,c => cn.s ! n ! c ++ sc.s
    }; -- SC type will change???

  -- : CN -> Adv -> CN ;   -- house on the hill
  AdvCN cn adv = cn ** {s = \\n,c => cn.s ! n ! c ++ adv.s};

-------------
-- Apposition
  -- : CN -> NP -> CN ;    -- city Paris (, numbers x and y)
  ApposCN cn np = cn ** {s=\\n,cas => cn.s ! n ! cas ++ np.s ! cas} ;

--------------------------------------
-- Possessive and partitive constructs

  -- : CN -> NP -> CN ;     -- house of Paris, house of mine
  PossNP cn np = cn ** {
    s=\\n,cas => cn.s ! n ! cas ++ np.s ! Gen ;   -- TODO: possessive pronouns P1, P2
    } ;

  -- : CN -> NP -> CN ;     -- glass of wine - стакан чаю (чая)
  PartNP cn np = cn ** {
    s=\\n,cas => cn.s ! n ! cas ++ np.s ! Ptv ;   -- also Gen
    } ;

  -- : Det -> NP -> NP ;    -- three of them, some of the boys
  CountNP det np = {
    s=\\cas => det.s ! Neut ! Inanimate ! cas ++ applyPrep from2 np ;
    pron=False ;
    a=numSizeGenAgr det.size Neut P3
    } ;

---------------------------------------------------
-- Conjoinable determiners and ones with adjectives

  -- DAP -> AP -> DAP ;    -- the large (one)
  AdjDAP dap ap = dap ** {
    s=\\g,anim,cas => ap.s ! GSg g ! anim ! cas ++ dap.s ! g ! anim ! cas
    } ;

  -- : Det -> DAP ;          -- this (or that)
  DetDAP det = det ;

---------------------------------------------------
-- Backwards compatibility
  --  : Quant ;       -- the (house), the (houses)
  DefArt = {
    s=\\gn,anim,cas=>[] ;
    type=EmptyDef ;
    short=\\a=>[] ;
    c=Nom ;
    size=Num1 ;
    preferShort=PreferFull
    } ;
  -- : Quant ;       -- a (house), (houses)
  IndefArt = {
    s=\\gn,anim,cas=>[] ;
    type=EmptyIndef ;
    short=\\a=>[] ;
    c=Nom ;
    size=Num1 ;
    preferShort=PreferFull
    } ;

  QuantityNP n m = {
    s = \\cas => preOrPost m.isPre m.s n.s;
    pron=False ;
    a=Ag (gennum Masc (numSizeNumber n.size)) P3
    } ;

}
