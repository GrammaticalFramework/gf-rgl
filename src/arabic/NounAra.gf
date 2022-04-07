concrete NounAra of Noun = CatAra ** open ResAra, Prelude in {

flags optimize=noexpand ;

lin

  DetCN det cn = let {
    cas : Case -> Case = if_then_else Case det.is1sg Bare ;
    number = case cn.isDual of {
                True =>
                  case sizeToNumber det.n of {
                         Sg => Sg ;
                         _  => Dl } ;
                False => sizeToNumber det.n } ;
    determiner : Case -> Str = \c ->
      det.s ! cn.h ! detGender cn.g det.n ! c ;
    noun : Case -> Str = \c ->
      cn.s ! number
           ! nounState det.d number
           ! nounCase c det.n det.d ;
    adj : Case -> Str = \c ->
      cn.s2 ! number
            ! (definite ! det.d) -- Indef remains Indef, rest become Def
            ! c
    } in emptyNP ** {
      s = \\c => -- Dat is just a hack for liPrep
        let c' = case c of {Dat => Gen ; x => x} in
        case cnB4det det of {
          False => determiner c'
                ++ noun c'
                ++ adj c'
                ++ cn.np ! c' ;
          True => noun (cas c) -- deal with possessive suffix + dative hack
               ++ determiner c'
               ++ adj c'
               ++ cn.np ! c'
        };
      a = { pgn = agrP3 cn.h cn.g number;
            isPron = False } ;
      isHeavy = cn.isHeavy
    } ;

  UsePN pn = emptyNP ** {
    s = pn.s;
    a = {pgn = Per3 pn.g Sg ; isPron = False}
    };

  UsePron p = p ;

  DetNP det = case det.isEmpty of {
    True => case <det.d,det.n> of {  -- if the s field is empty, make up some other determiner
              <Def,One>   => he_Pron ;
              <Def,_>     => theyMasc_Pron ;
              <Indef,One> => emptyNP ** {s = someSg_Det.s ! NoHum ! Masc} ;
              _           => emptyNP ** {s = somePl_Det.s ! NoHum ! Masc}
            } ;
    False => emptyNP ** {s = det.s ! NoHum ! Masc} } ;

  PredetNP det np = np ** {
    s = \\c => case det.isDecl of {
      True  => det.s ! c ++ bindIf np.a.isPron ++ np.s ! Gen ; -- akvaru l-awlAdi
      False => det.s ! c ++ np.s ! c
      } ;
    a = np.a ** {isPron=False}
    } ;

{-
  PPartNP np v2 =
    let x = case np.a.pgn of {
      Per3 g n => positAdj (v2.s ! VPPart) ) ! g ! n ! Indef ; -- doesn't work because trying to glue runtime tokens
      Per2 g n => \\_ => [] ;
      _        => \\_ => []
    } in np ** {
      s = \\c => np.s ! c ++ v2.s ! VPPart ---- TODO: agreement
      };
-}

  AdvNP np adv = np ** {
    s = \\c => np.s ! c ++ adv.s ;
    isHeavy = True ;
    };

  DetQuantOrd quant num ord = quant ** {
    s = \\h,g,c => let d = toDef quant.d num.n in
         quant.s ! Pl ! h ! g ! c -- TODO what is this Pl? Was there when I started /IL
      ++ num.s ! g ! d ! c
      --FIXME check this:
      ++ ord.s ! g
               ! case d of {Poss => Def ; _ => d}
               ! c ;
    n = num.n;
    isNum = orB num.isNum ord.isNum  ;
    -- ord may come from OrdDigits or OrdNumeral
    -- num may come from NumCard : Card -> Num
    isEmpty = False
    } ;

  DetQuant quant num = quant ** {
    s = \\h,g,c => quant.s ! sizeToNumber num.n ! h ! g ! c
      ++ num.s ! g ! (toDef quant.d num.n) ! c ;
    n = num.n;
    isNum = -- Num may come from NumCard : Card -> Num
      case num.n of {
        None => False;
        _    => num.isNum
      } ;
    isEmpty =
      case quant.isEmpty of {
        True => notB num.isNum ;
        _    => False }
    } ;

  PossPron p = baseQuant ** {
    s = \\_,_,_,_ => BIND ++ p.s ! Gen;
    d = Poss;
    is1sg = is1sg p.a ;
    isPron = p.a.isPron} ;

  NumSg = {
    s = \\_,_,_ => [] ;
    n = One ;
    isNum = False } ;

  NumPl = {
    s = \\_,_,_ => [] ;
    n = None ;
    isNum = False } ;

  NumDigits digits = digits ** {
    s = \\_,_,_ => digits.s ;
    isNum = True
    };

  NumNumeral numeral = numeral ** {
    s = numeral.s ! NCard ;
    isNum = True
    };

  NumCard n = n ;

  AdNum adn num = num ** {
    s = \\g,d,c => adn.s ++ num.s ! g ! d ! c ;
    } ;

  OrdDigits digits = digits ** {
    s = \\_,d,_ => Al ! d ++ digits.s;
    isNum = True
    };

  -- OrdNumeral : Numeral -> Ord ; -- fifty-first
  OrdNumeral numeral = numeral ** {
    s = numeral.s ! NOrd ;
    isNum = True
    };

  -- FIXME, "the biggest house" would better translate into
  -- akbaru baytin rather than al-baytu l-2akbaru
  -- DetCN (DetSg DefArt (OrdSuperl big_A)) (UseN house_N)
  OrdSuperl a = {
    s = \\_,d,c => a.s ! AComp d c;
    n = One ;
    isNum = False
    } ;

  DefArt = {
    s = \\_,_,_,_ => [];
    d = Def ;
    isNum,isPron,is1sg = False ;
    isEmpty = True
    } ;

  IndefArt = {
    s = \\_,_,_,_ => [];
    d = Indef ;
    isNum,isPron,is1sg = False ;
    isEmpty = True
    } ;

  MassNP cn = emptyNP ** {
    s = \\c => cn2str cn Sg Indef c ;
    a = {pgn = Per3 cn.g Sg ; isPron = False} ;
    isHeavy = cn.isHeavy ;
    } ;

  UseN,
  UseN2 = useN ;
  Use2N3 n3 = n3 ;
  Use3N3 n3 = n3 ** {c2 = n3.c3} ;

  ComplN2 n2 np = UseN n2 ** {
    np = \\c => n2.c2.s ++ bindIf n2.c2.binds ++ np.s ! n2.c2.c
    } ;

  ComplN3 n3 np = ComplN2 n3 np ** {c2 = n3.c3} ;

  AdjCN ap cn = cn ** {
    s2 = \\n,d,c => cn.s2 ! n ! d ! c ++ ap.s ! cn.h ! cn.g ! n ! (definite ! d) ! c
    };

  RelCN cn rs = cn ** {
    s2 = \\n,s,c => cn.s2 ! n ! s ! c
        ++ rs.s ! Per3 cn.g n ! c ;
    isHeavy = True
    } ;


  RelNP np rs = np ** {
    s = \\c => np.s ! c ++ rs.s ! np.a.pgn ! c ;
    isHeavy = True
    } ;

  AdvCN,
  SentCN = \cn,ss -> cn ** {
    np = \\c => cn.np ! c ++ ss.s ;
    isHeavy = True
    } ;

  ApposCN cn np = cn ** {
    np = \\c => cn.np ! c ++ np.s ! c
    } ;

  -- : CN -> NP -> CN ;     -- house of Paris, house of mine
  PossNP cn np = cn ** {
    s  = \\n,d,c => cn.s  ! n ! case d of {Poss=>d ; _=>Const} ! c ;
    s2 = \\n,d,c => cn.s2 ! n ! case d of {Poss=>d ; _=>Const} ! Gen ;
    np = \\c => cn.np ! c
          ++ case is1sg np.a of {
                True => "لَدَي" ++ np.empty ;
                False =>
                  case np.a.isPron of {
                    True => "لَدَي" ++ BIND ++ np.s ! Gen ;
                    False =>  np.s ! Gen }
             }
    };

  -- : CN -> NP -> CN ;     -- glass of wine
  --PartNP
}
