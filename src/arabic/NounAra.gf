concrete NounAra of Noun = CatAra ** open ResAra, Prelude in {

flags optimize=noexpand ;

lin

  DetCN det cn = let {
    cas : Case -> Case = if_then_else Case det.is1sg Bare ;
    number = case cn.isDual of {True => Dl ; _ => sizeToNumber det.n} ;
    determiner : Case -> Str = \c ->
      det.s ! cn.h ! (detGender cn.g det.n) ! c ;
    noun : Case -> Str = \c -> 
      cn.s ! number 
           ! nounState det.d number 
           ! nounCase c det.n det.d ;
    adj : Case -> Str = \c ->
      cn.s2 ! number
            ! (definite ! det.d) -- Indef remains Indef, rest become Def
            ! c
    } in {
      s = \\c =>
        case cnB4det det.isPron det.isNum det.n det.d of {
          False => determiner c 
                ++ noun c 
                ++ adj c
                ++ cn.np ! c ; 
          True => noun (cas c) -- deal with possessive suffix
               ++ determiner c -- (nounCase c det.n det.d) --??
               ++ adj c
               ++ cn.np ! c
        };  
      a = { pgn = agrP3 cn.h cn.g number;
            isPron = False } ;
      empty = []
    };

  UsePN pn = {
    s =  pn.s;
    a = {pgn = Per3 pn.g Sg ; isPron = False} ;
    empty = []
    };

  UsePron p = p ;

  DetNP det = emptyNP ** {s = det.s ! NoHum ! Masc} ; ----

  PredetNP pred np = np ** {
    s = \\c => case pred.isDecl of {
      True => pred.s!c ++ np.s ! Gen ; -- akvaru l-awlAdi
      False => pred.s!c ++ np.s ! c
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
    s = \\c => np.s ! c ++ adv.s
    };

  DetQuantOrd quant num ord = quant ** {
    s = \\h,g,c => let d = toDef quant.d num.n in
         quant.s ! Pl ! h ! g ! c
      ++ num.s ! g ! d ! c
      --FIXME check this:
      ++ ord.s ! g 
               ! case d of {Poss => Def ; _ => d}
               ! c ;
    n = num.n;
    isNum = orB num.isNum ord.isNum ;
    -- ord may come from OrdDigits or OrdNumeral
    -- num may come from NumCard : Card -> Num

    } ;

  DetQuant quant num = quant ** {
    s = \\h,g,c => quant.s ! sizeToNumber num.n ! h ! g ! c
      ++ num.s ! g ! (toDef quant.d num.n) ! c ;
    n = num.n;
    isNum = -- Num may come from NumCard : Card -> Num
      case num.n of {
        None => False;
        _    => num.isNum
      }
    } ;

  PossPron p = {
    s = \\_,_,_,_ => p.s ! Gen;
    d = Poss;
    is1sg = case p.a.pgn of { Per1 Sing => True ; _ => False } ;
    isPron = True;
    isNum = False } ;

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
    isNum,isPron,is1sg = False
    } ;

  IndefArt = {
    s = \\_,_,_,_ => [];
    d = Indef ;
    isNum,isPron,is1sg = False
    } ;

  MassNP cn =
    {s = \\c => cn2str cn Sg Indef c ; 
     a = {pgn = Per3 cn.g Sg ; isPron = False} ;
     empty = []} ;


  UseN,
  UseN2 = useN ;
  Use2N3 n3 = n3 ;
  Use3N3 n3 = n3 ** {c2 = n3.c3} ;

  ComplN2 n2 np = UseN n2 ** {np=np.s} ;

  ComplN3 n3 np = ComplN2 n3 np ** {c2 = n3.c3} ;

  AdjCN ap cn = cn ** {
    s2 = \\n,d,c => cn.s2 ! n ! d ! c ++ ap.s ! cn.h ! cn.g ! n ! (definite ! d) ! c 
    };

  RelCN cn rs = cn ** {
    s2 = \\n,s,c => cn.s2 ! n ! s ! c ++ rs.s ! {pgn=Per3 cn.g n ; isPron=False} ! c};

  RelNP np rs = np ** {s = \\c => np.s ! c ++ rs.s ! np.a ! c} ;

  AdvCN,
  SentCN = \cn,ss -> cn ** {s2 = \\n,d,c => cn.s2 ! n ! d ! c ++ ss.s} ;

  ApposCN cn np = cn ** { np = \\c => cn.np ! c ++ np.s ! c } ;

  -- : CN -> NP -> CN ;     -- house of Paris, house of mine
  PossNP cn np = cn ** {
    s  = \\n,_d,c => cn.s  ! n ! Const ! c ;
    s2 = \\n,_d,c => cn.s2 ! n ! Const ! c ;
    np = \\c => cn.np ! c ++ np.s ! Gen
    };

  -- : CN -> NP -> CN ;     -- glass of wine
  --PartNP
}
