concrete NounAra of Noun = CatAra ** open ResAra, Prelude in {

flags optimize=noexpand ;

lin

  DetCN det cn = let {
    number = sizeToNumber det.n;
    state = possState det.d;
    determiner : Case -> Str = \c ->
      det.s ! cn.h ! (detGender cn.g det.n) ! c;
    noun : Case -> NTable -> Str = \c,nt -> 
      let cas = if_then_else Case det.is1sg Bare c -- no case vowel with 1sg poss. suff.
       in nt ! number 
             ! nounState det.d number 
             ! nounCase cas det.n det.d
    } in {
      s = \\c =>
        case cnB4det det.isPron det.isNum det.n det.d of {
          False => determiner c 
                ++ noun c cn.s -- deal with poss. suffix
                ++ cn.adj ! number ! state ! c -- normal case+state
                ++ cn.np ! c ;
          True => noun c cn.s -- deal with poss. suffix
            --   ++ determiner c    -- or this? 
               ++ det.s ! cn.h ! cn.g ! c
               ++ cn.adj ! number ! state ! c -- normal case+state
               ++ cn.np ! c
        };  
      a = { pgn = agrP3 cn.h cn.g number;
            isPron = False }
    };

  UsePN pn = {
    s =  pn.s;
    a = {pgn = (Per3 pn.g Sg); isPron = False }
    };

  UsePron p = p ;

  PredetNP pred np = {
    s = \\c => case pred.isDecl of {
      True => pred.s!c ++ np.s ! Gen ; -- akvaru l-awlAdi
      False => pred.s!c ++ np.s ! c
      };
    a = np.a
    } ;
  {-
  --should compile.. not working :( wierd error message.. bug?
  PPartNP np v2 =
    let x = case np.a.pgn of {
      Per3 g n =>  ( positAdj (v2.s ! VPPart) ) ! g ! n ! Indef ;
  _ => \\_ => [] -- not occuring anyway
    } in {
      s = \\c => np.s ! c ++ x ! c ;
  a = np.a
    };
  -}

  -- FIXME try parsing something like "this house now" and you'll get
  -- an internal compiler error, but it still works.. wierd..
  AdvNP np adv = {
    s = \\c => np.s ! c ++ adv.s;
    a = np.a
    };
{-
  DetSg quant ord = {
    s = \\h,g,c =>
      quant.s ! Sg ! h ! g ! c ++ ord.s ! g ! quant.d ! c ;
    n = One;
    d = quant.d;
    isPron = quant.isPron;
    isNum =
      case ord.n of {
        None => False;
        _ => True
      }
    } ;
-}

  DetQuantOrd quant num ord = quant ** {
    s = \\h,g,c => quant.s ! Pl ! h ! g ! c
      ++ num.s ! g ! (toDef quant.d num.n) ! c
      --FIXME check this:
      ++ ord.s ! g ! (toDef quant.d num.n) ! c ;
    n = num.n;
    isNum = orB num.isNum ord.isNum ;
    -- ord may come from OrdDigits or OrdNumeral
    -- num may come from NumCard : Card -> Num

    } ;

  DetQuant quant num = quant ** {
    s = \\h,g,c => quant.s ! Pl ! h ! g ! c
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
    is1sg = case p.a.pgn of { Per1 _ => True ; _ => False } ;
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

  MassNP cn = ---- AR
    {s = \\c => cn.s ! Sg ! Indef ! c ++ cn.adj ! Sg ! Indef ! c ; 
     a = {pgn = Per3 cn.g Sg ; isPron = False}} ;

--  MassDet = {s = \\_,_,_,_ => [] ; d = Indef;
--             isNum = False; isPron = False} ;

  UseN,
  UseN2 = \n -> n ** {
    adj = \\_,_,_ => [];
    np  = \\_     => []};
  Use2N3 n3 = n3 ;
  Use3N3 n3 = n3 ** {c2 = n3.c3} ;

  ComplN2 n2 np = UseN n2 **  --- IL
    {s = \\n,s,c => n2.s ! n ! s ! c ++ n2.c2 ++ np.s ! Gen} ;


  ComplN3 n3 np = ComplN2 n3 np ** {c2 = n3.c3} ;

  AdjCN ap cn = cn ** {
    adj = \\n,d,c => ap.s ! cn.h ! cn.g ! n ! (definite ! d) ! c 
    };
  --    RelCN cn rs = {s = \\n,c => cn.s ! n ! c ++ rs.s ! {n = n ; p = P3}} ;
  --    AdvCN cn ad = {s = \\n,c => cn.s ! n ! c ++ ad.s} ;
  --
  --    SentCN cn sc = {s = \\n,c => cn.s ! n ! c ++ sc.s} ;
  ApposCN cn np = cn ** { np = \\c => cn.np ! c ++ np.s ! c } ;

  -- : CN -> NP -> CN ;     -- house of Paris, house of mine
  PossNP cn np = cn ** {
    s = \\n,_d,c => cn.s ! n ! Const ! c ;
    np = \\c => cn.np ! c ++ np.s ! Gen
    };


  -- : CN -> NP -> CN ;     -- glass of wine
  --PartNP
}
