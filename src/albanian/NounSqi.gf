concrete NounSqi of Noun = CatSqi ** open MorphoSqi, ResSqi, Prelude in {

  lin
    DetCN det cn = {
      s = \\c => det.s ! c ! cn.g ++ cn.s ! det.sp ! c ! det.n ++
                  det.post ! det.sp ! c ! cn.g ;
      a = agrgP3 cn.g det.n
      } ;

    UsePron p = {s=p.s; a=p.a} ;
    UsePN p = {s=\\_=>p.s; a={gn=GSg Masc;p=P3}} ;
    PredetNP p np = np ** {s=\\c=>p.s ++ np.s ! c} ;
    AdvNP np adv = np ** {s=\\c=>np.s ! c ++ adv.s} ;
    ExtAdvNP np adv = np ** {s=\\c=>np.s ! c ++ SOFT_BIND ++ "," ++ adv.s} ;
    MassNP cn = {s=\\c=>cn.s ! Indef ! c ! Sg; a=agrgP3 cn.g Sg} ;

    DetQuant quant num = {
      s  = \\c,g => case quant.isPoss of {
        True => num.s;
        False => quant.s ! c ! g ! num.n ++ num.s
        } ;
      post = \\_,c,g => case quant.isPoss of {
        True => quant.s ! c ! g ! num.n;
        False => []
        } ;
      n  = num.n ;
      sp = quant.sp
      } ;

    DetQuantOrd quant num ord = {
      s = \\c,g => case quant.isPoss of {
        True => num.s;
        False => quant.s ! c ! g ! num.n ++ num.s
        } ;
      post = \\_,c,g => case quant.isPoss of {
        True => quant.s ! c ! g ! num.n ++ ord.s ! c ! g ! num.n;
        False => ord.s ! c ! g ! num.n
        } ;
      n=num.n; sp=quant.sp
      } ;

    NumSg = {s = []; n = Sg} ;
    NumPl = {s = []; n = Pl} ;
    NumCard c = {s=c.s; n=Pl} ;
    NumNumeral n = n ;
    NumDecimal n = {s=n.s} ;
    AdNum a n = {s=a.s ++ n.s} ;

    OrdNumeral n = {s=\\_,_,_=>n.s} ;
    OrdDigits n = {s=\\_,_,_=>n.s} ;
    OrdSuperl a = {s=\\c,g,n=>"më" ++
      case a.clit of {
        True => case <c,g,n> of {
          <Nom,Masc,Sg> => "i" ++ a.s ! c ! g ! n;
          <Nom,Fem,Sg> => "e" ++ a.s ! c ! g ! n;
          _ => "të" ++ a.s ! c ! g ! n
          };
        False => a.s ! c ! g ! n
        }} ;
    OrdNumeralSuperl n a = {s=\\c,g,num=>n.s ++ a.s ! c ! g ! num} ;

    DefArt = {
      s  = \\c,g,n => [] ;
      sp = Def;
      isPoss = False
      } ;

    IndefArt = {
      s = \\c,g => table Number ["një"; []] ;
      sp = Indef;
      isPoss = False
      } ;

    UseN n = n ;

    ComplN2 n np = {
      s=\\sp,c,num=>n.s ! sp ! c ! num ++ n.c2.s ++ np.s ! n.c2.c;
      g=n.g
      } ;
    ComplN3 n np = n ** {s=\\sp,c,num=>n.s ! sp ! c ! num ++ n.c2.s ++ np.s ! n.c2.c; c2=n.c3} ;
    Use2N3 n = n ** {c2=n.c2} ;
    Use3N3 n = n ** {c2=n.c3} ;
    RelCN cn rs = cn ** {s=\\sp,c,n=>cn.s ! sp ! c ! n ++ rs.s ! agrgP3 cn.g n} ;
    AdvCN cn adv = cn ** {s=\\sp,c,n=>cn.s ! sp ! c ! n ++ adv.s} ;
    SentCN cn sc = cn ** {s=\\sp,c,n=>cn.s ! sp ! c ! n ++ sc.s} ;
    ApposCN cn np = cn ** {s=\\sp,c,n=>cn.s ! sp ! c ! n ++ np.s ! c} ;
    PossNP cn np = cn ** {
      s=\\sp,c,n=>cn.s ! sp ! c ! n ++
                    link_clitic ! sp ! c ! cn.g ! n ++ np.s ! Ablat
      } ;
    PartNP cn np = cn ** {s=\\sp,c,n=>cn.s ! sp ! c ! n ++ "me" ++ np.s ! Acc} ;
    CountNP det np = {s=\\c=>det.s ! c ! Masc ++ "nga" ++ np.s ! Ablat; a=agrgP3 Masc det.n} ;

    PossPron p = {
      s=p.poss;
      sp=Def;
      isPoss=True
      } ;

    DetDAP det = {
      s=det.s ! Nom ! Masc ++ det.post ! det.sp ! Nom ! Masc;
      n=det.n
      } ;
    AdjDAP dap ap = {s=dap.s ++ ap.s ! Indef ! Nom ! Masc ! dap.n; n=dap.n} ;
    QuantityNP dec mu = {s=\\_=>case mu.isPre of {True=>mu.s++dec.s; False=>dec.s++mu.s}; a=agrgP3 Masc Pl} ;

    AdjCN ap cn = {
      s = \\sp,c,n => cn.s ! sp ! c ! n ++ ap.s ! sp ! c ! cn.g ! n ;
      g = cn.g
      } ;

}
