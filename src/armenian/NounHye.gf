concrete NounHye of Noun = CatHye ** open Prelude, ResHye in {
  lin AdjCN ap cn = {
        s = \\sp,c,n =>
                case ap.isPre of {
                  True  => ap.s ! Indef ! Nom ! Sg ++ cn.s ! sp ! c ! n;
                  False => cn.s ! sp ! c ! n ++ ap.s ! Indef ! Nom ! Sg
                }
      } ;
  lin AdvCN cn adv = {
        s = \\sp,c,num => adv.s ++ cn.s ! sp ! c ! num
      } ;
  lin AdvNP np adv = {s = \\c => np.s ! c ++ adv.s; a = np.a} ;
  lin ExtAdvNP np adv = {s = \\c => np.s ! c ++ "," ++ adv.s; a = np.a} ;
  lin PredetNP pred np = {s = \\c => pred.s ++ np.s ! c; a = np.a} ;
  lin UsePN pn = {s = \\_ => pn.s; a = {n = Sg; p = P3}} ;
  lin DefArt = {s = []; sp = Def} ;
  lin DetCN det cn = {s = \\c => det.s ++ cn.s ! det.sp ! c ! det.n;
                      a = {n=det.n; p=P3}} ;
  lin DetQuant quant num = {s = quant.s ++ num.s; n = num.n; sp=quant.sp} ;
  lin DetQuantOrd quant num ord = {
        s = quant.s ++ num.s ++ ord.s; n = num.n; sp = quant.sp
      } ;
  lin IndefArt = {s = []; sp = Indef} ;
  lin NumPl = {s = []; n = Pl} ;
  lin NumSg = {s = []; n = Sg} ;
  lin NumCard card = {s = card.s; n = Pl} ;
  lin NumDigits digits = {s = digits.s ! NCard} ;
  lin NumDecimal decimal = {s = decimal.s ! NCard} ;
  lin NumNumeral numeral = {s = numeral.s ! NCard} ;
  lin AdNum adn card = {s = adn.s ++ card.s} ;
  lin OrdDigits digits = {s = digits.s ! NOrd} ;
  lin OrdNumeral numeral = {s = numeral.s ! NOrd} ;
  lin OrdSuperl a = {s = "ամենա" ++ a.s ! Nom ! Sg} ;
  lin OrdNumeralSuperl numeral a = {s = numeral.s ! NOrd ++ "ամենա" ++ a.s ! Nom ! Sg} ;
  lin MassNP cn = {
        s = \\c => cn.s ! Indef ! c ! Sg;
        a = {n=Sg; p=P3}
      } ;
  lin ComplN2 n np = {
        s = \\sp,c,num => (case n.c2.isPre of {
                             True => n.c2.s ++ np.s ! n.c2.c;
                             False => np.s ! n.c2.c ++ n.c2.s
                           }) ++
                           case <sp,c> of {
                             <Def,Nom> => n.def_nom ! num;
                             <Def,Dat> => n.def_dat ! num;
                             <Poss P1,_> => n.poss1 ! c ! num;
                             <Poss P2,_> => n.poss2 ! c ! num;
                             _ => n.s ! c ! num
                           }
      } ;
  lin ComplN3 n np = n ** {c2 = n.c3} ;
  lin Use2N3 n = lin N2 {s=n.s;def_dat=n.def_dat;def_nom=n.def_nom;
    poss1=n.poss1;poss2=n.poss2;c2=n.c2} ;
  lin Use3N3 n = lin N2 {s=n.s;def_dat=n.def_dat;def_nom=n.def_nom;
    poss1=n.poss1;poss2=n.poss2;c2=n.c3} ;
  lin PossNP cn np = {
        s = \\sp,c,num => np.s ! Dat ++ cn.s ! sp ! c ! num
      } ;
  lin PartNP cn np = {
        s = \\sp,c,num => cn.s ! sp ! c ! num ++ np.s ! Ablat
      } ;
  lin SentCN cn sc = {s = \\sp,c,n => cn.s ! sp ! c ! n ++ sc.s} ;
  lin ApposCN cn np = {s = \\sp,c,n => cn.s ! sp ! c ! n ++ np.s ! Nom} ;
  lin RelCN cn rs = {s = \\sp,c,n => cn.s ! sp ! c ! n ++ rs.s} ;
  lin CountNP det np = {s = \\c => det.s ++ np.s ! c; a = {n=det.n;p=P3}} ;
  lin DetDAP det = {s = det.s} ;
  lin AdjDAP dap ap = {s = dap.s ++ ap.s ! Indef ! Nom ! Sg} ;
  lin QuantityNP decimal mu = {
        s = \\_ => case mu.isPre of {
          True => mu.s ++ decimal.s ! NCard;
          False => decimal.s ! NCard ++ mu.s
          };
        a = {n=Pl;p=P3}
      } ;
  lin PossPron pron = {s = pron.empty; sp = Poss pron.a.p} ;
  lin UseN n = {
        s = \\sp,c,num =>
                case <sp,c> of {
                  <Def,Nom> => n.def_nom ! num ;
                  <Def,Dat> => n.def_dat ! num ;
                  <Poss P1,_> => n.poss1 ! c ! num ;
                  <Poss P2,_> => n.poss2 ! c ! num ;
                  _         => n.s ! c ! num
                }
      } ;
  lin UsePron pron = {s = pron.s; a=pron.a} ;
}
