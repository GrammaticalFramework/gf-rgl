concrete NounKaz of Noun = CatKaz ** open Prelude, ResKaz in {
  lin
    UseN n = n ;
    AdjCN ap cn = cn ** {
      s=\\c,n => ap.s ++ cn.s ! c ! n;
      poss=\\o,p,n => ap.s ++ cn.poss ! o ! p ! n
    } ;
    AdvCN cn adv = cn ** {
      s=\\c,n => adv.s ++ cn.s ! c ! n;
      poss=\\o,p,n => adv.s ++ cn.poss ! o ! p ! n
    } ;
    RelCN cn rs = cn ** {
      s=\\c,n => rs.s ++ cn.s ! c ! n;
      poss=\\o,p,n => rs.s ++ cn.poss ! o ! p ! n
    } ;
    SentCN cn sc = cn ** {
      s=\\c,n => sc.s ++ cn.s ! c ! n;
      poss=\\o,p,n => sc.s ++ cn.poss ! o ! p ! n
    } ;
    ApposCN cn np = cn ** {
      s=\\c,n => cn.s ! c ! n ++ np.s ! c;
      poss=\\o,p,n => cn.poss ! o ! p ! n ++ np.s ! Nom
    } ;
    ComplN2 n np = n ** {
      s=\\c,k => complNP n.c2 np ++ n.s ! c ! k;
      poss=\\o,p,k => complNP n.c2 np ++ n.poss ! o ! p ! k
    } ;
    ComplN3 n np = n ** {
      s=\\c,k => complNP n.c2 np ++ n.s ! c ! k;
      poss=\\o,p,k => complNP n.c2 np ++ n.poss ! o ! p ! k; c2=n.c3
    } ;
    Use2N3 n = {
      s=n.s;
      poss=n.poss;
      c2=n.c2
    } ;
    Use3N3 n = {
      s=n.s;
      poss=n.poss;
      c2=n.c3
    } ;
    UseN2 n = {
      s=n.s;
      poss=n.poss
    } ;
    UsePN pn = {s=\\_ => pn.s;a=defaultAgr} ;
    UsePron p = p ;
    DetCN det cn = {s=\\c => det.s ++ case det.poss of {
      NoPoss => cn.s ! c ! det.n; Poss p n => possForm cn n (nounPerson p) det.n};
      a={p=P3;n=det.n}} ;
    MassNP cn = {s=\\c => cn.s ! c ! Sg;a=defaultAgr} ;
    DetQuant q num = {s=q.s ++ num.s;n=num.n;poss=q.poss} ;
    DetQuantOrd q num ord = {s=q.s ++ num.s ++ ord.s;n=num.n;poss=q.poss} ;
    DefArt = {s=[];poss=NoPoss} ; IndefArt = {s=[];poss=NoPoss} ;
    PossPron p = {s=[];poss=Poss p.a.p p.a.n} ;
    NumSg = {s=[];n=Sg} ; NumPl = {s=[];n=Pl} ;
    NumCard c = {s=c.s;n=Sg} ; NumNumeral n = {s=n.s;n=Sg} ; NumDecimal n = {s=n.s;n=Sg} ;
    AdNum a n = {s=a.s ++ n.s} ; OrdNumeral n = {s=n.s} ; OrdDigits n = {s=n.s} ;
    OrdSuperl a = {s="ең" ++ a.s} ; OrdNumeralSuperl n a = {s=n.s ++ "ең" ++ a.s} ;
    AdvNP np adv = {s=\\c => np.s ! c ++ adv.s;a=np.a} ;
    ExtAdvNP np adv = {s=\\c => np.s ! c ++ "," ++ adv.s;a=np.a} ;
    PredetNP p np = {s=\\c => p.s ++ np.s ! c;a=np.a} ;
    PossNP cn np = cn ** {s=\\c,n => np.s ! Gen ++ possForm cn np.a.n (nounPerson np.a.p) n;
                          poss=\\o,p,n => np.s ! Gen ++ cn.poss ! o ! p ! n} ;
    PartNP cn np = cn ** {s=\\c,n => np.s ! Ablat ++ cn.s ! c ! n;
                          poss=\\o,p,n => np.s ! Ablat ++ cn.poss ! o ! p ! n} ;
    CountNP det np = {s=\\c => det.s ++ np.s ! c;a={p=P3;n=det.n}} ;
    DetDAP det = {s=det.s} ; AdjDAP dap ap = {s=dap.s ++ ap.s} ;
    QuantityNP dec mu = {s=\\_ => dec.s ++ mu.s;a={p=P3;n=Pl}} ;
}
