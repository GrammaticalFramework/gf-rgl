concrete NounMkd of Noun = CatMkd ** open Prelude,ResMkd in {
  lin AdNum a c = {s = a.s ++ c.s} ;
  lin AdjCN ap cn = {
       s = case ap.isPre of {
             True  => \\s,n => ap.s ! s
                                    ! case n of {
                                        Sg => GSg cn.g;
                                        Pl => GPl
                                      } ++
                               cn.s ! Indef ! n;
             False => \\s,n => cn.s ! s ! n ++
                               ap.s ! Indef
                                    ! case n of {
                                        Sg => GSg cn.g;
                                        Pl => GPl
                                      }
           };
       vocative = \\n => ap.s ! Indef
                              ! case n of {
                                  Sg => GSg cn.g;
                                  Pl => GPl
                                } ++
                         cn.vocative ! n;
       count_form = ap.s ! Indef ! GPl ++ cn.count_form; g = cn.g} ;
  lin AdjDAP d ap = {
        s = \\g => d.s ! g ++ ap.s ! Indef ! genNum g (nnum2num d.n) ;
        n = d.n ;
        sp = d.sp
      } ;
  lin AdvCN cn adv = {
        s = \\s,n => cn.s ! s ! n ++ adv.s;
        count_form = cn.count_form ++ adv.s;
        vocative = \\n => cn.vocative ! n ++ adv.s;
        g = cn.g
      } ;
  lin AdvNP np a = {s = \\r => np.s ! r ++ a.s;
                    vocative = np.vocative ++ a.s; a = np.a} ;
  lin ApposCN cn np = {s = \\s,n => cn.s ! s ! n ++ np.s ! RSubj;
                       count_form = cn.count_form ++ np.s ! RSubj;
                       vocative = \\n => cn.vocative ! n ++ np.vocative; g = cn.g} ;
  lin ComplN2 n2 np = {s = \\s,n => n2.s ! s ! n ++ n2.c2.s ++ np.s ! RObj n2.c2.c;
                       count_form = n2.count_form ++ n2.c2.s ++ np.s ! RObj n2.c2.c;
                       vocative = \\n => n2.vocative ! n ++ n2.c2.s ++ np.s ! RObj n2.c2.c;
                       g = n2.g} ;
  lin ComplN3 n3 np = {s = \\s,n => n3.s ! s ! n ++ n3.c2.s ++ np.s ! RObj n3.c2.c;
                       count_form = n3.count_form ++ n3.c2.s ++ np.s ! RObj n3.c2.c;
                       vocative = \\n => n3.vocative ! n ++ n3.c2.s ++ np.s ! RObj n3.c2.c;
                       rel = \\s,g => n3.s ! s ! genNum2num g ++ n3.c2.s ++ np.s ! RObj n3.c2.c;
                       relType = AdvMod;
                       g = n3.g;
                       c2 = n3.c3} ;
  lin CountNP d np = {s = \\r => d.s ! Masc ++ np.s ! r;
                      vocative = d.s ! Masc ++ np.vocative;
                      a = np.a} ;
  lin DefArt = {s = \\_=>[]; sp = Def Unspecified} ;
  lin DetCN det cn = {
        s = \\r => case det.n of {
                     NNum n     => det.s ! cn.g ++ cn.s ! det.sp ! n;
                     NCountable => det.s ! cn.g ++ cn.count_form
                   } ;
        vocative = det.s ! cn.g ++ cn.vocative ! nnum2num det.n ;
        a = {g = genNum cn.g (nnum2num det.n) ;
             p = P3};
      } ;
  lin DetDAP d = d ;
  lin DetNP d = {s = \\r => d.s ! Masc; vocative = d.s ! Masc;
                 a = {g = GSg Masc; p = P1}} ;
  lin DetQuant q num = {s = \\g => q.s ! genNum g (nnum2num num.n) ++ num.s;
                        n = num.n;
                        sp = q.sp} ;
  lin DetQuantOrd q n o = {
        s = \\g => q.s ! GSg Masc ++ n.s ++ o.s ! q.sp ! genNum g (nnum2num n.n);
        n = n.n;
        sp = Indef
      } ;
  lin ExtAdvNP np a = {
        s = \\r => np.s ! r ++ SOFT_BIND++"," ++ a.s;
        vocative = np.vocative ++ SOFT_BIND++"," ++ a.s;
        a = np.a
      } ;
  lin IndefArt = {s = \\_=>[]; sp = Indef} ;
  lin MassNP cn = {s = \\r => cn.s ! Indef ! Sg;
                   vocative = cn.vocative ! Sg;
                   a = {g = GSg cn.g; p = P3}} ;
  lin NumCard c = {s = c.s; n = NNum Sg} ;
  lin NumDecimal d = {s = d.s} ;
  lin NumDigits d = {s = d.s} ;
  lin NumNumeral n = {s = n.s} ;
  lin NumPl = {s = []; n = NNum Pl} ;
  lin NumSg = {s = []; n = NNum Sg} ;
  lin UseN s = s ;
  lin UsePron p = p ** {vocative=p.s ! RSubj} ;

  lin RelNP np rs = {
        s = \\r => np.s ! r ++ rs.s ! np.a.g ;
        vocative = np.vocative ++ rs.s ! np.a.g ;
        a = np.a;
      } ;
  lin OrdDigits d = {s = \\s,gn => d.s} ;
  lin OrdNumeral n = {s = \\s,gn => n.s} ;
  lin OrdNumeralSuperl n a = {s = \\s,gn => n.s ++ "нај" ++ BIND ++ a.s ! s ! gn} ;
  lin OrdSuperl a = {s = \\s,gn => "нај" ++ BIND ++ a.s ! s ! gn} ;
  lin PPartNP np v2 = {s = \\r => np.s ! r
                                    ++ v2.present ! Imperfective ! Sg ! np.a.p;
                       vocative = np.vocative ++ v2.present ! Imperfective ! Sg ! np.a.p;
                       a = {g = np.a.g; p = np.a.p}} ;
  lin PartNP cn np = {s = \\s,n => cn.s ! s ! n ++ np.s ! RSubj;
                      count_form = cn.count_form ++ np.s ! RSubj;
                      vocative = \\n => cn.vocative ! n ++ np.vocative; g = cn.g} ;
  lin PossNP cn np = {
        s = \\s,n => cn.s ! s ! n ++ "на" ++ np.s ! RObj Acc;
        count_form = cn.count_form ++ "на" ++ np.s ! RObj Acc;
        vocative = \\n => cn.vocative ! n ++ "на" ++ np.s ! RObj Acc;
        g = cn.g
      } ;
  lin PossPron p = {s = p.poss ! Def Unspecified; sp = Indef} ;
  lin PredetNP p np = {
        s = \\r => p.s ++ np.s ! r;
        vocative = p.s ++ np.vocative;
        a = np.a
      } ;
  lin QuantityNP d mu = 
        let s = case mu.isPre of {
                  True  => mu.s ++ d.s ;
                  False => d.s ++ mu.s
                }
        in { s = \\r => s;
             vocative = s;
             a = {g = GSg Masc; p = P3}
           } ;
  lin RelCN cn rs = {
        s = \\sp,n => cn.s ! sp ! n ++ rs.s ! genNum cn.g n ;
        count_form = cn.count_form ++ rs.s ! GPl ;
        vocative = \\n => cn.vocative ! n ++ rs.s ! genNum cn.g n ;
        g = cn.g;
      } ;
  lin SentCN cn sc = {s = \\s,n => cn.s ! s ! n ++ sc.s;
                      count_form = cn.count_form ++ sc.s;
                      vocative = \\n => cn.vocative ! n ++ sc.s; g = cn.g} ;
  lin Use2N3 n3 = {s = \\s,n => n3.s ! s ! n;
                   count_form = n3.count_form; vocative = \\n => n3.vocative ! n;
                   rel = \\s,g => n3.rel ! s ! g; relType = n3.relType; g = n3.g;
                   c2 = {s = n3.c2.s; c = n3.c2.c}} ;
  lin Use3N3 n3 = {s = \\s,n => n3.s ! s ! n;
                   count_form = n3.count_form; vocative = \\n => n3.vocative ! n;
                   rel = \\s,g => n3.rel ! s ! g; relType = n3.relType; g = n3.g;
                   c2 = {s = n3.c2.s; c = n3.c2.c}} ;
  lin UseN2 n2 = n2 ;
  lin UsePN pn = {
        s = \\r => pn.s;
        vocative = pn.s;
        a = {g = GSg Masc; p = P3}
      } ;
}
