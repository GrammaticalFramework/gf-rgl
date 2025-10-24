concrete AdjectiveMkd of Adjective = CatMkd ** open Prelude,ResMkd in {

  lin AdAP a ap = {s = \\s,g => a.s ++ ap.s ! s ! g;
                   isPre = ap.isPre} ;
  lin AdjOrd o = {s = o.s; isPre = False} ;
  lin AdvAP ap adv = {
        s = \\s,g => ap.s ! s ! g ++ adv.s;
        isPre = False
      } ;
  lin CAdvAP c ap np = {s = \\s,g => c.s
                                       ++ ap.s ! s ! g ++ np.s ! RSubj;
                        isPre = ap.isPre} ;
  lin ComparA a np = {s = \\s,g => "по" ++ BIND ++ a.s ! s ! g ++ "од" ++ np.s ! RPrep;
                      isPre = False} ;
  lin ComplA2 a2 np = {s = \\s,g => a2.s ! s ! g ++ a2.c2.s ++ np.s ! RObj a2.c2.c;
                       isPre = False} ;
  lin PositA a = a ** {isPre = True} ;
  lin ReflA2 a2 = {s = \\s,g => a2.s ! s ! g; isPre = False} ;
  lin SentAP ap sc = {s = \\s,g => ap.s ! s ! g ++ sc.s;
                      isPre = ap.isPre} ;
  lin UseA2 a2 = {s = a2.s; isPre = True} ;
  lin UseComparA a = {s = \\s,g => "по" ++ BIND ++ a.s ! s ! g; isPre = True} ;
}
