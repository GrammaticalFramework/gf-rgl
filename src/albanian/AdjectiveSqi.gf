concrete AdjectiveSqi of Adjective = CatSqi ** open ResSqi, Prelude in {

  oper
    adjForm : A -> Species => Case => Gender => Number => Str = \a -> \\sp,c,g,n =>
      case a.clit of {
        True  => link_clitic ! sp ! c ! g ! n ++ a.s ! c ! g ! n ;
        False => a.s ! c ! g ! n
      } ;

  lin
    PositA  a = {
      s = adjForm a
    } ;

    ComparA a np = {s=\\sp,c,g,n=>"më" ++ adjForm a ! sp ! c ! g ! n ++ "se" ++ np.s ! Ablat} ;
    UseComparA a = {s=\\sp,c,g,n=>"më" ++ adjForm a ! sp ! c ! g ! n} ;
    ComplA2 a np = {s=\\sp,c,g,n=>adjForm (lin A a) ! sp ! c ! g ! n ++ a.c2.s ++ np.s ! a.c2.c} ;
    UseA2 a = PositA a ;
    ReflA2 a = {s=\\sp,c,g,n=>adjForm (lin A a) ! sp ! c ! g ! n ++ a.c2.s ++ "vetveten"} ;
    CAdvAP cadv ap np = {s=\\sp,c,g,n=>cadv.s ++ ap.s ! sp ! c ! g ! n ++ cadv.p ++ np.s ! Ablat} ;
    AdjOrd ord = {s=\\_,c,g,n=>ord.s ! c ! g ! n} ;
    SentAP ap sc = {s=\\sp,c,g,n=>ap.s ! sp ! c ! g ! n ++ "që" ++ sc.s} ;
    AdAP ada ap = {s=\\sp,c,g,n=>ada.s ++ ap.s ! sp ! c ! g ! n} ;
    AdvAP ap adv = {s=\\sp,c,g,n=>ap.s ! sp ! c ! g ! n ++ adv.s} ;

}
