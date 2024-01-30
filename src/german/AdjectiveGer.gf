concrete AdjectiveGer of Adjective = CatGer ** open ResGer, Prelude in {

  flags optimize=all_subs ;

  lin
    PositA  a = {
      s = a.s ! Posit ;
      s2 = \\_ => [] ;
      isPre = True ;
      c = <[],[]> ;
      ext = []
      } ;
    ComparA a np = {
      s = \\af => a.s ! Compar ! af ;
      s2 = \\c => conjThan ++ np.s ! False ! c ++ np.ext ++ np.rc ;
      isPre = True ;
      c = <[],[]> ;
      ext = []
      } ;
    CAdvAP adv ap np = ap ** {
      s = \\af => adv.s ++ ap.s ! af ;
      s2 = \\c => adv.p ++ np.s ! False ! c ++ np.ext ++ np.rc ;
      isPre = True -- HL 1/2023
      } ;
    UseComparA a = {
      s = \\af => a.s ! Compar ! af ;
      s2 = \\_ => [] ;
      isPre = True ;
      c = <[],[]> ;
      ext = []
      } ;

    AdjOrd a = {
      s = a.s ;
      s2 = \\_ => [] ;
      isPre = True ;
      c = <[],[]> ;
      ext = []
      } ; 

-- $SuperlA$ belongs to determiner syntax in $Noun$.

    ComplA2 a np =
      let
        obj = appPrepNP a.c2 np
      in {
        s = a.s ! Posit ;
        s2 = \\_ => [] ;
        isPre = True ;
        c = case a.c2.t of {isCase => <obj, []> ; _ => <[], obj>} ;
        ext = []
      } ;

    ReflA2 a = 
      let
	obj = appPrep a.c2 (reflPron ! agrP3 Sg) ;
      in {
        s = a.s ! Posit ;
        s2 = \\_ => [] ;
        isPre = True ;
        c = case a.c2.t of {isCase => <obj, []> ; _ => <[], obj>} ;
        ext = []
      } ;

    SentAP ap sc = ap ** {
      isPre = False ;
      ext = ap.ext ++ sc.s
      } ;

    AdAP ada ap = ap ** {s = \\a => ada.s ++ ap.s ! a} ;

    UseA2 a = {
      s = a.s ! Posit ;
      s2 = \\_ => [] ;
      isPre = True ;
      c = <[],[]> ;
      ext = []
      } ;

    AdvAP ap adv = ap ** {s = \\a => adv.s ++ ap.s ! a} ; -- HL 1/2024

}
