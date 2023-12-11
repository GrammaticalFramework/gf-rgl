concrete AdjectiveGer of Adjective = CatGer ** open ResGer, Prelude in {

  flags optimize=all_subs ;

  lin

    PositA  a = {
      s = a.s ! Posit ;
      isPre = True ;
      c = <[],[]> ;
      ext = []
      } ;
    ComparA a np =
      let nps = np.s ! False ! Nom ++ bigNP np
      in {
      s = \\af => a.s ! Compar ! af ++ conjThan ++ nps ;
      isPre = True ;
      c = <[],[]> ;
      ext = []
      } ;
    CAdvAP ad ap np =
      let nps = np.s ! False ! Nom ++ bigNP np in
      ap ** {
      s = \\af => ad.s ++ ap.s ! af ++ ad.p ++ nps ;
      isPre = False 
      } ;
    UseComparA a = {
      s = \\af => a.s ! Compar ! af ;
      isPre = True ;
      c = <[],[]> ;
      ext = []
      } ;

    AdjOrd a = {
      s = a.s ;
      isPre = True ;
      c = <[],[]> ;
      ext = []
      } ; 

-- $SuperlA$ belongs to determiner syntax in $Noun$.

    ComplA2 a np =
      let CExt = case a.c2.isPrep of {
            isCase => <appPrepNP a.c2 np, []> ;
	    _ => <[], appPrepNP a.c2 np> }
      in {
           s = a.s ! Posit ;
           isPre = True ;
           c = CExt ;
           ext = []
      } ;

    ReflA2 a = 
      let
	compl = appPrep a.c2 (reflPron ! agrP3 Sg) ;
	CExt = case a.c2.isPrep of
		{isCase => <compl, []> ; _ => <[], compl> }
      in {
        s = a.s ! Posit ;
        isPre = True ;
        c = CExt ;
        ext = []
      } ;

    SentAP ap sc = ap ** {
      isPre = False ;
      ext = ap.ext ++ sc.s
      } ;

    AdAP ada ap = ap ** {s = \\a => ada.s ++ ap.s ! a} ;

    UseA2 a = {
      s = a.s ! Posit ;
      isPre = True ;
      c = <[],[]> ;
      ext = []
      } ;

}
