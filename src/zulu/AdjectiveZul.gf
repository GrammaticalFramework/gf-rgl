concrete AdjectiveZul of Adjective = CatZul ** open ResZul, Prelude in {

  lin

    PositA adj = adj ;

    -- ComparA a np = {
    --   s = \\_ => a.s ! AAdj Compar Nom ++ "than" ++ np.s ! npNom ;
    --   isPre = False
    --   } ;
    -- UseComparA a = {
    --   s = \\_ => a.s ! AAdj Compar Nom ;
    --   isPre = a.isPre
    --   } ;
    --
    -- AdjOrd ord = {
    --   s = \\_ => ord.s ! Nom ;
    --   isPre = True
    --   } ;
    --
    -- CAdvAP ad ap np = {
    --   s = \\a => ad.s ! Pos ++ ap.s ! a ++ ad.p ++ np.s ! npNom ;
    --   isPre = False
    --   } ;
    --
    -- ComplA2 a np = {
    --   s = \\_ => a.s ! AAdj Posit Nom ++ a.c2 ++ np.s ! NPAcc ;
    --   isPre = False
    --   } ;
    --
    -- ReflA2 a = {
    --   s = \\ag => a.s ! AAdj Posit Nom ++ a.c2 ++ reflPron ! ag ;
    --   isPre = False
    --   } ;
    --
    -- SentAP ap sc = {
    --   s = \\a => ap.s ! a ++ sc.s ;
    --   isPre = False
    --   } ;

    AdAP ada ap = {
      s = \\a => ap.s!a ++ ada.s ;
      b = ap.b ;
      empty = ap.empty ;
      t = AdjType
    } ;

    -- UseA2 a = {
    --   s = \\_ => a.s ! AAdj Posit Nom ;
    --   isPre = True
    --   } ;
    --
    -- AdvAP ap adv = {s = \\a => ap.s ! a ++ adv.s ; isPre = False} ;

}
