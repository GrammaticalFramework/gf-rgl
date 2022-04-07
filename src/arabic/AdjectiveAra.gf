concrete AdjectiveAra of Adjective = CatAra ** open ResAra, Prelude in {

 flags coding = utf8 ;
  lin

  PositA a = {
    s = \\h,g,n,d,c => case h of {
        Hum => a.s ! APosit g n d c;
        NoHum => case n of {
          Pl => a.s ! APosit Fem Sg d c ;
          _  => a.s ! APosit g n d c
          }
        }
    };
---- guessed by AR
  ComparA a np = {
    s = \\sp,g,n,st,c => a.s ! AComp st c ++ "مِنْ" ++ np.s ! Gen ;
    } ;
--
-- $SuperlA$ belongs to determiner syntax in $Noun$.
--
  ComplA2 a np = {
    s = \\sp,g,n,st,c => a.s ! APosit g n st c ++ a.c2.s ++ bindIf a.c2.binds ++ np.s ! a.c2.c ;
    } ;

  -- full PerGenNum should be added to make this work properly
  ReflA2 a = {
    s = \\sp,g,n,st,c => a.s ! APosit g n st c ++ a.c2.s ++ reflPron c (gn2pgn {g=g;n=n})
    } ;

  AdAP ada ap = {
    s = \\sp,g,n,st,c => ap.s ! sp ! g ! n ! st ! c ++ ada.s
    } ;

  AdvAP,
  SentAP = \ap,ss -> AdAP ss ap ;

  UseA2 = PositA ;

  UseComparA a = {
    s = \\h,g,n,d,c => a.s ! AComp d c
    };

  -- : Ord -> AP ;       -- warmest
  AdjOrd ord = {s = \\h,g,n,s,c => ord.s ! g ! s ! c} ;
}
