concrete NamesGer of Names = CatGer ** open ResGer, Prelude in {

lin GivenName gn = {
      s = \\_,c => gn.s ! c ;
      a = agrgP3 (sex2gender gn.g) Sg ;
      w = WLight ;
      rc, ext = []
      } ;

lin MaleSurname sn = {
      s = \\_,c => sn.s ! Male ! c ;
      a = agrgP3 Masc Sg ;
      w = WLight ;
      rc, ext = []
      } ;

lin FemaleSurname sn = {
      s = \\_,c => sn.s ! Female ! c ;
      a = agrgP3 Fem Sg ;
      w = WLight ;
      rc, ext = []
      } ;

lin PlSurname sn = {
      s = \\_,c => sn.s ! Male ! c ;
      a = agrgP3 Masc Pl ;
      w = WLight ;
      rc, ext = []
      } ;

lin FullName gn sn = {
      s = \\_,c => gn.s ! Nom ++ sn.s ! gn.g ! c ;
      a = agrgP3 (sex2gender gn.g) Sg ;
      w = WLight ;
      rc, ext = []
      } ;

-- UseLN : LN -> NP ;
lin UseLN ln = notYet "UseLN" ;
{- Old version by Krasimir: {
      s = \\c => case ln.hasArt of {
                   True  => artDefContr (gennum ln.g ln.n) c ++ usePrepC c (\k -> ln.s ! Weak ! k) ;
                   False => usePrepC c (\k -> ln.s ! adjfCase Strong k ! k)
                 } ;
      a = agrgP3 ln.g ln.n ;
      w = WLight ;
      rc, ext = []
      } ;
-}

-- PlainLN : LN -> NP ;
lin PlainLN ln = notYet "PlainLN" ;
{- {
      s = \\c => usePrepC c (\k -> ln.s ! adjfCase Strong k ! k) ;
      a = agrgP3 ln.g ln.n ;
      w = WLight ;
      rc, ext = []
      } ;
-}

-- InLN : LN -> Adv ;
lin InLN ln = notYet "InLN" ;
{- {
      s = let c = NPP CInDat
          in case ln.hasArt of {
               True  => artDefContr (gennum ln.g ln.n) c ++ usePrepC c (\k -> ln.s ! Weak ! k) ;
               False => usePrepC c (\k -> ln.s ! adjfCase Strong k ! k)
             } ;
      } ;
-}

-- AdjLN : AP -> LN -> LN ;
lin AdjLN ap ln = ln ** {
      s = \\a,c =>
               preOrPost ap.isPre
                 (ap.c.p1 ++ ap.c.p2 ++ ap.s ! agrAdj ln.g a ln.n c ++ ap.ext)
                 (ln.s ! a ! c) ;
      } ;

}
