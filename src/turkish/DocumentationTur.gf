--# -path=.:../abstract:../common
concrete DocumentationTur of Documentation = CatTur ** open
  ResTur,
  HTML in {

lincat
  Inflection = {t : Str; s1,s2 : Str} ;
  Definition = {s : Str} ;
  Document = {s : Str} ;
  Tag      = {s : Str} ;

lin
  InflectionN, InflectionN2, InflectionN3 = \noun -> {
    t  = "n" ;
    s1 = heading1 "Ad" ;
    s2 = frameTable (
           tr (th ""         ++ th "tekil"               ++ th "çoğul") ++
           tr (th "yalın"    ++ td (noun.s ! Sg ! Nom)   ++ td (noun.s ! Pl ! Nom)) ++
           tr (th "belirtme" ++ td (noun.s ! Sg ! Acc)   ++ td (noun.s ! Pl ! Acc)) ++
           tr (th "yönelme"  ++ td (noun.s ! Sg ! Dat)   ++ td (noun.s ! Pl ! Dat)) ++
           tr (th "bulunma"  ++ td (noun.s ! Sg ! Loc)   ++ td (noun.s ! Pl ! Loc)) ++
           tr (th "ayrılma"  ++ td (noun.s ! Sg ! Ablat) ++ td (noun.s ! Pl ! Ablat)) ++
           tr (th "tamlayan" ++ td (noun.s ! Sg ! Gen)   ++ td (noun.s ! Pl ! Gen)) ++
           intagAttr "th" "colspan=\"3\"" "iyelik" ++
           tr (th "1. tekil" ++ td (noun.gen ! Sg ! {n = Sg; p = P1}) ++ td (noun.gen ! Pl ! {n = Sg; p = P1})) ++
           tr (th "2. tekil" ++ td (noun.gen ! Sg ! {n = Sg; p = P2}) ++ td (noun.gen ! Pl ! {n = Sg; p = P2})) ++
           tr (th "3. tekil" ++ td (noun.gen ! Sg ! {n = Sg; p = P3}) ++ td (noun.gen ! Pl ! {n = Sg; p = P3})) ++
           tr (th "1. çoğul" ++ td (noun.gen ! Sg ! {n = Pl; p = P1}) ++ td (noun.gen ! Pl ! {n = Pl; p = P1})) ++
           tr (th "2. çoğul" ++ td (noun.gen ! Sg ! {n = Pl; p = P2}) ++ td (noun.gen ! Pl ! {n = Pl; p = P2})) ++
           tr (th "3. çoğul" ++ td (noun.gen ! Sg ! {n = Pl; p = P3}) ++ td (noun.gen ! Pl ! {n = Pl; p = P3}))
         )
    } ;

lin
  NoDefinition   t     = {s=t.s};

lin
  MkDocument d i e = {s = i.s1 ++ d.s ++ i.s2 ++ paragraph e.s} ;
  MkTag i = {s = i.t} ;

}
