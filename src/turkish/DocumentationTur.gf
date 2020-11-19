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

  InflectionA, InflectionA2 = \adj -> {
    t  = "n" ;
    s1 = heading1 "Ön Ad" ;
    s2 = frameTable (
           tr (th ""         ++ th "tekil"               ++ th "çoğul") ++
           tr (th "yalın"    ++ td (adj.s ! Sg ! Nom)   ++ td (adj.s ! Pl ! Nom)) ++
           tr (th "belirtme" ++ td (adj.s ! Sg ! Acc)   ++ td (adj.s ! Pl ! Acc)) ++
           tr (th "yönelme"  ++ td (adj.s ! Sg ! Dat)   ++ td (adj.s ! Pl ! Dat)) ++
           tr (th "bulunma"  ++ td (adj.s ! Sg ! Loc)   ++ td (adj.s ! Pl ! Loc)) ++
           tr (th "ayrılma"  ++ td (adj.s ! Sg ! Ablat) ++ td (adj.s ! Pl ! Ablat)) ++
           tr (th "tamlayan" ++ td (adj.s ! Sg ! Gen)   ++ td (adj.s ! Pl ! Gen)) ++
           intagAttr "th" "colspan=\"3\"" "iyelik" ++
           tr (th "1. tekil" ++ td (adj.gen ! Sg ! {n = Sg; p = P1}) ++ td (adj.gen ! Pl ! {n = Sg; p = P1})) ++
           tr (th "2. tekil" ++ td (adj.gen ! Sg ! {n = Sg; p = P2}) ++ td (adj.gen ! Pl ! {n = Sg; p = P2})) ++
           tr (th "3. tekil" ++ td (adj.gen ! Sg ! {n = Sg; p = P3}) ++ td (adj.gen ! Pl ! {n = Sg; p = P3})) ++
           tr (th "1. çoğul" ++ td (adj.gen ! Sg ! {n = Pl; p = P1}) ++ td (adj.gen ! Pl ! {n = Pl; p = P1})) ++
           tr (th "2. çoğul" ++ td (adj.gen ! Sg ! {n = Pl; p = P2}) ++ td (adj.gen ! Pl ! {n = Pl; p = P2})) ++
           tr (th "3. çoğul" ++ td (adj.gen ! Sg ! {n = Pl; p = P3}) ++ td (adj.gen ! Pl ! {n = Pl; p = P3}))
         ) ++
        heading1 ("Belirteç") ++
        paragraph (adj.adv) ;
    } ;

  InflectionAdv = \adv -> {
    t = "b" ;
    s1= heading1 ("Belirteç") ;
    s2= paragraph (adv.s) ;
    } ;

  InflectionPrep = \prep -> {
    t = "il" ;
    s1= heading1 ("Ilgeç") ;
    s2= paragraph (prep.s++"+"++
                   case prep.c of {
                     Nom     => "yalın" ;
                     Acc     => "belirtme" ;
                     Dat     => "yönelme" ;
                     Gen     => "tamlayan" ;
                     Loc     => "bulunma" ;
                     Ablat   => "ayrılma" ;
                     Abess _ => ""
                   }) ;
    s3= ""
    } ;

  InflectionV, InflectionV2, InflectionVV, InflectionVS, InflectionVQ,
  InflectionVA, InflectionV3, InflectionV2V, InflectionV2S,
  InflectionV2Q, InflectionV2A = \v -> {
    t = "f" ;
    s1= heading1 ("Fiil") ;
    s2= inflVerb v
  } ;

lin
  NoDefinition   t     = {s=t.s};
  MkDefinition   t d   = {s="<p><b>Tarif:</b>"++t.s++d.s++"</p>"};
  MkDefinitionEx t d e = {s="<p><b>Tarif:</b>"++t.s++d.s++"</p><p><b>Örnek:</b>"++e.s++"</p>"};

lin
  MkDocument d i e = {s = i.s1 ++ d.s ++ i.s2 ++ paragraph e.s} ;
  MkTag i = {s = i.t} ;

oper
  inflVerb : Verb -> Str = \v ->
    (heading2 ("Şimdiki zaman") ++
     finite VPres ++
     tag "br" ++
     finite VProg ++
     heading2 ("Geçmiş zaman") ++
     finite VPast ++
     heading2 ("Gelecek zaman") ++
     finite VFuture ++
     heading2 ("Emir kipi") ++
     paragraph (v.s ! VImperative) ++
     heading2 ("Eylemlik") ++
     paragraph (v.s ! VInfinitive) ++
     heading2 ("Ulaç") ++
     nounForm Gerund ++
     heading2 ("Ad") ++
     nounForm VNoun)
  where {
    finite : (Agr -> VForm) -> Str = \f ->
      frameTable (
        tr (th "" ++
            th "tekil" ++
            th "çoğul") ++
        tr (th "1." ++ 
            td (v.s ! f {n=Sg; p=P1}) ++ 
            td (v.s ! f {n=Pl; p=P1})) ++
        tr (th "2." ++ 
            td (v.s ! f {n=Sg; p=P2}) ++
            td (v.s ! f {n=Pl; p=P2})) ++
        tr (th "3." ++
            td (v.s ! f {n=Sg; p=P3}) ++
            td (v.s ! f {n=Pl; p=P3}))
      ) ;

    nounForm : (Number -> Case -> VForm) -> Str = \f ->
      frameTable (
        tr (th ""         ++ th "tekil"            ++ th "çoğul") ++
        tr (th "yalın"    ++ td (v.s ! f Sg Nom)   ++ td (v.s ! f Pl Nom)) ++
        tr (th "belirtme" ++ td (v.s ! f Sg Acc)   ++ td (v.s ! f Pl Acc)) ++
        tr (th "yönelme"  ++ td (v.s ! f Sg Dat)   ++ td (v.s ! f Pl Dat)) ++
        tr (th "bulunma"  ++ td (v.s ! f Sg Loc)   ++ td (v.s ! f Pl Loc)) ++
        tr (th "ayrılma"  ++ td (v.s ! f Sg Ablat) ++ td (v.s ! f Pl Ablat)) ++
        tr (th "tamlayan" ++ td (v.s ! f Sg Gen)   ++ td (v.s ! f Pl Gen))
      ) ;
   } ;
}
