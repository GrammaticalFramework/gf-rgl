concrete DocumentationGla of Documentation = CatGla ** open
  ResGla, Prelude, HTML in {

lincat
  Inflection = {t : Str; s1,s2,s3 : Str} ;
  Definition = {s : Str} ;
  Document   = {s : Str} ;
  Tag        = {s : Str} ;

lin
  InflectionN,InflectionN2,InflectionN3 = \x -> {
      t="n" ;
      s1=heading1 ("Noun"++
                   case x.g of {
                     Masc => "(masculine)" ;
                     Fem  => "(feminine)"
                   }) ;
      s2=frameTable (
           tr (th "" ++ th "Sg" ++ th "Pl") ++
           tr (intagAttr "th" "colspan=\"3\"" "indefinite") ++
           tr (th "nom" ++ td (x.s ! Nom NoMutation ! Indef ! Sg) ++ td (x.s ! Nom NoMutation ! Indef ! Pl)) ++
           tr (th "dat" ++ td (x.s ! Dat NoMutation ! Indef ! Sg) ++ td (x.s ! Dat NoMutation ! Indef ! Pl)) ++
           tr (th "gen" ++ td (x.s ! Gen            ! Indef ! Sg) ++ td (x.s ! Gen            ! Indef ! Pl)) ++
           tr (intagAttr "th" "colspan=\"3\"" "definite") ++
           tr (th "nom" ++ td (x.s ! Nom NoMutation ! Def   ! Sg) ++ td (x.s ! Nom NoMutation ! Def ! Pl)) ++
           tr (th "dat" ++ td (x.s ! Dat NoMutation ! Def   ! Sg) ++ td (x.s ! Dat NoMutation ! Def ! Pl)) ++
           tr (th "gen" ++ td (x.s ! Gen            ! Def   ! Sg) ++ td (x.s ! Gen            ! Def   ! Pl)) ++
           tr (th "voc" ++ td (x.voc ! Sg)                        ++ td (x.voc ! Pl))) ;
      s3=[]
    } ;
lin
  InflectionV,InflectionV2,InflectionV2A,InflectionV2Q,InflectionV2S,InflectionV2V,InflectionV3,InflectionVA,InflectionVQ,InflectionVS,InflectionVV = \x -> {
      t="v" ;
      s1=heading1 "Verb" ;
      s2=frameTable (
           tr (th "s" ++ td (x.s)) ++
           tr (intagAttr "th" "rowspan=\"2\"" "Conditional" ++ th "Sg" ++ td (x.conditional ! Sg)) ++
           tr (th "Pl" ++ td (x.conditional ! Pl)) ++
           tr (intagAttr "th" "rowspan=\"6\"" "Imperative" ++ intagAttr "th" "rowspan=\"2\"" "P1" ++ th "Sg" ++ td (x.imperative ! P1 ! Sg)) ++
           tr (th "Pl" ++ td (x.imperative ! P1 ! Pl)) ++
           tr (intagAttr "th" "rowspan=\"2\"" "P2" ++ th "Sg" ++ td (x.imperative ! P2 ! Sg)) ++
           tr (th "Pl" ++ td (x.imperative ! P2 ! Pl)) ++
           tr (intagAttr "th" "rowspan=\"2\"" "P3" ++ th "Sg" ++ td (x.imperative ! P3 ! Sg)) ++
           tr (th "Pl" ++ td (x.imperative ! P3 ! Pl)) ++
           tr (intagAttr "th" "rowspan=\"4\"" "Indicative" ++ th "Fut" ++ td (x.future ! Indep)) ++
           tr (th "Past" ++ td (x.past ! Indep)) ++
           tr (th "Participle" ++ td (x.participle))) ;
      s3=[]
    } ;
lin
  InflectionA,InflectionA2 = \x -> {
      t="a" ;
      s1=heading1 "Adjective" ;
      s2=frameTable (
           tr (intagAttr "th" "rowspan=\"2\"" "" ++ 
                           th "masculine" ++ th "feminine") ++
           tr (intagAttr "th" "colspan=\"2\"" "singular") ++
           tr (th "Nom" ++ td (x.s ! ASg (Nom NoMutation) Masc) ++ td (x.s ! ASg (Nom NoMutation) Fem)) ++
           tr (th "Dat" ++ td (x.s ! ASg (Dat NoMutation) Masc) ++ td (x.s ! ASg (Dat NoMutation) Fem)) ++
           tr (th "Gen" ++ td (x.s ! ASg Gen Masc) ++ td (x.s ! ASg Gen Fem)) ++
           tr (th "Voc" ++ td (x.voc ! Masc)       ++ td (x.voc ! Fem)) ++
           tr (intagAttr "th" "rowspan=\"2\"" "" ++ intagAttr "th" "colspan=\"2\"" "plural") ++
           tr (                                     intagAttr "td" "colspan=\"2\""  (x.s ! APl))) ++
         heading2 "Comparative" ++
         paragraph (x.compar) ;
      s3=[]
    } ;

lin
  NoDefinition   t     = {s=t.s};
  MkDefinition   t d   = {s="<p><b>Definition:</b>"++t.s++d.s++"</p>"};
  MkDefinitionEx t d e = {s="<p><b>Definition:</b>"++t.s++d.s++"</p><p><b>Example:</b>"++e.s++"</p>"};

lin
  MkDocument d i e = {s = i.s1 ++ d.s ++ i.s2 ++ i.s3 ++ e.s} ;
  MkTag i = {s = i.t} ;
}
