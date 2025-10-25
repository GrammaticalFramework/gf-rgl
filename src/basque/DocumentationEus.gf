--# -path=.:../abstract:../common
concrete DocumentationEus of Documentation = CatEus ** open
  ResEus,
  Prelude,
  HTML in {

lincat
  Inflection = {t : Str; s1,s2 : Str} ;
  Definition = {s : Str} ;
  Document = {s : Str} ;
  Tag      = {s : Str} ;

lin
  InflectionN, InflectionN2, InflectionN3 = \n -> {
    t  = "n" ;
    s1 = heading1 "Noun" ;
    s2 = frameTable (
          tr (th ""            ++ th "Sg"                            ++ th "Pl") ++
          tr (th "absolutive"  ++ td (n.s++artDef ! Sg ! Abs ! n.ph) ++ td (n.s++artDef ! Pl ! Abs ! n.ph)) ++
          tr (th "ergative"    ++ td (n.s++artDef ! Sg ! Erg ! n.ph) ++ td (n.s++artDef ! Pl ! Erg ! n.ph)) ++
          tr (th "dative"      ++ td (n.s++artDef ! Sg ! Dat ! n.ph) ++ td (n.s++artDef ! Pl ! Dat ! n.ph)) ++
          tr (th "genitive"    ++ td (n.s++artDef ! Sg ! Gen ! n.ph) ++ td (n.s++artDef ! Pl ! Gen ! n.ph)) ++
          tr (th "commitative" ++ td (n.s++artDef ! Sg ! Soc ! n.ph) ++ td (n.s++artDef ! Pl ! Soc ! n.ph)) ++
          tr (th "instrumental"++ td (n.s++artDef ! Sg ! Ins ! n.ph) ++ td (n.s++artDef ! Pl ! Ins ! n.ph)) ++
          tr (th "inessive"    ++ td (n.s++artDef ! Sg ! Ine ! n.ph) ++ td (n.s++artDef ! Pl ! Ine ! n.ph)) ++
          tr (th "partitive"   ++ td (n.s++artDef ! Sg ! Par ! n.ph) ++ td (n.s++artDef ! Pl ! Par ! n.ph))
          ) ;
    } ;

  InflectionA, InflectionA2, InflectionA3 = \a -> {
    t  = "a" ;
    s1 = heading1 "Adjective" ;
    s2 = frameTable (
          tr (th ""            ++ th "Sg"                            ++ th "Pl") ++
          tr (th "absolutive"  ++ td (a.s ! AF Posit++artDef ! Sg ! Abs ! a.ph) ++ td (a.s ! AF Posit++artDef ! Pl ! Abs ! a.ph)) ++
          tr (th "ergative"    ++ td (a.s ! AF Posit++artDef ! Sg ! Erg ! a.ph) ++ td (a.s ! AF Posit++artDef ! Pl ! Erg ! a.ph)) ++
          tr (th "dative"      ++ td (a.s ! AF Posit++artDef ! Sg ! Dat ! a.ph) ++ td (a.s ! AF Posit++artDef ! Pl ! Dat ! a.ph)) ++
          tr (th "genitive"    ++ td (a.s ! AF Posit++artDef ! Sg ! Gen ! a.ph) ++ td (a.s ! AF Posit++artDef ! Pl ! Gen ! a.ph)) ++
          tr (th "commitative" ++ td (a.s ! AF Posit++artDef ! Sg ! Soc ! a.ph) ++ td (a.s ! AF Posit++artDef ! Pl ! Soc ! a.ph)) ++
          tr (th "instrumental"++ td (a.s ! AF Posit++artDef ! Sg ! Ins ! a.ph) ++ td (a.s ! AF Posit++artDef ! Pl ! Ins ! a.ph)) ++
          tr (th "inessive"    ++ td (a.s ! AF Posit++artDef ! Sg ! Ine ! a.ph) ++ td (a.s ! AF Posit++artDef ! Pl ! Ine ! a.ph)) ++
          tr (th "partitive"   ++ td (a.s ! AF Posit++artDef ! Sg ! Par ! a.ph) ++ td (a.s ! AF Posit++artDef ! Pl ! Par ! a.ph))
          ) ;
    } ;

  InflectionV, InflectionV2, InflectionV3, InflectionVA, InflectionVS, InflectionVQ, InflectionV2A, InflectionV2S, InflectionV2Q = \v -> {
    t  = "v" ;
    s1 = heading1 "Verb" ;
    s2 = frameTable (
          tr (th "present" ++ td (v.prc ! Pres)) ++
          tr (th "past"    ++ td (v.prc ! Past)) ++
          tr (th "future"  ++ td (v.prc ! Fut))
          ) ;
    } ;

lin
  NoDefinition   t     = {s=t.s};
  MkDefinition   t d   = {s="<p><b>Definition:</b>"++t.s++d.s++"</p>"};
  MkDefinitionEx t d e = {s="<p><b>Definition:</b>"++t.s++d.s++"</p><p><b>Example:</b>"++e.s++"</p>"};

lin
  MkDocument d i e = {s = i.s1 ++ d.s ++ i.s2 ++ paragraph e.s} ;
  MkTag i = {s = i.t} ;

}
