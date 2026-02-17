--# -path=.:../abstract:../common
concrete DocumentationSwa of Documentation = CatSwa ** open
  ResSwa,ParamX,Prelude,
  HTML in {

lincat
  Inflection = {t : Str; s1,s2 : Str} ;
  Definition = {s : Str} ;
  Document = {s : Str} ;
  Tag      = {s : Str} ;

lin InflectionN,InflectionN2,InflectionN3 = \x -> {
      t="n" ;
      s1=heading1 ("Noun (class" ++
                   case x.g of {
                     G1 => "I" ;
                     G2 => "II" ;
                     G3 => "III" ;
                     G4 => "IV" ;
                     G5 => "V" ;
                     G6 => "VI" ;
                     G7 => "VII" ;
                     G8 => "VIII" ;
                     G9 => "IX" ;
                     G10=> "X" ;
                     G11=> "XI" ;
                     G12=> "XII" ;
                     G13=> "XIII"
                   } ++ BIND ++ ")") ;
      s2=frameTable (
           tr (th "" ++ th "Sg" ++ th "Pl") ++ 
           tr (th "Nom" ++ td (x.s ! Sg ! Nom) ++ td (x.s ! Pl ! Nom)) ++
           tr (th "Loc" ++ td (x.s ! Sg ! Loc) ++ td (x.s ! Pl ! Loc)))
    } ;

lin
  NoDefinition   t     = {s=t.s};
  MkDefinition   t d   = {s="<p><b>Definition:</b>"++t.s++d.s++"</p>"};
  MkDefinitionEx t d e = {s="<p><b>Definition:</b>"++t.s++d.s++"</p><p><b>Example:</b>"++e.s++"</p>"};

lin
  MkDocument d i e = {s = i.s1 ++ d.s ++ i.s2 ++ paragraph e.s} ;
  MkTag i = {s = i.t} ;

}
