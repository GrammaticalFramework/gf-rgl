concrete DocumentationFao of Documentation = CatFao ** open
  ResFao, Prelude, HTML in {

lincat
  Inflection = {t : Str; s1,s2,s3 : Str} ;
  Definition = {s : Str} ;
  Document   = {s : Str} ;
  Tag        = {s : Str} ;

lin
  InflectionN,InflectionN2,InflectionN3 = \x -> {
      t="n" ;
      s1="" ;
      s2=frameTable (
           tr (intagAttr "th" "colspan=\"2\"" "" ++ th "Indef" ++ th "Def") ++
           tr (intagAttr "th" "rowspan=\"4\"" "Sg" ++
               th "Nom" ++ td (x.s ! Indef ! Sg ! Nom) ++ td (x.s ! Def ! Sg ! Nom)) ++
           tr (th "Acc" ++ td (x.s ! Indef ! Sg ! Acc) ++ td (x.s ! Def ! Sg ! Acc)) ++
           tr (th "Dat" ++ td (x.s ! Indef ! Sg ! Dat) ++ td (x.s ! Def ! Sg ! Dat)) ++
           tr (th "Gen" ++ td (x.s ! Indef ! Sg ! Gen) ++ td (x.s ! Def ! Sg ! Gen)) ++
           tr (intagAttr "th" "rowspan=\"4\"" "Pl" ++
               th "Nom" ++ td (x.s ! Indef ! Pl ! Nom) ++ td (x.s ! Def ! Pl ! Nom)) ++
           tr (th "Acc" ++ td (x.s ! Indef ! Pl ! Acc) ++ td (x.s ! Def ! Pl ! Acc)) ++
           tr (th "Dat" ++ td (x.s ! Indef ! Pl ! Dat) ++ td (x.s ! Def ! Pl ! Dat)) ++
           tr (th "Gen" ++ td (x.s ! Indef ! Pl ! Gen) ++ td (x.s ! Def ! Pl ! Gen))) ;
      s3=[]
    } ;
lin
  InflectionA,InflectionA2 = \x -> {
      t="a" ;
      s1="" ;
      s2=frameTable (
           tr (intagAttr "th" "colspan=\"2\"" "" ++ th "Masc" ++ th "Fem" ++ th "Neutr") ++
           tr (intagAttr "th" "rowspan=\"4\"" "Sg" ++
               th "Nom" ++ td (x.s ! Masc ! Sg ! Nom) ++ td (x.s ! Fem ! Sg ! Nom) ++ td (x.s ! Neutr ! Sg ! Nom)) ++
           tr (th "Acc" ++ td (x.s ! Masc ! Sg ! Acc) ++ td (x.s ! Fem ! Sg ! Acc) ++ td (x.s ! Neutr ! Sg ! Acc)) ++
           tr (th "Dat" ++ td (x.s ! Masc ! Sg ! Dat) ++ td (x.s ! Fem ! Sg ! Dat) ++ td (x.s ! Neutr ! Sg ! Dat)) ++
           tr (th "Gen" ++ td (x.s ! Masc ! Sg ! Gen) ++ td (x.s ! Fem ! Sg ! Gen) ++ td (x.s ! Neutr ! Sg ! Gen)) ++
           tr (intagAttr "th" "rowspan=\"4\"" "Pl" ++
               th "Nom" ++ td (x.s ! Masc ! Pl ! Nom) ++ td (x.s ! Fem ! Pl ! Nom) ++ td (x.s ! Neutr ! Pl ! Nom)) ++
           tr (th "Acc" ++ td (x.s ! Masc ! Pl ! Acc) ++ td (x.s ! Fem ! Pl ! Acc) ++ td (x.s ! Neutr ! Pl ! Acc)) ++
           tr (th "Dat" ++ td (x.s ! Masc ! Pl ! Dat) ++ td (x.s ! Fem ! Pl ! Dat) ++ td (x.s ! Neutr ! Pl ! Dat)) ++
           tr (th "Gen" ++ td (x.s ! Masc ! Pl ! Gen) ++ td (x.s ! Fem ! Pl ! Gen) ++ td (x.s ! Neutr ! Pl ! Gen))) ;
      s3=[]
    } ;
lin
  InflectionV,InflectionV2,InflectionV2A,InflectionV2Q,InflectionV2S,InflectionV2V,InflectionV3,InflectionVA,InflectionVQ,InflectionVS,InflectionVV = \x -> {
      t="v" ;
      s1="" ;
      s2=heading2 "Converb" ++
         paragraph x.Converb ++
         heading2 "Imperative" ++
         frameTable (
           tr (th "Sg" ++ td (x.Imperative_Jussive ! Sg)) ++
           tr (th "Pl" ++ td (x.Imperative_Jussive ! Pl))) ++
         heading2 "Indicative" ++
         frameTable (
           tr (intagAttr "th" "rowspan=\"4\"" "Pres" ++ th "Sg P1" ++ td (x.Indicative ! Pres ! PSg P1)) ++
           tr (th "Sg P2" ++ td (x.Indicative ! Pres ! PSg P2)) ++
           tr (th "Sg P3" ++ td (x.Indicative ! Pres ! PSg P3)) ++
           tr (th "Pl" ++ td (x.Indicative ! Pres ! PPl)) ++
           tr (intagAttr "th" "rowspan=\"4\"" "Past" ++ th "Sg P1" ++ td (x.Indicative ! Past ! PSg P1)) ++
           tr (th "Sg P2" ++ td (x.Indicative ! Past ! PSg P2)) ++
           tr (th "Sg P3" ++ td (x.Indicative ! Past ! PSg P3)) ++
           tr (th "Pl" ++ td (x.Indicative ! Past ! PPl))) ++
         heading2 "Nonfinite" ++
         paragraph x.Nonfinite ++
         heading2 "Participle" ++
         frameTable (
           tr (th "Pres" ++ td (x.Participle ! Pres)) ++
           tr (th "Past" ++ td (x.Participle ! Past))) ;
      s3=[]
    } ;
lin
  InflectionAdA,InflectionAdN,InflectionAdV,InflectionAdv = \x -> {t="adv"; s1=""; s2=x.s; s3=""} ;

  InflectionPrep = \x -> {t="prep"; s1=""; s2=x.s; s3=""} ;

lin
  NoDefinition   t     = {s=t.s};
  MkDefinition   t d   = {s="<p><b>Definition:</b>"++t.s++d.s++"</p>"};
  MkDefinitionEx t d e = {s="<p><b>Definition:</b>"++t.s++d.s++"</p><p><b>Example:</b>"++e.s++"</p>"};

lin
  MkDocument d i e = {s = i.s1 ++ d.s ++ i.s2 ++ i.s3 ++ e.s} ;
  MkTag i = {s = i.t} ;
}
