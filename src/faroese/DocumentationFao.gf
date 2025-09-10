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
           tr (intagAttr "th" "rowspan=\"16\"" "s" ++ intagAttr "th" "rowspan=\"8\"" "Indef" ++ intagAttr "th" "rowspan=\"4\"" "Sg" ++ th "Nom" ++ td (x.s ! Indef ! Sg ! Nom)) ++
           tr (th "Acc" ++ td (x.s ! Indef ! Sg ! Acc)) ++
           tr (th "Dat" ++ td (x.s ! Indef ! Sg ! Dat)) ++
           tr (th "Gen" ++ td (x.s ! Indef ! Sg ! Gen)) ++
           tr (intagAttr "th" "rowspan=\"4\"" "Pl" ++ th "Nom" ++ td (x.s ! Indef ! Pl ! Nom)) ++
           tr (th "Acc" ++ td (x.s ! Indef ! Pl ! Acc)) ++
           tr (th "Dat" ++ td (x.s ! Indef ! Pl ! Dat)) ++
           tr (th "Gen" ++ td (x.s ! Indef ! Pl ! Gen)) ++
           tr (intagAttr "th" "rowspan=\"8\"" "Def" ++ intagAttr "th" "rowspan=\"4\"" "Sg" ++ th "Nom" ++ td (x.s ! Def ! Sg ! Nom)) ++
           tr (th "Acc" ++ td (x.s ! Def ! Sg ! Acc)) ++
           tr (th "Dat" ++ td (x.s ! Def ! Sg ! Dat)) ++
           tr (th "Gen" ++ td (x.s ! Def ! Sg ! Gen)) ++
           tr (intagAttr "th" "rowspan=\"4\"" "Pl" ++ th "Nom" ++ td (x.s ! Def ! Pl ! Nom)) ++
           tr (th "Acc" ++ td (x.s ! Def ! Pl ! Acc)) ++
           tr (th "Dat" ++ td (x.s ! Def ! Pl ! Dat)) ++
           tr (th "Gen" ++ td (x.s ! Def ! Pl ! Gen))) ;
      s3=[]
    } ;
lin
  InflectionA,InflectionA2 = \x -> {
      t="a" ;
      s1="" ;
      s2=frameTable (
           tr (intagAttr "th" "rowspan=\"24\"" "s" ++ intagAttr "th" "rowspan=\"8\"" "Masc" ++ intagAttr "th" "rowspan=\"4\"" "Sg" ++ th "Nom" ++ td (x.s ! Masc ! Sg ! Nom)) ++
           tr (th "Acc" ++ td (x.s ! Masc ! Sg ! Acc)) ++
           tr (th "Dat" ++ td (x.s ! Masc ! Sg ! Dat)) ++
           tr (th "Gen" ++ td (x.s ! Masc ! Sg ! Gen)) ++
           tr (intagAttr "th" "rowspan=\"4\"" "Pl" ++ th "Nom" ++ td (x.s ! Masc ! Pl ! Nom)) ++
           tr (th "Acc" ++ td (x.s ! Masc ! Pl ! Acc)) ++
           tr (th "Dat" ++ td (x.s ! Masc ! Pl ! Dat)) ++
           tr (th "Gen" ++ td (x.s ! Masc ! Pl ! Gen)) ++
           tr (intagAttr "th" "rowspan=\"8\"" "Fem" ++ intagAttr "th" "rowspan=\"4\"" "Sg" ++ th "Nom" ++ td (x.s ! Fem ! Sg ! Nom)) ++
           tr (th "Acc" ++ td (x.s ! Fem ! Sg ! Acc)) ++
           tr (th "Dat" ++ td (x.s ! Fem ! Sg ! Dat)) ++
           tr (th "Gen" ++ td (x.s ! Fem ! Sg ! Gen)) ++
           tr (intagAttr "th" "rowspan=\"4\"" "Pl" ++ th "Nom" ++ td (x.s ! Fem ! Pl ! Nom)) ++
           tr (th "Acc" ++ td (x.s ! Fem ! Pl ! Acc)) ++
           tr (th "Dat" ++ td (x.s ! Fem ! Pl ! Dat)) ++
           tr (th "Gen" ++ td (x.s ! Fem ! Pl ! Gen)) ++
           tr (intagAttr "th" "rowspan=\"8\"" "Neutr" ++ intagAttr "th" "rowspan=\"4\"" "Sg" ++ th "Nom" ++ td (x.s ! Neutr ! Sg ! Nom)) ++
           tr (th "Acc" ++ td (x.s ! Neutr ! Sg ! Acc)) ++
           tr (th "Dat" ++ td (x.s ! Neutr ! Sg ! Dat)) ++
           tr (th "Gen" ++ td (x.s ! Neutr ! Sg ! Gen)) ++
           tr (intagAttr "th" "rowspan=\"4\"" "Pl" ++ th "Nom" ++ td (x.s ! Neutr ! Pl ! Nom)) ++
           tr (th "Acc" ++ td (x.s ! Neutr ! Pl ! Acc)) ++
           tr (th "Dat" ++ td (x.s ! Neutr ! Pl ! Dat)) ++
           tr (th "Gen" ++ td (x.s ! Neutr ! Pl ! Gen))) ;
      s3=[]
    } ;
lin
  InflectionV,InflectionV2,InflectionV2A,InflectionV2Q,InflectionV2S,InflectionV2V,InflectionV3,InflectionVA,InflectionVQ,InflectionVS,InflectionVV = \x -> {
      t="v" ;
      s1="" ;
      s2=frameTable (
           tr (th "Converb" ++ td (x.Converb)) ++
           tr (intagAttr "th" "rowspan=\"2\"" "Imperative_Jussive" ++ th "Sg" ++ td (x.Imperative_Jussive ! Sg)) ++
           tr (th "Pl" ++ td (x.Imperative_Jussive ! Pl)) ++
           tr (intagAttr "th" "rowspan=\"8\"" "Indicative" ++ intagAttr "th" "rowspan=\"4\"" "Pres" ++ th "PSg P1" ++ td (x.Indicative ! Pres ! PSg P1)) ++
           tr (th "PSg P2" ++ td (x.Indicative ! Pres ! PSg P2)) ++
           tr (th "PSg P3" ++ td (x.Indicative ! Pres ! PSg P3)) ++
           tr (th "PPl" ++ td (x.Indicative ! Pres ! PPl)) ++
           tr (intagAttr "th" "rowspan=\"4\"" "Past" ++ th "PSg P1" ++ td (x.Indicative ! Past ! PSg P1)) ++
           tr (th "PSg P2" ++ td (x.Indicative ! Past ! PSg P2)) ++
           tr (th "PSg P3" ++ td (x.Indicative ! Past ! PSg P3)) ++
           tr (th "PPl" ++ td (x.Indicative ! Past ! PPl)) ++
           tr (th "Nonfinite" ++ td (x.Nonfinite)) ++
           tr (intagAttr "th" "rowspan=\"2\"" "Particle" ++ th "Pres" ++ td (x.Particle ! Pres)) ++
           tr (th "Past" ++ td (x.Particle ! Past))) ;
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
