concrete DocumentationUkr of Documentation = CatUkr ** open
  ResUkr, Prelude, HTML in {

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
           tr (intagAttr "th" "rowspan=\"2\"" "Nom" ++ th "Sg" ++ td (x.s ! Nom ! Sg)) ++
           tr (th "Pl" ++ td (x.s ! Nom ! Pl)) ++
           tr (intagAttr "th" "rowspan=\"2\"" "Acc" ++ th "Sg" ++ td (x.s ! Acc ! Sg)) ++
           tr (th "Pl" ++ td (x.s ! Acc ! Pl)) ++
           tr (intagAttr "th" "rowspan=\"2\"" "Dat" ++ th "Sg" ++ td (x.s ! Dat ! Sg)) ++
           tr (th "Pl" ++ td (x.s ! Dat ! Pl)) ++
           tr (intagAttr "th" "rowspan=\"2\"" "Gen" ++ th "Sg" ++ td (x.s ! Gen ! Sg)) ++
           tr (th "Pl" ++ td (x.s ! Gen ! Pl)) ++
           tr (intagAttr "th" "rowspan=\"2\"" "Loc" ++ th "Sg" ++ td (x.s ! Loc ! Sg)) ++
           tr (th "Pl" ++ td (x.s ! Loc ! Pl)) ++
           tr (intagAttr "th" "rowspan=\"2\"" "Instr" ++ th "Sg" ++ td (x.s ! Instr ! Sg)) ++
           tr (th "Pl" ++ td (x.s ! Instr ! Pl)) ++
           tr (intagAttr "th" "rowspan=\"2\"" "Voc" ++ th "Sg" ++ td (x.Voc ! Sg)) ++
           tr (th "Pl" ++ td (x.Voc ! Pl))) ;
      s3=[]
    } ;
lin
  InflectionV,InflectionV2,InflectionV2A,InflectionV2Q,InflectionV2S,InflectionV2V,InflectionV3,InflectionVA,InflectionVQ,InflectionVS,InflectionVV = \x -> {
      t="v" ;
      s1="" ;
      s2=heading1 "Infinitive" ++
         paragraph (x.infinitive) ++
         heading1 "Present" ++
         frameTable (
           tr (intagAttr "th" "rowspan=\"6\"" "Pres" ++ intagAttr "th" "rowspan=\"2\"" "P1" ++ th "Sg" ++ td ((x.active ! Imperf).Pres ! P1 ! Sg)) ++
           tr (th "Pl" ++ td ((x.active ! Imperf).Pres ! P1 ! Pl)) ++
           tr (intagAttr "th" "rowspan=\"2\"" "P2" ++ th "Sg" ++ td ((x.active ! Imperf).Pres ! P2 ! Sg)) ++
           tr (th "Pl" ++ td ((x.active ! Imperf).Pres ! P2 ! Pl)) ++
           tr (intagAttr "th" "rowspan=\"2\"" "P3" ++ th "Sg" ++ td ((x.active ! Imperf).Pres ! P3 ! Sg)) ++
           tr (th "Pl" ++ td ((x.active ! Imperf).Pres ! P3 ! Pl))) ++
         heading1 "Imperative" ++
         paragraph (x.imperative1) ++
         frameTable (
           tr (th "Sg" ++ td (x.imperative2 ! Sg)) ++
           tr (th "Pl" ++ td (x.imperative2 ! Pl))) ++
         heading1 "participle" ++
         frameTable (
           tr (th "" ++ th "Sg" ++ th "Pl") ++
           tr (th "Masc"   ++ td (x.participle ! Masc   ! Sg) ++ td (x.participle ! Masc   ! Pl)) ++
           tr (th "Fem"    ++ td (x.participle ! Fem    ! Sg) ++ td (x.participle ! Fem    ! Pl)) ++
           tr (th "Neuter" ++ td (x.participle ! Neuter ! Sg) ++ td (x.participle ! Neuter ! Pl))) ;
      s3=[]
    } ;
lin
  InflectionA,InflectionA2 = \x -> {
      t="a" ;
      s1="" ;
      s2=frameTable (
           tr (th ""    ++ th "Masc"                 ++ th "Fem"                 ++ th "Neuter"                 ++ th "Pl") ++ 
           tr (th "Nom" ++ td (x.s ! Nom ! GSg Masc) ++ td (x.s ! Nom ! GSg Fem) ++ td (x.s ! Nom ! GSg Neuter) ++ td (x.s ! Nom ! GPl)) ++
           tr (th "Acc" ++ td (x.s ! Acc ! GSg Masc) ++ td (x.s ! Acc ! GSg Fem) ++ td (x.s ! Acc ! GSg Neuter) ++ td (x.s ! Acc ! GPl)) ++
           tr (th "Dat" ++ td (x.s ! Dat ! GSg Masc) ++ td (x.s ! Dat ! GSg Fem) ++ td (x.s ! Dat ! GSg Neuter) ++ td (x.s ! Dat ! GPl)) ++
           tr (th "Gen" ++ td (x.s ! Gen ! GSg Masc) ++ td (x.s ! Gen ! GSg Fem) ++ td (x.s ! Gen ! GSg Neuter) ++ td (x.s ! Gen ! GPl)) ++
           tr (th "Loc" ++ td (x.s ! Loc ! GSg Masc) ++ td (x.s ! Loc ! GSg Fem) ++ td (x.s ! Loc ! GSg Neuter) ++ td (x.s ! Loc ! GPl)) ++
           tr (th "Instr"++td (x.s ! Instr ! GSg Masc)++td (x.s ! Instr ! GSg Fem)++td (x.s ! Instr ! GSg Neuter)++td (x.s ! Instr ! GPl))) ;
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
