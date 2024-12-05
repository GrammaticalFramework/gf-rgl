concrete DocumentationSqi of Documentation = CatSqi ** open
  ParamX, ResSqi, Prelude, HTML in {

lincat
  Inflection = {t : Str; s1,s2,s3 : Str} ;
  Definition = {s : Str} ;
  Document   = {s : Str} ;
  Tag        = {s : Str} ;

lin InflectionN = \x -> {
      t="noun" ;
      s1="" ;
      s2=frameTable (
           tr (intagAttr "th" "colspan=\"2\"" "" ++ th "Sg" ++ th "Pl") ++
           tr (intagAttr "th" "rowspan=\"4\"" "Indef" ++ th "Nom" ++ td (x.s ! Indef ! Nom ! Sg) ++ td (x.s ! Indef ! Nom ! Pl)) ++
           tr (th "Acc" ++ td (x.s ! Indef ! Acc ! Sg) ++ td (x.s ! Indef ! Acc ! Pl)) ++
           tr (th "Dat" ++ td (x.s ! Indef ! Dat ! Sg) ++ td (x.s ! Indef ! Dat ! Pl)) ++
           tr (th "Ablat" ++ td (x.s ! Indef ! Ablat ! Sg) ++ td (x.s ! Indef ! Ablat ! Pl)) ++
           tr (intagAttr "th" "rowspan=\"4\"" "Def" ++ th "Nom" ++ td (x.s ! Def ! Nom ! Sg) ++ td (x.s ! Def ! Nom ! Pl)) ++
           tr (th "Acc" ++ td (x.s ! Def ! Acc ! Sg) ++ td (x.s ! Def ! Acc ! Pl)) ++
           tr (th "Dat" ++ td (x.s ! Def ! Dat ! Sg) ++ td (x.s ! Def ! Dat ! Pl)) ++
           tr (th "Ablat" ++ td (x.s ! Def ! Ablat ! Sg) ++ td (x.s ! Def ! Ablat ! Pl))) ;
      s3=[]
    } ;
lin InflectionA = \x -> {
      t="adj" ;
      s1="" ;
      s2=frameTable (
           tr (intagAttr "th" "rowspan=\"16\"" "s" ++ intagAttr "th" "rowspan=\"4\"" "Nom" ++ intagAttr "th" "rowspan=\"2\"" "Masc" ++ th "Sg" ++ td (x.s ! Nom ! Masc ! Sg)) ++
           tr (th "Pl" ++ td (x.s ! Nom ! Masc ! Pl)) ++
           tr (intagAttr "th" "rowspan=\"2\"" "Fem" ++ th "Sg" ++ td (x.s ! Nom ! Fem ! Sg)) ++
           tr (th "Pl" ++ td (x.s ! Nom ! Fem ! Pl)) ++
           tr (intagAttr "th" "rowspan=\"4\"" "Acc" ++ intagAttr "th" "rowspan=\"2\"" "Masc" ++ th "Sg" ++ td (x.s ! Acc ! Masc ! Sg)) ++
           tr (th "Pl" ++ td (x.s ! Acc ! Masc ! Pl)) ++
           tr (intagAttr "th" "rowspan=\"2\"" "Fem" ++ th "Sg" ++ td (x.s ! Acc ! Fem ! Sg)) ++
           tr (th "Pl" ++ td (x.s ! Acc ! Fem ! Pl)) ++
           tr (intagAttr "th" "rowspan=\"4\"" "Dat" ++ intagAttr "th" "rowspan=\"2\"" "Masc" ++ th "Sg" ++ td (x.s ! Dat ! Masc ! Sg)) ++
           tr (th "Pl" ++ td (x.s ! Dat ! Masc ! Pl)) ++
           tr (intagAttr "th" "rowspan=\"2\"" "Fem" ++ th "Sg" ++ td (x.s ! Dat ! Fem ! Sg)) ++
           tr (th "Pl" ++ td (x.s ! Dat ! Fem ! Pl)) ++
           tr (intagAttr "th" "rowspan=\"4\"" "Ablat" ++ intagAttr "th" "rowspan=\"2\"" "Masc" ++ th "Sg" ++ td (x.s ! Ablat ! Masc ! Sg)) ++
           tr (th "Pl" ++ td (x.s ! Ablat ! Masc ! Pl)) ++
           tr (intagAttr "th" "rowspan=\"2\"" "Fem" ++ th "Sg" ++ td (x.s ! Ablat ! Fem ! Sg)) ++
           tr (th "Pl" ++ td (x.s ! Ablat ! Fem ! Pl))) ;
      s3=[]
    } ;
lin InflectionV = \x -> {
      t="verb" ;
      s1="" ;
      s2=frameTable (
           tr (intagAttr "th" "rowspan=\"24\"" "Indicative" ++ intagAttr "th" "rowspan=\"6\"" "Pres" ++ intagAttr "th" "rowspan=\"3\"" "Sg" ++ th "P1" ++ td (x.Indicative ! Pres ! Sg ! P1)) ++
           tr (th "P2" ++ td (x.Indicative ! Pres ! Sg ! P2)) ++
           tr (th "P3" ++ td (x.Indicative ! Pres ! Sg ! P3)) ++
           tr (intagAttr "th" "rowspan=\"3\"" "Pl" ++ th "P1" ++ td (x.Indicative ! Pres ! Pl ! P1)) ++
           tr (th "P2" ++ td (x.Indicative ! Pres ! Pl ! P2)) ++
           tr (th "P3" ++ td (x.Indicative ! Pres ! Pl ! P3)) ++
           tr (intagAttr "th" "rowspan=\"6\"" "Past" ++ intagAttr "th" "rowspan=\"3\"" "Sg" ++ th "P1" ++ td (x.Indicative ! Past ! Sg ! P1)) ++
           tr (th "P2" ++ td (x.Indicative ! Past ! Sg ! P2)) ++
           tr (th "P3" ++ td (x.Indicative ! Past ! Sg ! P3)) ++
           tr (intagAttr "th" "rowspan=\"3\"" "Pl" ++ th "P1" ++ td (x.Indicative ! Past ! Pl ! P1)) ++
           tr (th "P2" ++ td (x.Indicative ! Past ! Pl ! P2)) ++
           tr (th "P3" ++ td (x.Indicative ! Past ! Pl ! P3)) ++
           tr (intagAttr "th" "rowspan=\"6\"" "Aorist" ++ intagAttr "th" "rowspan=\"3\"" "Sg" ++ th "P1" ++ td (x.Indicative ! Aorist ! Sg ! P1)) ++
           tr (th "P2" ++ td (x.Indicative ! Aorist ! Sg ! P2)) ++
           tr (th "P3" ++ td (x.Indicative ! Aorist ! Sg ! P3)) ++
           tr (intagAttr "th" "rowspan=\"3\"" "Pl" ++ th "P1" ++ td (x.Indicative ! Aorist ! Pl ! P1)) ++
           tr (th "P2" ++ td (x.Indicative ! Aorist ! Pl ! P2)) ++
           tr (th "P3" ++ td (x.Indicative ! Aorist ! Pl ! P3)) ++
           tr (intagAttr "th" "rowspan=\"6\"" "Imperfect" ++ intagAttr "th" "rowspan=\"3\"" "Sg" ++ th "P1" ++ td (x.Indicative ! Imperfect ! Sg ! P1)) ++
           tr (th "P2" ++ td (x.Indicative ! Imperfect ! Sg ! P2)) ++
           tr (th "P3" ++ td (x.Indicative ! Imperfect ! Sg ! P3)) ++
           tr (intagAttr "th" "rowspan=\"3\"" "Pl" ++ th "P1" ++ td (x.Indicative ! Imperfect ! Pl ! P1)) ++
           tr (th "P2" ++ td (x.Indicative ! Imperfect ! Pl ! P2)) ++
           tr (th "P3" ++ td (x.Indicative ! Imperfect ! Pl ! P3)) ++
           tr (intagAttr "th" "rowspan=\"2\"" "Imperative" ++ th "Sg" ++ td (x.Imperative ! Sg)) ++
           tr (th "Pl" ++ td (x.Imperative ! Pl)) ++
           tr (th "participle" ++ td (x.participle)) ++
           tr (intagAttr "th" "rowspan=\"6\"" "pres_optative" ++ intagAttr "th" "rowspan=\"3\"" "Sg" ++ th "P1" ++ td (x.pres_optative ! Sg ! P1)) ++
           tr (th "P2" ++ td (x.pres_optative ! Sg ! P2)) ++
           tr (th "P3" ++ td (x.pres_optative ! Sg ! P3)) ++
           tr (intagAttr "th" "rowspan=\"3\"" "Pl" ++ th "P1" ++ td (x.pres_optative ! Pl ! P1)) ++
           tr (th "P2" ++ td (x.pres_optative ! Pl ! P2)) ++
           tr (th "P3" ++ td (x.pres_optative ! Pl ! P3)) ++
           tr (intagAttr "th" "rowspan=\"6\"" "perf_optative" ++ intagAttr "th" "rowspan=\"3\"" "Sg" ++ th "P1" ++ td (x.perf_optative ! Sg ! P1)) ++
           tr (th "P2" ++ td (x.perf_optative ! Sg ! P2)) ++
           tr (th "P3" ++ td (x.perf_optative ! Sg ! P3)) ++
           tr (intagAttr "th" "rowspan=\"3\"" "Pl" ++ th "P1" ++ td (x.perf_optative ! Pl ! P1)) ++
           tr (th "P2" ++ td (x.perf_optative ! Pl ! P2)) ++
           tr (th "P3" ++ td (x.perf_optative ! Pl ! P3)) ++
           tr (intagAttr "th" "rowspan=\"6\"" "pres_admirative" ++ intagAttr "th" "rowspan=\"3\"" "Sg" ++ th "P1" ++ td (x.pres_admirative ! Sg ! P1)) ++
           tr (th "P2" ++ td (x.pres_admirative ! Sg ! P2)) ++
           tr (th "P3" ++ td (x.pres_admirative ! Sg ! P3)) ++
           tr (intagAttr "th" "rowspan=\"3\"" "Pl" ++ th "P1" ++ td (x.pres_admirative ! Pl ! P1)) ++
           tr (th "P2" ++ td (x.pres_admirative ! Pl ! P2)) ++
           tr (th "P3" ++ td (x.pres_admirative ! Pl ! P3)) ++
           tr (intagAttr "th" "rowspan=\"6\"" "imperf_admirative" ++ intagAttr "th" "rowspan=\"3\"" "Sg" ++ th "P1" ++ td (x.imperf_admirative ! Sg ! P1)) ++
           tr (th "P2" ++ td (x.imperf_admirative ! Sg ! P2)) ++
           tr (th "P3" ++ td (x.imperf_admirative ! Sg ! P3)) ++
           tr (intagAttr "th" "rowspan=\"3\"" "Pl" ++ th "P1" ++ td (x.imperf_admirative ! Pl ! P1)) ++
           tr (th "P2" ++ td (x.imperf_admirative ! Pl ! P2)) ++
           tr (th "P3" ++ td (x.imperf_admirative ! Pl ! P3))) ;
      s3=[]
    } ;
lin InflectionAdv = \x -> {
      t="adv" ;
      s1="" ;
      s2=frameTable (
           tr (th "s" ++ td (x.s))) ;
      s3=[]
    } ;
lin
NoDefinition   t     = {s=t.s};
MkDefinition   t d   = {s="<p><b>Definition:</b>"++t.s++d.s++"</p>"};
MkDefinitionEx t d e = {s="<p><b>Definition:</b>"++t.s++d.s++"</p><p><b>Example:</b>"++e.s++"</p>"};lin  MkDocument d i e = {s = i.s1 ++ d.s ++ i.s2 ++ i.s3 ++ e.s} ;  MkTag i = {s = i.t} ;}
