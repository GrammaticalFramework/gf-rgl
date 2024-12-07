concrete DocumentationSqi of Documentation = CatSqi ** open
  ParamX, ResSqi, Prelude, HTML in {

lincat
  Inflection = {t : Str; s1,s2,s3 : Str} ;
  Definition = {s : Str} ;
  Document   = {s : Str} ;
  Tag        = {s : Str} ;

lin InflectionN = \x -> {
      t="em" ;
      s1=heading1 ("Emër" ++
                   case x.g of {
                     Masc => "(mashkullor)" ;
                     Fem  => "(femëror)"
                   }) ;
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
      t="mb" ;
      s1=heading1 "Mbiemër" ;
      s2=frameTable (
           tr (intagAttr "th" "colspan=\"2\"" "" ++ th "Sg" ++ th "Pl") ++
           tr (intagAttr "th" "rowspan=\"2\"" "Nom" ++ th "Masc" ++ td (y ! Nom ! Masc ! Sg) ++ td (y ! Nom ! Masc ! Pl)) ++
           tr (th "Fem" ++ td (y ! Nom ! Fem ! Sg) ++ td (y ! Nom ! Fem ! Pl)) ++
           tr (intagAttr "th" "rowspan=\"2\"" "Acc" ++ th "Masc" ++ td (y ! Acc ! Masc ! Sg) ++ td (y ! Acc ! Masc ! Pl)) ++
           tr (th "Fem" ++ td (y ! Acc ! Fem ! Sg) ++ td (y ! Acc ! Fem ! Pl)) ++
           tr (intagAttr "th" "rowspan=\"2\"" "Dat" ++ th "Masc" ++ td (y ! Dat ! Masc ! Sg) ++ td (y ! Dat ! Masc ! Pl)) ++
           tr (th "Fem" ++ td (y ! Dat ! Fem ! Sg) ++ td (y ! Dat ! Fem ! Pl)) ++
           tr (intagAttr "th" "rowspan=\"2\"" "Ablat" ++ th "Masc" ++ td (y ! Ablat ! Masc ! Sg) ++ td (y ! Ablat ! Masc ! Pl)) ++
           tr (th "Fem" ++ td (y ! Ablat ! Fem ! Sg) ++ td (y ! Ablat ! Fem ! Pl))) ;
      s3=[]
    } where {
        y : Case => Gender => Number => Str =
          \\c,g,n => case x.clit of {
                       True  => link_clitic ! Indef ! c ! g ! n ++ x.s ! c ! g ! n ;
                       False => x.s ! c ! g ! n
                     } ;
    } ;
lin InflectionV = \x -> {
      t="fl" ;
      s1=heading1 "Folje" ;
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
      t="nd" ;
      s1=heading1 "Ndajfolje" ;
      s2=frameTable (
           tr (th "s" ++ td (x.s))) ;
      s3=[]
    } ;
lin
NoDefinition   t     = {s=t.s};
MkDefinition   t d   = {s="<p><b>Definition:</b>"++t.s++d.s++"</p>"};
MkDefinitionEx t d e = {s="<p><b>Definition:</b>"++t.s++d.s++"</p><p><b>Example:</b>"++e.s++"</p>"};lin  MkDocument d i e = {s = i.s1 ++ d.s ++ i.s2 ++ i.s3 ++ e.s} ;  MkTag i = {s = i.t} ;}
