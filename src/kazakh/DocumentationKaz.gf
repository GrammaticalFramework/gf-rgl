concrete DocumentationKaz of Documentation = CatKaz ** open
  ResKaz, Prelude, HTML in {

lincat
  Inflection = {t : Str; s1,s2,s3 : Str} ;
  Definition = {s : Str} ;
  Document   = {s : Str} ;
  Tag        = {s : Str} ;

lin InflectionN,InflectionN2,InflectionN3 = \x -> {
      t="n" ;
      s1="" ;
      s2=frameTable (
           tr (th "" ++ th "Sg" ++ th "Pl") ++ 
           tr (th "Nom" ++ td (x.s ! Nom ! Sg) ++ td (x.s ! Nom ! Pl)) ++
           tr (th "Acc" ++ td (x.s ! Acc ! Sg) ++ td (x.s ! Acc ! Pl)) ++
           tr (th "Dat" ++ td (x.s ! Dat ! Sg) ++ td (x.s ! Dat ! Pl)) ++
           tr (th "Gen" ++ td (x.s ! Gen ! Sg) ++ td (x.s ! Gen ! Pl)) ++
           tr (th "Ablat" ++ td (x.s ! Ablat ! Sg) ++ td (x.s ! Ablat ! Pl)) ++
           tr (th "Instr" ++ td (x.s ! Instr ! Sg) ++ td (x.s ! Instr ! Pl)) ++
           tr (th "Loc" ++ td (x.s ! Loc ! Sg) ++ td (x.s ! Loc ! Pl)) ++
           tr (intagAttr "th" "colspan=\"3\"" "poss") ++
           tr (th "" ++ th "Sg" ++ th "Pl") ++
           tr (th "P1 Sg" ++ td (x.poss ! Sg ! P1 ! Sg) ++ td (x.poss ! Sg ! P1 ! Pl)) ++
           tr (th "P1 Pl" ++ td (x.poss ! Pl ! P1 ! Sg) ++ td (x.poss ! Pl ! P1 ! Pl)) ++
           tr (th "P2 Sg Informal" ++ td (x.poss ! Sg ! P2 Informal ! Sg) ++ td (x.poss ! Sg ! P2 Informal ! Pl)) ++
           tr (th "P2 Sg Formal" ++ td (x.poss ! Sg ! P2 Formal ! Sg) ++ td (x.poss ! Sg ! P2 Formal ! Pl)) ++
           tr (th "P3 Sg" ++ td (x.poss ! Sg ! P3 ! Sg) ++ td (x.poss ! Sg ! P3 ! Pl))) ;
      s3=[]
    } ;

lin InflectionV, InflectionV2, InflectionV3, InflectionV2V, InflectionV2S,
    InflectionV2Q, InflectionV2A, InflectionVV, InflectionVS,
    InflectionVQ, InflectionVA = \x -> {
      t="v" ;
      s1="" ;
      s2=heading2 "Infinitive" ++ paragraph (x.infinitive) ++
           -- tr (th "Indicative" ++ th "Fut" ++ td (x.indicative.Fut)) ++
         heading2 "Present"++
         frameTable (
           tr (intagAttr "th" "rowspan=\"6\"" "Pos" ++ intagAttr "th" "rowspan=\"2\"" "P1" ++ th "Sg" ++ td (x.indicative.pres.noAspect ! Pos ! P1 ! Sg)) ++
           tr (th "Pl" ++ td (x.indicative.pres.noAspect ! Pos ! P1 ! Pl))  ++
           tr (intagAttr "th" "rowspan=\"2\"" "P2 Informal" ++ th "Sg" ++ td (x.indicative.pres.noAspect ! Pos ! P2 Informal ! Sg)) ++
           tr (th "Pl" ++ td (x.indicative.pres.noAspect ! Pos ! P2 Informal ! Pl)) ++
           tr (intagAttr "th" "rowspan=\"2\"" "P2 Formal" ++ th "Sg" ++ td (x.indicative.pres.noAspect ! Pos ! P2 Formal ! Sg)) ++
           tr (th "Pl" ++ td (x.indicative.pres.noAspect ! Pos ! P2 Formal ! Pl)) ++
           tr (intagAttr "th" "rowspan=\"2\"" "P3" ++ th "Sg" ++ td (x.indicative.pres.noAspect ! Pos ! P3 ! Sg)) ++
           tr (th "Pl" ++ td (x.indicative.pres.noAspect ! Pos ! P3 ! Pl)) ++
           tr (intagAttr "th" "rowspan=\"6\"" "Neg" ++ intagAttr "th" "rowspan=\"2\"" "P1" ++ th "Sg" ++ td (x.indicative.pres.noAspect ! Neg ! P1 ! Sg)) ++
           tr (th "Pl" ++ td (x.indicative.pres.noAspect ! Neg ! P1 ! Pl)) ++
           tr (intagAttr "th" "rowspan=\"2\"" "P2 Informal" ++ th "Sg" ++ td (x.indicative.pres.noAspect ! Neg ! P2 Informal ! Sg)) ++
           tr (th "Pl" ++ td (x.indicative.pres.noAspect ! Neg ! P2 Formal ! Pl)) ++
           tr (intagAttr "th" "rowspan=\"2\"" "P2 Formal" ++ th "Sg" ++ td (x.indicative.pres.noAspect ! Neg ! P2 Formal ! Sg)) ++
           tr (th "Pl" ++ td (x.indicative.pres.noAspect ! Neg ! P2 Formal ! Pl)) ++
           tr (intagAttr "th" "rowspan=\"2\"" "P3" ++ th "Sg" ++ td (x.indicative.pres.noAspect ! Neg ! P3 ! Sg)) ++
           tr (th "Pl" ++ td (x.indicative.pres.noAspect ! Neg ! P3 ! Pl))) ++
         heading2 "Present Progressive"++
         frameTable (
           tr (intagAttr "th" "rowspan=\"6\"" "Pos" ++ intagAttr "th" "rowspan=\"2\"" "P1" ++ th "Sg" ++ td (x.indicative.pres.progressive ! Pos ! P1 ! Sg))++
           tr (th "Pl" ++ td (x.indicative.pres.progressive ! Pos ! P1 ! Pl)) ++
           tr (intagAttr "th" "rowspan=\"2\"" "P2 Informal" ++ th "Sg" ++ td (x.indicative.pres.progressive ! Pos ! P2 Informal ! Sg)) ++
           tr (th "Pl" ++ td (x.indicative.pres.progressive ! Pos ! P2 Informal ! Pl)) ++
           tr (intagAttr "th" "rowspan=\"2\"" "P2 Formal" ++ th "Sg" ++ td (x.indicative.pres.progressive ! Pos ! P2 Formal ! Sg)) ++
           tr (th "Pl" ++ td (x.indicative.pres.progressive ! Pos ! P2 Formal ! Pl)) ++
           tr (intagAttr "th" "rowspan=\"2\"" "P3" ++ th "Sg" ++ td (x.indicative.pres.progressive ! Pos ! P3 ! Sg)) ++
           tr (th "Pl" ++ td (x.indicative.pres.progressive ! Pos ! P3 ! Pl)) ++
           tr (intagAttr "th" "rowspan=\"6\"" "Neg" ++ intagAttr "th" "rowspan=\"2\"" "P1" ++ th "Sg" ++ td (x.indicative.pres.progressive ! Neg ! P1 ! Sg)) ++
           tr (th "Pl" ++ td (x.indicative.pres.progressive ! Neg ! P1 ! Pl)) ++
           tr (intagAttr "th" "rowspan=\"2\"" "P2 Informal" ++ th "Sg" ++ td (x.indicative.pres.progressive ! Neg ! P2 Informal ! Sg)) ++
           tr (th "Pl" ++ td (x.indicative.pres.progressive ! Neg ! P2 Informal ! Pl)) ++
           tr (intagAttr "th" "rowspan=\"2\"" "P2 Formal" ++ th "Sg" ++ td (x.indicative.pres.progressive ! Neg ! P2 Formal ! Sg)) ++
           tr (th "Pl" ++ td (x.indicative.pres.progressive ! Neg ! P2 Formal ! Pl)) ++
           tr (intagAttr "th" "rowspan=\"2\"" "P3" ++ th "Sg" ++ td (x.indicative.pres.progressive ! Neg ! P3 ! Sg)) ++
           tr (th "Pl" ++ td (x.indicative.pres.progressive ! Neg ! P3 ! Pl))) ++
         heading2 "Past" ++ 
         frameTable (
           tr (intagAttr "th" "rowspan=\"6\"" "Pos" ++ intagAttr "th" "rowspan=\"2\"" "P1" ++ th "Sg" ++ td (x.indicative.past.noAspect ! Pos ! P1 ! Sg)) ++
           tr (th "Pl" ++ td (x.indicative.past.noAspect ! Pos ! P1 ! Pl)) ++
           tr (intagAttr "th" "rowspan=\"2\"" "P2 Informal" ++ th "Sg" ++ td (x.indicative.past.noAspect ! Pos ! P2 Informal ! Sg)) ++
           tr (th "Pl" ++ td (x.indicative.past.noAspect ! Pos ! P2 Informal ! Pl)) ++
           tr (intagAttr "th" "rowspan=\"2\"" "P2 Formal" ++ th "Sg" ++ td (x.indicative.past.noAspect ! Pos ! P2 Formal ! Sg)) ++
           tr (th "Pl" ++ td (x.indicative.past.noAspect ! Pos ! P2 Formal ! Pl)) ++
           tr (intagAttr "th" "rowspan=\"2\"" "P3" ++ th "Sg" ++ td (x.indicative.past.noAspect ! Pos ! P3 ! Sg)) ++
           tr (th "Pl" ++ td (x.indicative.past.noAspect ! Pos ! P3 ! Pl)) ++
           tr (intagAttr "th" "rowspan=\"6\"" "Neg" ++ intagAttr "th" "rowspan=\"2\"" "P1" ++ th "Sg" ++ td (x.indicative.past.noAspect ! Neg ! P1 ! Sg)) ++
           tr (th "Pl" ++ td (x.indicative.past.noAspect ! Neg ! P1 ! Pl)) ++
           tr (intagAttr "th" "rowspan=\"2\"" "P2 Informal" ++ th "Sg" ++ td (x.indicative.past.noAspect ! Neg ! P2 Informal ! Sg)) ++
           tr (th "Pl" ++ td (x.indicative.past.noAspect ! Neg ! P2 Informal ! Pl)) ++
           tr (intagAttr "th" "rowspan=\"2\"" "P2 Formal" ++ th "Sg" ++ td (x.indicative.past.noAspect ! Neg ! P2 Formal ! Sg)) ++
           tr (th "Pl" ++ td (x.indicative.past.noAspect ! Neg ! P2 Formal ! Pl)) ++
           tr (intagAttr "th" "rowspan=\"2\"" "P3" ++ th "Sg" ++ td (x.indicative.past.noAspect ! Neg ! P3 ! Sg)) ++
           tr (th "Pl" ++ td (x.indicative.past.noAspect ! Neg ! P3 ! Pl))) ++
         heading2 "Past Perfect" ++ 
         frameTable (
           tr (intagAttr "th" "rowspan=\"6\"" "Pos" ++ intagAttr "th" "rowspan=\"2\"" "P1" ++ th "Sg" ++ td (x.indicative.past.perfect ! Pos ! P1 ! Sg)) ++
           tr (th "Pl" ++ td (x.indicative.past.perfect ! Pos ! P1 ! Pl)) ++
           tr (intagAttr "th" "rowspan=\"2\"" "P2 Informal" ++ th "Sg" ++ td (x.indicative.past.perfect ! Pos ! P2 Informal ! Sg)) ++
           tr (th "Pl" ++ td (x.indicative.past.perfect ! Pos ! P2 Informal ! Pl)) ++
           tr (intagAttr "th" "rowspan=\"2\"" "P2 Formal" ++ th "Sg" ++ td (x.indicative.past.perfect ! Pos ! P2 Formal ! Sg)) ++
           tr (th "Pl" ++ td (x.indicative.past.perfect ! Pos ! P2 Formal ! Pl)) ++
           tr (intagAttr "th" "rowspan=\"2\"" "P3" ++ th "Sg" ++ td (x.indicative.past.perfect ! Pos ! P3 ! Sg)) ++
           tr (th "Pl" ++ td (x.indicative.past.perfect ! Pos ! P3 ! Pl)) ++
           tr (intagAttr "th" "rowspan=\"6\"" "Neg" ++ intagAttr "th" "rowspan=\"2\"" "P1" ++ th "Sg" ++ td (x.indicative.past.perfect ! Neg ! P1 ! Sg)) ++
           tr (th "Pl" ++ td (x.indicative.past.perfect ! Neg ! P1 ! Pl)) ++
           tr (intagAttr "th" "rowspan=\"2\"" "P2 Informal" ++ th "Sg" ++ td (x.indicative.past.perfect ! Neg ! P2 Informal ! Sg)) ++
           tr (th "Pl" ++ td (x.indicative.past.perfect ! Neg ! P2 Informal ! Pl)) ++
           tr (intagAttr "th" "rowspan=\"2\"" "P2 Formal" ++ th "Sg" ++ td (x.indicative.past.perfect ! Neg ! P2 Formal ! Sg)) ++
           tr (th "Pl" ++ td (x.indicative.past.perfect ! Neg ! P2 Formal ! Pl)) ++
           tr (intagAttr "th" "rowspan=\"2\"" "P3" ++ th "Sg" ++ td (x.indicative.past.perfect ! Neg ! P3 ! Sg)) ++
           tr (th "Pl" ++ td (x.indicative.past.perfect ! Neg ! P3 ! Pl))) ++
         heading2 "Past Progressive" ++ 
         frameTable (
           tr (intagAttr "th" "rowspan=\"6\"" "Pos" ++ intagAttr "th" "rowspan=\"2\"" "P1" ++ th "Sg" ++ td (x.indicative.past.progressive ! Pos ! P1 ! Sg)) ++
           tr (th "Pl" ++ td (x.indicative.past.progressive ! Pos ! P1 ! Pl)) ++
           tr (intagAttr "th" "rowspan=\"2\"" "P2 Informal" ++ th "Sg" ++ td (x.indicative.past.progressive ! Pos ! P2 Informal ! Sg)) ++
           tr (th "Pl" ++ td (x.indicative.past.progressive ! Pos ! P2 Informal ! Pl)) ++
           tr (intagAttr "th" "rowspan=\"2\"" "P2 Formal" ++ th "Sg" ++ td (x.indicative.past.progressive ! Pos ! P2 Formal ! Sg)) ++
           tr (th "Pl" ++ td (x.indicative.past.progressive ! Pos ! P2 Formal ! Pl)) ++
           tr (intagAttr "th" "rowspan=\"2\"" "P3" ++ th "Sg" ++ td (x.indicative.past.progressive ! Pos ! P3 ! Sg)) ++
           tr (th "Pl" ++ td (x.indicative.past.progressive ! Pos ! P3 ! Pl)) ++
           tr (intagAttr "th" "rowspan=\"6\"" "Neg" ++ intagAttr "th" "rowspan=\"2\"" "P1" ++ th "Sg" ++ td (x.indicative.past.progressive ! Neg ! P1 ! Sg)) ++
           tr (th "Pl" ++ td (x.indicative.past.progressive ! Neg ! P1 ! Pl)) ++
           tr (intagAttr "th" "rowspan=\"2\"" "P2 Informal" ++ th "Sg" ++ td (x.indicative.past.progressive ! Neg ! P2 Informal ! Sg)) ++
           tr (th "Pl" ++ td (x.indicative.past.progressive ! Neg ! P2 Informal ! Pl)) ++
           tr (intagAttr "th" "rowspan=\"2\"" "P2 Formal" ++ th "Sg" ++ td (x.indicative.past.progressive ! Neg ! P2 Formal ! Sg)) ++
           tr (th "Pl" ++ td (x.indicative.past.progressive ! Neg ! P2 Formal ! Pl)) ++
           tr (intagAttr "th" "rowspan=\"2\"" "P3" ++ th "Sg" ++ td (x.indicative.past.progressive ! Neg ! P3 ! Sg)) ++
           tr (th "Pl" ++ td (x.indicative.past.progressive ! Neg ! P3 ! Pl))) ++
         heading2 "Imperative" ++
         frameTable (
           tr (intagAttr "th" "rowspan=\"4\"" "Pos" ++ intagAttr "th" "rowspan=\"2\"" "Informal" ++ th "Sg" ++ td (x.imperative ! Pos ! Informal ! Sg)) ++
           tr (th "Pl" ++ td (x.imperative ! Pos ! Informal ! Pl)) ++
           tr (intagAttr "th" "rowspan=\"2\"" "Formal" ++ th "Sg" ++ td (x.imperative ! Pos ! Formal ! Sg)) ++
           tr (th "Pl" ++ td (x.imperative ! Pos ! Formal ! Pl)) ++
           tr (intagAttr "th" "rowspan=\"4\"" "Neg" ++ intagAttr "th" "rowspan=\"2\"" "Informal" ++ th "Sg" ++ td (x.imperative ! Neg ! Informal ! Sg)) ++
           tr (th "Pl" ++ td (x.imperative ! Neg ! Informal ! Pl)) ++
           tr (intagAttr "th" "rowspan=\"2\"" "Formal" ++ th "Sg" ++ td (x.imperative ! Neg ! Formal ! Sg)) ++
           tr (th "Pl" ++ td (x.imperative ! Neg ! Formal ! Pl))) ++
         heading2 "Subjunctive" ++
         frameTable (
           tr (intagAttr "th" "rowspan=\"2\"" "P1" ++ th "Sg" ++ td (x.subjunctive ! P1 ! Sg)) ++
           tr (th "Pl" ++ td (x.subjunctive ! P1 ! Pl)) ++
           tr (intagAttr "th" "rowspan=\"2\"" "P2 Informal" ++ th "Sg" ++ td (x.subjunctive ! P2 Informal ! Sg)) ++
           tr (th "Pl" ++ td (x.subjunctive ! P2 Informal ! Pl)) ++
           tr (intagAttr "th" "rowspan=\"2\"" "P2 Formal" ++ th "Sg" ++ td (x.subjunctive ! P2 Formal ! Sg)) ++
           tr (th "Pl" ++ td (x.subjunctive ! P2 Formal ! Pl)) ++
           tr (intagAttr "th" "rowspan=\"2\"" "P3" ++ th "Sg" ++ td (x.subjunctive ! P3 ! Sg)) ++
           tr (th "Pl" ++ td (x.subjunctive ! P3 ! Pl))) ;
      s3=[]
    } ;

lin
  NoDefinition   t     = {s=t.s};
  MkDefinition   t d   = {s="<p><b>Дефиниция:</b>"++t.s++d.s++"</p>"};
  MkDefinitionEx t d e = {s="<p><b>Дефиниция:</b>"++t.s++d.s++"</p><p><b>Пример:</b>"++e.s++"</p>"};

lin
  MkDocument d i e = {s = i.s1 ++ d.s ++ i.s2 ++ i.s3 ++ e.s} ;
  MkTag i = {s = i.t} ;

}
