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
           tr (intagAttr "th" "rowspan=\"2\"" "Nom" ++ th "Sg" ++ td (x.s ! Nom ! Sg)) ++
           tr (th "Pl" ++ td (x.s ! Nom ! Pl)) ++
           tr (intagAttr "th" "rowspan=\"2\"" "Acc" ++ th "Sg" ++ td (x.s ! Acc ! Sg)) ++
           tr (th "Pl" ++ td (x.s ! Acc ! Pl)) ++
           tr (intagAttr "th" "rowspan=\"2\"" "Dat" ++ th "Sg" ++ td (x.s ! Dat ! Sg)) ++
           tr (th "Pl" ++ td (x.s ! Dat ! Pl)) ++
           tr (intagAttr "th" "rowspan=\"2\"" "Gen" ++ th "Sg" ++ td (x.s ! Gen ! Sg)) ++
           tr (th "Pl" ++ td (x.s ! Gen ! Pl)) ++
           tr (intagAttr "th" "rowspan=\"2\"" "Ablat" ++ th "Sg" ++ td (x.s ! Ablat ! Sg)) ++
           tr (th "Pl" ++ td (x.s ! Ablat ! Pl)) ++
           tr (intagAttr "th" "rowspan=\"2\"" "Instr" ++ th "Sg" ++ td (x.s ! Instr ! Sg)) ++
           tr (th "Pl" ++ td (x.s ! Instr ! Pl)) ++
           tr (intagAttr "th" "rowspan=\"2\"" "Loc" ++ th "Sg" ++ td (x.s ! Loc ! Sg)) ++
           tr (th "Pl" ++ td (x.s ! Loc ! Pl))) ++
         tag "br" ++
         frameTable (
           tr (intagAttr "th" "rowspan=\"2\"" "Poss1Pl" ++ th "Sg" ++ td (x.poss ! Poss1Pl ! Sg)) ++
           tr (th "Pl" ++ td (x.poss ! Poss1Pl ! Pl)) ++
           tr (intagAttr "th" "rowspan=\"2\"" "Poss1Sg" ++ th "Sg" ++ td (x.poss ! Poss1Sg ! Sg)) ++
           tr (th "Pl" ++ td (x.poss ! Poss1Sg ! Pl)) ++
           tr (intagAttr "th" "rowspan=\"2\"" "Poss2Sg Informal" ++ th "Sg" ++ td (x.poss ! Poss2Sg Informal ! Sg)) ++
           tr (th "Pl" ++ td (x.poss ! Poss2Sg Informal ! Pl)) ++
           tr (intagAttr "th" "rowspan=\"2\"" "Poss2Sg Formal" ++ th "Sg" ++ td (x.poss ! Poss2Sg Formal ! Sg)) ++
           tr (th "Pl" ++ td (x.poss ! Poss2Sg Formal ! Pl)) ++
           tr (intagAttr "th" "rowspan=\"2\"" "Poss3Sg" ++ th "Sg" ++ td (x.poss ! Poss3Sg ! Sg)) ++
           tr (th "Pl" ++ td (x.poss ! Poss3Sg ! Pl))) ;
      s3=[]
    } ;

lin InflectionV, InflectionV2, InflectionV3, InflectionV2V, InflectionV2S,
    InflectionV2Q, InflectionV2A, InflectionVV, InflectionVS,
    InflectionVQ, InflectionVA = \x -> {
      t="v" ;
      s1="" ;
      s2=heading2 "Infinitive" ++ paragraph (x.Infinitive) ++
           -- tr (th "Indicative" ++ th "Fut" ++ td (x.Indicative.Fut)) ++
         heading2 "Present"++
         frameTable (
           tr (intagAttr "th" "rowspan=\"6\"" "Pos" ++ intagAttr "th" "rowspan=\"2\"" "P1" ++ th "Sg" ++ td (x.Indicative.Pres.noAspect ! Pos ! P1 ! Sg)) ++
           tr (th "Pl" ++ td (x.Indicative.Pres.noAspect ! Pos ! P1 ! Pl))  ++
           tr (intagAttr "th" "rowspan=\"2\"" "P2" ++ th "Sg" ++ td (x.Indicative.Pres.noAspect ! Pos ! P2 ! Sg)) ++
           tr (th "Pl" ++ td (x.Indicative.Pres.noAspect ! Pos ! P2 ! Pl)) ++
           tr (intagAttr "th" "rowspan=\"2\"" "P3" ++ th "Sg" ++ td (x.Indicative.Pres.noAspect ! Pos ! P3 ! Sg)) ++
           tr (th "Pl" ++ td (x.Indicative.Pres.noAspect ! Pos ! P3 ! Pl)) ++
           tr (intagAttr "th" "rowspan=\"6\"" "Neg" ++ intagAttr "th" "rowspan=\"2\"" "P1" ++ th "Sg" ++ td (x.Indicative.Pres.noAspect ! Neg ! P1 ! Sg)) ++
           tr (th "Pl" ++ td (x.Indicative.Pres.noAspect ! Neg ! P1 ! Pl)) ++
           tr (intagAttr "th" "rowspan=\"2\"" "P2" ++ th "Sg" ++ td (x.Indicative.Pres.noAspect ! Neg ! P2 ! Sg)) ++
           tr (th "Pl" ++ td (x.Indicative.Pres.noAspect ! Neg ! P2 ! Pl)) ++
           tr (intagAttr "th" "rowspan=\"2\"" "P3" ++ th "Sg" ++ td (x.Indicative.Pres.noAspect ! Neg ! P3 ! Sg)) ++
           tr (th "Pl" ++ td (x.Indicative.Pres.noAspect ! Neg ! P3 ! Pl))) ++
         heading2 "Present Progressive"++
         frameTable (
           tr (intagAttr "th" "rowspan=\"6\"" "Pos" ++ intagAttr "th" "rowspan=\"2\"" "P1" ++ th "Sg" ++ td (x.Indicative.Pres.Progressive ! Pos ! P1 ! Sg))++
           tr (th "Pl" ++ td (x.Indicative.Pres.Progressive ! Pos ! P1 ! Pl)) ++
           tr (intagAttr "th" "rowspan=\"2\"" "P2" ++ th "Sg" ++ td (x.Indicative.Pres.Progressive ! Pos ! P2 ! Sg)) ++
           tr (th "Pl" ++ td (x.Indicative.Pres.Progressive ! Pos ! P2 ! Pl)) ++
           tr (intagAttr "th" "rowspan=\"2\"" "P3" ++ th "Sg" ++ td (x.Indicative.Pres.Progressive ! Pos ! P3 ! Sg)) ++
           tr (th "Pl" ++ td (x.Indicative.Pres.Progressive ! Pos ! P3 ! Pl)) ++
           tr (intagAttr "th" "rowspan=\"6\"" "Neg" ++ intagAttr "th" "rowspan=\"2\"" "P1" ++ th "Sg" ++ td (x.Indicative.Pres.Progressive ! Neg ! P1 ! Sg)) ++
           tr (th "Pl" ++ td (x.Indicative.Pres.Progressive ! Neg ! P1 ! Pl)) ++
           tr (intagAttr "th" "rowspan=\"2\"" "P2" ++ th "Sg" ++ td (x.Indicative.Pres.Progressive ! Neg ! P2 ! Sg)) ++
           tr (th "Pl" ++ td (x.Indicative.Pres.Progressive ! Neg ! P2 ! Pl)) ++
           tr (intagAttr "th" "rowspan=\"2\"" "P3" ++ th "Sg" ++ td (x.Indicative.Pres.Progressive ! Neg ! P3 ! Sg)) ++
           tr (th "Pl" ++ td (x.Indicative.Pres.Progressive ! Neg ! P3 ! Pl))) ++
         heading2 "Past" ++ 
         frameTable (
           tr (intagAttr "th" "rowspan=\"6\"" "Pos" ++ intagAttr "th" "rowspan=\"2\"" "P1" ++ th "Sg" ++ td (x.Indicative.Past.noAspect ! Pos ! P1 ! Sg)) ++
           tr (th "Pl" ++ td (x.Indicative.Past.noAspect ! Pos ! P1 ! Pl)) ++
           tr (intagAttr "th" "rowspan=\"2\"" "P2" ++ th "Sg" ++ td (x.Indicative.Past.noAspect ! Pos ! P2 ! Sg)) ++
           tr (th "Pl" ++ td (x.Indicative.Past.noAspect ! Pos ! P2 ! Pl)) ++
           tr (intagAttr "th" "rowspan=\"2\"" "P3" ++ th "Sg" ++ td (x.Indicative.Past.noAspect ! Pos ! P3 ! Sg)) ++
           tr (th "Pl" ++ td (x.Indicative.Past.noAspect ! Pos ! P3 ! Pl)) ++
           tr (intagAttr "th" "rowspan=\"6\"" "Neg" ++ intagAttr "th" "rowspan=\"2\"" "P1" ++ th "Sg" ++ td (x.Indicative.Past.noAspect ! Neg ! P1 ! Sg)) ++
           tr (th "Pl" ++ td (x.Indicative.Past.noAspect ! Neg ! P1 ! Pl)) ++
           tr (intagAttr "th" "rowspan=\"2\"" "P2" ++ th "Sg" ++ td (x.Indicative.Past.noAspect ! Neg ! P2 ! Sg)) ++
           tr (th "Pl" ++ td (x.Indicative.Past.noAspect ! Neg ! P2 ! Pl)) ++
           tr (intagAttr "th" "rowspan=\"2\"" "P3" ++ th "Sg" ++ td (x.Indicative.Past.noAspect ! Neg ! P3 ! Sg)) ++
           tr (th "Pl" ++ td (x.Indicative.Past.noAspect ! Neg ! P3 ! Pl))) ++
         heading2 "Past Perfect" ++ 
         frameTable (
           tr (intagAttr "th" "rowspan=\"6\"" "Pos" ++ intagAttr "th" "rowspan=\"2\"" "P1" ++ th "Sg" ++ td (x.Indicative.Past.Perfect ! Pos ! P1 ! Sg)) ++
           tr (th "Pl" ++ td (x.Indicative.Past.Perfect ! Pos ! P1 ! Pl)) ++
           tr (intagAttr "th" "rowspan=\"2\"" "P2" ++ th "Sg" ++ td (x.Indicative.Past.Perfect ! Pos ! P2 ! Sg)) ++
           tr (th "Pl" ++ td (x.Indicative.Past.Perfect ! Pos ! P2 ! Pl)) ++
           tr (intagAttr "th" "rowspan=\"2\"" "P3" ++ th "Sg" ++ td (x.Indicative.Past.Perfect ! Pos ! P3 ! Sg)) ++
           tr (th "Pl" ++ td (x.Indicative.Past.Perfect ! Pos ! P3 ! Pl)) ++
           tr (intagAttr "th" "rowspan=\"6\"" "Neg" ++ intagAttr "th" "rowspan=\"2\"" "P1" ++ th "Sg" ++ td (x.Indicative.Past.Perfect ! Neg ! P1 ! Sg)) ++
           tr (th "Pl" ++ td (x.Indicative.Past.Perfect ! Neg ! P1 ! Pl)) ++
           tr (intagAttr "th" "rowspan=\"2\"" "P2" ++ th "Sg" ++ td (x.Indicative.Past.Perfect ! Neg ! P2 ! Sg)) ++
           tr (th "Pl" ++ td (x.Indicative.Past.Perfect ! Neg ! P2 ! Pl)) ++
           tr (intagAttr "th" "rowspan=\"2\"" "P3" ++ th "Sg" ++ td (x.Indicative.Past.Perfect ! Neg ! P3 ! Sg)) ++
           tr (th "Pl" ++ td (x.Indicative.Past.Perfect ! Neg ! P3 ! Pl))) ++
         heading2 "Past Progressive" ++ 
         frameTable (
           tr (intagAttr "th" "rowspan=\"6\"" "Pos" ++ intagAttr "th" "rowspan=\"2\"" "P1" ++ th "Sg" ++ td (x.Indicative.Past.Progressive ! Pos ! P1 ! Sg)) ++
           tr (th "Pl" ++ td (x.Indicative.Past.Progressive ! Pos ! P1 ! Pl)) ++
           tr (intagAttr "th" "rowspan=\"2\"" "P2" ++ th "Sg" ++ td (x.Indicative.Past.Progressive ! Pos ! P2 ! Sg)) ++
           tr (th "Pl" ++ td (x.Indicative.Past.Progressive ! Pos ! P2 ! Pl)) ++
           tr (intagAttr "th" "rowspan=\"2\"" "P3" ++ th "Sg" ++ td (x.Indicative.Past.Progressive ! Pos ! P3 ! Sg)) ++
           tr (th "Pl" ++ td (x.Indicative.Past.Progressive ! Pos ! P3 ! Pl)) ++
           tr (intagAttr "th" "rowspan=\"6\"" "Neg" ++ intagAttr "th" "rowspan=\"2\"" "P1" ++ th "Sg" ++ td (x.Indicative.Past.Progressive ! Neg ! P1 ! Sg)) ++
           tr (th "Pl" ++ td (x.Indicative.Past.Progressive ! Neg ! P1 ! Pl)) ++
           tr (intagAttr "th" "rowspan=\"2\"" "P2" ++ th "Sg" ++ td (x.Indicative.Past.Progressive ! Neg ! P2 ! Sg)) ++
           tr (th "Pl" ++ td (x.Indicative.Past.Progressive ! Neg ! P2 ! Pl)) ++
           tr (intagAttr "th" "rowspan=\"2\"" "P3" ++ th "Sg" ++ td (x.Indicative.Past.Progressive ! Neg ! P3 ! Sg)) ++
           tr (th "Pl" ++ td (x.Indicative.Past.Progressive ! Neg ! P3 ! Pl))) ++
         heading2 "Imperative" ++
         frameTable (
           tr (intagAttr "th" "rowspan=\"4\"" "Pos" ++ intagAttr "th" "rowspan=\"2\"" "Informal" ++ th "Sg" ++ td (x.Imperative_Jussive ! Pos ! Informal ! Sg)) ++
           tr (th "Pl" ++ td (x.Imperative_Jussive ! Pos ! Informal ! Pl)) ++
           tr (intagAttr "th" "rowspan=\"2\"" "Formal" ++ th "Sg" ++ td (x.Imperative_Jussive ! Pos ! Formal ! Sg)) ++
           tr (th "Pl" ++ td (x.Imperative_Jussive ! Pos ! Formal ! Pl)) ++
           tr (intagAttr "th" "rowspan=\"4\"" "Neg" ++ intagAttr "th" "rowspan=\"2\"" "Informal" ++ th "Sg" ++ td (x.Imperative_Jussive ! Neg ! Informal ! Sg)) ++
           tr (th "Pl" ++ td (x.Imperative_Jussive ! Neg ! Informal ! Pl)) ++
           tr (intagAttr "th" "rowspan=\"2\"" "Formal" ++ th "Sg" ++ td (x.Imperative_Jussive ! Neg ! Formal ! Sg)) ++
           tr (th "Pl" ++ td (x.Imperative_Jussive ! Neg ! Formal ! Pl))) ++
         heading2 "Subjunctive" ++
         frameTable (
           tr (intagAttr "th" "rowspan=\"2\"" "P1" ++ th "Sg" ++ td (x.Subjunctive ! P1 ! Sg)) ++
           tr (th "Pl" ++ td (x.Subjunctive ! P1 ! Pl)) ++
           tr (intagAttr "th" "rowspan=\"2\"" "P2" ++ th "Sg" ++ td (x.Subjunctive ! P2 ! Sg)) ++
           tr (th "Pl" ++ td (x.Subjunctive ! P2 ! Pl)) ++
           tr (intagAttr "th" "rowspan=\"2\"" "P3" ++ th "Sg" ++ td (x.Subjunctive ! P3 ! Sg)) ++
           tr (th "Pl" ++ td (x.Subjunctive ! P3 ! Pl))) ;
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
