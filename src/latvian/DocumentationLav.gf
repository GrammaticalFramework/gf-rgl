--# -path=.:../abstract:../common
concrete DocumentationLav of Documentation = CatLav ** open
  ResLav,
  Prelude,
  HTML in {

lincat
  Inflection = {t : Str; s1,s2,s3 : Str} ;
  Definition = {s : Str} ;
  Document   = {s : Str} ;
  Tag        = {s : Str} ;

lin InflectionN, InflectionN2, InflectionN3 = \x -> {
      t="n" ;
      s1=heading1 ("Noun" ++
                   case x.gend of {
                     Masc => "(masculine)" ;
                     Fem  => "(feminine)"
                   }) ;
      s2=frameTable (
           tr (th ""    ++ th "Sg" ++             th "Pl") ++
           tr (th "Nom" ++ td (x.s ! Sg ! Nom) ++ td (x.s ! Pl ! Nom)) ++
           tr (th "Acc" ++ td (x.s ! Sg ! Acc) ++ td (x.s ! Pl ! Acc)) ++
           tr (th "Dat" ++ td (x.s ! Sg ! Dat) ++ td (x.s ! Pl ! Dat)) ++
           tr (th "Gen" ++ td (x.s ! Sg ! Gen) ++ td (x.s ! Pl ! Gen)) ++
           tr (th "Loc" ++ td (x.s ! Sg ! Loc) ++ td (x.s ! Pl ! Loc)) ++
           tr (th "Voc" ++ td (x.s ! Sg ! Voc) ++ td (x.s ! Pl ! Voc))) ;
      s3=[]
    } ;

lin InflectionPN = \pn -> {
      t  = "pn" ;
      s1 = heading1 ("Proper Name" ++ "(" ++ BIND ++
                     case pn.gend of {
                       Masc  => "masculine";
                       Fem   => "feminine"
                     } ++
                     case pn.num of {
                       Sg => "singular";
                       Pl => "plural"
                     } ++
                     BIND ++ ")") ;
      s2 = frameTable (
             tr (th "Nom" ++ td (pn.s ! Nom)) ++
             tr (th "Acc" ++ td (pn.s ! Acc)) ++
             tr (th "Dat" ++ td (pn.s ! Dat)) ++
             tr (th "Gen" ++ td (pn.s ! Gen)) ++
             tr (th "Loc" ++ td (pn.s ! Loc)) ++
             tr (th "Voc" ++ td (pn.s ! Voc))) ;
      s3=[]
    } ;

lin InflectionLN = \pn -> {
      t  = "ln" ;
      s1 = heading1 ("Location Name" ++ "(" ++ BIND ++
                     case pn.gend of {
                       Masc  => "masculine";
                       Fem   => "feminine"
                     } ++
                     case pn.num of {
                       Sg => "singular";
                       Pl => "plural"
                     } ++
                     BIND ++ ")") ;
      s2 = frameTable (
             tr (th "Nom" ++ td (pn.s ! Nom)) ++
             tr (th "Acc" ++ td (pn.s ! Acc)) ++
             tr (th "Dat" ++ td (pn.s ! Dat)) ++
             tr (th "Gen" ++ td (pn.s ! Gen)) ++
             tr (th "Loc" ++ td (pn.s ! Loc)) ++
             tr (th "Voc" ++ td (pn.s ! Voc))) ;
      s3=[]
    } ;

lin InflectionGN = \pn -> {
      t  = "ln" ;
      s1 = heading1 ("Given Name" ++
                     case pn.gend of {
                       Masc  => "(masculine)";
                       Fem   => "(feminine)"
                     }) ;
      s2 = frameTable (
             tr (th "Nom" ++ td (pn.s ! Nom)) ++
             tr (th "Acc" ++ td (pn.s ! Acc)) ++
             tr (th "Dat" ++ td (pn.s ! Dat)) ++
             tr (th "Gen" ++ td (pn.s ! Gen)) ++
             tr (th "Loc" ++ td (pn.s ! Loc)) ++
             tr (th "Voc" ++ td (pn.s ! Voc))) ;
      s3=[]
    } ;

lin InflectionSN = \pn -> {
      t  = "ln" ;
      s1 = heading1 "Family Name" ;
      s2 = frameTable (
             tr (th "Nom" ++ td (pn.s ! Male ! Nom)) ++
             tr (th "Acc" ++ td (pn.s ! Male ! Acc)) ++
             tr (th "Dat" ++ td (pn.s ! Male ! Dat)) ++
             tr (th "Gen" ++ td (pn.s ! Male ! Gen)) ++
             tr (th "Loc" ++ td (pn.s ! Male ! Loc)) ++
             tr (th "Voc" ++ td (pn.s ! Male ! Voc))) ;
      s3=[]
    } ;

lin InflectionA, InflectionA2 = \adj -> {
      t="a" ;
      s1=heading1 "Adjective" ;
      s2=frameTable (
           tr (intagAttr "th" "rowspan=\"2\" colspan=\"2\"" "" ++ intagAttr "th" "colspan=\"2\"" "Masculine" ++ intagAttr "th" "colspan=\"2\"" "Feminine") ++
           tr (                                     th "Sg" ++ th "Pl" ++ th "Sg" ++ th "Pl") ++
           intagAttr "th" "colspan=\"6\"" "Positive" ++
           tr (intagAttr "th" "rowspan=\"6\"" "Indef" ++
               th "Nom" ++ td (adj.s ! AAdj Posit Indef Masc Sg Nom) ++ td (adj.s ! AAdj Posit Indef Masc Pl Nom) ++ td (adj.s ! AAdj Posit Indef Fem Sg Nom) ++ td (adj.s ! AAdj Posit Indef Fem Pl Nom)) ++
           tr (th "Acc" ++ td (adj.s ! AAdj Posit Indef Masc Sg Acc) ++ td (adj.s ! AAdj Posit Indef Masc Pl Acc) ++ td (adj.s ! AAdj Posit Indef Fem Sg Nom) ++ td (adj.s ! AAdj Posit Indef Fem Pl Nom)) ++
           tr (th "Dat" ++ td (adj.s ! AAdj Posit Indef Masc Sg Dat) ++ td (adj.s ! AAdj Posit Indef Masc Pl Dat) ++ td (adj.s ! AAdj Posit Indef Fem Sg Nom) ++ td (adj.s ! AAdj Posit Indef Fem Pl Nom)) ++
           tr (th "Gen" ++ td (adj.s ! AAdj Posit Indef Masc Sg Gen) ++ td (adj.s ! AAdj Posit Indef Masc Pl Gen) ++ td (adj.s ! AAdj Posit Indef Fem Sg Nom) ++ td (adj.s ! AAdj Posit Indef Fem Pl Nom)) ++
           tr (th "Loc" ++ td (adj.s ! AAdj Posit Indef Masc Sg Loc) ++ td (adj.s ! AAdj Posit Indef Masc Pl Loc) ++ td (adj.s ! AAdj Posit Indef Fem Sg Nom) ++ td (adj.s ! AAdj Posit Indef Fem Pl Nom)) ++
           tr (th "Voc" ++ td (adj.s ! AAdj Posit Indef Masc Sg Voc) ++ td (adj.s ! AAdj Posit Indef Masc Pl Voc) ++ td (adj.s ! AAdj Posit Indef Fem Sg Nom) ++ td (adj.s ! AAdj Posit Indef Fem Pl Nom)) ++
           tr (intagAttr "th" "rowspan=\"6\"" "Def" ++
               th "Nom" ++ td (adj.s ! AAdj Posit Def Masc Sg Nom) ++ td (adj.s ! AAdj Posit Def Masc Pl Nom) ++ td (adj.s ! AAdj Posit Def Fem Sg Nom) ++ td (adj.s ! AAdj Posit Def Fem Pl Nom)) ++
           tr (th "Acc" ++ td (adj.s ! AAdj Posit Def Masc Sg Acc) ++ td (adj.s ! AAdj Posit Def Masc Pl Acc) ++ td (adj.s ! AAdj Posit Def Fem Sg Nom) ++ td (adj.s ! AAdj Posit Def Fem Pl Nom)) ++
           tr (th "Dat" ++ td (adj.s ! AAdj Posit Def Masc Sg Dat) ++ td (adj.s ! AAdj Posit Def Masc Pl Dat) ++ td (adj.s ! AAdj Posit Def Fem Sg Nom) ++ td (adj.s ! AAdj Posit Def Fem Pl Nom)) ++
           tr (th "Gen" ++ td (adj.s ! AAdj Posit Def Masc Sg Gen) ++ td (adj.s ! AAdj Posit Def Masc Pl Gen) ++ td (adj.s ! AAdj Posit Def Fem Sg Nom) ++ td (adj.s ! AAdj Posit Def Fem Pl Nom)) ++
           tr (th "Loc" ++ td (adj.s ! AAdj Posit Def Masc Sg Loc) ++ td (adj.s ! AAdj Posit Def Masc Pl Loc) ++ td (adj.s ! AAdj Posit Def Fem Sg Nom) ++ td (adj.s ! AAdj Posit Def Fem Pl Nom)) ++
           tr (th "Voc" ++ td (adj.s ! AAdj Posit Def Masc Sg Voc) ++ td (adj.s ! AAdj Posit Def Masc Pl Voc) ++ td (adj.s ! AAdj Posit Def Fem Sg Nom) ++ td (adj.s ! AAdj Posit Def Fem Pl Nom)) ++
           intagAttr "th" "colspan=\"6\"" "Comparative" ++
           tr (intagAttr "th" "rowspan=\"6\"" "Indef" ++
               th "Nom" ++ td (adj.s ! AAdj Compar Indef Masc Sg Nom) ++ td (adj.s ! AAdj Compar Indef Masc Pl Nom) ++ td (adj.s ! AAdj Compar Indef Fem Sg Nom) ++ td (adj.s ! AAdj Compar Indef Fem Pl Nom)) ++
           tr (th "Acc" ++ td (adj.s ! AAdj Compar Indef Masc Sg Acc) ++ td (adj.s ! AAdj Compar Indef Masc Pl Acc) ++ td (adj.s ! AAdj Compar Indef Fem Sg Nom) ++ td (adj.s ! AAdj Compar Indef Fem Pl Nom)) ++
           tr (th "Dat" ++ td (adj.s ! AAdj Compar Indef Masc Sg Dat) ++ td (adj.s ! AAdj Compar Indef Masc Pl Dat) ++ td (adj.s ! AAdj Compar Indef Fem Sg Nom) ++ td (adj.s ! AAdj Compar Indef Fem Pl Nom)) ++
           tr (th "Gen" ++ td (adj.s ! AAdj Compar Indef Masc Sg Gen) ++ td (adj.s ! AAdj Compar Indef Masc Pl Gen) ++ td (adj.s ! AAdj Compar Indef Fem Sg Nom) ++ td (adj.s ! AAdj Compar Indef Fem Pl Nom)) ++
           tr (th "Loc" ++ td (adj.s ! AAdj Compar Indef Masc Sg Loc) ++ td (adj.s ! AAdj Compar Indef Masc Pl Loc) ++ td (adj.s ! AAdj Compar Indef Fem Sg Nom) ++ td (adj.s ! AAdj Compar Indef Fem Pl Nom)) ++
           tr (th "Voc" ++ td (adj.s ! AAdj Compar Indef Masc Sg Voc) ++ td (adj.s ! AAdj Compar Indef Masc Pl Voc) ++ td (adj.s ! AAdj Compar Indef Fem Sg Nom) ++ td (adj.s ! AAdj Compar Indef Fem Pl Nom)) ++
           tr (intagAttr "th" "rowspan=\"6\"" "Def" ++
               th "Nom" ++ td (adj.s ! AAdj Compar Def Masc Sg Nom) ++ td (adj.s ! AAdj Compar Def Masc Pl Nom) ++ td (adj.s ! AAdj Compar Def Fem Sg Nom) ++ td (adj.s ! AAdj Compar Def Fem Pl Nom)) ++
           tr (th "Acc" ++ td (adj.s ! AAdj Compar Def Masc Sg Acc) ++ td (adj.s ! AAdj Compar Def Masc Pl Acc) ++ td (adj.s ! AAdj Compar Def Fem Sg Nom) ++ td (adj.s ! AAdj Compar Def Fem Pl Nom)) ++
           tr (th "Dat" ++ td (adj.s ! AAdj Compar Def Masc Sg Dat) ++ td (adj.s ! AAdj Compar Def Masc Pl Dat) ++ td (adj.s ! AAdj Compar Def Fem Sg Nom) ++ td (adj.s ! AAdj Compar Def Fem Pl Nom)) ++
           tr (th "Gen" ++ td (adj.s ! AAdj Compar Def Masc Sg Gen) ++ td (adj.s ! AAdj Compar Def Masc Pl Gen) ++ td (adj.s ! AAdj Compar Def Fem Sg Nom) ++ td (adj.s ! AAdj Compar Def Fem Pl Nom)) ++
           tr (th "Loc" ++ td (adj.s ! AAdj Compar Def Masc Sg Loc) ++ td (adj.s ! AAdj Compar Def Masc Pl Loc) ++ td (adj.s ! AAdj Compar Def Fem Sg Nom) ++ td (adj.s ! AAdj Compar Def Fem Pl Nom)) ++
           tr (th "Voc" ++ td (adj.s ! AAdj Compar Def Masc Sg Voc) ++ td (adj.s ! AAdj Compar Def Masc Pl Voc) ++ td (adj.s ! AAdj Compar Def Fem Sg Nom) ++ td (adj.s ! AAdj Compar Def Fem Pl Nom)) ++
           intagAttr "th" "colspan=\"6\"" "Superlative" ++
           tr (intagAttr "th" "rowspan=\"6\"" "Def" ++
               th "Nom" ++ td (adj.s ! AAdj Superl Def Masc Sg Nom) ++ td (adj.s ! AAdj Superl Def Masc Pl Nom) ++ td (adj.s ! AAdj Superl Def Fem Sg Nom) ++ td (adj.s ! AAdj Superl Def Fem Pl Nom)) ++
           tr (th "Acc" ++ td (adj.s ! AAdj Superl Def Masc Sg Acc) ++ td (adj.s ! AAdj Superl Def Masc Pl Acc) ++ td (adj.s ! AAdj Superl Def Fem Sg Nom) ++ td (adj.s ! AAdj Superl Def Fem Pl Nom)) ++
           tr (th "Dat" ++ td (adj.s ! AAdj Superl Def Masc Sg Dat) ++ td (adj.s ! AAdj Superl Def Masc Pl Dat) ++ td (adj.s ! AAdj Superl Def Fem Sg Nom) ++ td (adj.s ! AAdj Superl Def Fem Pl Nom)) ++
           tr (th "Gen" ++ td (adj.s ! AAdj Superl Def Masc Sg Gen) ++ td (adj.s ! AAdj Superl Def Masc Pl Gen) ++ td (adj.s ! AAdj Superl Def Fem Sg Nom) ++ td (adj.s ! AAdj Superl Def Fem Pl Nom)) ++
           tr (th "Loc" ++ td (adj.s ! AAdj Superl Def Masc Sg Loc) ++ td (adj.s ! AAdj Superl Def Masc Pl Loc) ++ td (adj.s ! AAdj Superl Def Fem Sg Nom) ++ td (adj.s ! AAdj Superl Def Fem Pl Nom)) ++
           tr (th "Voc" ++ td (adj.s ! AAdj Superl Def Masc Sg Voc) ++ td (adj.s ! AAdj Superl Def Masc Pl Voc) ++ td (adj.s ! AAdj Superl Def Fem Sg Nom) ++ td (adj.s ! AAdj Superl Def Fem Pl Nom))) ++
           
           heading1 "Adverb" ++
           frameTable (
             tr (th "Positive" ++ td (adj.s ! AAdv Posit)) ++
             tr (th "Comparative" ++ td (adj.s ! AAdv Compar)) ++
             tr (th "Superlative" ++ td (adj.s ! AAdv Superl))) ;
      s3=[]
    } ;

lin InflectionAdv, InflectionAdV, InflectionAdA, InflectionAdN = \adv -> {
      t = "adv" ;
      s1= heading1 "Adverb" ;
      s2= paragraph (adv.s) ;
      s3= ""
    } ;

lin InflectionPrep = \prep -> {
      t = "prep" ;
      s1= heading1 "Preposition" ;
      s2= paragraph (prep.s) ;
      s3= ""
    } ;

lin
  NoDefinition   t     = {s=t.s};
  MkDefinition   t d   = {s="<p><b>Definition:</b>"++t.s++d.s++"</p>"};
  MkDefinitionEx t d e = {s="<p><b>Definition:</b>"++t.s++d.s++"</p><p><b>Example:</b>"++e.s++"</p>"};

lin
  MkDocument d i e = {s = i.s1 ++ d.s ++ i.s2 ++ paragraph e.s} ;
  MkTag i = {s = i.t} ;

}
