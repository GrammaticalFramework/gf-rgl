concrete DocumentationHye of Documentation = CatHye ** open
  ResHye, Prelude, HTML in {

lincat
  Inflection = {t : Str; s1,s2,s3 : Str} ;
  Definition = {s : Str} ;
  Document   = {s : Str} ;
  Tag        = {s : Str} ;

lin
  InflectionV,InflectionV2,InflectionV2A,InflectionV2Q,InflectionV2S,InflectionV2V,InflectionV3,InflectionVA,InflectionVQ,InflectionVS,InflectionVV = \x -> {
      t="v" ;
      s1="" ;
      s2=heading1 "Infinitive" ++
         paragraph (x.s) ++
         heading1 "Converb" ++
         frameTable (
           tr (th "Imperfective" ++ td (x.converb.imperfective)) ++
           tr (th "Simultaneous" ++ td (x.converb.simultaneous)) ++
           tr (th "Perfective" ++ td (x.converb.perfective)) ++
           tr (th "Futute I" ++ td (x.converb.futCon1)) ++
           tr (th "Futute II" ++ td (x.converb.futCon2)) ++
           tr (th "Connegative" ++ td (x.converb.negative))) ++
        heading1 "Passive" ++
        paragraph x.passive ++
        heading1 "Participle" ++
        frameTable (
           tr (th "Resultative" ++ td (x.participle ! Resultative)) ++
           tr (th "Subject" ++ td (x.participle ! Subject))) ++
         heading1 "Past" ++
         frameTable (
           tr (th "" ++ th "Sg" ++ th "Pl") ++
           tr (th "P1" ++ td (x.past ! P1 ! Sg) ++ td (x.past ! P1 ! Pl)) ++
           tr (th "P2" ++ td (x.past ! P2 ! Sg) ++ td (x.past ! P2 ! Pl)) ++
           tr (th "P3" ++ td (x.past ! P3 ! Sg) ++ td (x.past ! P3 ! Pl))) ++
        heading1 "Subjunctive" ++
        frameTable (
           tr (intagAttr "th" "colspan=\"2\"" "" ++ th "Sg" ++ th "Pl") ++ 
           tr (intagAttr "th" "rowspan=\"3\"" "Perfect" ++
               th "P1" ++ td (x.subjunctive ! Perfect ! P1 ! Sg) ++ td (x.subjunctive ! Perfect ! P1 ! Pl)) ++
           tr (th "P2" ++ td (x.subjunctive ! Perfect ! P2 ! Sg) ++ td (x.subjunctive ! Perfect ! P2 ! Pl)) ++
           tr (th "P3" ++ td (x.subjunctive ! Perfect ! P3 ! Sg) ++ td (x.subjunctive ! Perfect ! P3 ! Pl)) ++
           tr (intagAttr "th" "rowspan=\"3\"" "Non_Past" ++
               th "P1" ++ td (x.subjunctive ! Non_Past ! P1 ! Sg) ++ td (x.subjunctive ! Non_Past ! P1 ! Pl)) ++
           tr (th "P2" ++ td (x.subjunctive ! Non_Past ! P2 ! Sg) ++ td (x.subjunctive ! Non_Past ! P2 ! Pl)) ++
           tr (th "P3" ++ td (x.subjunctive ! Non_Past ! P3 ! Sg) ++ td (x.subjunctive ! Non_Past ! P3 ! Pl))) ++
         heading1 "Conditional" ++
         frameTable (           
           tr (intagAttr "th" "colspan=\"2\"" "" ++ th "Sg" ++ th "Pl") ++
           tr (intagAttr "th" "rowspan=\"3\"" "Perfect" ++
               th "P1" ++ td (x.conditional ! Perfect ! P1 ! Sg) ++ td (x.conditional ! Perfect ! P1 ! Pl)) ++
           tr (th "P2" ++ td (x.conditional ! Perfect ! P2 ! Sg) ++ td (x.conditional ! Perfect ! P2 ! Pl)) ++
           tr (th "P3" ++ td (x.conditional ! Perfect ! P3 ! Sg) ++ td (x.conditional ! Perfect ! P3 ! Pl)) ++
           tr (intagAttr "th" "rowspan=\"4\"" "Non_Past" ++ 
               th "P1" ++ td (x.conditional ! Non_Past ! P1 ! Sg) ++ td (x.conditional ! Non_Past ! P1 ! Pl)) ++
           tr (th "P2" ++ td (x.conditional ! Non_Past ! P2 ! Sg) ++ td (x.conditional ! Non_Past ! P2 ! Pl)) ++
           tr (th "P3" ++ td (x.conditional ! Non_Past ! P3 ! Sg) ++ td (x.conditional ! Non_Past ! P3 ! Pl))) ++
         heading1 "Imperative" ++
         frameTable (
           tr (th "Sg" ++ th "Pl") ++
           tr (td (x.imperative ! Sg) ++ td (x.imperative ! Pl))) ;
      s3=[]
    } ;
lin
  InflectionN,InflectionN2,InflectionN3 = \x -> {
      t="n" ;
      s1="" ;
      s2=frameTable (
           tr (th "" ++ th "Sg" ++ th "Pl") ++
           tr (th "Nom" ++ td (x.s ! Nom ! Sg) ++ td (x.s ! Nom ! Pl)) ++
           tr (th "Dat" ++ td (x.s ! Dat ! Sg) ++ td (x.s ! Dat ! Pl)) ++
           tr (th "Ablat" ++ td (x.s ! Ablat ! Sg) ++ td (x.s ! Ablat ! Pl)) ++
           tr (th "Instr" ++ td (x.s ! Instr ! Sg) ++ td (x.s ! Instr ! Pl)) ++
           tr (th "Loc"   ++ td (x.s ! Loc ! Sg) ++ td (x.s ! Loc ! Pl)) ++
           tr (intagAttr "th" "colspan=\"3\"" "definite forms") ++
           tr (th "Nom" ++ td (x.def_nom ! Sg) ++ td (x.def_nom ! Pl)) ++
           tr (th "Dat" ++ td (x.def_dat ! Sg) ++ td (x.def_dat ! Pl)) ++
           tr (intagAttr "th" "colspan=\"3\"" "1st person possessive forms") ++
           tr (th "Nom" ++ td (x.poss1 ! Nom ! Sg) ++ td (x.poss1 ! Nom ! Pl)) ++
           tr (th "Dat" ++ td (x.poss1 ! Dat ! Sg) ++ td (x.poss1 ! Dat ! Pl)) ++
           tr (th "Ablat" ++ td (x.poss1 ! Ablat ! Sg) ++ td (x.poss1 ! Ablat ! Pl)) ++
           tr (th "Instr" ++ td (x.poss1 ! Instr ! Sg) ++ td (x.poss1 ! Instr ! Pl)) ++
           tr (th "Loc" ++ td (x.poss1 ! Loc ! Sg) ++ td (x.poss1 ! Loc ! Pl)) ++
           tr (intagAttr "th" "colspan=\"3\"" "2nd person possessive forms") ++
           tr (th "Nom" ++ td (x.poss2 ! Nom ! Sg) ++ td (x.poss2 ! Nom ! Pl)) ++
           tr (th "Dat" ++ td (x.poss2 ! Dat ! Sg) ++ td (x.poss2 ! Dat ! Pl)) ++
           tr (th "Ablat" ++ td (x.poss2 ! Ablat ! Sg) ++ td (x.poss2 ! Ablat ! Pl)) ++
           tr (th "Instr" ++ td (x.poss2 ! Instr ! Sg) ++ td (x.poss2 ! Instr ! Pl)) ++
           tr (th "Loc" ++ td (x.poss2 ! Loc ! Sg) ++ td (x.poss2 ! Loc ! Pl))) ;
      s3=[]
    } ;
lin
  InflectionA,InflectionA2 = \x -> {
      t="a" ;
      s1="" ;
      s2=frameTable (
           tr (th "" ++ th "Sg" ++ th "Pl") ++
           tr (th "Nom" ++ td (x.s ! Nom ! Sg) ++ td (x.s ! Nom ! Pl)) ++
           tr (th "Dat" ++ td (x.s ! Dat ! Sg) ++ td (x.s ! Dat ! Pl)) ++
           tr (th "Ablat" ++ td (x.s ! Ablat ! Sg) ++ td (x.s ! Ablat ! Pl)) ++
           tr (th "Instr" ++ td (x.s ! Instr ! Sg) ++ td (x.s ! Instr ! Pl)) ++
           tr (th "Loc"   ++ td (x.s ! Loc ! Sg) ++ td (x.s ! Loc ! Pl)) ++
           tr (intagAttr "th" "colspan=\"3\"" "definite forms") ++
           tr (th "Nom" ++ td (x.def_nom ! Sg) ++ td (x.def_nom ! Pl)) ++
           tr (th "Dat" ++ td (x.def_dat ! Sg) ++ td (x.def_dat ! Pl)) ++
           tr (intagAttr "th" "colspan=\"3\"" "1st person possessive forms") ++
           tr (th "Nom" ++ td (x.poss1 ! Nom ! Sg) ++ td (x.poss1 ! Nom ! Pl)) ++
           tr (th "Dat" ++ td (x.poss1 ! Dat ! Sg) ++ td (x.poss1 ! Dat ! Pl)) ++
           tr (th "Ablat" ++ td (x.poss1 ! Ablat ! Sg) ++ td (x.poss1 ! Ablat ! Pl)) ++
           tr (th "Instr" ++ td (x.poss1 ! Instr ! Sg) ++ td (x.poss1 ! Instr ! Pl)) ++
           tr (th "Loc" ++ td (x.poss1 ! Loc ! Sg) ++ td (x.poss1 ! Loc ! Pl)) ++
           tr (intagAttr "th" "colspan=\"3\"" "2nd person possessive forms") ++
           tr (th "Nom" ++ td (x.poss2 ! Nom ! Sg) ++ td (x.poss2 ! Nom ! Pl)) ++
           tr (th "Dat" ++ td (x.poss2 ! Dat ! Sg) ++ td (x.poss2 ! Dat ! Pl)) ++
           tr (th "Ablat" ++ td (x.poss2 ! Ablat ! Sg) ++ td (x.poss2 ! Ablat ! Pl)) ++
           tr (th "Instr" ++ td (x.poss2 ! Instr ! Sg) ++ td (x.poss2 ! Instr ! Pl)) ++
           tr (th "Loc" ++ td (x.poss2 ! Loc ! Sg) ++ td (x.poss2 ! Loc ! Pl))) ;
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
