concrete DocumentationSom of Documentation = CatSom ** open ResSom,Prelude,HTML in {
  lincat Definition = {s : Str} ;
  lincat Document = {s : Str} ;
  lincat Inflection = {t : Str; s1 : Str; s2 : Str} ;
  lincat Tag = {s : Str} ;
  lin InflectionA, InflectionA2 = \adj -> {
        t = "a";
        s1 = heading1 "Adjective";
        s2 = frameTable (tr (td "" ++ th "Sg" ++ th "Pl") ++
                         tr (th "Nom" ++ td (adj.s ! AF Sg Nom) ++ td (adj.s ! AF Pl Nom)) ++
                         tr (th "Abs" ++ td (adj.s ! AF Sg Abs) ++ td (adj.s ! AF Pl Abs)))
      } ;
  lin InflectionAdA, InflectionAdN, InflectionAdV = \adv -> {
        t = "adv";
        s1 = heading1 "Adverb";
        s2 = paragraph adv.s
      } ;
  lin InflectionAdv adv = {t = "adv"; s1 = heading1 "Adverb";
                           s2 = frameTable (tr (th "sii" ++ td adv.sii)
                                              ++ tr (th "dhex" ++ td adv.dhex)
                                                   ++ tr (th "berri" ++ td adv.berri)
                                                        ++ tr (th "miscAdv" ++ td adv.miscAdv)
                                                             ++ tr (th "np" ++ td adv.np.s))} ;
  lin InflectionGN pn = {t = "pn"; s1 = heading1 "Name";
                         s2 = frameTable (tr (th "" ++ td pn.s))} ;
  lin InflectionLN pn = {t = "pn"; s1 = heading1 "Name";
                         s2 = frameTable (tr (th "" ++ td pn.s))} ;
  lin InflectionN,InflectionN2,InflectionN3 = \noun -> {
        t = "n";
        s1 = heading1 "Noun";
        s2 = frameTable (tr (td "" ++ th "Sg" ++ th "Pl") ++
                         tr (th "Indef" ++ td (noun.s ! Indef Sg) ++ td (noun.s ! Indef Pl)) ++
                         tr (th "Def"   ++ td (noun.s ! Def Sg)   ++ td (noun.s ! Def Pl)) ++
                         tr (th "Nom"   ++ td (noun.s ! NomSg)    ++ td []) ++
                         tr (th "Numerative" ++ intagAttr "td" "colspan=2" (noun.s ! Numerative)))
       } ;
  lin InflectionPN pn = {t = "pn"; s1 = heading1 "Name";
                         s2 = frameTable (tr (th "" ++ td pn.s))} ;
  lin InflectionPrep prep = {t = "prep"; s1 = heading1 "Preposition";
                             s2 = frameTable (tr (th "Sg1Obj" ++ td (prep.s ! Sg1Obj))
                                                ++ tr (th "Sg2Obj" ++ td (prep.s ! Sg2Obj))
                                                     ++ tr (th "(Pl1Obj Excl)"
                                                              ++ td (prep.s ! Pl1Obj Excl))
                                                          ++ tr (th "(Pl1Obj Incl)"
                                                                   ++ td (prep.s ! Pl1Obj Incl))
                                                               ++ tr (th "Pl2Obj"
                                                                        ++ td (prep.s ! Pl2Obj))
                                                                    ++ tr (th "ReflexiveObj"
                                                                             ++ td (prep.s
                                                                                      ! ReflexiveObj))
                                                                         ++ tr (th "ZeroObj"
                                                                                  ++ td (prep.s
                                                                                           ! ZeroObj))
                                                                              ++ tr (th "berri"
                                                                                       ++ td prep.berri)
                                                                                   ++ tr (th "sii"
                                                                                            ++ td prep.sii)
                                                                                        ++ tr (th "dhex"
                                                                                                 ++ td prep.dhex)
                                                                                             ++ tr (th "hoostiisa Sg1"
                                                                                                      ++ td (prep.hoostiisa
                                                                                                               ! Sg1))
                                                                                                  ++ tr (th "hoostiisa Sg2"
                                                                                                           ++ td (prep.hoostiisa
                                                                                                                    ! Sg2))
                                                                                                       ++ tr (th "hoostiisa (Sg3 Masc)"
                                                                                                                ++ td (prep.hoostiisa
                                                                                                                         ! Sg3 Masc))
                                                                                                            ++ tr (th "hoostiisa (Sg3 Fem)"
                                                                                                                     ++ td (prep.hoostiisa
                                                                                                                              ! Sg3 Fem))
                                                                                                                 ++ tr (th "hoostiisa (Pl1 Excl)"
                                                                                                                          ++ td (prep.hoostiisa
                                                                                                                                   ! Pl1 Excl))
                                                                                                                      ++ tr (th "hoostiisa (Pl1 Incl)"
                                                                                                                               ++ td (prep.hoostiisa
                                                                                                                                        ! Pl1 Incl))
                                                                                                                           ++ tr (th "hoostiisa Pl2"
                                                                                                                                    ++ td (prep.hoostiisa
                                                                                                                                             ! Pl2))
                                                                                                                                ++ tr (th "hoostiisa Pl3"
                                                                                                                                         ++ td (prep.hoostiisa
                                                                                                                                                  ! Pl3))
                                                                                                                                     ++ tr (th "hoostiisa Impers"
                                                                                                                                              ++ td (prep.hoostiisa
                                                                                                                                                       ! Impers)))} ;
  lin InflectionSN pn = {t = "pn"; s1 = heading1 "Name";
                         s2 = frameTable (tr (th "" ++ td pn.s))} ;
  lin InflectionV v = {t = "v"; s1 = heading1 "Verb"; s2 = inflectV v} ;
  lin InflectionV2 v = {t = "v"; s1 = heading1 "Verb"; s2 = inflectV v} ;
  lin InflectionV2A v = {t = "v"; s1 = heading1 "Verb"; s2 = inflectV v} ;
  lin InflectionV2Q v = {t = "v"; s1 = heading1 "Verb"; s2 = inflectV v} ;
  lin InflectionV2S v = {t = "v"; s1 = heading1 "Verb"; s2 = inflectV v} ;
  lin InflectionV2V v = {t = "v"; s1 = heading1 "Verb"; s2 = inflectV v} ;
  lin InflectionV3 v = {t = "v"; s1 = heading1 "Verb"; s2 = inflectV v} ;
  lin InflectionVA v = {t = "v"; s1 = heading1 "Verb"; s2 = inflectV v} ;
  lin InflectionVQ v = {t = "v"; s1 = heading1 "Verb"; s2 = inflectV v} ;
  lin InflectionVS v = {t = "v"; s1 = heading1 "Verb"; s2 = inflectV v} ;
  lin InflectionVV v = {t = "v"; s1 = heading1 "Verb"; s2 = inflectV v} ;
  lin MkDefinition t d = {s = "<p><b>Definition:</b>"
                                ++ t.s ++ d.s ++ "</p>"} ;
  lin MkDefinitionEx t d e = {s = "<p><b>Definition:</b>"
                                    ++ t.s ++ d.s ++ "<p><b>Example:</b>" ++ t.s ++ "</p>"} ;
  lin MkDocument d i e = {s = i.s1 ++ i.s2} ;
  lin MkTag i = {s = i.t} ;
  lin NoDefinition t = t ;

  oper inflectV : Verb -> Str = \v ->
         heading2 "Infinitive" ++
         paragraph (v.s ! VInf) ++
         heading2 "Present" ++
         heading3 "Simple" ++
         frameTable (tr (td ""            ++ th "Pos"                                ++ th "Neg") ++
                     tr (th "Sg1_Sg3Masc" ++ td (v.s ! VPres Simple Sg1_Sg3Masc Pos) ++ td (v.s ! VPres Simple Sg1_Sg3Masc Neg)) ++
                     tr (th "Sg2_Sg3Fem"  ++ td (v.s ! VPres Simple Sg2_Sg3Fem  Pos) ++ td (v.s ! VPres Simple Sg2_Sg3Fem  Neg))  ++
                     tr (th "Pl1"         ++ td (v.s ! VPres Simple Pl1_ Pos)        ++ td (v.s ! VPres Simple Pl1_ Neg)) ++
                     tr (th "Pl2"         ++ td (v.s ! VPres Simple Pl2_ Pos)        ++ td (v.s ! VPres Simple Pl2_ Neg)) ++
                     tr (th "Pl3"         ++ td (v.s ! VPres Simple Pl3_ Pos)        ++ td (v.s ! VPres Simple Pl3_ Neg))) ++
         heading3 "Progressive" ++
         frameTable (tr (td ""            ++ th "Pos"                                ++ th "Neg") ++
                     tr (th "Sg1_Sg3Masc" ++ td (v.s ! VPres Progressive Sg1_Sg3Masc Pos) ++ td (v.s ! VPres Progressive Sg1_Sg3Masc Neg)) ++
                     tr (th "Sg2_Sg3Fem"  ++ td (v.s ! VPres Progressive Sg2_Sg3Fem  Pos) ++ td (v.s ! VPres Progressive Sg2_Sg3Fem  Neg))  ++
                     tr (th "Pl1"         ++ td (v.s ! VPres Progressive Pl1_ Pos)        ++ td (v.s ! VPres Progressive Pl1_ Neg)) ++
                     tr (th "Pl2"         ++ td (v.s ! VPres Progressive Pl2_ Pos)        ++ td (v.s ! VPres Progressive Pl2_ Neg)) ++
                     tr (th "Pl3"         ++ td (v.s ! VPres Progressive Pl3_ Pos)        ++ td (v.s ! VPres Progressive Pl3_ Neg))) ++
         heading2 "Past" ++
         heading3 "Simple" ++
         frameTable (tr (td ""            ++ th "Pos"                            ++ th "Neg") ++
                     tr (th "Sg1_Sg3Masc" ++ td (v.s ! VPast Simple Sg1_Sg3Masc) ++ intagAttr "td" "rowspan=5" (v.s ! VNegPast Simple)) ++
                     tr (th "Sg2_Sg3Fem"  ++ td (v.s ! VPast Simple Sg2_Sg3Fem)) ++
                     tr (th "Pl1"         ++ td (v.s ! VPast Simple Pl1_)) ++
                     tr (th "Pl2"         ++ td (v.s ! VPast Simple Pl2_)) ++
                     tr (th "Pl3"         ++ td (v.s ! VPast Simple Pl3_))) ++
         heading3 "Progressive" ++
         frameTable (tr (td ""            ++ th "Pos"                                 ++ th "Neg") ++
                     tr (th "Sg1_Sg3Masc" ++ td (v.s ! VPast Progressive Sg1_Sg3Masc) ++ intagAttr "td" "rowspan=5" (v.s ! VNegPast Progressive)) ++
                     tr (th "Sg2_Sg3Fem"  ++ td (v.s ! VPast Progressive Sg2_Sg3Fem))  ++
                     tr (th "Pl1"         ++ td (v.s ! VPast Progressive Pl1_)) ++
                     tr (th "Pl2"         ++ td (v.s ! VPast Progressive Pl2_)) ++
                     tr (th "Pl3"         ++ td (v.s ! VPast Progressive Pl3_))) ++
         heading2 "Imperative" ++
         frameTable (tr (td ""   ++ th "Pos"               ++ th "Neg") ++
                     tr (th "Sg" ++ td (v.s ! VImp Sg Pos) ++ td (v.s ! VImp Sg Neg)) ++
                     tr (th "Pl" ++ td (v.s ! VImp Pl Pos) ++ td (v.s ! VImp Pl Neg))) ++
         heading2 "Relative" ++
         frameTable (tr (td ""       ++ th "Pos"                                 ++ th "Neg") ++
                     tr (th "SgMasc" ++ td (v.s ! VRel SgMasc) ++ intagAttr "td" "rowspan=3" (v.s ! VRelNeg)) ++
                     tr (th "SgFem"  ++ td (v.s ! VRel SgFem)) ++
                     tr (th "PlInv"  ++ td (v.s ! VRel PlInv))) ++
         heading2 "Negative Conditional" ++            
         frameTable (tr (th "SgMasc" ++ td (v.s ! VNegCond SgMasc)) ++
                     tr (th "SgFem"  ++ td (v.s ! VNegCond SgFem)) ++
                     tr (th "PlInv"  ++ td (v.s ! VNegCond PlInv))) ;
}
