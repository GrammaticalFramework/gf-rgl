--# -path=.:../abstract:../common
concrete DocumentationPol of Documentation = CatPol ** open
  ResPol,
  HTML in {

lincat
  Inflection = {t : Str; s1,s2 : Str} ;
  Definition = {s : Str} ;
  Document = {s : Str} ;
  Tag      = {s : Str} ;

lin
  InflectionN, InflectionN2, InflectionN3 = \noun -> {
    t  = "rz" ;
    s1 = heading1 ("rzeczownik" ++ case noun.g of {
                               Masc Animate   => "(męskozwierzęcy)";
                               Masc Inanimate => "(męskorzeczowy)";
                               Masc Personal  => "(męskoosobowy)";
                               Fem            => "(żeński)";
                               NeutGr         => "(nijaki)";
                               Neut           => "(nijaki)";
                               Plur           => ""
                             }) ;
    s2 = frameTable (
           tr (th ""   ++ th "liczba pojedyncza" ++ th "liczba mnoga") ++
           tr (th "mianownik" ++ td (noun.s ! SF Sg Nom)   ++ td (noun.s ! SF Pl Nom)) ++
           tr (th "dopełniacz" ++ td (noun.s ! SF Sg Gen)   ++ td (noun.s ! SF Pl Gen)) ++
           tr (th "celownik" ++ td (noun.s ! SF Sg Dat)   ++ td (noun.s ! SF Pl Dat)) ++
           tr (th "biernik" ++ td (noun.s ! SF Sg Acc)   ++ td (noun.s ! SF Pl Acc)) ++
           tr (th "narzędnik" ++ td (noun.s ! SF Sg Instr) ++ td (noun.s ! SF Pl Instr)) ++
           tr (th "miejscownik" ++ td (noun.s ! SF Sg Loc)   ++ td (noun.s ! SF Pl Loc)) ++
           tr (th "wołacz" ++ td (noun.s ! SF Sg VocP)  ++ td (noun.s ! SF Pl VocP))
         )
    } ;

  InflectionPN = \pn -> {
    t  = "im" ;
    s1 = heading1 ("Imię" ++ case pn.gn of {
                               MascPersSg|MascAniSg|MascInaniSg|MascPersPl => "(męskorzeczowy)";
                               FemSg  => "(żeński)";
                               NeutSg => "(nijaki)";
                               _      => ""
                             }) ;
    s2 = frameTable (
           tr (th "mianownik" ++ td (pn.nom)) ++
           tr (th "dopełniacz" ++ td (pn.dep ! GenPrep)) ++
           tr (th "celownik" ++ td (pn.dep ! DatPrep)) ++
           tr (th "biernik" ++ td (pn.dep ! AccPrep)) ++
           tr (th "narzędnik" ++ td (pn.dep ! InstrC)) ++
           tr (th "miejscownik" ++ td (pn.dep ! LocPrep)) ++
           tr (th "wołacz" ++ td (pn.voc))
         )
    } ;

  InflectionLN = \pn -> {
    t  = "im" ;
    s1 = heading1 ("Imię" ++ case pn.gn of {
                               MascPersSg|MascAniSg|MascInaniSg|MascPersPl => "(męskorzeczowy)";
                               FemSg  => "(żeński)";
                               NeutSg => "(nijaki)";
                               _      => ""
                             }) ;
    s2 = frameTable (
           tr (th "mianownik" ++ td (pn.nom)) ++
           tr (th "dopełniacz" ++ td (pn.dep ! GenPrep)) ++
           tr (th "celownik" ++ td (pn.dep ! DatPrep)) ++
           tr (th "biernik" ++ td (pn.dep ! AccPrep)) ++
           tr (th "narzędnik" ++ td (pn.dep ! InstrC)) ++
           tr (th "miejscownik" ++ td (pn.dep ! LocPrep)) ++
           tr (th "wołacz" ++ td (pn.voc))
         )
    } ;

  InflectionGN = \pn -> {
    t  = "im" ;
    s1 = heading1 ("Imię" ++ case pn.gn of {
                               MascPersSg|MascAniSg|MascInaniSg|MascPersPl => "(męskorzeczowy)";
                               FemSg  => "(żeński)";
                               NeutSg => "(nijaki)";
                               _      => ""
                             }) ;
    s2 = frameTable (
           tr (th "mianownik" ++ td (pn.nom)) ++
           tr (th "dopełniacz" ++ td (pn.dep ! GenPrep)) ++
           tr (th "celownik" ++ td (pn.dep ! DatPrep)) ++
           tr (th "biernik" ++ td (pn.dep ! AccPrep)) ++
           tr (th "narzędnik" ++ td (pn.dep ! InstrC)) ++
           tr (th "miejscownik" ++ td (pn.dep ! LocPrep)) ++
           tr (th "wołacz" ++ td (pn.voc))
         )
    } ;

  InflectionSN = \pn -> {
    t  = "im" ;
    s1 = heading1 ("Nazwisko" ++ case pn.gn of {
                                   MascPersSg|MascAniSg|MascInaniSg|MascPersPl => "(męskorzeczowy)";
                                   FemSg  => "(żeński)";
                                   NeutSg => "(nijaki)";
                                   _      => ""
                                 }) ;
    s2 = frameTable (
           tr (th "mianownik" ++ td (pn.nom)) ++
           tr (th "dopełniacz" ++ td (pn.dep ! GenPrep)) ++
           tr (th "celownik" ++ td (pn.dep ! DatPrep)) ++
           tr (th "biernik" ++ td (pn.dep ! AccPrep)) ++
           tr (th "narzędnik" ++ td (pn.dep ! InstrC)) ++
           tr (th "miejscownik" ++ td (pn.dep ! LocPrep)) ++
           tr (th "wołacz" ++ td (pn.voc))
         )
    } ;

  InflectionA, InflectionA2 = \adj -> {
    t  = "a" ;
    s1 = heading1 "Przymiotnik" ;
    s2 = inflAdj (mkAtable adj.pos) ++
         heading1 "Stopień wyższy" ++
         inflAdj (mkAtable adj.comp) ++
         heading1 "Stopień najwyższy" ++
         inflAdj (mkAtable adj.super)
    } ;

  InflectionAdv, InflectionAdV, InflectionAdA, InflectionAdN = \adv -> {
    t = "prs" ;
    s1= heading1 "Przysłówek" ;
    s2= paragraph (adv.s) ;
    s3= ""
    } ;

  InflectionPrep = \prep -> {
    t = "pri" ;
    s1= heading1 "Przyimek" ;
    s2= paragraph (prep.s) ;
    s3= ""
    } ;

  InflectionV, InflectionV2, InflectionV3, InflectionV2V,
  InflectionV2S, InflectionV2Q, InflectionV2A, InflectionVV,
  InflectionVS, InflectionVQ, InflectionVA = \v -> {
    t = "cz" ;
    s1= heading1 "Czasownik" ;
    s2= heading2 "Niedokonany"++
        inflVerb v.si (mkAtable (table2record v.pparti))++
        heading2 "Dokonany"++
        inflVerb v.sp (mkAtable (table2record v.ppartp))
    } ;

lin
  NoDefinition   t     = {s=t.s};
  MkDefinition   t d   = {s="<p><b>Definicja:</b>"++t.s++d.s++"</p>"};
  MkDefinitionEx t d e = {s="<p><b>Definicja:</b>"++t.s++d.s++"</p><p><b>Przykład:</b>"++e.s++"</p>"};

lin
  MkDocument d i e = {s = i.s1 ++ d.s ++ i.s2 ++ paragraph e.s} ;
  MkTag i = {s = i.t} ;

oper
  inflAdj : (AForm => Str) -> Str = \forms ->
    frameTable (
       tr (th ""            ++ th "męskorzeczowy"                ++ th "żeński"                 ++ th "nijaki") ++
       tr (intagAttr "th" "colspan=\"4\"" "liczba pojedyncza")   ++
       tr (th "mianownik"   ++ td (forms ! AF MascInaniSg Nom)   ++ td (forms ! AF FemSg Nom)   ++ td (forms ! AF NeutSg Nom)) ++
       tr (th "dopełniacz"  ++ td (forms ! AF MascInaniSg Gen)   ++ td (forms ! AF FemSg Gen)   ++ td (forms ! AF NeutSg Gen)) ++
       tr (th "celownik"    ++ td (forms ! AF MascInaniSg Dat)   ++ td (forms ! AF FemSg Dat)   ++ td (forms ! AF NeutSg Dat)) ++
       tr (th "biernik"     ++ td (forms ! AF MascInaniSg Acc    ++ "/" ++ 
                                   forms ! AF MascAniSg   Acc)   ++ td (forms ! AF FemSg Acc)   ++ td (forms ! AF NeutSg Acc)) ++
       tr (th "narzędnik"   ++ td (forms ! AF MascInaniSg Instr) ++ td (forms ! AF FemSg Instr) ++ td (forms ! AF NeutSg Instr)) ++
       tr (th "miejscownik" ++ td (forms ! AF MascInaniSg Loc)   ++ td (forms ! AF FemSg Loc)   ++ td (forms ! AF NeutSg Loc)) ++
       tr (th "wołacz"      ++ td (forms ! AF MascInaniSg VocP)  ++ td (forms ! AF FemSg VocP)  ++ td (forms ! AF NeutSg VocP)) ++
       tr (intagAttr "th" "colspan=\"4\"" "liczba mnoga") ++
       tr (th "mianownik"   ++ td (forms ! AF MascPersPl Nom)  ++ intagAttr "td" "colspan=\"2\"" (forms ! AF OthersPl Nom)) ++
       tr (th "dopełniacz"  ++ intagAttr "td" "colspan=\"3\"" (forms ! AF OthersPl Gen)) ++
       tr (th "celownik"    ++ intagAttr "td" "colspan=\"3\"" (forms ! AF OthersPl Dat)) ++
       tr (th "biernik"     ++ td (forms ! AF MascPersPl Acc)  ++ intagAttr "td" "colspan=\"2\"" (forms ! AF OthersPl Acc)) ++
       tr (th "narzędnik"   ++ intagAttr "td" "colspan=\"3\"" (forms ! AF OthersPl Instr)) ++
       tr (th "miejscownik" ++ intagAttr "td" "colspan=\"3\"" (forms ! AF OthersPl Loc)) ++
       tr (th "wołacz"      ++ td (forms ! AF MascPersPl VocP) ++ intagAttr "td" "colspan=\"2\"" (forms ! AF OthersPl VocP))
    ) ;

  inflVerb : (VFormM => Str) -> (AForm => Str) -> Str = \forms,part ->
    heading3 "Bezokolicznik"++
    paragraph(forms ! VInfM)++
    heading3 "Czas Teraźniejszy"++
    frameTable (
       tr (th ""     ++ th "liczba pojedyncza"   ++ th "liczba mnoga") ++
       tr (th "1 o." ++ td (forms ! VFinM Sg P1) ++ td (forms ! VFinM Pl P1)) ++
       tr (th "2 o." ++ td (forms ! VFinM Sg P2) ++ td (forms ! VFinM Pl P2)) ++
       tr (th "3 o." ++ td (forms ! VFinM Sg P3) ++ td (forms ! VFinM Pl P3))
     ) ++
     heading3 "Czas Przeszły"++
    frameTable (
       tr (intagAttr "th" "colspan=\"2\"" ""    ++th "liczba pojedyncza"++ th "liczba mnoga") ++
       tr (intagAttr "th" "rowspan=\"5\"" "1 o."++th "męskoosobowy"     ++ td (forms ! VPraetM MascPersSg  P1)  ++ td (forms ! VPraetM MascPersPl P1)) ++
       tr (                                       th "męskozwierzęcy"   ++ td (forms ! VPraetM MascAniSg   P1)  ++ intagAttr "td" "rowspan=\"4\"" (forms ! VPraetM OthersPl P1)) ++
       tr (                                       th "męskorzeczowy"    ++ td (forms ! VPraetM MascInaniSg P1)) ++
       tr (                                       th "żeński"           ++ td (forms ! VPraetM FemSg       P1)) ++
       tr (                                       th "nijaki"           ++ td (forms ! VPraetM NeutSg      P1)) ++
       tr (intagAttr "th" "rowspan=\"5\"" "2 o."++th "męskoosobowy"     ++ td (forms ! VPraetM MascPersSg  P2)  ++ td (forms ! VPraetM MascPersPl P2)) ++
       tr (                                       th "męskozwierzęcy"   ++ td (forms ! VPraetM MascAniSg   P2)  ++ intagAttr "td" "rowspan=\"4\"" (forms ! VPraetM OthersPl P2)) ++
       tr (                                       th "męskorzeczowy"    ++ td (forms ! VPraetM MascInaniSg P2)) ++
       tr (                                       th "żeński"           ++ td (forms ! VPraetM FemSg       P2)) ++
       tr (                                       th "nijaki"           ++ td (forms ! VPraetM NeutSg      P2)) ++
       tr (intagAttr "th" "rowspan=\"5\"" "3 o."++th "męskoosobowy"     ++ td (forms ! VPraetM MascPersSg  P3)  ++ td (forms ! VPraetM MascPersPl P3)) ++
       tr (                                       th "męskozwierzęcy"   ++ td (forms ! VPraetM MascAniSg   P3)  ++ intagAttr "td" "rowspan=\"4\"" (forms ! VPraetM OthersPl P3)) ++
       tr (                                       th "męskorzeczowy"    ++ td (forms ! VPraetM MascInaniSg P3)) ++
       tr (                                       th "żeński"           ++ td (forms ! VPraetM FemSg       P3)) ++
       tr (                                       th "nijaki"           ++ td (forms ! VPraetM NeutSg      P3))
     ) ++
    heading3 "Tryb Przypuszczający"++
    frameTable (
       tr (intagAttr "th" "colspan=\"2\"" ""    ++th "liczba pojedyncza"++ th "liczba mnoga") ++
       tr (intagAttr "th" "rowspan=\"5\"" "1 o."++th "męskoosobowy"     ++ td (forms ! VCondM MascPersSg  P1)  ++ td (forms ! VCondM MascPersPl P1)) ++
       tr (                                       th "męskozwierzęcy"   ++ td (forms ! VCondM MascAniSg   P1)  ++ intagAttr "td" "rowspan=\"4\"" (forms ! VCondM OthersPl P1)) ++
       tr (                                       th "męskorzeczowy"    ++ td (forms ! VCondM MascInaniSg P1)) ++
       tr (                                       th "żeński"           ++ td (forms ! VCondM FemSg       P1)) ++
       tr (                                       th "nijaki"           ++ td (forms ! VCondM NeutSg      P1)) ++
       tr (intagAttr "th" "rowspan=\"5\"" "2 o."++th "męskoosobowy"     ++ td (forms ! VCondM MascPersSg  P2)  ++ td (forms ! VPraetM MascPersPl P2)) ++
       tr (                                       th "męskozwierzęcy"   ++ td (forms ! VCondM MascAniSg   P2)  ++ intagAttr "td" "rowspan=\"4\"" (forms ! VCondM OthersPl P2)) ++
       tr (                                       th "męskorzeczowy"    ++ td (forms ! VCondM MascInaniSg P2)) ++
       tr (                                       th "żeński"           ++ td (forms ! VCondM FemSg       P2)) ++
       tr (                                       th "nijaki"           ++ td (forms ! VCondM NeutSg      P2)) ++
       tr (intagAttr "th" "rowspan=\"5\"" "3 o."++th "męskoosobowy"     ++ td (forms ! VCondM MascPersSg  P3)  ++ td (forms ! VPraetM MascPersPl P3)) ++
       tr (                                       th "męskozwierzęcy"   ++ td (forms ! VCondM MascAniSg   P3)  ++ intagAttr "td" "rowspan=\"4\"" (forms ! VCondM OthersPl P3)) ++
       tr (                                       th "męskorzeczowy"    ++ td (forms ! VCondM MascInaniSg P3)) ++
       tr (                                       th "żeński"           ++ td (forms ! VCondM FemSg       P3)) ++
       tr (                                       th "nijaki"           ++ td (forms ! VCondM NeutSg      P3))
     ) ++
    heading3 "Bezokolicznik"++
    frameTable (
       tr (th ""     ++ th "liczba pojedyncza"  ++ th "liczba mnoga") ++
       tr (th "1 o." ++ td ("")                 ++ td (forms ! VImperPl1M)) ++
       tr (th "2 o." ++ td (forms ! VImperSg2M) ++ td (forms ! VImperPl2M))
     )++
    heading3 "Imiesłów"++
    inflAdj part ;

}
