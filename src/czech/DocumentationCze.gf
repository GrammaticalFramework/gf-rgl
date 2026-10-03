--# -path=.:../abstract:../common:../prelude

concrete DocumentationCze of Documentation = CatCze ** open
  ResCze, Prelude, HTML in {

flags coding=utf8 ;

lincat
  Inflection = {t : Str ; s1,s2 : Str} ;
  Definition = {s : Str} ;
  Document   = {s : Str} ;
  Tag        = {s : Str} ;

lin
  InflectionN, InflectionN2 = \n -> {
    t = "n" ;
    s1 = heading1 ("Podstatné jméno" ++ genderName n.g) ;
    s2 = nounTable n
    } ;

  -- N3 still has the default string representation in CatCze.
  InflectionN3 = \n -> {
    t = "n" ;
    s1 = heading1 "Podstatné jméno" ;
    s2 = paragraph n.s
    } ;

  InflectionPN = \n -> {
    t = "pn" ;
    s1 = heading1 ("Vlastní jméno" ++ genderName n.g) ;
    s2 = caseTable n.s
    } ;

  InflectionLN = \n -> {
    t = "ln" ;
    s1 = heading1 "Zeměpisné jméno" ;
    s2 = paragraph n.s
    } ;

  InflectionGN = \n -> {
    t = "gn" ;
    s1 = heading1 "Rodné jméno" ;
    s2 = paragraph n.s
    } ;

  InflectionSN = \n -> {
    t = "sn" ;
    s1 = heading1 "Příjmení" ;
    s2 = paragraph n.s
    } ;

  InflectionA, InflectionA2 = \a -> {
    t = "a" ;
    s1 = heading1 "Přídavné jméno" ;
    s2 = heading2 "Pozitiv" ++ adjectiveTable a ++
         heading2 "Komparativ" ++ adjectiveTable a.compar ++
         heading2 "Superlativ" ++ adjectiveTable a.superl ++
         heading2 "Příslovce" ++ paragraph a.adv
    } ;

  InflectionAdv, InflectionAdV, InflectionAdA, InflectionAdN = \adv -> {
    t = "adv" ;
    s1 = heading1 "Příslovce" ;
    s2 = paragraph adv.s
    } ;

  InflectionPrep = \prep -> {
    t = "prep" ;
    s1 = heading1 "Předložka" ;
    s2 = paragraph (prep.s ++ caseName prep.c)
    } ;

  InflectionV, InflectionV2, InflectionV3, InflectionV2V,
  InflectionV2S, InflectionV2Q, InflectionV2A, InflectionVV,
  InflectionVS, InflectionVQ, InflectionVA = \v -> {
    t = "v" ;
    s1 = heading1 "Sloveso" ;
    s2 = verbTable v
    } ;

  InflectionCl = \cl -> {
    t = "cl" ;
    s1 = heading1 "Věta" ;
    s2 = frameTable (
           tr (th "čas / způsob" ++ th "kladná" ++ th "záporná") ++
           clauseRow "přítomný čas" Pres cl ++
           clauseRow "minulý čas" Past cl ++
           clauseRow "budoucí čas" Fut cl ++
           clauseRow "podmiňovací způsob" Cond cl
         )
    } ;

  NoDefinition t = {s = t.s} ;
  MkDefinition t d = {
    s = "<p><b>Definice:</b>" ++ t.s ++ d.s ++ "</p>"
    } ;
  MkDefinitionEx t d e = {
    s = "<p><b>Definice:</b>" ++ t.s ++ d.s ++ "</p>" ++
        "<p><b>Příklad:</b>" ++ e.s ++ "</p>"
    } ;

  MkDocument d i e = {s = i.s1 ++ d.s ++ i.s2 ++ paragraph e.s} ;
  MkTag i = {s = i.t} ;

oper
  genderName : Gender -> Str = \g -> case g of {
    Masc Anim   => "(mužský životný)" ;
    Masc Inanim => "(mužský neživotný)" ;
    Fem         => "(ženský)" ;
    Neutr       => "(střední)"
    } ;

  caseName : ResCze.Case -> Str = \c -> case c of {
    Nom => "nominativ" ; Gen => "genitiv" ; Dat => "dativ" ;
    Acc => "akuzativ" ; ResCze.Voc => "vokativ" ; Loc => "lokál" ;
    Ins => "instrumentál"
    } ;

  caseTable : (ResCze.Case => Str) -> Str = \forms ->
    frameTable (
      tr (th "pád" ++ th "tvar") ++
      tr (th "nominativ" ++ td (forms ! Nom)) ++
      tr (th "genitiv" ++ td (forms ! Gen)) ++
      tr (th "dativ" ++ td (forms ! Dat)) ++
      tr (th "akuzativ" ++ td (forms ! Acc)) ++
      tr (th "vokativ" ++ td (forms ! ResCze.Voc)) ++
      tr (th "lokál" ++ td (forms ! Loc)) ++
      tr (th "instrumentál" ++ td (forms ! Ins))
    ) ;

  nounTable : NounForms -> Str = \n ->
    frameTable (
      tr (th "pád" ++ th "jednotné číslo" ++ th "množné číslo") ++
      tr (th "nominativ" ++ td n.snom ++ td n.pnom) ++
      tr (th "genitiv" ++ td n.sgen ++ td n.pgen) ++
      tr (th "dativ" ++ td n.sdat ++ td n.pdat) ++
      tr (th "akuzativ" ++ td n.sacc ++ td n.pacc) ++
      tr (th "vokativ" ++ td n.svoc ++ td n.pnom) ++
      tr (th "lokál" ++ td n.sloc ++ td n.ploc) ++
      tr (th "instrumentál" ++ td n.sins ++ td n.pins)
    ) ;

  adjectiveTable : AdjForms -> Str = \forms ->
    let a : Adjective = adjFormsAdjective forms
    in frameTable (
      tr (intagAttr "th" "rowspan=\"2\"" "pád" ++
          intagAttr "th" "colspan=\"4\"" "jednotné číslo" ++
          intagAttr "th" "colspan=\"3\"" "množné číslo") ++
      tr (th "muž. živ." ++ th "muž. neživ." ++ th "žen." ++ th "stř." ++
          th "muž. živ." ++ th "ostatní" ++ th "stř.") ++
      adjectiveRow "nominativ" Nom a ++
      adjectiveRow "genitiv" Gen a ++
      adjectiveRow "dativ" Dat a ++
      adjectiveRow "akuzativ" Acc a ++
      adjectiveRow "vokativ" ResCze.Voc a ++
      adjectiveRow "lokál" Loc a ++
      adjectiveRow "instrumentál" Ins a
    ) ;

  adjectiveRow : Str -> ResCze.Case -> Adjective -> Str = \label,c,a ->
    tr (th label ++
        td (a.s ! Masc Anim ! Sg ! c) ++
        td (a.s ! Masc Inanim ! Sg ! c) ++
        td (a.s ! Fem ! Sg ! c) ++
        td (a.s ! Neutr ! Sg ! c) ++
        td (a.s ! Masc Anim ! Pl ! c) ++
        td (a.s ! Fem ! Pl ! c) ++
        td (a.s ! Neutr ! Pl ! c)) ;

  verbTable : VerbForms -> Str = \v ->
    heading2 "Infinitiv" ++ paragraph v.inf ++
    heading2 "Přítomný čas" ++
    finiteTable v.pressg1 v.pressg2 v.pressg3 v.prespl1 v.prespl2 v.prespl3 ++
    heading2 "Záporný přítomný čas" ++
    finiteTable v.negpressg1 v.negpressg2 v.negpressg3
                v.negprespl1 v.negprespl2 v.negprespl3 ++
    heading2 "Minulé příčestí" ++
    frameTable (
      tr (th "rod" ++ th "jednotné číslo" ++ th "množné číslo") ++
      tr (th "mužský" ++ td v.pastpartsg ++ td v.pastpartpl) ++
      tr (th "ženský" ++ td v.pastpartfsg ++ td v.pastpartfpl) ++
      tr (th "střední" ++ td v.pastpartnsg ++ td v.pastpartnpl)
    ) ++
    heading2 "Rozkazovací způsob" ++
    frameTable (
      tr (th "osoba" ++ th "jednotné číslo" ++ th "množné číslo") ++
      tr (th "1." ++ td "" ++ td v.imppl1) ++
      tr (th "2." ++ td v.impsg2 ++ td v.imppl2)
    ) ++
    heading2 "Záporný rozkazovací způsob" ++
    frameTable (
      tr (th "osoba" ++ th "jednotné číslo" ++ th "množné číslo") ++
      tr (th "1." ++ td "" ++ td v.negimppl1) ++
      tr (th "2." ++ td v.negimpsg2 ++ td v.negimppl2)
    ) ++
    heading2 "Přítomné příčestí" ++ adjectiveTable v.prespart ++
    heading2 "Trpné příčestí" ++ adjectiveTable v.passpart ;

  finiteTable : (sg1,sg2,sg3,pl1,pl2,pl3 : Str) -> Str =
    \sg1,sg2,sg3,pl1,pl2,pl3 -> frameTable (
      tr (th "osoba" ++ th "jednotné číslo" ++ th "množné číslo") ++
      tr (th "1." ++ td sg1 ++ td pl1) ++
      tr (th "2." ++ td sg2 ++ td pl2) ++
      tr (th "3." ++ td sg3 ++ td pl3)
    ) ;

  clauseRow : Str -> ResCze.Tense -> Clause -> Str = \label,tense,cl ->
    tr (th label ++ td (clauseForm tense Pos cl) ++ td (clauseForm tense Neg cl)) ;

  clauseForm : ResCze.Tense -> Polarity -> Clause -> Str = \tense,pol,cl ->
    let aux : Str = cl.auxiliary ! tense ;
        verb : Str = cl.finite ! tense ! pol
    in case cl.isDrop of {
      True => cl.subj ++ verb ++ aux ++ cl.clit ++ cl.compl ;
      False => cl.subj ++ aux ++ cl.clit ++ verb ++ cl.compl
      } ;
}
