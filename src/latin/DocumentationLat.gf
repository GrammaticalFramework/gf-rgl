concrete DocumentationLat of Documentation = CatLat ** open
  ResLat, ParamX, Prelude, HTML in {

flags coding=utf8 ;

lincat
  Inflection = {t : Str ; s1,s2 : Str} ;
  Definition = {s : Str} ;
  Document   = {s : Str} ;
  Tag        = {s : Str} ;

lin
  InflectionN = \n -> nounInflection "n" "Nomen substantivum" n ;
  InflectionN2 = \n -> nounInflection "n" "Nomen substantivum" n ;
  InflectionN3 = \n -> nounInflection "n" "Nomen substantivum" n ;

  InflectionPN = \n -> {
    t = "pn" ; s1 = heading1 ("Nomen proprium" ++ genderName n.g) ;
    s2 = caseTable n.s
    } ;
  InflectionLN = \n -> {
    t = "ln" ; s1 = heading1 ("Nomen loci" ++ genderName n.g) ;
    s2 = caseTable n.s
    } ;
  InflectionGN = \n -> {
    t = "gn" ;
    s1 = heading1 (case n.g of {
      Male => "Praenomen masculinum" ; Female => "Praenomen femininum"
      }) ;
    s2 = paragraph n.s
    } ;
  InflectionSN = \n -> {
    t = "sn" ; s1 = heading1 "Nomen gentilicium" ;
    s2 = frameTable (
      tr (th "genus" ++ th "singularis" ++ th "pluralis") ++
      tr (th "masculinum" ++ td (n.s ! Male) ++ td n.pl) ++
      tr (th "femininum" ++ td (n.s ! Female) ++ td n.pl))
    } ;

  InflectionA = \a -> adjectiveInflection "a" a ;
  InflectionA2 = \a -> adjectiveInflection "a" a ;

  InflectionV = \v -> verbInflection "v" v ;
  InflectionV2 = \v -> verbInflection "v" v ;
  InflectionVV = \v -> verbInflection "v" v ;
  InflectionVS = \v -> verbInflection "v" v ;
  InflectionVQ = \v -> verbInflection "v" v ;
  InflectionVA = \v -> verbInflection "v" v ;
  InflectionV3 = \v -> verbInflection "v" v ;
  InflectionV2V = \v -> verbInflection "v" v ;
  InflectionV2S = \v -> verbInflection "v" v ;
  InflectionV2Q = \v -> verbInflection "v" v ;
  InflectionV2A = \v -> verbInflection "v" v ;

  InflectionAdv = \a -> {
    t = "adv" ; s1 = heading1 "Adverbium" ;
    s2 = frameTable (
      tr (th "gradus" ++ th "forma") ++
      tr (th "positivus" ++ td (a.s ! Posit)) ++
      tr (th "comparativus" ++ td (a.s ! Compar)) ++
      tr (th "superlativus" ++ td (a.s ! Superl)))
    } ;
  InflectionAdV = \a -> indeclinableInflection "adv" "Adverbium verbi" a.s ;
  InflectionAdA = \a -> indeclinableInflection "adv" "Adverbium adiectivi" a.s ;
  InflectionAdN = \a -> indeclinableInflection "adv" "Adverbium numerale" a.s ;

  InflectionPrep = \p -> {
    t = "prep" ; s1 = heading1 "Praepositio" ;
    s2 = frameTable (
      tr (th "forma" ++ td p.s) ++
      tr (th "casus" ++ td (caseName p.c)) ++
      tr (th "positio" ++ td (case p.isPost of {
        True => "post nomen" ; False => "ante nomen"
        })))
    } ;
  InflectionCl = \cl -> {
    t = "cl" ; s1 = heading1 "Oratio" ;
    s2 = frameTable (
      tr (th "tempus" ++ th "affirmativa" ++ th "negativa") ++
      clauseRow "praesens" Pres Simul cl ++
      clauseRow "imperfectum" Past Simul cl ++
      clauseRow "futurum" Fut Simul cl ++
      clauseRow "conditionalis" Cond Simul cl ++
      clauseRow "perfectum" Pres Anter cl ++
      clauseRow "plusquamperfectum" Past Anter cl ++
      clauseRow "futurum exactum" Fut Anter cl ++
      clauseRow "conditionalis exacta" Cond Anter cl)
    } ;

  NoDefinition t = {s = t.s} ;
  MkDefinition t d = {s = "<p><b>Definitio:</b>" ++ t.s ++ d.s ++ "</p>"} ;
  MkDefinitionEx t d e = {
    s = "<p><b>Definitio:</b>" ++ t.s ++ d.s ++ "</p>" ++
        "<p><b>Exemplum:</b>" ++ e.s ++ "</p>"
    } ;
  MkDocument d i e = {s = i.s1 ++ d.s ++ i.s2 ++ paragraph e.s} ;
  MkTag i = {s = i.t} ;

oper
  nounInflection : Str -> Str -> Noun -> {t : Str ; s1,s2 : Str} =
    \tag,title,n -> {
      t = tag ; s1 = heading1 (title ++ genderName n.g) ; s2 = nounTable n
      } ;
  adjectiveInflection : Str -> Adjective -> {t : Str ; s1,s2 : Str} =
    \tag,a -> {
      t = tag ; s1 = heading1 "Nomen adiectivum" ;
      s2 = heading2 "Gradus positivus" ++ adjectiveTable Posit a ++
           heading2 "Gradus comparativus" ++ adjectiveTable Compar a ++
           heading2 "Gradus superlativus" ++ adjectiveTable Superl a ++
           heading2 "Adverbium" ++ frameTable (
             tr (th "positivus" ++ td (a.adv.s ! Posit)) ++
             tr (th "comparativus" ++ td (a.adv.s ! Compar)) ++
             tr (th "superlativus" ++ td (a.adv.s ! Superl)))
      } ;
  verbInflection : Str -> Verb -> {t : Str ; s1,s2 : Str} =
    \tag,v -> {t = tag ; s1 = heading1 "Verbum" ; s2 = verbTable v} ;
  indeclinableInflection : Str -> Str -> Str -> {t : Str ; s1,s2 : Str} =
    \tag,title,form -> {t = tag ; s1 = heading1 title ; s2 = paragraph form} ;

  genderName : Gender -> Str = \g -> case g of {
    Masc => "(masculinum)" ; Fem => "(femininum)" ; Neutr => "(neutrum)"
    } ;
  caseName : ResLat.Case -> Str = \c -> case c of {
    Nom => "nominativus" ; Acc => "accusativus" ; Gen => "genitivus" ;
    Dat => "dativus" ; Abl => "ablativus" ; ResLat.Voc => "vocativus"
    } ;
  caseTable : (ResLat.Case => Str) -> Str = \forms -> frameTable (
    tr (th "casus" ++ th "forma") ++
    tr (th "nominativus" ++ td (forms ! Nom)) ++
    tr (th "accusativus" ++ td (forms ! Acc)) ++
    tr (th "genitivus" ++ td (forms ! Gen)) ++
    tr (th "dativus" ++ td (forms ! Dat)) ++
    tr (th "ablativus" ++ td (forms ! Abl)) ++
    tr (th "vocativus" ++ td (forms ! ResLat.Voc))) ;
  nounTable : Noun -> Str = \n -> frameTable (
    tr (th "casus" ++ th "singularis" ++ th "pluralis") ++
    nounRow "nominativus" Nom n ++ nounRow "accusativus" Acc n ++
    nounRow "genitivus" Gen n ++ nounRow "dativus" Dat n ++
    nounRow "ablativus" Abl n ++ nounRow "vocativus" ResLat.Voc n) ;
  nounRow : Str -> ResLat.Case -> Noun -> Str = \label,c,n ->
    tr (th label ++ td (n.s ! Sg ! c) ++ td (n.s ! Pl ! c)) ;

  adjectiveTable : Degree -> Adjective -> Str = \degree,a -> frameTable (
    tr (intagAttr "th" "rowspan=\"2\"" "casus" ++
        intagAttr "th" "colspan=\"3\"" "singularis" ++
        intagAttr "th" "colspan=\"3\"" "pluralis") ++
    tr (th "m." ++ th "f." ++ th "n." ++ th "m." ++ th "f." ++ th "n.") ++
    adjectiveRow "nominativus" Nom degree a ++
    adjectiveRow "accusativus" Acc degree a ++
    adjectiveRow "genitivus" Gen degree a ++
    adjectiveRow "dativus" Dat degree a ++
    adjectiveRow "ablativus" Abl degree a ++
    adjectiveRow "vocativus" ResLat.Voc degree a) ;
  adjectiveRow : Str -> ResLat.Case -> Degree -> Adjective -> Str =
    \label,c,d,a -> tr (th label ++
      td (a.s ! d ! Ag Masc Sg c) ++ td (a.s ! d ! Ag Fem Sg c) ++
      td (a.s ! d ! Ag Neutr Sg c) ++ td (a.s ! d ! Ag Masc Pl c) ++
      td (a.s ! d ! Ag Fem Pl c) ++ td (a.s ! d ! Ag Neutr Pl c)) ;

  finiteTable : VAnter -> VTense -> Verb -> Str = \anter,tense,v -> frameTable (
    tr (th "persona" ++ th "singularis" ++ th "pluralis") ++
    tr (th "prima" ++ td (v.act ! VAct anter tense Sg P1) ++ td (v.act ! VAct anter tense Pl P1)) ++
    tr (th "secunda" ++ td (v.act ! VAct anter tense Sg P2) ++ td (v.act ! VAct anter tense Pl P2)) ++
    tr (th "tertia" ++ td (v.act ! VAct anter tense Sg P3) ++ td (v.act ! VAct anter tense Pl P3))) ;
  passiveTable : VTense -> Verb -> Str = \tense,v -> frameTable (
    tr (th "persona" ++ th "singularis" ++ th "pluralis") ++
    tr (th "prima" ++ td (v.pass ! VPass tense Sg P1) ++ td (v.pass ! VPass tense Pl P1)) ++
    tr (th "secunda" ++ td (v.pass ! VPass tense Sg P2) ++ td (v.pass ! VPass tense Pl P2)) ++
    tr (th "tertia" ++ td (v.pass ! VPass tense Sg P3) ++ td (v.pass ! VPass tense Pl P3))) ;
  nonFiniteTable : Verb -> Str = \v ->
    heading2 "Infinitivus" ++ frameTable (
      tr (th "praesens activum" ++ td (v.inf ! VInfActPres)) ++
      tr (th "perfectum activum" ++ td (v.inf ! VInfActPerf Masc)) ++
      tr (th "futurum activum, masculinum" ++ td (v.inf ! VInfActFut Masc)) ++
      tr (th "futurum activum, femininum" ++ td (v.inf ! VInfActFut Fem)) ++
      tr (th "futurum activum, neutrum" ++ td (v.inf ! VInfActFut Neutr)) ++
      tr (th "praesens passivum" ++ td (v.inf ! VInfPassPres)) ++
      tr (th "perfectum passivum, masculinum" ++ td (v.inf ! VInfPassPerf Masc)) ++
      tr (th "perfectum passivum, femininum" ++ td (v.inf ! VInfPassPerf Fem)) ++
      tr (th "perfectum passivum, neutrum" ++ td (v.inf ! VInfPassPerf Neutr)) ++
      tr (th "futurum passivum" ++ td (v.inf ! VinfPassFut))) ++
    heading2 "Imperativus" ++ frameTable (
      tr (th "tempus/persona" ++ th "singularis" ++ th "pluralis") ++
      tr (th "praesens, secunda" ++ td (v.imp ! VImp1 Sg) ++ td (v.imp ! VImp1 Pl)) ++
      tr (th "futurum, secunda" ++ td (v.imp ! VImp2 Sg P2) ++ td (v.imp ! VImp2 Pl P2)) ++
      tr (th "futurum, tertia" ++ td (v.imp ! VImp2 Sg P3) ++ td (v.imp ! VImp2 Pl P3))) ++
    heading2 "Gerundium" ++ frameTable (
      tr (th "accusativus" ++ td (v.ger ! VGenAcc)) ++
      tr (th "genitivus" ++ td (v.ger ! VGenGen)) ++
      tr (th "dativus" ++ td (v.ger ! VGenDat)) ++
      tr (th "ablativus" ++ td (v.ger ! VGenAbl))) ++
    heading2 "Supinum" ++ frameTable (
      tr (th "accusativus" ++ td (v.sup ! VSupAcc)) ++
      tr (th "ablativus" ++ td (v.sup ! VSupAbl))) ;

  verbTable : Verb -> Str = \v ->
    heading2 "Praesens activum indicativum" ++ finiteTable VSim (VPres VInd) v ++
    heading2 "Praesens activum coniunctivum" ++ finiteTable VSim (VPres VConj) v ++
    heading2 "Imperfectum activum indicativum" ++ finiteTable VSim (VImpf VInd) v ++
    heading2 "Imperfectum activum coniunctivum" ++ finiteTable VSim (VImpf VConj) v ++
    heading2 "Futurum activum" ++ finiteTable VSim VFut v ++
    heading2 "Perfectum activum indicativum" ++ finiteTable VAnt (VPres VInd) v ++
    heading2 "Perfectum activum coniunctivum" ++ finiteTable VAnt (VPres VConj) v ++
    heading2 "Plusquamperfectum activum indicativum" ++ finiteTable VAnt (VImpf VInd) v ++
    heading2 "Plusquamperfectum activum coniunctivum" ++ finiteTable VAnt (VImpf VConj) v ++
    heading2 "Futurum exactum activum" ++ finiteTable VAnt VFut v ++
    heading2 "Praesens passivum indicativum" ++ passiveTable (VPres VInd) v ++
    heading2 "Praesens passivum coniunctivum" ++ passiveTable (VPres VConj) v ++
    heading2 "Imperfectum passivum indicativum" ++ passiveTable (VImpf VInd) v ++
    heading2 "Imperfectum passivum coniunctivum" ++ passiveTable (VImpf VConj) v ++
    heading2 "Futurum passivum" ++ passiveTable VFut v ++ nonFiniteTable v ++
    heading2 "Participium praesentis activum" ++ agreementTable (v.part ! VActPres) ++
    heading2 "Participium futuri activum" ++ agreementTable (v.part ! VActFut) ++
    heading2 "Participium perfecti passivum" ++ agreementTable (v.part ! VPassPerf) ++
    heading2 "Gerundivum" ++ agreementTable v.geriv ;

  agreementTable : (Agr => Str) -> Str = \forms -> frameTable (
    tr (intagAttr "th" "rowspan=\"2\"" "casus" ++
        intagAttr "th" "colspan=\"3\"" "singularis" ++
        intagAttr "th" "colspan=\"3\"" "pluralis") ++
    tr (th "m." ++ th "f." ++ th "n." ++ th "m." ++ th "f." ++ th "n.") ++
    agreementRow "nominativus" Nom forms ++ agreementRow "accusativus" Acc forms ++
    agreementRow "genitivus" Gen forms ++ agreementRow "dativus" Dat forms ++
    agreementRow "ablativus" Abl forms ++ agreementRow "vocativus" ResLat.Voc forms) ;
  agreementRow : Str -> ResLat.Case -> (Agr => Str) -> Str = \label,c,forms -> tr (
    th label ++ td (forms ! Ag Masc Sg c) ++ td (forms ! Ag Fem Sg c) ++
    td (forms ! Ag Neutr Sg c) ++ td (forms ! Ag Masc Pl c) ++
    td (forms ! Ag Fem Pl c) ++ td (forms ! Ag Neutr Pl c)) ;

  clauseRow : Str -> ParamX.Tense -> Anteriority -> Clause -> Str = \label,tense,anter,cl -> tr (
    th label ++
    td (defaultSentence (combineClause [] cl tense anter Pos VQFalse) ! SOV) ++
    td (defaultSentence (combineClause [] cl tense anter Neg VQFalse) ! SOV)) ;
}
