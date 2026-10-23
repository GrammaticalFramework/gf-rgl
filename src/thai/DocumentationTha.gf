--# -path=.:../abstract:../common
concrete DocumentationTha of Documentation = CatTha ** open
  ResTha, Prelude, HTML in {

flags coding=utf8 ;

lincat
  Inflection = {t : Str; s1,s2 : Str} ;
  Definition = {s : Str} ;
  Document = {s : Str} ;
  Tag      = {s : Str} ;

lin
  InflectionN, InflectionN2, InflectionN3 = \noun -> {
    t = "n" ;
    s1 = heading1 "Noun" ;
    s2 = frameTable (
      tr (th "Form" ++ td noun.s) ++
      tr (th "Classifier" ++ td noun.c)
      )
    } ;

  InflectionPN = \name -> nameInflection "pn" "Proper name" name.s ;
  InflectionLN = \name -> nameInflection "ln" "Location name" name.s ;
  InflectionGN = \name -> nameInflection "gn" "Given name" name.s ;
  InflectionSN = \name -> nameInflection "sn" "Surname" name.s ;

  InflectionA, InflectionA2 = \adj -> {
    t = "a" ;
    s1 = heading1 "Adjective" ;
    s2 = paragraph adj.s
    } ;

  InflectionV, InflectionV2, InflectionVS, InflectionVQ,
  InflectionVA, InflectionV3, InflectionV2V, InflectionV2S,
  InflectionV2Q, InflectionV2A = \verb -> {
    t = "v" ;
    s1 = heading1 "Verb" ;
    s2 = verbTable verb
    } ;

  InflectionVV = \verb -> {
    t = "v" ;
    s1 = heading1 "Verb" ;
    s2 = paragraph verb.s
    } ;

  InflectionAdv, InflectionAdV, InflectionAdA, InflectionAdN = \adv -> {
    t = "adv" ;
    s1 = heading1 "Adverb" ;
    s2 = paragraph adv.s
    } ;

  InflectionPrep = \prep -> {
    t = "prep" ;
    s1 = heading1 "Preposition" ;
    s2 = paragraph prep.s
    } ;

  InflectionCl = \clause -> {
    t = "cl" ;
    s1 = heading1 "Clause" ;
    s2 = frameTable (
      tr (th "" ++ th "Positive" ++ th "Negative") ++
      tr (th "Declarative" ++
          td (clause.s ! ClDecl ! Pos) ++
          td (clause.s ! ClDecl ! Neg)) ++
      tr (th "Interrogative" ++
          td (clause.s ! ClQuest ! Pos) ++
          td (clause.s ! ClQuest ! Neg))
      )
    } ;

  NoDefinition title = {s = title.s} ;
  MkDefinition title definition = {
    s = paragraph ("<b>คำนิยาม:</b>" ++ title.s ++ definition.s)
    } ;
  MkDefinitionEx title definition example = {
    s = paragraph ("<b>คำนิยาม:</b>" ++ title.s ++ definition.s) ++
        paragraph ("<b>ตัวอย่าง:</b>" ++ example.s)
    } ;

  MkDocument definition inflection example = {
    s = inflection.s1 ++ definition.s ++ inflection.s2 ++ paragraph example.s
    } ;
  MkTag inflection = {s = inflection.t} ;

oper
  nameInflection : Str -> Str -> Str -> {t, s1, s2 : Str} =
    \tag,title,name -> {
      t = tag ;
      s1 = heading1 title ;
      s2 = paragraph name
      } ;

  verbTable : Verb -> Str = \verb ->
    let vp = predV verb in
    frameTable (
      tr (th "Positive" ++ td (vp.s ! Pos)) ++
      tr (th "Negative" ++ td (vp.s ! Neg))
      ) ;

}
