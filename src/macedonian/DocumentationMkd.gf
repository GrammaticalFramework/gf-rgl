--# -path=.:../abstract:../common
concrete DocumentationMkd of Documentation = CatMkd ** open 
  ResMkd, Prelude,
  HTML in {

lincat
  Inflection = {t : Str; s1,s2,s3 : Str} ;
  Definition = {s : Str} ;
  Document   = {s : Str} ;
  Tag        = {s : Str} ;

lin
  InflectionN,InflectionN2,InflectionN3 = \n -> {
    t = "им" ;
    s1= heading1 ("Именка"++
                  case n.g of {
                    Masc => "(м.р.)" ;
                    Fem  => "(ж.р.)" ;
                    Neut => "(ср.р.)"
                  }) ;
    s2= frameTable (
          tr (intagAttr "th" "colspan=\"2\"" "" ++ th "еднина" ++ th "множина") ++ 
          tr (intagAttr "th" "colspan=\"2\"" "неопределена" ++ td (n.s ! Indef ! Sg) ++ td (n.s ! Indef ! Pl)) ++
          tr (intagAttr "th" "rowspan=\"3\"" "определеност" ++ th "општа" ++ td (n.s ! Def Unspecified ! Sg) ++ td (n.s ! Def Unspecified ! Pl)) ++
          tr (th "одд." ++ td (n.s ! Def Distal ! Sg) ++ td (n.s ! Def Distal ! Pl))  ++
          tr (th "бл." ++ td (n.s ! Def Proximal ! Sg) ++ td (n.s ! Def Proximal ! Pl)) ++
          tr (intagAttr "th" "colspan=\"2\"" "вокатив" ++ td (n.vocative ! Sg) ++ td (n.vocative ! Pl)) ++
          tr (intagAttr "th" "colspan=\"2\"" "бройна форма" ++ td [] ++ td n.count_form)
        ) ;
    s3= case n.relType of {
          Pref   => [] ;
          AdjMod => heading1 ("Придавка") ++
                    frameTable (
                      tr (intagAttr "th" "rowspan=\"2\" colspan=\"2\"" [] ++
                          intagAttr "th" "colspan=\"3\"" "еднина" ++ 
                          intagAttr "th" "rowspan=\"2\"" "множина") ++
                      tr (th "м.р." ++ th "ж.р." ++ th "с.р.") ++
                      tr (intagAttr "th" "colspan=\"2\"" "неопределена"  ++ td (n.rel ! Indef ! GSg Masc) ++ td (n.rel ! Indef ! GSg Fem) ++ td (n.rel ! Indef ! GSg Neuter) ++ td (n.rel ! Indef ! GPl)) ++
                      tr (intagAttr "th" "rowspan=\"3\"" "определеност" ++ td [] ++ td (n.rel ! Def Unspecified ! GSg Masc) ++ td (n.rel ! Def Unspecified ! GSg Fem) ++ td (n.rel ! Def Unspecified ! GSg Neuter) ++ td (n.rel ! Def Unspecified ! GPl)) ++
                      tr (td "одд." ++ td (n.rel ! Def Distal ! GSg Masc) ++ td (n.rel ! Def Distal ! GSg Fem) ++ td (n.rel ! Def Distal ! GSg Neuter) ++ td (n.rel ! Def Distal ! GPl)) ++
                      tr (td "бл." ++ td (n.rel ! Def Proximal ! GSg Masc) ++ td (n.rel ! Def Proximal ! GSg Fem) ++ td (n.rel ! Def Proximal ! GSg Neuter) ++ td (n.rel ! Def Proximal ! GPl))
                    ) ;
          AdvMod => heading1 ("Прилог") ++
                    paragraph (n.rel ! Indef ! GSg Masc)
        }
  } ;

  InflectionPN = \pn -> {
    t = "същ.с." ;
    s1= heading1 ("Съществително Собствено") ;
    s2 = paragraph pn.s ;
    s3 = ""
    } ;

  InflectionLN = \ln -> {
    t = "същ.с." ;
    s1= heading1 ("Име за Място") ;
    s2 = paragraph ln.s ;
    s3 = ""
    } ;

  InflectionGN = \gn -> {
    t = "същ.с.л." ;
    s1= "Име" ;
    s2 = gn.s ;
    s3 = ""
    } ;

  InflectionSN = \sn -> {
    t = "същ.с.ф." ;
    s1= heading1 ("Фамилно Име") ;
    s2 = sn.s  ;
    s3 = ""
    } ;

  InflectionA, InflectionA2 = \a -> {
    t = "пр" ;
    s1= heading1 ("Придавка") ;
    s2= frameTable (
          tr (intagAttr "th" "rowspan=\"2\" colspan=\"2\"" [] ++
              intagAttr "th" "colspan=\"3\"" "еднина" ++ 
              intagAttr "th" "rowspan=\"2\"" "множина") ++
          tr (th "м.р." ++ th "ж.р." ++ th "с.р.") ++
          tr (intagAttr "th" "colspan=\"2\"" "неопределена"  ++ td (a.s ! Indef ! GSg Masc) ++ td (a.s ! Indef ! GSg Fem) ++ td (a.s ! Indef ! GSg Neuter) ++ td (a.s ! Indef ! GPl)) ++
          tr (intagAttr "th" "rowspan=\"3\"" "определеност" ++ td [] ++ td (a.s ! Def Unspecified ! GSg Masc) ++ td (a.s ! Def Unspecified ! GSg Fem) ++ td (a.s ! Def Unspecified ! GSg Neuter) ++ td (a.s ! Def Unspecified ! GPl)) ++
          tr (td "одд." ++ td (a.s ! Def Distal ! GSg Masc) ++ td (a.s ! Def Distal ! GSg Fem) ++ td (a.s ! Def Distal ! GSg Neuter) ++ td (a.s ! Def Distal ! GPl)) ++
          tr (td "бл." ++ td (a.s ! Def Proximal ! GSg Masc) ++ td (a.s ! Def Proximal ! GSg Fem) ++ td (a.s ! Def Proximal ! GSg Neuter) ++ td (a.s ! Def Proximal ! GPl))
        ) ++
        heading1 ("Прилог") ++
        paragraph (a.adverb) ;
    s3= ""
    } ;

  InflectionAdv, InflectionAdV, InflectionAdA, InflectionAdN = \adv -> {
    t = "прил" ;
    s1= heading1 ("Прилог") ;
    s2= paragraph (adv.s) ;
    s3= ""
    } ;

  InflectionPrep = \prep -> {
    t = "пред" ;
    s1= heading1 ("Предлог") ;
    s2= paragraph (prep.s) ;
    s3= ""
    } ;

  InflectionV, InflectionV2, InflectionV3, InflectionV2V, InflectionV2S,
  InflectionV2Q, InflectionV2A, InflectionVV, InflectionVS,
  InflectionVQ, InflectionVA = \v -> {
    t = "гл" ;
    s1= heading1 ("Глагол") ;
    s2= heading2 ("Несвършен вид") ++
        heading3 ("Изявително наклонение") ++
        heading4 ("Сегашно време") ++
        finite (v.present ! Imperfective) ++
        heading4 ("Минато несвршено време (имперфект)") ++
        finite (v.imperfect ! Imperfective) ++
        heading3 ("Повелително наклонение") ++
        imperative (v.Imperative ! Imperfective) ++
        heading3 ("Партицип") ++
        adjForms (v.participle.aorist ! Imperfective) ++
        tag "br" ++
        adjForms v.participle.imperfect ++
        heading2 ("Свършен вид") ++
        heading3 ("Изявително наклонение") ++
        heading4 ("Сегашно време") ++
        finite (v.present ! Perfective) ++
        heading4 ("Минатото свршено време (аорист)") ++
        finite v.aorist ++
        heading4 ("Минато несвршено време (имперфект)") ++
        finite (v.imperfect ! Perfective) ++
        heading3 ("Повелително наклонение") ++
        imperative (v.Imperative ! Perfective) ++
        heading3 ("Партицип") ++
        adjForms (v.participle.aorist ! Perfective) ++
        heading1 ("Именка") ++
        v.noun_from_verb ;
    s3= ""
  } where {
    finite : (Number => Person => Str) -> Str = \f ->
      frameTable (
        tr (th "" ++
            th "ед.ч." ++
            th "мн.ч.") ++
        tr (th "1 л." ++ 
            td (f ! Sg ! P1) ++ 
            td (f ! Pl ! P1)) ++
        tr (th "2 л." ++ 
            td (f ! Sg ! P2) ++
            td (f ! Pl ! P2)) ++
        tr (th "3 л." ++
            td (f ! Sg ! P3) ++
            td (f ! Pl ! P3))
      ) ;
    imperative : (Number => Str) -> Str = \f ->
      frameTable (
        tr (th "ед.ч." ++
            th "мн.ч.") ++
        tr (td (f ! Sg) ++ 
            td (f ! Pl))
      ) ;
    adjForms : (GenNum => Str) -> Str = \f ->
        frameTable (
          tr (intagAttr "th" "colspan=\"3\"" "еднина" ++ 
              intagAttr "th" "rowspan=\"2\"" "множина") ++
          tr (th "м.р." ++ th "ж.р." ++ th "с.р.") ++
          tr (td (f ! GSg Masc) ++ td (f ! GSg Fem) ++ td (f ! GSg Neuter) ++ td (f ! GPl))
        )
  } ;

lin
  NoDefinition   t     = {s=t.s};
  MkDefinition   t d   = {s="<p><b>Дефиниция:</b>"++t.s++d.s++"</p>"};
  MkDefinitionEx t d e = {s="<p><b>Дефиниция:</b>"++t.s++d.s++"</p><p><b>Пример:</b>"++e.s++"</p>"};

lin
  MkDocument d i e = {s = i.s1 ++ d.s ++ i.s2 ++ i.s3 ++ e.s} ;
  MkTag i = {s = i.t} ;

}
