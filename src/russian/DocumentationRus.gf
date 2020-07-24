concrete DocumentationRus of Documentation = CatRus ** open ResRus, HTML in {
flags coding=utf8 ;

lincat
  Inflection = {t : Str; s1,s2,s3 : Str} ;
  Definition = {s : Str} ;
  Document   = {s : Str} ;
  Tag        = {s : Str} ;

lin
  InflectionN = \n ->  {
    t = "сущ" ;
    s1= heading1 (
      "Существительное" ++
      case n.g of {
        Masc    => "(м.р.)" ;
        Fem     => "(ж.р.)" ;
        Neut    => "(ср.р.)"
        } ++
  
      case n.anim of {
          Animate => "Одушевлённое" ;
          Inanimate => "Неодушевлённое"
        }
      ) ;
    s2= frameTable (
      tr (th "Падеж"     ++ th "ед. ч."  ++ th "мн. ч.") ++
      tr (th "Им."       ++ td (n.snom)  ++ td (n.pnom)) ++
      tr (th "Р."        ++ td (n.sgen)  ++ td (n.pgen)) ++
      tr (th "Д."        ++ td (n.sdat)  ++ td (n.pdat)) ++
      tr (th "В."        ++ td (n.sacc)  ++ td (n.pacc)) ++
      tr (th "Тв."       ++ td (n.sins)  ++ td (n.pins)) ++
      tr (th "Пр."       ++ td (n.sprep) ++ td (n.pprep)) ++
      tr (th "Мест.(П2)" ++ td (n.sloc)  ++ td "") ++
      tr (th "Разд.(Р2)" ++ td (n.sptv)  ++ td "") ++
      tr (th "Зват."     ++ td (n.svoc)  ++ td "")
    ) ;
    s3= ""
  } ;


lin
  MkDocument d i e = {s = i.s1 ++ d.s ++ i.s2 ++ i.s3 ++ e.s} ;
  MkTag i = {s = i.t} ;

  NoDefinition   t     = {s=t.s};
  MkDefinition   t d   = {s="<p><b>Определение:</b>" ++ t.s ++ d.s ++ "</p>"};
  MkDefinitionEx t d e = {s="<p><b>Определение:</b>" ++ t.s ++ d.s ++ "</p><p><b>Пример:</b>" ++ e.s ++ "</p>"};
}
