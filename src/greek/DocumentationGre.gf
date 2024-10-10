--# -path=.:../abstract:../common

concrete DocumentationGre of Documentation = CatGre ** open
  ResGre, HTML, Prelude
in {

lincat
  Inflection = {t : Str; s1,s2 : Str} ;
  Definition = {s : Str} ;
  Document = {s : Str} ;
  Tag = {s : Str} ;

lin
  InflectionN, InflectionN2, InflectionN3 = \noun -> {
    t  = "ο" ;
    s1 = heading1 "Ουσιαστικό" ;
    s2 = inflNoun noun.s
    } ;
    
  InflectionPN = \pn -> {
    t  = "ό" ;
    s1 = heading1 "Όνομα" ;
    s2 = inflNoun pn.s
    } ;

  InflectionLN ln = {
    t  = "ό" ;
    s1 = heading1 "Όνομα" ;
    s2 = paragraph ln.s
    } ;

  InflectionGN gn = {
    t  = "ό" ;
    s1 = heading1 "Όνομα" ;
    s2 = paragraph gn.s
    } ;

  InflectionSN sn = {
    t  = "ό" ;
    s1 = heading1 "Όνομα" ;
    s2 = paragraph sn.s
    } ;

  InflectionA, InflectionA2 = \adj -> {
    t  = "ε" ;
    s1 = heading1 "Επίθετο" ;
    s2 = inflAdj (adj.s ! Posit) ++
         inflAdj (adj.s ! Compar) ++
         inflAdj (adj.s ! Superl)
    } ;

  InflectionAdv, InflectionAdV, InflectionAdA, InflectionAdN = \adv -> {
    t  = "επίρ" ;
    s1 = heading1 "Επίρρημα" ;
    s2 = paragraph adv.s
    } ;

  InflectionPrep p = {
    t  = "πρ" ;
    s1 = heading1 "Πρόθεση" ;
    s2 = paragraph p.s
    } ;

  InflectionV,InflectionV2,InflectionV3,InflectionV2V,InflectionV2S,InflectionV2Q,InflectionV2A,InflectionVV,InflectionVS,InflectionVQ,InflectionVA = \v -> {
    t  = "ρ" ;
    s1 = heading1 "ρήμα" ;
    s2 = inflVerb v.s
    } ;

oper
  inflNoun : (Number => Case => Str) -> Str = \n ->
    frameTable (
          tr (th ""           ++ th "ενικός"            ++ th "πληθυντικός") ++
          tr (th "ονομαστική" ++ td (n ! Sg ! Nom)      ++ td (n ! Pl ! Nom)) ++
          tr (th "γενική"     ++ td (n ! Sg ! Gen)      ++ td (n ! Pl ! Gen)) ++
          tr (th "αιτιατική"  ++ td (n ! Sg ! Acc)      ++ td (n ! Pl ! Acc)) ++
          tr (th "κλητική"    ++ td (n ! Sg ! Vocative) ++ td (n ! Pl ! Vocative))
          ) ;

  inflAdj : (Gender => Number => Case => Str) -> Str = \n ->
    frameTable (
          tr (th ""           ++ th "ενικός"                   ++ th "πληθυντικός"              ++ th "ενικός"                  ++ th "πληθυντικός"             ++ th "ενικός"                   ++ th "πληθυντικός") ++
          tr (th "ονομαστική" ++ td (n ! Masc ! Sg ! Nom)      ++ td (n ! Masc ! Pl ! Nom)      ++ td (n ! Fem ! Sg ! Nom)      ++ td (n ! Fem ! Pl ! Nom)      ++ td (n ! Neut ! Sg ! Nom)      ++ td (n ! Neut ! Pl ! Nom)) ++
          tr (th "γενική"     ++ td (n ! Masc ! Sg ! Gen)      ++ td (n ! Masc ! Pl ! Gen)      ++ td (n ! Fem ! Sg ! Gen)      ++ td (n ! Fem ! Pl ! Gen)      ++ td (n ! Neut ! Sg ! Gen)      ++ td (n ! Neut ! Pl ! Gen)) ++
          tr (th "αιτιατική"  ++ td (n ! Masc ! Sg ! Acc)      ++ td (n ! Masc ! Pl ! Acc)      ++ td (n ! Fem ! Sg ! Acc)      ++ td (n ! Fem ! Pl ! Acc)      ++ td (n ! Neut ! Sg ! Acc)      ++ td (n ! Neut ! Pl ! Acc)) ++
          tr (th "κλητική"    ++ td (n ! Masc ! Sg ! Vocative) ++ td (n ! Masc ! Pl ! Vocative) ++ td (n ! Fem ! Sg ! Vocative) ++ td (n ! Fem ! Pl ! Vocative) ++ td (n ! Neut ! Sg ! Vocative) ++ td (n ! Neut ! Pl ! Vocative))
          ) ;

  inflVerb : (VForm => Str) -> Str = \v -> v ! VPres Ind Sg P1 Active Imperf {-
    frameTable (
          tr (th ""  ++ th "ενικός"                            ++ th "πληθυντικός") ++
          tr (th "1" ++ td (v ! VPres Ind Sg P1 Active Imperf) ++ td (v ! VPres Ind Pl P1 Active Imperf)) ++
          tr (th "2" ++ td (v ! VPres Ind Sg P2 Active Imperf) ++ td (v ! VPres Ind Pl P2 Active Imperf)) ++
          tr (th "3" ++ td (v ! VPres Ind Sg P3 Active Imperf) ++ td (v ! VPres Ind Pl P3 Active Imperf))
          ) ++
    frameTable (
          tr (th ""  ++ th "ενικός"                            ++ th "πληθυντικός") ++
          tr (th "1" ++ td (v ! VPast Ind Sg P1 Active Imperf) ++ td (v ! VPast Ind Pl P1 Active Imperf)) ++
          tr (th "2" ++ td (v ! VPast Ind Sg P2 Active Imperf) ++ td (v ! VPast Ind Pl P2 Active Imperf)) ++
          tr (th "3" ++ td (v ! VPast Ind Sg P3 Active Imperf) ++ td (v ! VPast Ind Pl P3 Active Imperf))
          ) ++
    frameTable (
          tr (th ""  ++ th "ενικός"                            ++ th "πληθυντικός") ++
          tr (th "1" ++ td (v ! VPres Ind Sg P1 Active Perf)   ++ td (v ! VPres Ind Pl P1 Active Perf)) ++
          tr (th "2" ++ td (v ! VPres Ind Sg P2 Active Perf)   ++ td (v ! VPres Ind Pl P2 Active Perf)) ++
          tr (th "3" ++ td (v ! VPres Ind Sg P3 Active Perf)   ++ td (v ! VPres Ind Pl P3 Active Perf))
          ) ++
    frameTable (
          tr (th ""  ++ th "ενικός"                            ++ th "πληθυντικός") ++
          tr (th "1" ++ td (v ! VPast Ind Sg P1 Active Perf)   ++ td (v ! VPast Ind Pl P1 Active Perf)) ++
          tr (th "2" ++ td (v ! VPast Ind Sg P2 Active Perf)   ++ td (v ! VPast Ind Pl P2 Active Perf)) ++
          tr (th "3" ++ td (v ! VPast Ind Sg P3 Active Perf)   ++ td (v ! VPast Ind Pl P3 Active Perf))
          ) -} ;

lin
  NoDefinition   t     = {s=t.s};
  MkDefinition   t d   = {s="<p><b>Παράδειγμα: </b>"++t.s++d.s++"</p>"};
  MkDefinitionEx t d e = {s="<p><b>Παράδειγμα: </b>"++t.s++d.s++"</p><p>"++e.s++"</p>"};

lin
  MkDocument d i e = ss (i.s1 ++ d.s ++ i.s2 ++ paragraph e.s) ;  -- explanation appended in a new paragraph
  MkTag i = ss (i.t) ;

}
