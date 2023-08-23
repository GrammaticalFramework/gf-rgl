--# -path=.:../abstract:../common

concrete DocumentationAfr of Documentation = CatAfr ** open 
  ResAfr,
  Prelude,
  HTML
in {


lincat
  Inflection = {t : Str; s1,s2 : Str} ;
  Definition = {s : Str} ;
  Document = {s : Str} ;
  Tag = {s : Str} ;

lin
  InflectionN, InflectionN2, InflectionN3 = \noun -> {
    t  = "s" ;
    s1 = heading1 "Selfstandige Naamwoord" ;
    s2 = frameTable ( 
           tr (th "Enkelvoud"            ++ th "Meervoud"   ) ++
           tr (td (noun.s ! NF Sg Nom) ++ td (noun.s ! NF Pl Nom)))
    } ;

  InflectionPN = \pn -> {
    t  = "pn" ;
    s1 = heading1 "Naam" ;
    s2 = paragraph (pn.s ! NPNom)
    } ;

  InflectionLN = \ln -> {
    t  = "pn" ;
    s1 = heading1 "Naam" ;
    s2 = paragraph (ln.s ! Strong ! NPNom)
    } ;

  InflectionGN = \pn -> {
    t  = "vnm" ;
    s1 = heading1 "Voornaam" ;
    s2 = paragraph (pn.s ! NPNom)
    } ;

  InflectionSN = \pn -> {
    t  = "van" ;
    s1 = heading1 "Van" ;
    s2 = paragraph (pn.s ! Male ! NPNom)
    } ;

  InflectionA, InflectionA2 = \adj ->
    let
      gforms : AForm -> Str = \a ->
        td (adj.s ! Posit  ! a) ++
        td (adj.s ! Compar ! a) ++
        case a of {
          AGen => td "-" ;    -- superlative partitive not used
          _ => td (adj.s ! Superl ! a)
          } ;
      dtable : Str = 
        frameTable ( 
          tr (th []  ++ th "Stellend" ++ th "Vergrotend" ++ 
                        th "Oortreffend") ++
          tr (th "Predikatief" ++ gforms APred) ++
          tr (th "Attributief" ++ gforms AAttr) ++
          tr (th "Partitief"   ++ gforms AGen)
          )
    in { t  = "bv" ;
         s1 = heading1 "Byvoeglike naamwoord" ;
         s2 = dtable
       } ;

  InflectionAdv, InflectionAdV, InflectionAdA, InflectionAdN = \adv -> {
    t  = "bw" ;
    s1 = heading1 "Bywoord" ;
    s2 = paragraph adv.s
    } ;

  InflectionPrep p = {
    t  = "prep" ;
    s1 = heading1 "Voorsetsel" ;
    s2 = paragraph p.s
    } ;

  InflectionV v = {
    t  = "w" ;
    s1 = heading1 "Werkwoord" ;
    s2 = inflVerb v
    } ;

  InflectionV2 v = {
    t  = "v" ;
    s1 = heading1 "Werkwoord" ;
    s2 = inflVerb v
    } ;

  InflectionV3 v = {
    t  = "v" ;
    s1 = heading1 "Werkwoord" ;
    s2 = inflVerb v
    } ;

  InflectionV2V v = {
    t  = "v" ;
    s1 = heading1 "Werkwoord" ;
    s2 = inflVerb v
    } ;

  InflectionV2S v = {
    t  = "v" ;
    s1 = heading1 "Werkwoord" ;
    s2 = inflVerb v
    } ;

  InflectionV2Q v = {
    t  = "v" ;
    s1 = heading1 "Werkwoord" ;
    s2 = inflVerb v
    } ;

  InflectionV2A v = {
    t  = "v" ;
    s1 = heading1 "Werkwoord" ;
    s2 = inflVerb v
    } ;

  InflectionVV v = {
    t  = "v" ;
    s1 = heading1 "Werkwoord" ;
    s2 = inflVerb v
    } ;

  InflectionVS v = {
    t  = "v" ;
    s1 = heading1 "Werkwoord" ;
    s2 = inflVerb v
    } ;

  InflectionVQ v = {
    t  = "v" ;
    s1 = heading1 "Werkwoord" ;
    s2 = inflVerb v
    } ;

  InflectionVA v = {
    t  = "v" ;
    s1 = heading1 "Werkwoord" ;
    s2 = inflVerb v
    } ;

lin
  NoDefinition   t     = {s=t.s};
  MkDefinition   t d   = {s="<p><b>Definisie:</b>"++t.s++d.s++"</p>"};
  MkDefinitionEx t d e = {s="<p><b>Definisie:</b>"++t.s++d.s++"</p><p><b>Voorbeeld:</b>"++e.s++"</p>"};

  MkDocument d i e = ss (i.s1 ++ d.s ++ i.s2 ++ paragraph e.s) ;  -- explanation appended in a new paragraph
  MkTag i = ss i.t ;

oper 
  inflVerb : VVerb -> Str = \verb -> 
     let 
       vfin : VForm -> Str = \f ->
         verb.s ! f ++ verb.prefix ; 
       gforms : VForm -> Str = \f -> 
         td (vfin f) ;

     in frameTable (
          tr (th "Infinitief"       ++ td (verb.s ! VInf)) ++
          tr (th "Teenwoordige Tyd" ++ td (verb.s ! VPres)) ++
          tr (th "Verlede Tyd"      ++ td (verb.s ! VPast)) ++
          tr (th "Perfectief"       ++ td (verb.s ! VPerf))
        ) ;
}
