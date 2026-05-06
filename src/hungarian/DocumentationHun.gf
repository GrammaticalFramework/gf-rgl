--# -path=.:../abstract:../common:../prelude

concrete DocumentationHun of Documentation = CatHun ** open
  ResHun, Prelude, HTML in {

lincat
  Inflection = {t : Str; s1,s2 : Str} ;
  Definition = {s : Str} ;
  Document   = {s : Str} ;
  Tag        = {s : Str} ;

lin
  InflectionN = \noun -> nounInflection "n" "Főnév" noun ;
  InflectionN2 = \noun -> nounInflection "n2" "Kétargumentumú főnév" noun ;
  InflectionN3 = \noun -> nounInflection "n3" "Háromargumentumú főnév" noun ;

  InflectionPN = \pn -> nameInflection "pn" "Tulajdonnév" pn ;
  InflectionLN = \ln -> nameInflection "ln" "Helynév" ln ;
  InflectionGN = \gn -> nameInflection "gn" "Keresztnév" gn ;
  InflectionSN = \sn -> nameInflection "sn" "Vezetéknév" sn ;

  InflectionA = \adj -> adjectiveInflection "a" "Melléknév" adj ;
  InflectionA2 = \adj -> adjectiveInflection "a2" "Vonzatos melléknév" adj ;

  InflectionV = \v -> verbInflection "v" "Ige" v ;
  InflectionVS = \v -> verbInflection "vs" "Mondatvonzatú ige" v ;
  InflectionVQ = \v -> verbInflection "vq" "Kérdő mondatvonzatú ige" v ;
  InflectionVA = \v -> verbInflection "va" "Melléknévi vonzatú ige" v ;

  InflectionV2 = \v -> verb2Inflection "v2" "Tárgyas ige" v ;
  InflectionVV = \v -> verb2Inflection "vv" "Igenévi vonzatú ige" v ;
  InflectionV3 = \v -> verb2Inflection "v3" "Háromargumentumú ige" v ;
  InflectionV2V = \v -> verb2Inflection "v2v" "Tárgyas és igenévi vonzatú ige" v ;
  InflectionV2S = \v -> verb2Inflection "v2s" "Tárgyas és mondatvonzatú ige" v ;
  InflectionV2Q = \v -> verb2Inflection "v2q" "Tárgyas és kérdő mondatvonzatú ige" v ;
  InflectionV2A = \v -> verb2Inflection "v2a" "Tárgyas és melléknévi vonzatú ige" v ;

  InflectionAdv = \adv -> adverbInflection "adv" "Határozószó" adv.s ;
  InflectionAdV = \adv -> adverbInflection "adv" "Igei határozószó" adv.s ;
  InflectionAdA = \adv -> adverbInflection "adv" "Melléknévi határozószó" adv.s ;
  InflectionAdN = \adv -> adverbInflection "adv" "Főnévi határozószó" adv.s ;

  InflectionPrep = \prep -> {
    t = "prep" ;
    s1 = heading1 "Névutó" ;
    s2 = frameTable (
      tr (th "előtag" ++ td prep.pr) ++
      tr (th "eset" ++ td (caseName prep.c)) ++
      tr (th "névutó" ++ td prep.s))
    } ;

lin
  NoDefinition t = {s = t.s} ;
  MkDefinition t d = {s = "<p><b>Meghatározás:</b>" ++ t.s ++ d.s ++ "</p>"} ;
  MkDefinitionEx t d e =
    {s = "<p><b>Meghatározás:</b>" ++ t.s ++ d.s ++
         "</p><p><b>Példa:</b>" ++ e.s ++ "</p>"} ;

lin
  MkDocument d i e = {s = i.s1 ++ d.s ++ i.s2 ++ paragraph e.s} ;
  MkTag i = {s = i.t} ;

oper
  nounInflection : Str -> Str -> Noun -> {t : Str; s1,s2 : Str} =
    \tag,title,noun -> {
      t = tag ;
      s1 = heading1 title ;
      s2 = caseTable2 (\n,c -> nounCase noun n c) ++
           heading2 "Birtokos alakok" ++
           possTable noun
      } ;

  nameInflection : Str -> Str -> NounPhrase -> {t : Str; s1,s2 : Str} =
    \tag,title,np -> {
      t = tag ;
      s1 = heading1 title ;
      s2 = frameTable (
        tr (th "alanyeset" ++ td (np.s ! NoPoss ! Nom)) ++
        tr (th "tárgyeset" ++ td (np.s ! NoPoss ! Acc)) ++
        tr (th "részes eset" ++ td (np.s ! NoPoss ! Dat)) ++
        tr (th "inessivus" ++ td (np.s ! NoPoss ! Ine)) ++
        tr (th "elativus" ++ td (np.s ! NoPoss ! Ela)) ++
        tr (th "allativus" ++ td (np.s ! NoPoss ! All)) ++
        tr (th "adessivus" ++ td (np.s ! NoPoss ! Ade)) ++
        tr (th "ablativus" ++ td (np.s ! NoPoss ! Abl)) ++
        tr (th "superessivus" ++ td (np.s ! NoPoss ! Sup)) ++
        tr (th "instrumentalis" ++ td (np.s ! NoPoss ! Ins)) ++
        tr (th "translativus" ++ td (np.s ! NoPoss ! Tra)))
      } ;

  adjectiveInflection : Str -> Str -> Adjective -> {t : Str; s1,s2 : Str} =
    \tag,title,adj -> {
      t = tag ;
      s1 = heading1 title ;
      s2 = heading2 "Alapfok" ++
           caseTable2 (\n,c -> adjCase adj Posit n c) ++
           heading2 "Középfok" ++
           caseTable2 (\n,c -> adjCase adj Compar n c) ++
           heading2 "Felsőfok" ++
           caseTable2 (\n,c -> adjCase adj Superl n c)
      } ;

  adverbInflection : Str -> Str -> Str -> {t : Str; s1,s2 : Str} =
    \tag,title,s -> {
      t = tag ;
      s1 = heading1 title ;
      s2 = paragraph s
      } ;

  verbInflection : Str -> Str -> Verb -> {t : Str; s1,s2 : Str} =
    \tag,title,v -> {
      t = tag ;
      s1 = heading1 title ;
      s2 = verbTable (\vf -> v.s ! vf)
      } ;

  verb2Inflection : Str -> Str -> Verb2 -> {t : Str; s1,s2 : Str} =
    \tag,title,v -> {
      t = tag ;
      s1 = heading1 title ;
      s2 = paragraph ("vonzat esete: " ++ caseName v.c2) ++
           heading2 "Határozatlan ragozás" ++
           verbTable (\vf -> v.s ! Indef ! vf) ++
           heading2 "Határozott ragozás" ++
           verbTable (\vf -> v.s ! Def ! vf)
      } ;

  caseTable2 : (Number -> Case -> Str) -> Str =
    \forms -> frameTable (
      tr (th "" ++ th "egyes szám" ++ th "többes szám") ++
      tr (th "alanyeset" ++ td (forms Sg Nom) ++ td (forms Pl Nom)) ++
      tr (th "tárgyeset" ++ td (forms Sg Acc) ++ td (forms Pl Acc)) ++
      tr (th "részes eset" ++ td (forms Sg Dat) ++ td (forms Pl Dat)) ++
      tr (th "inessivus" ++ td (forms Sg Ine) ++ td (forms Pl Ine)) ++
      tr (th "elativus" ++ td (forms Sg Ela) ++ td (forms Pl Ela)) ++
      tr (th "allativus" ++ td (forms Sg All) ++ td (forms Pl All)) ++
      tr (th "adessivus" ++ td (forms Sg Ade) ++ td (forms Pl Ade)) ++
      tr (th "ablativus" ++ td (forms Sg Abl) ++ td (forms Pl Abl)) ++
      tr (th "superessivus" ++ td (forms Sg Sup) ++ td (forms Pl Sup)) ++
      tr (th "instrumentalis" ++ td (forms Sg Ins) ++ td (forms Pl Ins)) ++
      tr (th "translativus" ++ td (forms Sg Tra) ++ td (forms Pl Tra)))
      ;

  possTable : Noun -> Str =
    \noun -> frameTable (
      tr (th "" ++ th "birtok egyes szám" ++ th "birtok többes szám") ++
      tr (th "1. személy egyes szám" ++ td (possForm noun Sg P1 Sg) ++ td (possForm noun Pl P1 Sg)) ++
      tr (th "2. személy egyes szám" ++ td (possForm noun Sg P2 Sg) ++ td (possForm noun Pl P2 Sg)) ++
      tr (th "3. személy egyes szám" ++ td (possForm noun Sg P3 Sg) ++ td (possForm noun Pl P3 Sg)) ++
      tr (th "1. személy többes szám" ++ td (possForm noun Sg P1 Pl) ++ td (possForm noun Pl P1 Pl)) ++
      tr (th "2. személy többes szám" ++ td (possForm noun Sg P2 Pl) ++ td (possForm noun Pl P2 Pl)) ++
      tr (th "3. személy többes szám" ++ td (possForm noun Sg P3 Pl) ++ td (possForm noun Pl P3 Pl)))
      ;

  verbTable : (VForm -> Str) -> Str =
    \forms ->
      heading2 "Főnévi igenév" ++
      paragraph (forms VInf) ++
      heading2 "Jelen idő" ++
      personNumberTable (\p,n -> forms (VPres p n)) ++
      heading2 "Múlt idő" ++
      personNumberTable (\p,n -> forms (VPast p n)) ;

  personNumberTable : (Person -> Number -> Str) -> Str =
    \forms -> frameTable (
      tr (th "" ++ th "egyes szám" ++ th "többes szám") ++
      tr (th "1. személy" ++ td (forms P1 Sg) ++ td (forms P1 Pl)) ++
      tr (th "2. személy" ++ td (forms P2 Sg) ++ td (forms P2 Pl)) ++
      tr (th "3. személy" ++ td (forms P3 Sg) ++ td (forms P3 Pl)))
      ;

  nounCase : Noun -> Number -> Case -> Str =
    \noun,n,c -> caseFromStem glue noun c n ;

  adjCase : Adjective -> Degree -> Number -> Case -> Str =
    \adj,d,n,c -> caseFromStem glue {s = adj.s ! d ; h = adj.h} c n ;

  possForm : Noun -> Number -> Person -> Number -> Str =
    \noun,possd,person,possr ->
      let det : Determiner = {
            s = \\_ => [] ;
            sp = \\_ => [] ;
            n = possd ;
            dt = DetPoss (agr2pstem <person,possr>) ;
            poss = possForms ! <person,possr> ;
            caseagr = False
            } ;
          cn : CNoun = noun ** {compl = \\_,_ => [] ; postmod = []} ;
       in caseFromPossStem cn det Nom ;

  caseName : Case -> Str = \c ->
    case c of {
      Nom => "alanyeset" ;
      Acc => "tárgyeset" ;
      Dat => "részes eset" ;
      Ine => "inessivus" ;
      Ela => "elativus" ;
      All => "allativus" ;
      Ade => "adessivus" ;
      Abl => "ablativus" ;
      Sup => "superessivus" ;
      Ins => "instrumentalis" ;
      Tra => "translativus" ;
      _ => case2str c
      } ;
}
