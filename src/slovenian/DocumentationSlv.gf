--# -path=.:../abstract:../common
concrete DocumentationSlv of Documentation = CatSlv ** open 
  ResSlv, (P=ParamX),
  HTML in {
flags coding=utf8 ;

lincat
  Inflection = {t : Str; s1,s2 : Str} ;
  Definition = {s : Str} ;
  Document   = {s : Str} ;
  Tag        = {s : Str} ;

lin
  InflectionN,InflectionN2,InflectionN3 = \n -> {
    t = "sam" ;
    s1= heading1 ("Samostalnik"++
                  case n.g of {
                    AMasc Animate   => "(m.s.ž.)" ;
                    AMasc Inanimate => "(m.s.)" ;
                    AFem            => "(ž.s.)" ;
                    ANeut           => "(s.s.)"
                  }) ;
    s2= frameTable (
          tr (th "" ++ th "ednina" ++ th "dvojina" ++ th "množina") ++
          tr (th "imen." ++ td (n.s!Nom!Sg) ++ td (n.s!Nom!Dl) ++ td (n.s!Nom!Pl))++
          tr (th "rod." ++ td (n.s!Gen!Sg) ++ td (n.s!Gen!Dl) ++ td (n.s!Gen!Pl))++
          tr (th "daj." ++ td (n.s!Dat!Sg) ++ td (n.s!Dat!Dl) ++ td (n.s!Dat!Pl))++
          tr (th "tož." ++ td (n.s!Acc!Sg) ++ td (n.s!Acc!Dl) ++ td (n.s!Acc!Pl))++
          tr (th "mest." ++ td (n.s!Loc!Sg) ++ td (n.s!Loc!Dl) ++ td (n.s!Loc!Pl))++
          tr (th "orod."++td (n.s!Instr!Sg)++td (n.s!Instr!Dl)++td (n.s!Instr!Pl))
        )
  } ;

  InflectionPN = \n -> {
    t = "li" ;
    s1= heading1 ("Lastno Ime"++
                  case n.g of {
                    AMasc Animate   => "(m.s.ž.)" ;
                    AMasc Inanimate => "(m.s.)" ;
                    AFem            => "(ž.s.)" ;
                    ANeut           => "(s.s.)"
                  }) ;
    s2= frameTable (
          tr (th "imen." ++ td (n.s!Nom))++
          tr (th "rod." ++ td (n.s!Gen))++
          tr (th "daj." ++ td (n.s!Dat))++
          tr (th "tož." ++ td (n.s!Acc))++
          tr (th "mest." ++ td (n.s!Loc))++
          tr (th "orod."++td (n.s!Instr))
        )
  } ;

  InflectionLN = \n -> {
    t = "li" ;
    s1= heading1 ("Ime Lokacije"++
                  case n.g of {
                    AMasc Animate   => "(m.s.ž.)" ;
                    AMasc Inanimate => "(m.s.)" ;
                    AFem            => "(ž.s.)" ;
                    ANeut           => "(s.s.)"
                  }) ;
    s2= frameTable (
          tr (th "imen." ++ td (n.s!Nom))++
          tr (th "rod." ++ td (n.s!Gen))++
          tr (th "daj." ++ td (n.s!Dat))++
          tr (th "tož." ++ td (n.s!Acc))++
          tr (th "mest." ++ td (n.s!Loc))++
          tr (th "orod."++td (n.s!Instr))
        )
  } ;

  InflectionGN = \n -> {
    t = "li" ;
    s1= heading1 ("Dano Ime"++
                  case n.g of {
                    P.Male   => "(moški)" ;
                    P.Female => "(ženski)"
                  }) ;
    s2= frameTable (
          tr (th "imen." ++ td (n.s!Nom))++
          tr (th "rod." ++ td (n.s!Gen))++
          tr (th "daj." ++ td (n.s!Dat))++
          tr (th "tož." ++ td (n.s!Acc))++
          tr (th "mest." ++ td (n.s!Loc))++
          tr (th "orod."++td (n.s!Instr))
        )
  } ;

  InflectionSN = \n -> {
    t = "li" ;
    s1= heading1 "Družinsko Ime" ;
    s2= frameTable (
          tr (th "imen." ++ td (n.s!P.Male!Nom))++
          tr (th "rod." ++ td (n.s!P.Male!Gen))++
          tr (th "daj." ++ td (n.s!P.Male!Dat))++
          tr (th "tož." ++ td (n.s!P.Male!Acc))++
          tr (th "mest." ++ td (n.s!P.Male!Loc))++
          tr (th "orod."++td (n.s!P.Male!Instr))
        )
  } ;

  InflectionA, InflectionA2 = \a -> {
    t = "pr" ;
    s1= heading1 ("Pridevnik") ;
    s2= heading2 ("Osnovnik") ++
        mkTable APosit ++
        paragraph ("Določen: "++a.s!APositDefNom) ++
        heading2 ("Primernik") ++
        mkTable ACompar ++
        paragraph ("Določen: "++a.s!AComparDefAcc)
    }
    where {
      mkTable : (Gender -> Number -> Case -> AForm) -> Str = \form ->
        frameTable (
          tr (intagAttr "th" "colspan=\"2\"" ""++th "imen."++th "rod."++th "daj."++th "tož."++th "mest."++th "orod.")++
          tr (intagAttr "th" "rowspan=\"3\"" "m.s."++
              th "edn."++td (a.s!form Masc Sg Nom)++td (a.s!form Masc Sg Gen)++td (a.s!form Masc Sg Dat)++td (a.s!form Masc Sg Acc)++td (a.s!form Masc Sg Loc)++td (a.s!form Masc Sg Instr)) ++ 
          tr (th "dvo."++td (a.s!form Masc Dl Nom)++td (a.s!form Masc Dl Gen)++td (a.s!form Masc Dl Dat)++td (a.s!form Masc Dl Acc)++td (a.s!form Masc Dl Loc)++td (a.s!form Masc Dl Instr)) ++ 
          tr (th "mno."++td (a.s!form Masc Pl Nom)++td (a.s!form Masc Pl Gen)++td (a.s!form Masc Pl Dat)++td (a.s!form Masc Pl Acc)++td (a.s!form Masc Pl Loc)++td (a.s!form Masc Pl Instr)) ++ 
          tr (intagAttr "th" "rowspan=\"3\"" "ž.s."++
              th "edn."++td (a.s!form Fem  Sg Nom)++td (a.s!form Fem  Sg Gen)++td (a.s!form Fem  Sg Dat)++td (a.s!form Fem  Sg Acc)++td (a.s!form Fem  Sg Loc)++td (a.s!form Fem  Sg Instr)) ++ 
          tr (th "dvo."++td (a.s!form Fem  Dl Nom)++td (a.s!form Fem  Dl Gen)++td (a.s!form Fem  Dl Dat)++td (a.s!form Fem  Dl Acc)++td (a.s!form Fem  Dl Loc)++td (a.s!form Fem  Dl Instr)) ++ 
          tr (th "mno."++td (a.s!form Fem  Pl Nom)++td (a.s!form Fem  Pl Gen)++td (a.s!form Fem  Pl Dat)++td (a.s!form Fem  Pl Acc)++td (a.s!form Fem  Pl Loc)++td (a.s!form Fem  Pl Instr)) ++ 
          tr (intagAttr "th" "rowspan=\"3\"" "s.s."++
              th "edn."++td (a.s!form Neut Sg Nom)++td (a.s!form Neut Sg Gen)++td (a.s!form Neut Sg Dat)++td (a.s!form Neut Sg Acc)++td (a.s!form Neut Sg Loc)++td (a.s!form Neut Sg Instr)) ++ 
          tr (th "dvo."++td (a.s!form Neut Dl Nom)++td (a.s!form Neut Dl Gen)++td (a.s!form Neut Dl Dat)++td (a.s!form Neut Dl Acc)++td (a.s!form Neut Dl Loc)++td (a.s!form Neut Dl Instr)) ++ 
          tr (th "mno."++td (a.s!form Neut Pl Nom)++td (a.s!form Neut Pl Gen)++td (a.s!form Neut Pl Dat)++td (a.s!form Neut Pl Acc)++td (a.s!form Neut Pl Loc)++td (a.s!form Neut Pl Instr))
        )
      } ;

  InflectionAdv, InflectionAdV, InflectionAdA, InflectionAdN = \adv -> {
    t = "prs" ;
    s1= heading1 ("Prislov") ;
    s2= paragraph (adv.s)
    } ;

  InflectionPrep = \prep -> {
    t = "pr" ;
    s1= heading1 ("Predlog") ;
    s2= paragraph (prep.s)
    } ;

  InflectionV v = {
    t = "gl" ;
    s1= heading1 ("Glagol") ++
        pp "osebek" ++ v.s ! VPres Sg P3 ++ v.refl ++ v.p ;
    s2= inflVerb v.s
  } ;

  InflectionV2 v = {
    t = "gl" ;
    s1= heading1 ("Glagol") ++
        pp "osebek" ++ v.s ! VPres Sg P3 ++ v.refl ++ v.p ++
        v.c2.s ++ pp "predmet";
    s2= inflVerb v.s
  } ;

  InflectionV3 v = {
    t = "gl" ;
    s1= heading1 ("Glagol") ++
        pp "osebek" ++ v.s ! VPres Sg P3 ++ v.refl ++ v.p ++
        v.c2.s ++ pp "predmet" ++ v.c3.s ++ pp "predmet";
    s2= inflVerb v.s
  } ;

  InflectionV2V v = {
    t = "gl" ;
    s1= heading1 ("Glagol") ++
        pp "osebek" ++ v.s ! VPres Sg P3 ++ v.refl ++ v.p ++
        pp "predmet" ++ pp "glagol";
    s2= inflVerb v.s
  } ;

  InflectionV2S v = {
    t = "gl" ;
    s1= heading1 ("Glagol") ++
        pp "osebek" ++ v.s ! VPres Sg P3 ++ v.refl ++ v.p ++
        pp "predmet" ++ pp "stavek";
    s2= inflVerb v.s
  } ;

  InflectionV2Q v = {
    t = "gl" ;
    s1= heading1 ("Glagol") ++
        pp "osebek" ++ v.s ! VPres Sg P3 ++ v.refl ++ v.p ++
        pp "predmet" ++ pp "vprašanje";
    s2= inflVerb v.s
  } ;

  InflectionV2A v = {
    t = "gl" ;
    s1= heading1 ("Glagol") ++
        pp "osebek" ++ v.s ! VPres Sg P3 ++ v.refl ++ v.p ++
        pp "predmet" ++ pp "pridevnik";
    s2= inflVerb v.s
  } ;

  InflectionVV = \v -> {
    t = "gl" ;
    s1= heading1 ("Glagol") ++
        pp "osebek" ++ v.s ! VPres Sg P3 ++
        pp "glagol";
    s2= inflVerb v.s
  } ;

  InflectionVS = \v -> {
    t = "gl" ;
    s1= heading1 ("Glagol") ++
        pp "osebek" ++ v.s ! VPres Sg P3 ++
        pp "stavek";
    s2= inflVerb v.s
  } ;

  InflectionVQ = \v -> {
    t = "gl" ;
    s1= heading1 ("Glagol") ++
        pp "osebek" ++ v.s ! VPres Sg P3 ++
        pp "vprašanje";
    s2= inflVerb v.s
  } ;

  InflectionVA = \v -> {
    t = "gl" ;
    s1= heading1 ("Glagol") ++
        pp "osebek" ++ v.s ! VPres Sg P3 ++
        pp "pridevnik";
    s2= inflVerb v.s
  } ;

oper
  inflVerb : (VForm => Str) -> Str = \v ->
    (heading2 ("Infinitiv") ++
     paragraph (v ! VInf) ++
     heading2 ("Supínum") ++
     paragraph (v ! VSup) ++
     heading2 ("Prítomný čas") ++
     finite VPres ++
     heading2 ("Príčastie") ++
     frameTable (
        tr (th "" ++
            th "ednina" ++
            th "dvojina" ++
            th "množina") ++
        tr (th "m.s."++td (v!VPastPart Masc Sg)++td (v!VPastPart Masc Dl)++td (v!VPastPart Masc Pl)) ++
        tr (th "ž.s."++td (v!VPastPart Fem  Sg)++td (v!VPastPart Fem  Dl)++td (v!VPastPart Fem  Pl)) ++
        tr (th "s.s."++td (v!VPastPart Neut Sg)++td (v!VPastPart Neut Dl)++td (v!VPastPart Neut Pl))
      )++
     heading2 ("Velelnik")++
     frameTable (
        tr (th "" ++
            th "ednina" ++
            th "dvojina" ++
            th "množina") ++
        tr (th "1 os." ++ 
            td (v ! VImper1Sg) ++ 
            td (v ! VImper1Dl) ++
            td "") ++
        tr (th "2 os." ++ 
            td (v ! VImper2 Sg) ++
            td (v ! VImper2 Dl) ++
            td (v ! VImper2 Pl))
      ))
  where {
    finite : (Number -> Person -> VForm) -> Str = \f ->
      frameTable (
        tr (th "" ++
            th "ednina" ++
            th "dvojina" ++
            th "množina") ++
        tr (th "1 os." ++ 
            td (v ! f Sg P1) ++ 
            td (v ! f Dl P1) ++
            td (v ! f Pl P1)) ++
        tr (th "2 os." ++ 
            td (v ! f Sg P2) ++
            td (v ! f Dl P2) ++
            td (v ! f Pl P2)) ++
        tr (th "3 os." ++
            td (v ! f Sg P3) ++
            td (v ! f Dl P3) ++
            td (v ! f Pl P3))
      ) ;
  } ;

  pp : Str -> Str = \s -> "&lt;"+s+"&gt;";

lin
  NoDefinition   t     = {s=t.s};
  MkDefinition   t d   = {s="<p><b>Definicija:</b>"++t.s++d.s++"</p>"};
  MkDefinitionEx t d e = {s="<p><b>Definicija:</b>"++t.s++d.s++"</p><p><b>Primer:</b>"++e.s++"</p>"};

lin
  MkDocument d i e = {s = i.s1 ++ d.s ++ i.s2 ++ e.s} ;
  MkTag i = {s = i.t} ;
}
