--# -path=.:../abstract:../common
concrete DocumentationMon of Documentation = CatMon ** open
  ResMon,
  Prelude,
  HTML in {

lincat
  Inflection = {t : Str; s1,s2,s3 : Str} ;
  Definition = {s : Str} ;
  Document   = {s : Str} ;
  Tag        = {s : Str} ;

lin InflectionN, InflectionN2, InflectionN3 = \x -> {
      t="n" ;
      s1=heading1 "Noun" ;
      s2=frameTable (
           tr (th ""    ++ th "Sg" ++             th "Pl") ++
           tr (th "Nom" ++ td (x.s ! SF Sg NNom) ++ td (x.s ! SF Pl NNom)) ++
           tr (th "Acc Def" ++ td (x.s ! SF Sg (NAcc Definite)) ++ td (x.s ! SF Pl (NAcc Definite))) ++
           tr (th "Acc Indef" ++ td (x.s ! SF Sg (NAcc Indefinite)) ++ td (x.s ! SF Pl (NAcc Indefinite))) ++
           tr (th "Dat" ++ td (x.s ! SF Sg NDat) ++ td (x.s ! SF Pl NDat)) ++
           tr (th "Gen" ++ td (x.s ! SF Sg NGen) ++ td (x.s ! SF Pl NGen)) ++
           tr (th "Abl" ++ td (x.s ! SF Sg NAbl) ++ td (x.s ! SF Pl NAbl)) ++
           tr (th "Inst" ++td (x.s ! SF Sg NInst) ++td (x.s ! SF Pl NInst)) ++
           tr (th "Com" ++ td (x.s ! SF Sg NCom) ++ td (x.s ! SF Pl NCom)) ++
           tr (th "Dir" ++ td (x.s ! SF Sg NDir) ++ td (x.s ! SF Pl NDir))) ;
      s3=[]
    } ;

lin InflectionPN = \pn -> {
      t="pn" ;
      s1=heading1 "Proper Name" ;
      s2=frameTable (
           tr (th "Nom" ++ td (pn.s ! Nom)) ++
           tr (th "Acc" ++ td (pn.s ! Acc)) ++
           tr (th "Dat" ++ td (pn.s ! Dat)) ++
           tr (th "Gen" ++ td (pn.s ! Gen)) ++
           tr (th "Abl" ++ td (pn.s ! Abl)) ++
           tr (th "Inst" ++td (pn.s ! Inst)) ++
           tr (th "Com" ++ td (pn.s ! Com)) ++
           tr (th "Dir" ++ td (pn.s ! Dir))) ;
      s3=[]
    } ;

lin InflectionA, InflectionA2 = \adj -> {
      t="a" ;
      s1=heading1 "Adjective" ;
      s2= paragraph (adj.s) ;
      s3=[]
    } ;

lin InflectionAdv, InflectionAdV, InflectionAdA, InflectionAdN = \adv -> {
      t = "adv" ;
      s1= heading1 "Adverb" ;
      s2= paragraph (adv.s) ;
      s3= ""
    } ;

lin InflectionPrep = \prep -> {
      t = "prep" ;
      s1= heading1 "Preposition" ;
      s2= paragraph (prep.s ++
                     case prep.rc of {
                       Nom => "nominative" ;
                       Acc => "accusative" ;
                       Dat => "dative" ;
                       Gen => "genitive" ;
                       Abl => "ablative" ;
                       Inst=> "instrumental" ;
                       Com => "commitative" ;
                       Dir => "directional"
                     }) ;
      s3= ""
    } ;

lin
  NoDefinition   t     = {s=t.s};
  MkDefinition   t d   = {s="<p><b>Definition:</b>"++t.s++d.s++"</p>"};
  MkDefinitionEx t d e = {s="<p><b>Definition:</b>"++t.s++d.s++"</p><p><b>Example:</b>"++e.s++"</p>"};

lin
  MkDocument d i e = {s = i.s1 ++ d.s ++ i.s2 ++ paragraph e.s} ;
  MkTag i = {s = i.t} ;

}
