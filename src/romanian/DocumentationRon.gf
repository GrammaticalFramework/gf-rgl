concrete DocumentationRon of Documentation = CatRon ** open
  ResRon, Prelude, HTML in {

lincat
  Inflection = {t : Str; s1,s2,s3 : Str} ;
  Definition = {s : Str} ;
  Document   = {s : Str} ;
  Tag        = {s : Str} ;

lin InflectionA, InflectionA2 = \x -> {
      t="a" ;
      s1="" ;
      s2=frameTable (
           tr (intagAttr "th" "rowspan=\"25\"" "s" ++ th "AF Masc Sg Indef ANomAcc" ++ td (x.s ! AF Masc Sg Indef ANomAcc)) ++
           tr (th "Masc Sg Indef gen/dat" ++ td (x.s ! AF Masc Sg Indef AGenDat)) ++
           tr (th "Masc Sg Indef voc" ++ td (x.s ! AF Masc Sg Indef AVoc)) ++
           tr (th "Masc Sg Def nom/acc" ++ td (x.s ! AF Masc Sg Def ANomAcc)) ++
           tr (th "Masc Sg Def gen/dat" ++ td (x.s ! AF Masc Sg Def AGenDat)) ++
           tr (th "Masc Sg Def voc" ++ td (x.s ! AF Masc Sg Def AVoc)) ++
           tr (th "Masc Pl Indef nom/acc" ++ td (x.s ! AF Masc Pl Indef ANomAcc)) ++
           tr (th "Masc Pl Indef gen/dat" ++ td (x.s ! AF Masc Pl Indef AGenDat)) ++
           tr (th "Masc Pl Indef voc" ++ td (x.s ! AF Masc Pl Indef AVoc)) ++
           tr (th "Masc Pl Def nom/acc" ++ td (x.s ! AF Masc Pl Def ANomAcc)) ++
           tr (th "Masc Pl Def gen/dat" ++ td (x.s ! AF Masc Pl Def AGenDat)) ++
           tr (th "Masc Pl Def voc" ++ td (x.s ! AF Masc Pl Def AVoc)) ++
           tr (th "Fem Sg Indef nom/acc" ++ td (x.s ! AF Fem Sg Indef ANomAcc)) ++
           tr (th "Fem Sg Indef gen/dat" ++ td (x.s ! AF Fem Sg Indef AGenDat)) ++
           tr (th "Fem Sg Indef voc" ++ td (x.s ! AF Fem Sg Indef AVoc)) ++
           tr (th "Fem Sg Def nom/acc" ++ td (x.s ! AF Fem Sg Def ANomAcc)) ++
           tr (th "Fem Sg Def gen/dat" ++ td (x.s ! AF Fem Sg Def AGenDat)) ++
           tr (th "Fem Sg Def voc" ++ td (x.s ! AF Fem Sg Def AVoc)) ++
           tr (th "Fem Pl Indef nom/acc" ++ td (x.s ! AF Fem Pl Indef ANomAcc)) ++
           tr (th "Fem Pl Indef gen/dat" ++ td (x.s ! AF Fem Pl Indef AGenDat)) ++
           tr (th "Fem Pl Indef voc" ++ td (x.s ! AF Fem Pl Indef AVoc)) ++
           tr (th "Fem Pl Def nom/acc" ++ td (x.s ! AF Fem Pl Def ANomAcc)) ++
           tr (th "Fem Pl Def gen/dat" ++ td (x.s ! AF Fem Pl Def AGenDat)) ++
           tr (th "Fem Pl Def voc" ++ td (x.s ! AF Fem Pl Def AVoc)) ++
           tr (th "" ++ td (x.s ! AA))) ;
      s3=[]
    } ;
lin InflectionAdA = \x -> {
      t="ada" ;
      s1="" ;
      s2=paragraph x.s ;
      s3=[]
    } ;
lin InflectionAdN = \x -> {
      t="adn" ;
      s1="" ;
      s2=paragraph x.s ;
      s3=[]
    } ;
lin InflectionAdV, InflectionAdv = \x -> {
      t="adv" ;
      s1="" ;
      s2=paragraph x.s ;
      s3=[]
    } ;
lin InflectionGN = \x -> {
      t="gn" ;
      s1="" ;
      s2=frameTable (
           tr (intagAttr "th" "rowspan=\"5\"" "s" ++ th "No" ++ td (x.s ! No)) ++
           tr (th "Da" ++ td (x.s ! Da)) ++
           tr (th "Ac" ++ td (x.s ! Ac)) ++
           tr (th "Ge" ++ td (x.s ! Ge)) ++
           tr (th "Vo" ++ td (x.s ! Vo))) ;
      s3=[]
    } ;
lin InflectionLN = \x -> {
      t="ln" ;
      s1="" ;
      s2=frameTable (
           tr (intagAttr "th" "rowspan=\"5\"" "s" ++ th "No" ++ td (x.s ! No)) ++
           tr (th "Da" ++ td (x.s ! Da)) ++
           tr (th "Ac" ++ td (x.s ! Ac)) ++
           tr (th "Ge" ++ td (x.s ! Ge)) ++
           tr (th "Vo" ++ td (x.s ! Vo))) ;
      s3=[]
    } ;
lin InflectionN = \x -> {
      t="n" ;
      s1="" ;
      s2=inflNoun x ;
      s3=[]
    } ;
lin InflectionN2 = \x -> {
      t="n2" ;
      s1="" ;
      s2=inflNoun x ;
      s3=[]
    } ;
lin InflectionN3 = \x -> {
      t="n3" ;
      s1="" ;
      s2=inflNoun x ;
      s3=[]
    } ;
lin InflectionPN = \x -> {
      t="pn" ;
      s1="" ;
      s2=frameTable (
           tr (intagAttr "th" "rowspan=\"5\"" "s" ++ th "No" ++ td (x.s ! No)) ++
           tr (th "Da" ++ td (x.s ! Da)) ++
           tr (th "Ac" ++ td (x.s ! Ac)) ++
           tr (th "Ge" ++ td (x.s ! Ge)) ++
           tr (th "Vo" ++ td (x.s ! Vo))) ;
      s3=[]
    } ;
lin InflectionPrep = \x -> {
      t="prep" ;
      s1="" ;
      s2=frameTable (
           tr (th "s" ++ td (x.s)) ++
           tr (th "prepDir" ++ td (x.prepDir))) ;
      s3=[]
    } ;
lin InflectionSN = \x -> {
      t="sn" ;
      s1="" ;
      s2=frameTable (
           tr (intagAttr "th" "rowspan=\"5\"" "s" ++ th "No" ++ td (x.s ! No)) ++
           tr (th "Da" ++ td (x.s ! Da)) ++
           tr (th "Ac" ++ td (x.s ! Ac)) ++
           tr (th "Ge" ++ td (x.s ! Ge)) ++
           tr (th "Vo" ++ td (x.s ! Vo))) ;
      s3=[]
    } ;
lin InflectionV = \x -> {
      t="v" ;
      s1="" ;
      s2=inflVerb x ;
      s3=[]
    } ;
lin InflectionV2 = \x -> {
      t="v2" ;
      s1="" ;
      s2=inflVerb x ;
      s3=[]
    } ;
lin InflectionV2A = \x -> {
      t="v2a" ;
      s1="" ;
      s2=inflVerb x ;
      s3=[]
    } ;
lin InflectionV2Q = \x -> {
      t="v2q" ;
      s1="" ;
      s2=inflVerb x ;
      s3=[]
    } ;
lin InflectionV2S = \x -> {
      t="v2s" ;
      s1="" ;
      s2=inflVerb x ;
      s3=[]
    } ;
lin InflectionV2V = \x -> {
      t="v2v" ;
      s1="" ;
      s2=inflVerb x ;
      s3=[]
    } ;
lin InflectionV3 = \x -> {
      t="v3" ;
      s1="" ;
      s2=inflVerb x ;
      s3=[]
    } ;
lin InflectionVA = \x -> {
      t="va" ;
      s1="" ;
      s2=inflVerb x ;
      s3=[]
    } ;
lin InflectionVQ = \x -> {
      t="vq" ;
      s1="" ;
      s2=inflVerb x ;
      s3=[]
    } ;
lin InflectionVS = \x -> {
      t="vs" ;
      s1="" ;
      s2=inflVerb x ;
      s3=[]
    } ;
lin InflectionVV = \x -> {
      t="vv" ;
      s1="" ;
      s2=inflVerb x ;
      s3=[]
    } ;

oper
  inflNoun : Noun -> Str = \x ->
      frameTable (
           tr (th "" ++ th "Sg" ++ th "Pl") ++
           tr (intagAttr "th" "colspan=\"3\"" "Indef") ++ 
           tr (th "nom/acc" ++ td (x.s ! Sg ! Indef ! ANomAcc) ++ td (x.s ! Pl ! Indef ! ANomAcc)) ++
           tr (th "gen/dat" ++ td (x.s ! Sg ! Indef ! AGenDat) ++ td (x.s ! Pl ! Indef ! AGenDat)) ++
           tr (th "voc"     ++ td (x.s ! Sg ! Indef ! AVoc)    ++ td (x.s ! Pl ! Indef ! AVoc)) ++
           tr (intagAttr "th" "colspan=\"3\"" "Def") ++
           tr (th "nom/acc" ++ td (x.s ! Sg ! Def ! ANomAcc) ++ td (x.s ! Pl ! Def ! ANomAcc)) ++
           tr (th "gen/dat" ++ td (x.s ! Sg ! Def ! AGenDat) ++ td (x.s ! Pl ! Def ! AGenDat)) ++
           tr (th "voc"     ++ td (x.s ! Sg ! Def ! AVoc)    ++ td (x.s ! Pl ! Def ! AVoc))) ;

  inflVerb : Verb -> Str = \x ->
      frameTable (
           tr (th "Inf" ++ td (x.s ! Inf)) ++
           tr (th "Indi Presn Sg P1" ++ td (x.s ! Indi Presn Sg P1)) ++
           tr (th "Indi Presn Sg P2" ++ td (x.s ! Indi Presn Sg P2)) ++
           tr (th "Indi Presn Sg P3" ++ td (x.s ! Indi Presn Sg P3)) ++
           tr (th "Indi Presn Pl P1" ++ td (x.s ! Indi Presn Pl P1)) ++
           tr (th "Indi Presn Pl P2" ++ td (x.s ! Indi Presn Pl P2)) ++
           tr (th "Indi Presn Pl P3" ++ td (x.s ! Indi Presn Pl P3)) ++
           tr (th "Indi Imparf Sg P1" ++ td (x.s ! Indi Imparf Sg P1)) ++
           tr (th "Indi Imparf Sg P2" ++ td (x.s ! Indi Imparf Sg P2)) ++
           tr (th "Indi Imparf Sg P3" ++ td (x.s ! Indi Imparf Sg P3)) ++
           tr (th "Indi Imparf Pl P1" ++ td (x.s ! Indi Imparf Pl P1)) ++
           tr (th "Indi Imparf Pl P2" ++ td (x.s ! Indi Imparf Pl P2)) ++
           tr (th "Indi Imparf Pl P3" ++ td (x.s ! Indi Imparf Pl P3)) ++
           tr (th "Indi PSimple Sg P1" ++ td (x.s ! Indi PSimple Sg P1)) ++
           tr (th "Indi PSimple Sg P2" ++ td (x.s ! Indi PSimple Sg P2)) ++
           tr (th "Indi PSimple Sg P3" ++ td (x.s ! Indi PSimple Sg P3)) ++
           tr (th "Indi PSimple Pl P1" ++ td (x.s ! Indi PSimple Pl P1)) ++
           tr (th "Indi PSimple Pl P2" ++ td (x.s ! Indi PSimple Pl P2)) ++
           tr (th "Indi PSimple Pl P3" ++ td (x.s ! Indi PSimple Pl P3)) ++
           tr (th "Indi PPerfect Sg P1" ++ td (x.s ! Indi PPerfect Sg P1)) ++
           tr (th "Indi PPerfect Sg P2" ++ td (x.s ! Indi PPerfect Sg P2)) ++
           tr (th "Indi PPerfect Sg P3" ++ td (x.s ! Indi PPerfect Sg P3)) ++
           tr (th "Indi PPerfect Pl P1" ++ td (x.s ! Indi PPerfect Pl P1)) ++
           tr (th "Indi PPerfect Pl P2" ++ td (x.s ! Indi PPerfect Pl P2)) ++
           tr (th "Indi PPerfect Pl P3" ++ td (x.s ! Indi PPerfect Pl P3)) ++
           tr (th "Subjo SPres Sg P1" ++ td (x.s ! Subjo SPres Sg P1)) ++
           tr (th "Subjo SPres Sg P2" ++ td (x.s ! Subjo SPres Sg P2)) ++
           tr (th "Subjo SPres Sg P3" ++ td (x.s ! Subjo SPres Sg P3)) ++
           tr (th "Subjo SPres Pl P1" ++ td (x.s ! Subjo SPres Pl P1)) ++
           tr (th "Subjo SPres Pl P2" ++ td (x.s ! Subjo SPres Pl P2)) ++
           tr (th "Subjo SPres Pl P3" ++ td (x.s ! Subjo SPres Pl P3)) ++
           tr (th "Imper SgP2" ++ td (x.s ! Imper SgP2)) ++
           tr (th "Imper PlP1" ++ td (x.s ! Imper PlP1)) ++
           tr (th "Imper PlP2" ++ td (x.s ! Imper PlP2)) ++
           tr (th "Ger" ++ td (x.s ! Ger)) ++
           tr (th "PPasse Masc Sg Indef ANomAcc" ++ td (x.s ! PPasse Masc Sg Indef ANomAcc)) ++
           tr (th "PPasse Masc Sg Indef AGenDat" ++ td (x.s ! PPasse Masc Sg Indef AGenDat)) ++
           tr (th "PPasse Masc Sg Indef AVoc" ++ td (x.s ! PPasse Masc Sg Indef AVoc)) ++
           tr (th "PPasse Masc Sg Def ANomAcc" ++ td (x.s ! PPasse Masc Sg Def ANomAcc)) ++
           tr (th "PPasse Masc Sg Def AGenDat" ++ td (x.s ! PPasse Masc Sg Def AGenDat)) ++
           tr (th "PPasse Masc Sg Def AVoc" ++ td (x.s ! PPasse Masc Sg Def AVoc)) ++
           tr (th "PPasse Masc Pl Indef ANomAcc" ++ td (x.s ! PPasse Masc Pl Indef ANomAcc)) ++
           tr (th "PPasse Masc Pl Indef AGenDat" ++ td (x.s ! PPasse Masc Pl Indef AGenDat)) ++
           tr (th "PPasse Masc Pl Indef AVoc" ++ td (x.s ! PPasse Masc Pl Indef AVoc)) ++
           tr (th "PPasse Masc Pl Def ANomAcc" ++ td (x.s ! PPasse Masc Pl Def ANomAcc)) ++
           tr (th "PPasse Masc Pl Def AGenDat" ++ td (x.s ! PPasse Masc Pl Def AGenDat)) ++
           tr (th "PPasse Masc Pl Def AVoc" ++ td (x.s ! PPasse Masc Pl Def AVoc)) ++
           tr (th "PPasse Fem Sg Indef ANomAcc" ++ td (x.s ! PPasse Fem Sg Indef ANomAcc)) ++
           tr (th "PPasse Fem Sg Indef AGenDat" ++ td (x.s ! PPasse Fem Sg Indef AGenDat)) ++
           tr (th "PPasse Fem Sg Indef AVoc" ++ td (x.s ! PPasse Fem Sg Indef AVoc)) ++
           tr (th "PPasse Fem Sg Def ANomAcc" ++ td (x.s ! PPasse Fem Sg Def ANomAcc)) ++
           tr (th "PPasse Fem Sg Def AGenDat" ++ td (x.s ! PPasse Fem Sg Def AGenDat)) ++
           tr (th "PPasse Fem Sg Def AVoc" ++ td (x.s ! PPasse Fem Sg Def AVoc)) ++
           tr (th "PPasse Fem Pl Indef ANomAcc" ++ td (x.s ! PPasse Fem Pl Indef ANomAcc)) ++
           tr (th "PPasse Fem Pl Indef AGenDat" ++ td (x.s ! PPasse Fem Pl Indef AGenDat)) ++
           tr (th "PPasse Fem Pl Indef AVoc" ++ td (x.s ! PPasse Fem Pl Indef AVoc)) ++
           tr (th "PPasse Fem Pl Def ANomAcc" ++ td (x.s ! PPasse Fem Pl Def ANomAcc)) ++
           tr (th "PPasse Fem Pl Def AGenDat" ++ td (x.s ! PPasse Fem Pl Def AGenDat)) ++
           tr (th "PPasse Fem Pl Def AVoc" ++ td (x.s ! PPasse Fem Pl Def AVoc)) ++
           tr (intagAttr "th" "rowspan=\"48\"" "isRefl" ++ intagAttr "th" "rowspan=\"4\"" "{g=Masc; n=Sg; p=P1}" ++ intagAttr "th" "rowspan=\"4\"" "s" ++ th "Normal" ++ td ((x.isRefl ! {g=Masc; n=Sg; p=P1}).s ! Normal)) ++
           tr (th "Composite" ++ td ((x.isRefl ! {g=Masc; n=Sg; p=P1}).s ! Composite)) ++
           tr (th "Short" ++ td ((x.isRefl ! {g=Masc; n=Sg; p=P1}).s ! Short)) ++
           tr (th "Imperative" ++ td ((x.isRefl ! {g=Masc; n=Sg; p=P1}).s ! Imperative)) ++
           tr (intagAttr "th" "rowspan=\"4\"" "{g=Masc; n=Sg; p=P2}" ++ intagAttr "th" "rowspan=\"4\"" "s" ++ th "Normal" ++ td ((x.isRefl ! {g=Masc; n=Sg; p=P2}).s ! Normal)) ++
           tr (th "Composite" ++ td ((x.isRefl ! {g=Masc; n=Sg; p=P2}).s ! Composite)) ++
           tr (th "Short" ++ td ((x.isRefl ! {g=Masc; n=Sg; p=P2}).s ! Short)) ++
           tr (th "Imperative" ++ td ((x.isRefl ! {g=Masc; n=Sg; p=P2}).s ! Imperative)) ++
           tr (intagAttr "th" "rowspan=\"4\"" "{g=Masc; n=Sg; p=P3}" ++ intagAttr "th" "rowspan=\"4\"" "s" ++ th "Normal" ++ td ((x.isRefl ! {g=Masc; n=Sg; p=P3}).s ! Normal)) ++
           tr (th "Composite" ++ td ((x.isRefl ! {g=Masc; n=Sg; p=P3}).s ! Composite)) ++
           tr (th "Short" ++ td ((x.isRefl ! {g=Masc; n=Sg; p=P3}).s ! Short)) ++
           tr (th "Imperative" ++ td ((x.isRefl ! {g=Masc; n=Sg; p=P3}).s ! Imperative)) ++
           tr (intagAttr "th" "rowspan=\"4\"" "{g=Masc; n=Pl; p=P1}" ++ intagAttr "th" "rowspan=\"4\"" "s" ++ th "Normal" ++ td ((x.isRefl ! {g=Masc; n=Pl; p=P1}).s ! Normal)) ++
           tr (th "Composite" ++ td ((x.isRefl ! {g=Masc; n=Pl; p=P1}).s ! Composite)) ++
           tr (th "Short" ++ td ((x.isRefl ! {g=Masc; n=Pl; p=P1}).s ! Short)) ++
           tr (th "Imperative" ++ td ((x.isRefl ! {g=Masc; n=Pl; p=P1}).s ! Imperative)) ++
           tr (intagAttr "th" "rowspan=\"4\"" "{g=Masc; n=Pl; p=P2}" ++ intagAttr "th" "rowspan=\"4\"" "s" ++ th "Normal" ++ td ((x.isRefl ! {g=Masc; n=Pl; p=P2}).s ! Normal)) ++
           tr (th "Composite" ++ td ((x.isRefl ! {g=Masc; n=Pl; p=P2}).s ! Composite)) ++
           tr (th "Short" ++ td ((x.isRefl ! {g=Masc; n=Pl; p=P2}).s ! Short)) ++
           tr (th "Imperative" ++ td ((x.isRefl ! {g=Masc; n=Pl; p=P2}).s ! Imperative)) ++
           tr (intagAttr "th" "rowspan=\"4\"" "{g=Masc; n=Pl; p=P3}" ++ intagAttr "th" "rowspan=\"4\"" "s" ++ th "Normal" ++ td ((x.isRefl ! {g=Masc; n=Pl; p=P3}).s ! Normal)) ++
           tr (th "Composite" ++ td ((x.isRefl ! {g=Masc; n=Pl; p=P3}).s ! Composite)) ++
           tr (th "Short" ++ td ((x.isRefl ! {g=Masc; n=Pl; p=P3}).s ! Short)) ++
           tr (th "Imperative" ++ td ((x.isRefl ! {g=Masc; n=Pl; p=P3}).s ! Imperative)) ++
           tr (intagAttr "th" "rowspan=\"4\"" "{g=Fem; n=Sg; p=P1}" ++ intagAttr "th" "rowspan=\"4\"" "s" ++ th "Normal" ++ td ((x.isRefl ! {g=Fem; n=Sg; p=P1}).s ! Normal)) ++
           tr (th "Composite" ++ td ((x.isRefl ! {g=Fem; n=Sg; p=P1}).s ! Composite)) ++
           tr (th "Short" ++ td ((x.isRefl ! {g=Fem; n=Sg; p=P1}).s ! Short)) ++
           tr (th "Imperative" ++ td ((x.isRefl ! {g=Fem; n=Sg; p=P1}).s ! Imperative)) ++
           tr (intagAttr "th" "rowspan=\"4\"" "{g=Fem; n=Sg; p=P2}" ++ intagAttr "th" "rowspan=\"4\"" "s" ++ th "Normal" ++ td ((x.isRefl ! {g=Fem; n=Sg; p=P2}).s ! Normal)) ++
           tr (th "Composite" ++ td ((x.isRefl ! {g=Fem; n=Sg; p=P2}).s ! Composite)) ++
           tr (th "Short" ++ td ((x.isRefl ! {g=Fem; n=Sg; p=P2}).s ! Short)) ++
           tr (th "Imperative" ++ td ((x.isRefl ! {g=Fem; n=Sg; p=P2}).s ! Imperative)) ++
           tr (intagAttr "th" "rowspan=\"4\"" "{g=Fem; n=Sg; p=P3}" ++ intagAttr "th" "rowspan=\"4\"" "s" ++ th "Normal" ++ td ((x.isRefl ! {g=Fem; n=Sg; p=P3}).s ! Normal)) ++
           tr (th "Composite" ++ td ((x.isRefl ! {g=Fem; n=Sg; p=P3}).s ! Composite)) ++
           tr (th "Short" ++ td ((x.isRefl ! {g=Fem; n=Sg; p=P3}).s ! Short)) ++
           tr (th "Imperative" ++ td ((x.isRefl ! {g=Fem; n=Sg; p=P3}).s ! Imperative)) ++
           tr (intagAttr "th" "rowspan=\"4\"" "{g=Fem; n=Pl; p=P1}" ++ intagAttr "th" "rowspan=\"4\"" "s" ++ th "Normal" ++ td ((x.isRefl ! {g=Fem; n=Pl; p=P1}).s ! Normal)) ++
           tr (th "Composite" ++ td ((x.isRefl ! {g=Fem; n=Pl; p=P1}).s ! Composite)) ++
           tr (th "Short" ++ td ((x.isRefl ! {g=Fem; n=Pl; p=P1}).s ! Short)) ++
           tr (th "Imperative" ++ td ((x.isRefl ! {g=Fem; n=Pl; p=P1}).s ! Imperative)) ++
           tr (intagAttr "th" "rowspan=\"4\"" "{g=Fem; n=Pl; p=P2}" ++ intagAttr "th" "rowspan=\"4\"" "s" ++ th "Normal" ++ td ((x.isRefl ! {g=Fem; n=Pl; p=P2}).s ! Normal)) ++
           tr (th "Composite" ++ td ((x.isRefl ! {g=Fem; n=Pl; p=P2}).s ! Composite)) ++
           tr (th "Short" ++ td ((x.isRefl ! {g=Fem; n=Pl; p=P2}).s ! Short)) ++
           tr (th "Imperative" ++ td ((x.isRefl ! {g=Fem; n=Pl; p=P2}).s ! Imperative)) ++
           tr (intagAttr "th" "rowspan=\"4\"" "{g=Fem; n=Pl; p=P3}" ++ intagAttr "th" "rowspan=\"4\"" "s" ++ th "Normal" ++ td ((x.isRefl ! {g=Fem; n=Pl; p=P3}).s ! Normal)) ++
           tr (th "Composite" ++ td ((x.isRefl ! {g=Fem; n=Pl; p=P3}).s ! Composite)) ++
           tr (th "Short" ++ td ((x.isRefl ! {g=Fem; n=Pl; p=P3}).s ! Short)) ++
           tr (th "Imperative" ++ td ((x.isRefl ! {g=Fem; n=Pl; p=P3}).s ! Imperative))) ;

lin
  NoDefinition   t     = {s=t.s};
  MkDefinition   t d   = {s="<p><b>Definition:</b>"++t.s++d.s++"</p>"};
  MkDefinitionEx t d e = {s="<p><b>Definition:</b>"++t.s++d.s++"</p><p><b>Example:</b>"++e.s++"</p>"};lin  MkDocument d i e = {s = i.s1 ++ d.s ++ i.s2 ++ i.s3 ++ e.s} ;  MkTag i = {s = i.t} ;
}
