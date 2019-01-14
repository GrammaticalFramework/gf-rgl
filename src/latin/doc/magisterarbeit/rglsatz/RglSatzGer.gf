--# -path=.:alltenses:prelude
concrete RglSatzGer of RglSatzAbs = 
open SyntaxGer, ParadigmsGer in {
  flags coding=utf8;
  lincat
    S = SyntaxGer.S ;
    NP = SyntaxGer.NP ;
    VP = SyntaxGer.VP ;
    V = SyntaxGer.V ;
    V2 = SyntaxGer.V2 ;
    N = SyntaxGer.N ;
    Det = SyntaxGer.Det ;
  lin
    mkS np vp = SyntaxGer.mkS 
      presentTense simultaneousAnt positivePol 
      (mkCl np vp) ;
    mkNP det n = SyntaxGer.mkNP det n ;
    mkVP v = SyntaxGer.mkVP v ;
    mkVP2 v2 np = SyntaxGer.mkVP v2 np ;
    Mann_N = mkN "Mann" "Männer" masculine ;
    Frau_N = mkN "Frau" "Frauen" feminine ;
    Buch_N = mkN "Buch" "Bücher" neuter;
    schlafen_V = 
      mkV "schlafen" "schläft" "schlief" 
        "schliefe" "geschlafen" ;
    sehen_V2 = 
      mkV2 ( mkV "sehen" "sieht" "sah" "sähe" "gesehen" ) ;
    lesen_V2 = 
      mkV2 ( mkV "lesen" "liest" "las" "läse" "gelesen" ) ;
    defArtSg_Det = theSg_Det ;
    defArtPl_Det = thePl_Det ;
}