--# -path=.:alltenses:prelude
concrete RglSatzEng of RglSatzAbs = 
open SyntaxEng,ParadigmsEng in {
  flags coding=utf8;
  lincat
    S = SyntaxEng.S ;
    NP = SyntaxEng.NP ;
    VP = SyntaxEng.VP ;
    V = SyntaxEng.V ;
    V2 = SyntaxEng.V2 ;
    N = SyntaxEng.N ;
    Det = SyntaxEng.Det ;
  lin
    mkS np vp = SyntaxEng.mkS 
      presentTense simultaneousAnt positivePol 
      (mkCl np vp) ;
    mkNP det n = SyntaxEng.mkNP det n ;
    mkVP v = SyntaxEng.mkVP v ;
    mkVP2 v2 np = SyntaxEng.mkVP v2 np ;
    Mann_N = mkN "man" "men" ;
    Frau_N = mkN "woman" "woman" ;
    Buch_N = mkN "book" ;
    schlafen_V = mkV "sleep" ;
    sehen_V2 = mkV2 "see" ;
    lesen_V2 = mkV2 "read" ;
    defArtSg_Det = theSg_Det ;
    defArtPl_Det = thePl_Det ;
}