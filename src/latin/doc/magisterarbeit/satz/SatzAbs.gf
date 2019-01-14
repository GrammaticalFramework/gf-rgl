abstract SatzAbs = {
  flags startcat = S ;
  cat S ; NP ; VP ; Det ; N ; V ; V2 ;
  fun
    mkNP : Det -> N -> NP ;
    mkVP : V -> VP ;
    mkVP2 : V2 -> NP -> VP ;
    mkS : NP -> VP -> S ;
    defArtSg_Det : Det ;
    defArtPl_Det : Det ;
    Mann_N : N ;
    Frau_N : N ;
    Buch_N : N ;
    schlafen_V : V ;
    sehen_V2 : V2 ;
    lesen_V2 : V2 ;
}