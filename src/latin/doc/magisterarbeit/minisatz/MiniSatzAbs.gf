abstract MiniSatzAbs = {
  flags startcat = S ;
  cat S ; NP ; VP ; Det ; N ; V ;
  fun
    mkNP : Det -> N -> NP ;
    mkVP : V -> VP ;
    mkS : NP -> VP -> S ;
    der_Det : Det ;
    Mann_N : N ;
    schlafen_V : V ;
}