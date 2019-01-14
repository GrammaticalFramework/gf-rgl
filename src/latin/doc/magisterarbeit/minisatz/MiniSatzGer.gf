concrete MiniSatzGer of MiniSatzAbs = {
  flags coding=utf8;
  lincat S,NP,VP,Det,N,V = Str;
  lin
    mkNP det n = det ++ n ;
    mkVP v = v ;           
    mkS np vp = np ++ vp ;
    der_Det = "der" ;     
    Mann_N = "Mann" ;
    schlafen_V = "schläft" ;
}