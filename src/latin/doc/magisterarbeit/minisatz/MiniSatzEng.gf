concrete MiniSatzEng of MiniSatzAbs = {
  lincat S,NP,VP,Det,N,V = Str;
  lin
    mkNP det n = det ++ n ;
    mkVP v = v ;           
    mkS np vp = np ++ vp ;
    der_Det = "the" ;     
    Mann_N = "man" ;
    schlafen_V = "sleeps" ;
}