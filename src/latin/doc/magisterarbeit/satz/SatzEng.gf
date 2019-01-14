concrete SatzEng of SatzAbs = {
  param Numerus = Sg | Pl ;
  lincat S = { s : Str } ;
	 NP = { s : Str ; n : Numerus } ;
	 VP = {s : Numerus => Str ; o : Str } ; 
	 Det = { s : Str ; n : Numerus } ;
	 N = { s : Numerus => Str } ;
	 V,V2 = { s: Numerus => Str };
  lin
    mkNP det noun = { s = det.s ++ noun.s ! det.n ; 
		      n = det.n } ;
    mkVP v = v ** { o = "" } ;
    mkVP2 v2 np = v2 ** { o = np.s } ;
    mkS np vp = { s = np.s ++ vp.s ! np.n ++ vp.o } ;
    defArtSg_Det = { s = "the" ; n = Sg } ;
    defArtPl_Det = { s = "the" ; n = Pl } ;
    Mann_N = { 
      s = table { Sg => "man" ; Pl => "men" } } ;
    Frau_N = { 
      s = table { Sg => "woman" ; Pl => "women" } } ;
    Buch_N = { 
      s = table { Sg => "book" ; Pl => "books" } } ;
    schlafen_V = { 
      s = table { Sg => "sleeps" ; Pl => "sleep" } } ;
    sehen_V2 = { 
      s = table { Sg => "sees" ; Pl => "see" } } ;
    lesen_V2 = { 
      s = table { Sg => "reads" ; Pl => "read" } } ;
}