concrete SatzGer of SatzAbs = {
  param Genus = Mask | Fem | Neutr ;
	Numerus = Sg | Pl ;
	Casus = Nom | Akk ;
  flags coding=utf8;
  lincat S = { s : Str } ;
	 Det = { s : Genus => Casus => Str ; n : Numerus };
	 N = { s : Numerus => Str ; g : Genus } ;
	 NP = { s : Casus => Str ; n : Numerus} ;
	 V,V2 = { s : Numerus => Str } ;
	 VP = { s : Numerus => Str ; o : Str };
  lin
    mkNP det noun = 
      { s = \\cas => det.s ! noun.g ! cas ++ noun.s ! det.n ; 
		      n = det.n } ;
    mkVP v = v ** { o = "" } ;
    mkVP2 v2 np = v2 ** { o = np.s ! Akk } ; 
    mkS np vp = { s = np.s ! Nom ++ vp.s ! np.n ++ vp.o } ;
    defArtSg_Det = { s = table { Mask => table { 
				   Nom => "der" ; 
				   Akk => "den"
				   } ;
				 Fem => table { 
				   Nom | Akk => "die" 
				   } ;
				 Neutr => table { 
				   Nom | Akk => "das" 
				   }
		       } ; 
		     n = Sg 
      } ;
    defArtPl_Det = { s = \\_,_ => "die" ; n = Pl } ;
    Mann_N = { s = table { Sg => "Mann" ; Pl => "Männer" } ; 
	       g = Mask 
      } ;
    Frau_N = { s = table { Sg => "Frau" ; Pl => "Frauen" } ; 
	       g = Fem 
      } ;
    Buch_N = { s = table { Sg => "Buch" ; Pl => "Bücher" } ; 
	       g = Neutr 
      } ; 
    schlafen_V = { 
      s = table { Sg => "schläft" ; Pl => "schlafen" } 
      };
    sehen_V2 = { 
      s = table { Sg => "sieht" ; Pl => "sehen" } 
      };
    lesen_V2 = { 
      s = table { Sg => "liest" ; Pl => "lesen" } 
      };
}