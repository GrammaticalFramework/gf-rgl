concrete NamesBul of Names = CatBul ** open ResBul in {

lin GivenName = \n -> {
       s  = table { RObj c => linCase c Pos ++ n.s;
                    _      => n.s
                  } ;
       gn = GSg (sex2gender n.g) ;
       p = NounP3 Pos
    } ;
lin MaleSurname = \n -> {
       s  = table { RObj c => linCase c Pos ++ n.s ! Male;
                    _      => n.s ! Male
                  } ;
       gn = GSg Masc ;
       p = NounP3 Pos
    } ;
lin FemaleSurname = \n -> {
       s  = table { RObj c => linCase c Pos ++ n.s ! Female;
                    _      => n.s ! Female
                  } ;
       gn = GSg Fem ;
       p = NounP3 Pos
    } ;
lin PlSurname = \n -> {
       s  = table { RObj c => linCase c Pos ++ n.pl ;
                    _      => n.pl
                  } ;
       gn = GPl ;
       p = NounP3 Pos
    } ;
lin FullName gn sn = {
       s  = table { RObj c => linCase c Pos ++ gn.s ++ sn.s ! gn.g ;
                    _      => gn.s ++ sn.s ! gn.g
                  } ;
       gn = GSg (sex2gender gn.g) ;
       p = NounP3 Pos
    } ;

}
