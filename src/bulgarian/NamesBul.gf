concrete NamesBul of Names = CatBul ** open ResBul, Prelude in {

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

lin UseLN = \n -> {
      s = table { RSubj   => n.defNom ;
                  RObj c  => linCase c Pos ++ n.s ! Def ;
                  RVoc    => n.s ! Indef
                } ;
      gn = n.gn ;
      p = NounP3 Pos
    } ;

lin PlainLN = \n -> {
      s = table { RObj c => linCase c Pos ++ n.s ! Indef ;
                  _      => n.s ! Indef
                } ;
      gn = n.gn ;
      p = NounP3 Pos
    } ;

    InLN n = {
      s = case n.onPrep of {
            True  => linCase Dat Pos ;
            False => vyv_Str
          } ++
          n.s ! Def
      } ;

    AdjLN ap n = n ** {
      s = \\sp => case <ap.isPre,n.hasArt> of {
                    <True,True > => ap.s ! aform n.gn sp    RSubj ! P3 ++ n.s ! Indef ;
                    <True,False> => ap.s ! aform n.gn Indef RSubj ! P3 ++ n.s ! Indef ;
                    <False,_>    => n.s ! sp ++ ap.s ! aform n.gn Indef RSubj ! P3
                  } ;
      defNom =    case <ap.isPre,n.hasArt> of {
                    <True,True > => ap.s ! ASgMascDefNom          ! P3 ++ n.s ! Indef ;
                    <True,False> => ap.s ! aform n.gn Indef RSubj ! P3 ++ n.s ! Indef ;
                    <False,_>    => n.defNom ++ ap.s ! aform n.gn Indef RSubj ! P3
                  } ;
      } ;

}
