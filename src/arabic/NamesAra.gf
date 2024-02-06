concrete NamesAra of Names = CatAra ** open ResAra, Prelude, (N=NounAra), (A=AdverbAra), (S=StructuralAra) in { 

lin GivenName, MaleSurname, FemaleSurname, PlSurname = \n -> emptyNP ** {
      s = n.s ;
      a = {pgn = Per3 n.g Sg ; isPron = False} ;
      } ;
lin FullName gn sn = emptyNP ** {
      s = \\c => gn.s ! c ++ sn.s ! c ;
      a = {pgn = Per3 gn.g Sg ; isPron = False} ;
      } ;

lin UseLN pn = emptyNP ** {
      s = pn.s ;
      a = {pgn = Per3 pn.g Sg ; isPron = False} ;
      } ;

lin InLN n = A.PrepNP S.in_Prep (N.UsePN n) ; ---- TODO: alternative prepositions

}