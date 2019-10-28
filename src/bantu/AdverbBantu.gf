incomplete concrete AdverbBantu of Adverb = 
  CatBantu ** open CommonBantu, ResBantu, Prelude in {

  lin
  --PositAdvAdj a =  {s = table{Ag g n p => a.s! Advv }}; 
  ComparAdvAdj cadv a np = let agr = complAgr np.a
    in{
    s = table {Ag g n p=>  a.s !AAdj g n ++ cadv.s ++ np.s ! npNom }
           } ;
  ComparAdvAdjS cadv a s = {
       s = table{Ag g n p =>   a.s! AAdj g n  ++ cadv.s ++ s.s} 
      } ;
  PrepNP prep np = let agr = complAgr np.a
    in {s =table{Ag g n p =>case prep.isFused of {
     True =>(np.s ! NCase Loc);
     False => prep.s!n!g ++ (np.s ! NCase Nom) }}} ; 
  AdAdv sub se =  { s=\\agr => se.s!agr ++ sub.s  } ;
  SubjS sub se =  { s=\\agr => sub.s ++ se.s} ;
  AdnCAdv cadv = {s = cadv.s ++ cadv.p} ;
}
