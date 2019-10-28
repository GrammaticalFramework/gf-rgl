incomplete concrete RelativeBantu of Relative = 
  CatBantu ** open Structural, ResBantu,Prelude in {

  flags optimize=all_subs ;

  lin
 
    RelCl cl = { s = \\ p,t,a => such ++ that ++ cl.s! p ! t ! a };
    
   RelVP rp vp = let  agr = nounAgr rp.a in
   {s=\\p,t,a => rp.s!agr.g!agr.n ++ vp.s!Ag agr.g agr.n agr.p !p!t!a ++ vp.compl!Ag agr.g agr.n agr.p};


    RelSlash rp slash = let  agr = nounAgr rp.a in
   {s=\\p,t,a => rp.s!agr.g!agr.n --++ slash.s2!Ag agr.g agr.n agr.p
   ++ slash.s!p!t!a }; 


  -- FunRP : Prep -> NP -> RP -> RP ; the mother of whom
    FunRP p np rp =let  agr = nounAgr np.a in
     {s = \\g,n => np.s !NCase Nom ++ p.s!n!g ++ rp.s ! g ! n;
      a= np.a } ;
    -- IdRP  : RP ;  -- which 
    IdRP = { s =\\g,n => which_IQuant.s!n!g; a=Ag G1 Sg P3};

}
 
