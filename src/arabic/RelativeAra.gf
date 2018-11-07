concrete RelativeAra of Relative = CatAra ** open ResAra, SentenceAra in {
 flags coding=utf8;

 lin

   -- RelCl cl = {
   --   s = \\t,p,agr,c => IdRP.s ! agr2ragr agr c ++ cl.s ! t ! p ! Nominal
   --   } ;

   -- : RP -> VP -> RCl ;      -- who loves John
   RelVP rp vp = {
     s = \\t,p,agr,c => 
       let 
         npS : Case => Str = \\_ => rp.s ! agr2ragr agr c ;
         np = {s = npS ; a = agr} ;
         cl = PredVP np vp ;
       in
       cl.s ! t ! p ! Nominal
     } ;

    -- : RP -> ClSlash -> RCl ; -- whom John loves 
    -- TODO: add resumptive pronouns
--    RelSlash rp slash = {
--      } ;
--
--    FunRP p np rp = {
--      s = \\c => np.s ! c ++ p.s ++ rp.s ! Acc ;
--      a = RAg np.a
--      } ;

    IdRP =
     { s = table {
        RSg Masc => "اَلَّذِي" ;
        RSg Fem  => "اَلَّتِي" ;
        RPl Masc => "اَلَّذِين" ;
        RPl Fem  => "اَللَّاتِي" ;
        RDl Masc Bare => "اَللَّذَيْن" ;
        RDl Masc Nom  => "اَللَّذَانِ" ;
        RDl Masc _    => "اَللَّذَيْنِ" ;
        RDl Fem  Bare => "اَللَّتَيْن" ;
        RDl Fem  Nom  => "اَللَّتَانِ" ;
        RDl Fem  _    => "اَللَّتَيْنِ"
        } 
      } ;
}
