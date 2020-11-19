concrete RelativeAra of Relative = CatAra **
  open Prelude,ResAra in {
 flags coding=utf8;

 lin

   RelCl cl = {
     s = \\t,p,agr,c => IdRP.s ! agr2ragr agr c ++ cl.s ! t ! p ! Nominal
     } ;

   -- : RP -> VP -> RCl ;      -- who loves John
   RelVP rp vp = {
     s = \\t,p,agr,c =>
       let
         npS : Case => Str = \\_ => rp.s ! agr2ragr agr c ;
         np : ResAra.NP = agrNP {pgn=agr;isPron=False} ** {s = npS} ;
         cl = predVP np vp ;
       in
       cl.s ! t ! p ! Nominal
     } ;

    -- : RP -> ClSlash -> RCl ; -- whom John loves
   RelSlash rp cls = cls ** {
     s = \\t,p,agr,c =>
        let --empty : Agr -> NP = emptyNP ;
            obj : ResAra.NP = pgn2pron agr ; -- head is repeated as a clitic object pronoun
            cl : ResAra.Cl = complClSlash obj cls ;
        in rp.s ! agr2ragr agr c ++ cl.s ! t ! p ! VOS
     } ;
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
