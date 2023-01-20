--# -path=.:../abstract:../common:../prelude

-- Adam Slaski, 2009 <adam.slaski@gmail.com>

concrete PhraseLit of Phrase = CatLit ** open Prelude, ResLit, MorphoLit in {

 lin
   PhrUtt pconj utt voc = {s = pconj.s ++ utt.s ++ voc.s} ;

   UttS s = s ;
   UttQS qs = {s = qs.s} ;
   UttImpSg  pol imp = {s = imp.s !pol.p ! Sg} ;
   UttImpPl  pol imp = {s = imp.s !pol.p ! Pl} ;
   UttImpPol pol imp = {s = imp.s !pol.p ! Sg} ;

   UttIP ip = {s = ip.nom};
   UttIAdv iadv = iadv ;
   UttNP np = {s = np.nom};
   UttVP vp = { -- I assume the positive polarization to avoid variants
        s = vp.preCompl !Pos !MascSg ++ vp.adv ++
            infinitiveForm vp.verb Pos MascSg ++ 
            vp.postCompl !Pos !MascSg 
   };
   UttAdv adv = adv ;
   UttCN n = {s = n.s ! Sg ! Nom};
   UttAP ap = {s = ap.s ! AF Sg Masc Nom} ;
   -- By chance...
   UttCard c = {s = c.s ! Nom ! SingPlur Masc } ;
   
   NoPConj = {s = []} ;
   PConjConj conj = {s = conj.s2} ; ---

   NoVoc = {s = []} ;
   VocNP np = {s = "," ++ np.voc} ;

}
