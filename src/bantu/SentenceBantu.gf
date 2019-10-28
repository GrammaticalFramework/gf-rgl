incomplete concrete SentenceBantu of Sentence = 
  CatBantu ** open Prelude, CommonBantu, ResBantu in {

  flags optimize=all_subs ;
    coding=utf8 ;

  lin
    PredVP np vp =  let  agr = nounAgr np.a in{s=\\pol,tense,anter =>let
                  verb: Str =  vp.s!Ag agr.g agr.n agr.p !pol!tense!anter;
                obj : Str =  vp.compl !Ag agr.g agr.n agr.p ; in  case np.isPron of
                {  True => verb ++ obj ;
                  False=> np.s!npNom ++ verb ++ obj }} ;
 
   PredSCVP sc vp= { s=\\pol,tense,anter => sc.s ++ vp.s!Ag G1 Sg P3 !pol!tense!anter };

   UseCl temp pol cl = {
     s = temp.s ++ pol.s ++ cl.s !pol.p ! temp.t ! temp.a
    } ;
   UseRCl t pol cl = {s =\\ag => t.s ++ pol.s ++ cl.s !pol.p ! t.t ! t.a} ;
    
   SlashPrep cl prep = cl ** {c2 = prep.s} ;
   SSubjS a s b = {s = a.s ++ frontComma ++ s.s ++ b.s} ;
   AdvS a s = {s = a.s!Ag G1 Sg P3 ++ s.s} ;
 --EmbedS  s  = {s = that ++ s.s} ;
   EmbedQS qs = {s = qs.s ! QIndir};
    RelS s r = {s = s.s ++ frontComma ++ r.s!Ag G1 Sg P3 } ;
    SlashVP np vp = { s=\\pol,tense,anter =>np.s!npNom ++ vp.s!np.a !pol!tense!anter};
  ExtAdvS a s = {s = a.s!Ag G1 Sg P3 ++ frontComma ++ s.s} ;
  UseQCl t p cl = {  s = \\q => t.s ++ p.s ++ cl.s!p.p ! t.t ! t.a!q  } ;
  EmbedVP vp = { s=vp.inf}; 
 ImpVP vp = { s = \\pol,iform => vp.imp!pol! ImpF (getNum iform) (getbool iform)
   ++ vp.compl!Ag G1 (getNum iform) P2};
 {-         

    AdvSlash slash adv = {
      s  = \\t,a,b,o => slash.s ! t ! a ! b ! o ++ adv.s ;
      c2 = slash.c2
    } ;


    SlashVS np vs slash = 
      mkClause (np.s ! npNom) np.a 
        (insertObj (\\_ => conjThat ++ slash.s) (predV vs))  **
        {c2 = slash.c2} ;

      
    UseSlash t p cl = {
      s = t.s ++ p.s ++ cl.s ! t.t ! t.a ! ctr p.p  ! oDir ;
      c2 = cl.c2
    } ;       
-}

}
