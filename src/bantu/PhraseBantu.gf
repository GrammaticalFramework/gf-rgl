incomplete concrete PhraseBantu of Phrase = 
  CatBantu ** open CommonBantu, ResBantu, Prelude in { 

  flags optimize = all_subs ;

  lin
    PhrUtt pconj utt voc = {s = pconj.s ++ utt.s ++ voc.s} ;
    UttS s = {s = s.s } ;
    UttNP np = {s = np.s ! NCase Nom} ;
    NoPConj = {s = []} ;
    NoVoc = {s = []} ;
    UttIAdv iadv = iadv ;
    UttIP ip = {s = ip.s } ;
    UttImpSg pol imp = {s = pol.s ++ imp.s ! pol.p ! ImpF Sg False} ;
    UttImpPl pol imp = {s = pol.s ++ imp.s ! pol.p ! ImpF Pl False} ;
    UttImpPol pol imp ={s = pol.s ++ imp.s ! pol.p ! ImpF Sg True };
    UttAP ap = {s = ap.s !G1 !Sg} ;
    UttAdv adv = {s= adv.s!Ag G1 Sg P3 };
    UttInterj i = i ;
    PConjConj conj = {s = conj.s2} ;
    UttCN n = {s = n.s ! Sg !Nom}; 
    VocNP np = {s = "," ++ np.s !NCase Nom} ;
    UttCard n = {s = n.s ! G1} ;
    UttVP vp = {s = vp.inf};
    UttQS qs = {s = qs.s ! QDir} ;

  
    } 
    
   
    

    
    

    
    

