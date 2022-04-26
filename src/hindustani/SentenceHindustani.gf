--concrete SentenceUrd of Sentence = CatUrd ** open Prelude, StringsHindustani, ResUrd in {
incomplete concrete SentenceHindustani of Sentence = 
  CatHindustani ** open CommonHindustani, ResHindustani, Prelude in {

  flags optimize=all_subs ;
  coding = utf8;

  lin

    PredVP np vp = mkClause np vp ;

    PredSCVP sc vp = mkSClause sc.s (defaultAgr) vp ;

    ImpVP vp = {
      s = \\pol,n => 
        let 
          agr   = Ag Masc (numImp n) Pers2_Casual ;
          verb  = infVP True vp agr ;
          dont  = case pol of {
            CNeg True => mt ;
            CNeg False => nh ;
            _ => []
            }
        in
        dont ++ verb
    } ;

    SlashVP np vp = 
      mkClause np vp ** {c2 = vp.c2} ;

    AdvSlash slash adv = {
	  s  = \\t,p,o =>  adv.s ! Masc ++ slash.s ! t ! p ! o  ;
      c2 = slash.c2
    } ;

    SlashPrep cl prep = cl **  {c2 = { s = prep.s ! Masc ; c = VIntrans}} ;

    SlashVS np vs slash = 
      mkClause  np 
        (insertObj2 (conjThat ++ slash.s) (predV vs))  **
        {c2 = slash.c2} ;

    EmbedS  s  = {s = conjThat ++ s.s ; fromVP = False} ;
    EmbedQS qs = {s = conjThat ++ qs.s ! QIndir ; fromVP = False} ;
--    EmbedVP vp = {s = infVP False vp defaultAgr ; fromVP = True} ; --- agr
    EmbedVP vp = {s = (vp.s ! VPInf).fin ; fromVP = True} ;

    UseCl  temp p cl = 
	 { s = case <temp.t,temp.a> of {
	      <Pres,Simul> => temp.s ++ p.s ++ cl.s ! VPGenPres ! p.p ! ODir;
          <Pres,Anter> => temp.s ++ p.s ++ cl.s ! VPPerfPres ! p.p ! ODir;
          <Past,Simul> => temp.s ++ p.s ++ cl.s ! VPImpPast ! p.p ! ODir;
          <Past,Anter> => temp.s ++ p.s ++ cl.s ! VPPerfPast ! p.p ! ODir;
          <Fut,Simul>  => temp.s ++ p.s ++ cl.s ! VPFut ! p.p ! ODir;
          <Fut,Anter>  => temp.s ++ p.s ++ cl.s ! VPPerfFut ! p.p ! ODir;
          <Cond,Simul> => temp.s ++ p.s ++ cl.s ! VPFut ! p.p ! ODir;  --cl.s ! VPSubj ! p.p ! ODir;
          <Cond,Anter> => temp.s ++ p.s ++ cl.s ! VPSubj ! p.p ! ODir -- this needs to be fixed by making SubjPerf in ResUrd		  

   };
  } ;
    UseQCl temp p cl = {
      s = \\q => case <temp.t,temp.a> of {
	      <Pres,Simul> => temp.s ++ p.s ++ cl.s ! VPGenPres ! p.p ! q;
          <Pres,Anter> => temp.s ++ p.s ++ cl.s ! VPPerfPres ! p.p ! q;
          <Past,Simul> => temp.s ++ p.s ++ cl.s ! VPImpPast ! p.p ! q;
          <Past,Anter> => temp.s ++ p.s ++ cl.s ! VPPerfPast ! p.p ! q;
          <Fut,Simul>  => temp.s ++ p.s ++ cl.s ! VPFut ! p.p ! q;
          <Fut,Anter>  => temp.s ++ p.s ++ cl.s ! VPPerfFut ! p.p ! q;
          <Cond,Simul> => temp.s ++ p.s ++ cl.s ! VPFut ! p.p ! q; -- cl.s ! VPSubj ! p.p ! q;
          <Cond,Anter> => temp.s ++ p.s ++ cl.s ! VPSubj ! p.p ! q		  
 
   };
  } ;
    UseRCl temp p rcl = {
      s = \\q => case <temp.t,temp.a> of {
	      <Pres,Simul> => temp.s ++ p.s ++ rcl.s ! VPGenPres ! p.p ! ODir ! q;
          <Pres,Anter> => temp.s ++ p.s ++ rcl.s ! VPPerfPres ! p.p ! ODir ! q;
          <Past,Simul> => temp.s ++ p.s ++ rcl.s ! VPImpPast ! p.p ! ODir ! q;
          <Past,Anter> => temp.s ++ p.s ++ rcl.s ! VPPerfPast ! p.p ! ODir ! q;
          <Fut,Simul>  => temp.s ++ p.s ++ rcl.s ! VPFut ! p.p ! ODir ! q;
          <Fut,Anter>  => temp.s ++ p.s ++ rcl.s ! VPPerfFut ! p.p ! ODir ! q;
          <Cond,Simul> => temp.s ++ p.s ++ rcl.s ! VPFut ! p.p ! ODir ! q; --rcl.s ! VPSubj ! p.p ! ODir ! q;
          <Cond,Anter> => temp.s ++ p.s ++ rcl.s ! VPSubj ! p.p ! ODir ! q
     };		  
      c = rcl.c
    } ;
    UseSlash temp p clslash = {
      s = case <temp.t,temp.a> of {
	      <Pres,Simul> => temp.s ++ p.s ++ clslash.s ! VPGenPres ! p.p ! ODir;
          <Pres,Anter> => temp.s ++ p.s ++ clslash.s ! VPPerfPres ! p.p ! ODir;
          <Past,Simul> => temp.s ++ p.s ++ clslash.s ! VPImpPast ! p.p ! ODir ;
          <Past,Anter> => temp.s ++ p.s ++ clslash.s ! VPPerfPast ! p.p ! ODir;
          <Fut,Simul>  => temp.s ++ p.s ++ clslash.s ! VPFut ! p.p ! ODir;
          <Fut,Anter>  => temp.s ++ p.s ++ clslash.s ! VPPerfFut ! p.p ! ODir;
          <Cond,Simul> => temp.s ++ p.s ++ clslash.s ! VPFut ! p.p ! ODir; --clslash.s ! VPSubj ! p.p ! ODir;
          <Cond,Anter> => temp.s ++ p.s ++ clslash.s ! VPSubj ! p.p ! ODir
     };		  
      c2 = clslash.c2
    } ;

    AdvS a s = {s = a.s ! Masc ++ s.s} ;
    ExtAdvS a s = {s = a.s ! Masc ++ "," ++ s.s} ;

    RelS s r = {s = s.s ++ r.s ! agrP3 Masc Sg} ;
    SSubjS s sj s = { s = s.s ++ sj.s ++ s.s};

}
