incomplete concrete VerbBantu of Verb = 
  CatBantu ** open Prelude, CommonBantu, ResBantu in {

  flags optimize=all_subs ;
  lin
     UseV verb = regVP verb ;
     ComplVV vv vp = { s =\\ag , pol,tes,ant => (polanttense.s!Pos!tes!Simul! ag).p1 ++ cbind ++vv.s!VGen ++ vp.inf ;
     --vp.s1!ag!Pos!tes!Simul                         
                    compl=\\a=> vp.compl!a ;
                    imp =\\po,imf => vp.imp!po!imf;
                     progV=vp.progV;
                      inf= vp.inf};
     CompAdv adv ={s= adv.s};
     AdVVP adv vp = insertAdV adv.s vp ;
     AdvVP vp adv = insertObj (\\agr => adv.s!agr) vp;
     UseComp comp =  { s = \\agr , pol , tense , anter =>
      auxBe.s!agr !pol!tense!anter ++  comp.s!agr;  compl=\\_=> [] ;progV=[]; imp =\\po,imf => "";inf= ""};
     SlashV2a v =  mkVPSlash v.c2 (regVP v)** {n3 = \\_ => [] ;c2 = v.c2 } ;
     Slash2V3 v np = insertObjc (
      \\agr=> v.c2.s!(nounAgr agr).n !(nounAgr agr).g ++ np.s ! NCase Nom  )
       (regVP v ** {c2 = v.c3 ;isFused = False}) ;
     Slash3V3 v np = insertObjc (
     \\agr => v.c3.s!(nounAgr agr).n !(nounAgr agr).g  ++ np.s ! NCase Nom) 
     (regVP2 v) ;-- ** {c2 = v.c2 } ) ;
     ComplSlash vp np = insertObj    (\\a => vp.c2.s! (nounAgr a).n! (nounAgr a).g ++ np.s!NCase Nom ) vp;
    -- ComplSlash vp np =  insertObjNP vp.c2 np (insertComplement (\\a => vp.c2.s! (nounAgr a).n! (nounAgr a).g ++ vp.compl ! np.a   ) vp )  ;
     VPSlashPrep vp p = vp ** {c2 = p ;  isFused=p.isFused };
     AdvVPSlash vp adv = insertObj (\\agr => adv.s!agr) vp ** {c2 = vp.c2} ;
     AdVVPSlash adv vp = insertAdV adv.s vp ** {c2 = vp.c2} ;
     PassV2 v ={s=\\agr,pol,tense,anter=> (polanttense.s!pol!tense!anter! agr).p1 ++cbind ++ v.s!VExtension  EPassive  ;
                   compl=\\a => [];
                    imp =\\po,imf => ""; progV=v.progV;inf=  v.s!VInf};
     CompAP ap =  {s=\\agr =>ap.s! (nounAgr agr).g ! (nounAgr agr).n} ;
     CompNP np =  {s = \\_ => np.s ! NCase Nom } ;
     CompCN cn =  {s = \\a => case (nounAgr a).n of { n => cn.s ! n! Nom ++cn.s2!n }}; 
     UseCopula =  auxBe ; 
     ComplVA v ap = { s=\\agr,pol,tense,anter=>   (polanttense.s!pol!tense!anter! agr).p1 ++ v.s!form ++ ap.s! (nounAgr agr).g ! (nounAgr agr).n  ; -- may not work for all
                   compl=\\a => [];
                  imp =\\po,imf => "";progV=v.progV; inf=  v.s!VInf};
    ReflVP vpslash = insertObjPre (\\agr => vpslash.compl !agr ++ reflPron !Ag (nounAgr agr).g  (nounAgr agr).n  (nounAgr agr).p) vpslash ;
    ComplVS v s  = insertObj ( \\_ =>s.s) (regVP v) ; 
   -- SlashVV vv vp =   insertObj (\\_ => vp.inf) (regVP2 vv)**{c2 = vv.c2; isFused=False} ;
  oper cbind : Str = Predef.BIND ;
}

