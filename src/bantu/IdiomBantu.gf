 incomplete concrete IdiomBantu of Idiom = CatBantu **
  open Prelude, CommonBantu, ResBantu  in {

  flags optimize=all_subs ;

  lin
  ImpersCl vp = { s=\\pol,tense,anter =>"" ++ vp.s!Ag G1 Sg P3!pol!tense!anter};
   -- GenericCl vp = "one" (agrP3 Sg) vp ;
 CleftNP np rs = { s=\\pol,tense,anter => np.s!NCase Nom ++ auxBe.s!np.a!pol!tense!anter ++ rs.s ! np.a};      
 ExistNP np =  { s=\\pol,tense,anter => kuna  ++ np.s!NCase Nom };
 ExistNPAdv np adv =  { s=\\pol,tense,anter => np.s!NCase Nom ++ adv.s!np.a};
 CleftAdv ad s = { s=\\pol,tense,anter => s.s ++ ad.s!Ag G1 Sg P3};
 --  ExistIP ip =  mkQuestion (ss (ip.s ! npNom))    (mkClause "there" (agrP3 ip.n) (predAux auxBe)) ;
 -- ExistIPAdv ip adv =       mkQuestion (ss (ip.s ! npNom))         (mkClause "there" (agrP3 ip.n) (insertObj (\\_ => adv.s) (predAux auxBe))) ;
 ProgrVP vp = {s=\\agr,pol,tense,anter=>auxProgBe.s !agr!pol!tense!anter ++ vp.s1!agr!pol!tense!anter ;
                   compl=\\a => vp.compl!a;
                   s1=\\_,_,_,_=> []; imp =\\po,n =>vp.imp!po!n;inf=vp.inf};
 SelfNP np = { s = \\c => np.s ! c ++ reflPron ! np.a ; 
      a = np.a ; isPron= np.isPron      } ;
 SelfAdvVP vp = insertObj reflPron vp ;
 ImpPl1 vp = {s =  let_s ++ cBind++  vp.imp!Pos!ImpF Sg True    ++ vp.compl!Ag G1 Pl P1} ;
 ImpP3 np vp = {s = lets ++ np.s ! NCase Nom ++
   subjectmarker np.a ++ cBind ++  vp.imp!Pos!ImpF Sg True } ;--see how to init
   -- SelfAdVVP vp = insertAdVAgr reflPron vp ;
    {-
    SelfAdvVP vp = insertObj reflPron vp ;
    SelfAdVVP vp = insertAdVAgr reflPron vp ;
 } -}
 }

