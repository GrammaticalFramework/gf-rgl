incomplete concrete QuestionBantu of Question = 
  CatBantu ** open CommonBantu, ResBantu, Prelude in {

  flags optimize=all_subs ;

  lin
   CompIAdv a = a ;
   CompIP ip = {s =  ip.s } ;
   IdetQuant idet num = let n = num.n   in {
          s = \\g => idet.s!n ! g ++ num.s !g    ; 
          n = n
        } ;
    IdetCN idet cn = {
            s =    cn.s ! idet.n !Nom  ++idet.s ! cn.g  ;
            n = idet.n } ;
    PrepIP p ip = {
      s = p.s!ip.n !G1 ++ ip.s 
      } ;
     -- AdvIAdv i a = {s = i.s ++ a.s} ;
      AdvIP ip adv = {
      s =  ip.s  ++ adv.s! Ag G1 ip.n P3 ;
      n= ip.n
      } ;

      QuestCl cl = {
      s = \\t,a,p =>
        let cls = cl.s ! t ! a ! p
        in table {
          QDir   => dQue ++cls  ;
          QIndir => inQue ++ cls
        }
      } ;
       QuestIAdv iadv cl = mkQuestion iadv cl ;
      QuestVP qp vp = {  s = \\t,a,b,_ => qp .s ++  vp.s!Ag G1 qp.n P3!t!a!b};
      
       QuestSlash ip slash ={  s = \\t,a,b,_ => ip .s ++  slash.s!t!a!b};  -- check the capability of string 
{-
   

    QuestVP qp vp = {
      s = \\t,a,b,_ => 
        let
          cl = mkClause (qp.s ! Nom) False False (agrP3 qp.a.g qp.a.n) vp  
        in
        cl.s ! DDir ! t ! a ! b ! Indic
      } ;   

    QuestSlash ip slash = {
      s = \\t,a,p => 
            let 
              cls : Direct -> Str = 
                    \d -> slash.s ! ip.a ! d ! t ! a ! p ! Indic ;
              who = slash.c2.s ++ ip.s ! slash.c2.c
            in table {
              QDir   => who ++ cls DInv ;
              QIndir => who ++ cls DDir
              }
      } ;

    QuestIAdv iadv cl = {iadv.s
      s = \\t,a,p => 
            let 
              ord = case q of {
                QDir   => iAdvQuestionInv ;
                QIndir => iAdvQuestionInv 
              } ;
              cls = cl.s ! ord ! t ! a ! p ! Indic ;
              why = iadv.s
            in why ++ cls
      } ;

    QuestIComp icomp np = {
      s = \\t,a,p,_ => 
            let 
              vp  = predV (selectCopula icomp.cop) ;
              cls = (mkClause (np.s ! Nom).comp np.hasClit np.isPol np.a vp).s ! 
                       DInv ! t ! a ! p ! Indic ;
              why = icomp.s ! complAgr np.a ;
            in why ++ cls
      } ;

    IdetIP idet = 
      let 
        g = G1 ; ---- Fem in Extra
        n = idet.n ;
           in {
      s =  idet.s ! g  ; 
      n = idet.n
      } ;

    IdetQuant idet num = 
      let 
        n = num.n ;
      in {
      s = \\g,c => idet.s ! n ! g ! c ++ num.s ! g ;
      n = n
      } ;

    

    CompIAdv a = {s =  a.s } ;

    CompIP p = {s =  p.s ! Sg!G1  ; n =p.n} ; -}

 -- lincat 
  --  QVP = QuestionBantu.VP ;
 -- lin
   -- ComplSlashIP vp ip = insertObject vp.c2 (heavyNP {s = ip.s ; a = {g = ip.a.g ; n = ip.a.n ; p = P3}}) vp ;
  --  AdvQVP vp adv = insertAdv adv.s vp ;
   {- AddAdvQVP vp adv = insertAdv adv.s vp ;

    QuestQVP qp vp = { 
      s = \\t,a,b,_ => 
        let
          cl = mkClause (qp.s ! Nom) False False (agrP3 qp.a.g qp.a.n) vp  
        in
        cl.s ! DDir ! t ! a ! b ! Indic
      } ;   
-}
}

