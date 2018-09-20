concrete QuestionAra of Question = CatAra ** open ResAra, ParamX, Prelude, VerbAra in {

  flags optimize=all_subs ; coding = utf8 ;

  lin

    QuestCl cl = {
      s = \\t,p => 
        table {
          QIndir => "إِذا" ++ cl.s ! t ! p ! Verbal ;
          QDir => cl.s ! t ! p ! Verbal 
        }
      };

    -- ComplSlashIP vps ip = {} ;

-- AR copied from PredVP
    QuestVP qp vp = 
      { s =\\t,p,_ => 
          let {
  ----            o = Verbal ; ---- AR
	    objgn = pgn2gn vp.obj.a.pgn ;
	    np = {s = qp.s ! objgn.g ! Def ; ----IL just guessing state
		  a ={pgn = Per3 Masc qp.n ; isPron = False}} ;
            pgn = np.a.pgn ;
            gn = pgn2gn pgn;
            kataba  = vp.s ! pgn ! VPPerf ;
            yaktubu = vp.s ! pgn ! VPImpf Ind ;
            yaktuba = vp.s ! pgn ! VPImpf Cnj ;
            yaktub  = vp.s ! pgn ! VPImpf Jus ;
            vStr : ResAra.Tense -> Polarity -> Str = 
              \tn,pl -> case<vp.isPred,tn,pl> of {
              <False, ResAra.Pres, Pos> => yaktubu ;
              <False, ResAra.Pres, Neg> => "لَا" ++ yaktubu ;
              <True, ResAra.Pres, Pos> => "" ;      --no verb "to be" in present 
              <True, ResAra.Pres, Neg> => "لَيسَ" ;--same here, just add negation particle
              <_, ResAra.Past, Pos> => kataba ;
              <_, ResAra.Past, Neg> => "لَمْ" ++ yaktub ;
              <_, ResAra.Fut,  Pos> => "سَ" ++ yaktubu ; 
              <_, ResAra.Fut,  Neg> => "لَنْ" ++ yaktuba 
              };
            pred : ResAra.Tense -> Polarity -> Str = 
              \tn,pl -> case <vp.isPred,tn,pl>  of {
              <True, ResAra.Pres, Pos> => vp.pred.s ! gn ! Nom; --xabar marfooc
              _ => vp.pred.s ! gn ! Acc --xabar kaana wa laysa manSoob
              }         ;
            
          } in
---          case o of { 
----            _ => 
              case <False, np.a.isPron> of {
---- AR workaround 18/12/2008  case <vp.obj.a.isPron, np.a.isPron> of {
                -- ya2kuluhu
                <False,True> => (vStr t p) ++ vp.obj.s  ++ vp.s2 ++ (pred t p);
                -- ya2kuluhu al-waladu, yakuluhu al-2awlaadu 
                <False,False> => (vStr t p) ++ np.s ! Nom ++ vp.obj.s  ++ vp.s2 ++ (pred t p);
                <True,False>  => (vStr t p) ++ vp.obj.s ++ np.s ! Nom ++ vp.s2 ++ (pred t p);
                <True,True>  => (vStr t p) ++ vp.obj.s ++ vp.s2 ++ (pred t p)
              };
     ----       Nominal =>
     ----         np.s ! Nom ++ (vStr t p) ++ vp.obj.s ++ vp.s2 ++ (pred t p)
         } 
   ; ----  };


---- AR guessed
   QuestIAdv iadv cl = {s = \\t,p,_ => iadv.s ++ cl.s ! t ! p ! Verbal} ;

---- IL guessed
   QuestIComp icomp np =
     let vp = kaan (CompNP np) in
      QuestVP icomp vp ;

   CompIP ip = ip ;
   -- old, when IComp = Comp { s = \\{g=g ; n=_},c => ip.s ! g ! Def ! c } ; ----

   CompIAdv iadv = mkIP iadv.s ResAra.Sg ;

   --  QCl = {s : R.Tense => Polarity => QForm => Str} ;
   QuestSlash ip cl = { ----IL just guessing
      s = \\t,p,qf => case qf of {
	QDir   => cl.s ! t ! p ! Verbal ++ cl.c2  ++ ip.s ! Masc ! Def ! Nom ; --VSO (purely guessing)
	QIndir => cl.s ! t ! p ! Nominal ++ cl.c2 ++ ip.s ! Masc ! Def ! Nom } --SVO (purely guessing)
      } ;

   PrepIP p ip = {s = p.s ++ ip.s ! Masc ! Def ! Acc} ; ----IL

   AdvIP ip adv = ip ** {
     s = \\g,s,c => ip.s ! g ! s ! c ++ adv.s ;
     n = ip.n
     } ;

----IL guessed with help of L and Google translate
   -- : IDet -> IP
   IdetIP idet = idet ; -- Gender still matters if turned into IComp

   -- : IDet -> CN -> IP
   IdetCN idet cn = idet ** {
     s = \\g,s,c => idet.s ! cn.g ! s ! c ++ -- gender is determined by the CN
                     cn.s ! idet.n ! Indef ! Gen ; --idaafa
     } ;

   -- : IQuant -> Num -> IDet
   IdetQuant iquant num = {
     s = \\g,s,c =>
          let gend = detGender g num.n -- gender flips with some numbers
          in  iquant.s ! s ! c ++ num.s ! gend ! s ! c ;
     n = sizeToNumber num.n
     } ;
}
