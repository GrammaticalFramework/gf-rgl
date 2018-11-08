concrete QuestionAra of Question = CatAra ** open ResAra, ParamX, Prelude, VerbAra, SentenceAra in {

  flags optimize=all_subs ; coding = utf8 ;

  lin

    QuestCl cl = {
      s = \\t,p =>
        table {
          QIndir => "إِذا" ++ cl.s ! t ! p ! Verbal ;
          QDir => "هَلْ" ++ cl.s ! t ! p ! Verbal
        }
      };

    -- ComplSlashIP vps ip = {} ;

    --IL guessed
    QuestVP qp vp =
     let np = ip2np qp vp.isPred ;
         cl = PredVP np vp ;
      in { s = \\t,p,_qf => cl.s ! t ! p ! Nominal } ;



---- AR guessed
   QuestIAdv iadv cl = {s = \\t,p,_ => iadv.s ++ cl.s ! t ! p ! Verbal} ;

---- IL guessed
   -- : IComp -> NP -> QCl
   QuestIComp ic np =
     let vp = kaan (CompNP np) ;
         ip : ResAra.IP = np ** {
            s = \\_,_,_ => ic.s ! pgn2gn np.a.pgn } ;
      in QuestVP ip vp ;

   -- : IP   -> IComp ;
   CompIP ip = ip ** { 
      s = \\_ => ip.s ! True -- True=IP will be a subject of predicative sentence
                      ! Def ! Nom ; -- IP will be a subject
      } ;

   CompIAdv iadv = { s = \\_ => iadv.s ; a = ResAra.Sg } ;

   --  QCl = {s : Tense => Polarity => QForm => Str} ;
   QuestSlash ip cl = { ----IL just guessing
      s = \\t,p,qf => 
         let o = case qf of { QDir => Nominal ; _ => Verbal } ; -- purely guessing
          in cl.c2.s ++ ip.s ! False ! Def ! Nom ++ cl.s ! t ! p ! o
      } ;

  --IL guessed
  PrepIP p ip = {
    s = p.s ++ ip.s ! False -- not used as a subject of predicative sentence
                    ! Def ! Gen
    } ;

  AdvIP ip adv = ip ** {
    s = \\g,s,c => ip.s ! g ! s ! c ++ adv.s ;
    } ;

   -- : IDet -> IP
   IdetIP idet = idet ** { 
     s = \\isPred => idet.s ! Masc ;
     a = { pgn = agrP3 NoHum Masc idet.n ; isPron = False }
     } ;

   -- : IDet -> CN -> IP
   IdetCN idet cn = {
     s = \\isPred,s,c 
          => idet.s ! cn.g ! s ! c ++
             cn.s ! idet.n ! Indef ! Gen ; --idaafa
     a = { pgn = agrP3 NoHum cn.g idet.n ; isPron = False }
     } ;

   -- : IQuant -> Num -> IDet
   IdetQuant iquant num = {
     s = \\g,s,c =>
          let gend = detGender g num.n -- gender flips with some numbers
          in  iquant.s ! s ! c ++ num.s ! gend ! s ! c ;
     n = sizeToNumber num.n
     } ;
}
