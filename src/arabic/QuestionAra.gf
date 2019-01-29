concrete QuestionAra of Question = CatAra ** open ResAra, ParamX, Prelude, VerbAra, SentenceAra in {

  flags optimize=all_subs ; coding = utf8 ;

lin
  -- : Cl -> QCl ;            -- does John walk
  QuestCl cl = {
    s = \\t,p =>
      table {
        QIndir => "إِذا" ++ cl.s ! t ! p ! toOrder QIndir ;
        QDir => "هَلْ" ++ cl.s ! t ! p ! Verbal -- yes/no question
      }
    };

  -- : IP -> VP -> QCl ;      -- who walks
  QuestVP qp vp =
   let np = ip2np qp (case vp.vtype of {Copula=>True ; _=>False}) ;
       cl = PredVP np vp ;
    in { s = \\t,p,qf => cl.s ! t ! p ! toOrder qf } ;

  -- QuestIAdv   : IAdv -> Cl -> QCl ;    -- why does John walk
  QuestIAdv iadv cl = {s = \\t,p,qf => iadv.s ++ cl.s ! t ! p ! Verbal} ; -- Verbal word order, because there is no pre-existing question word /IL

---- IL guessed
  -- : IComp -> NP -> QCl
  QuestIComp ic np =
    let vp = UseComp (CompNP np) ; -- puts NP in nominative
        ip : ResAra.IP = np ** { -- NP's s is already present in VP, we only want its agr
               s = \\_,_,_,_ => ic.s ! pgn2gn np.a.pgn } ;
     in QuestVP ip vp ;

  -- : IP -> IComp ;
  CompIP ip = ip ** {
    s = \\gn => ip.s ! True  -- True=IP will be a subject of predicative sentence
                     ! gn.g  -- IComp agrees in gender with eventual head
                     ! Def ! Nom ; -- IP will be a subject
      } ;

  -- : IAdv -> IComp ;          -- where (is it)
  CompIAdv iadv = { s = \\_ => iadv.s ; a = ResAra.Sg } ;

   -- : IP -> ClSlash -> QCl ; -- whom does John love
  QuestSlash ip cls = {
    s = \\t,p,qf =>        -- remove cls's c2 so it won't show up in the wrong place
        let cl : ResAra.Cl = complClSlash (cls ** {c2=noPrep});
            o = toOrder qf
          in cls.c2.s ++ bindIf cls.c2.binds -- Put cls's c2 ("with") before the IP ("whom")
          ++ ip.s ! False ! Masc ! Def ! Nom
          ++ cl.s ! t ! p ! o
      } ;

  -- : Prep -> IP -> IAdv ;     -- with whom
  PrepIP prep ip = {
    s = prep.s ++ bindIf prep.binds
        ++ ip.s ! False -- not used as a subject of predicative sentence
                ! Masc ----
                ! Def -- not sure /IL
                ! prep.c
    } ;

  -- : IP -> Adv -> IP ;        -- who in Paris
  AdvIP ip adv = ip ** {
    s = \\isPred,g,s,c => ip.s ! isPred ! g ! s ! c ++ adv.s ;
    } ;

  -- : IAdv -> Adv -> IAdv ;    -- where in Paris
  AdvIAdv iadv adv = {s = iadv.s ++ adv.s} ;

  -- : IDet -> IP
  IdetIP idet = idet ** {
    s = \\isPred => idet.s ;
    a = { pgn = agrP3 NoHum Masc idet.n ; isPron = False }
    } ;

  -- : IDet -> CN -> IP
  IdetCN idet cn = {
    s = \\isPred,g,s,c
          => idet.s ! cn.g ! s ! c ++
             cn2str cn idet.n idet.d Gen ;
    a = { pgn = agrP3 NoHum cn.g idet.n ; isPron = False }
    } ;

   -- : IQuant -> Num -> IDet
  IdetQuant iquant num = {
    s = \\g,s,c =>
          let gend = detGender g num.n -- gender flips with some numbers
          in  iquant.s ! s ! c ++ num.s ! gend ! s ! c ;
    n = sizeToNumber num.n ;
    d = Indef ---- TODO check
    } ;

-- IL guessed all following
lincat
  QVP = ResAra.VP ;          -- buy what where
lin
  ComplSlashIP vps ip = ComplSlash vps (ip2np ip False) ; -- : VPSlash -> IP -> QVP ;   -- buys what
  AdvQVP = AdvVP ;    -- : VP  -> IAdv -> QVP ;   -- lives where
  AddAdvQVP = AdvVP ; -- : QVP -> IAdv -> QVP ;   -- buys what where
  QuestQVP = QuestVP ;   -- : IP -> QVP -> QCl ;  -- who buys what where
}
