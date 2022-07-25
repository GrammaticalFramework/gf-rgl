concrete QuestionEst of Question = CatEst ** open ResEst, Prelude in {

  flags optimize=all_subs ; coding=utf8;

  lin

    QuestCl cl = {
      s = \\t,a,p => "kas" ++ cl.s ! t ! a ! p
      } ;

    QuestVP ip vp = mkClause (subjForm (ip ** {isPron = False ; a = agrP3 ip.n}) vp.sc) (agrP3 ip.n) vp ;

    QuestSlash ip slash = {
      s = \\t,a,p =>
            let
              cls = slash.s ! t ! a ! p ;
              who = appCompl True p slash.c2 (ip ** {a = agrP3 ip.n ; isPron = False})
            in
            who ++ cls
      } ;

    QuestIAdv iadv cl = {
      s = \\t,a,p => iadv.s ++ cl.s ! t ! a ! p
      } ;

    QuestIComp icomp np = {
      s = \\t,a,p =>
        let
          vp = predV (verbOlema ** {sc = NPCase Nom}) ;
          cl = mkClause (subjForm np vp.sc) np.a vp ;
        in
        icomp.s ! np.a ++ cl.s ! t ! a ! p
      } ;

    PrepIP p ip = {s =
      appCompl True Pos p (ip ** {a = agrP3 ip.n ; isPron = False})} ;

    AdvIP ip adv = ip ** {
      postmod = ip.postmod ++ adv.s ;
      } ;

-- The computation of $IdetCN$ is a special case of that in $NounEst.DetCN$,
-- because the interrogative doesn't agree.

   IdetCN idet cn = emptyIP ** {
      s = \\c =>
        let
          k : Case = npform2case n c ;
          ncase : NForm = case <k,idet.isNum> of {
            <Nom,  True> => NCase Sg Part ; -- TODO estonian example (Fin was "mitkä kolme kytkintä")
            <_,    True> => NCase Sg k ;    -- TODO estonian example (Fin was "miksi kolmeksi kytkimeksi")
            _            => NCase n  k      -- TODO estonian example (Fin was "mitkä kytkimet")
          }
        in
          idet.s ! Nom ++        -- mis
          idet.post ! k ++       -- kolme
          cn.s ! ncase ;         -- kassi+ga
      n = idet.n ; -- needed for agreement, "mis kolm kassi mängivad"
  } where {
    n : Number = case idet.isNum of {
                  True  => Sg ;
                  False => idet.n } ;
  } ;

    IdetIP idet = let n = idet.n in emptyIP ** {
      s = \\c =>
        let
          k = npform2case n c ;
        in
        case idet.isNum of {
          True  => idet.s ! Nom ++ idet.post ! k ;
          False => idet.s ! k ++ idet.post ! k
        } ;

      n = n
      } ;
    -- The quant and the num may be inflected in different cases:
      -- * mis kolme koeraga, mis kolmega
      -- * millega
      -- * mille 3-ga (this would be the preferable output, but currently outputs "mis 3ga")
    IdetQuant idet num = num ** {
      s = \\c => idet.s ! num.n ! c ;
      post = \\c => num.s ! Sg ! c ;
      } ;

    AdvIAdv i a = {s = i.s ++ a.s} ;

    CompIAdv a = {s = \\_ => a.s} ;
    CompIP ip = {s = \\_ => linIP (NPCase Nom) ip} ;

}
