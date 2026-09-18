concrete VerbTel of Verb = CatTel ** open ResTel, Prelude in {

  flags optimize=all_subs ;

  lin
    UseV = predV ;

    SlashV2a v = predV v ** {c2 = v.c2} ;

    SlashV2A v ap = predV v ** {
      c2 = v.c2 ;
      comp = \\agr => case agr of {Ag g n _ => ap.s ! g ! n ! Dir}
      } ;

    SlashV2S v sent = predV v ** {
      c2 = v.c2 ;
      comp = \\_ => sent.s ++ "అని"
      } ;

    Slash2V3 v np =
      let vp = insertObject np (predV v ** {c2 = v.c2})
      in vp ** {c2 = v.c3} ;

    Slash3V3 v np =
      let vp = insertObject np (predV v ** {c2 = v.c3})
      in vp ** {c2 = v.c2} ;

    AdvVPSlash vps adv = vps ** {
      comp = \\agr => vps.comp ! agr ++ adv.s
      } ;

    AdVVPSlash adv vps = vps ** {
      comp = \\agr => adv.s ++ vps.comp ! agr
      } ;

    ComplSlash vp np = insertObject np vp ;

    ComplVV v vp = predV v ** {
      comp = \\agr => let f = vp.s ! Pos ! VPInf in
        f.inf ++ f.fin
      } ;

    ComplVS v s = predV v ** {comp = \\_ => s.s ++ "అని"} ;
    ComplVQ v q = predV v ** {comp = \\_ => q.s ++ "అని"} ;
    ComplVA v ap = predV v ** {comp = \\agr => case agr of {
      Ag g n _ => ap.s ! g ! n ! Dir
      }} ;

    VPSlashPrep vp prep = vp ** {
      c2 = {s = prep.s ; c = VTransPost}
      } ;

    AdVVP adv vp = insertAdv adv.s vp ;

    UseComp comp = predCopula ** {comp = comp.s} ;

    CompAP ap = {s = \\agr => case agr of {
      Ag g n _ => ap.s ! g ! n ! Dir
      }} ;
    CompNP np = {s = \\_ => np.s ! NPC Obl} ;
    CompAdv adv = {s = \\_ => adv.s} ;
    CompCN cn = {s = \\_ => cn.s ! Sg ! Dir} ;

    AdvVP vp adv = insertAdv adv.s vp ;
    ExtAdvVP vp adv = insertAdv adv.s vp ;

}
