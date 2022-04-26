concrete VerbAra of Verb = CatAra ** open Prelude, ResAra, ParamX in {

  flags optimize=all_subs ;

  lin
    UseV = predV ;

    -- : VV -> VPSlash -> VPSlash
    SlashVV vv vps = vps ** ComplVV vv vps ;

    -- : V2V -> VP -> VPSlash ;  -- beg (her) to go
    SlashV2V v2v vp = let v2vVP = predV v2v in -- IL
      vp ** {
        s = v2vVP.s ;
        agrObj = \\pgn => v2v.s2  -- أَنْ
                       ++ vp.s ! pgn ! VPImpf Cnj -- this will agree with the object added by ComplSlash
                       ++ vp.obj.s ;
        obj = emptyObj ;
        vtype = NotPred ;
        c2 = v2v.c2 ; -- preposition for the direct object
        sc = v2v.sc
      } ;

    -- : V2V -> NP -> VPSlash -> VPSlash ; -- beg me to buy
    SlashV2VNP v2v np vps =  let v2vVP = slashV2 v2v in -- IL
      vps ** {
        s = \\pgn,vpf => v2vVP.s ! pgn ! vpf -- main verb agrees with subject
                      ++ bindIfPron np v2vVP
                      ++ v2v.s2  -- أَنْ
                      ++ vps.s ! np.a.pgn ! VPImpf Cnj -- verb from old VP agrees with object
                      ++ vps.obj.s ; -- otherwise obj appears in a weird place /IL
        obj = emptyObj ;
        vtype = NotPred ;
                    -- preposition for the direct object comes from VP
        sc = v2v.sc
      } ;

    -- : V2S -> S -> VPSlash ;  -- answer (to him) that it is good
    SlashV2S v2s s = slashV2 v2s ** { -- IL
      agrObj = -- this is put into agrObj even though it doesn't depend on agr, because insertObj puts agrObj *after* the new object.
        \\pgn => v2s.s2 -- أَنَّ
              ++ s.s ! v2s.o ;
      } ;

    SlashV2a = slashV2 ;
    Slash2V3 v np = insertObj np (slashV2 v) ** {c2 = v.c3 ; agrObj = \\_ => []};

    Slash3V3 v np =
      let vp = slashV2 v ** {c2 =
            v.c2 ** {
              s = case np.a.isPron of {
                    True => case v.c2.binds of {
                        True  => v.c2.s ; -- to make sure there's something for the object to attach to
                        False => v.c2.s ++ "إِيَّا" } ; -- see https://en.wiktionary.org/wiki/%D8%A5%D9%8A%D8%A7#Particle /IL
                    False => v.c2.s }
            }
          }
       in vp ** {
            c2 = v.c3 ;
            agrObj = \\_ => bindIfPron np vp -- will be emptied when insertObj is called /IL
          } ;

    ComplSlash vp np = insertObj np vp ;

    -- : VV  -> VP -> VP ;  -- want to run
    ComplVV vv vp = let vvVP = predV vv in -- IL
      vp ** {
        s = \\pgn,vpf => vvVP.s ! pgn ! vpf
                      ++ vv.s2  -- أَنْ
                      ++ vp.s ! pgn ! VPImpf Cnj ;
        vtype = NotPred ;
        sc = vv.sc
      } ;

    -- : VS -> S -> VP ;  -- say that she runs
    ComplVS vs s = predV vs ** { -- IL
      obj = emptyObj ** {s = vs.s2 -- أَنَّ
                          ++ s.s ! vs.o}
      } ;

    -- : VQ -> QS -> VP ;  -- wonder who runs
    ComplVQ vq qs = predV vq ** { -- IL
      obj = emptyObj ** {s = qs.s ! QIndir}
      } ;

    -- : VA -> AP -> VP ;  -- they become red
    ComplVA v ap = predV v ** {pred = CompAP ap} ;

--    ComplV2A v np ap =
--      insertObj (\\_ => v.c2 ++ np.s ! Acc ++ ap.s ! np.a) (predV v) ;
--
    UseComp xabar =
      case xabar.isNP of {
        False => kaan xabar ;
        True  => predV copula ** {obj = xabar.obj ; vtype=Copula}
      } ;

    UseCopula = predV copula ;

    -- : VP -> Prep -> VPSlash ;  -- live in (it)
    VPSlashPrep vp prep = vp ** {
      c2 = prep ;
      agrObj = \\_ => [] -- to make it into VPSlash, VP didn't have that field before
      } ;

    AdvVP vp adv = insertStr adv.s vp ;

    AdVVP adv = insertStr adv.s ;
    AdVVPSlash adv vps = vps ** insertStr adv.s vps ;

    -- : VPSlash -> VP ;         -- love himself
    ReflVP vps = vps ** {
      s = \\pgn,vf => vps.s ! pgn ! vf
                   ++ vps.obj.s -- only relevant if the VPSlash has been through VPSlashPrep
                   ++ vps.c2.s ++ bindIf vps.c2.binds
                   ++ reflPron vps.c2.c pgn ;
      c2 = accPrep ;
      obj = emptyObj ; -- because old obj was moved in s
      } ;

    PassV2 = passPredV ;

    CompAP ap = {s   = \\agr,c => ap.s ! Hum ! agr.g ! agr.n ! Indef ! c ; --FIXME
                 obj = emptyObj ; isNP = False} ;
    CompAdv a = {s   = \\_,_ => a.s ;
                 obj = emptyObj ; isNP = False} ;

    CompCN cn = {s = \\agr,c => cn2str cn agr.n Indef Nom ;
                 obj = emptyObj ; isNP = False} ;
    CompNP np = {s = \\_,_ => [] ;
                 obj = {s = np.s ! Nom ; a = agrLite np.a} ;
                 isNP = True} ;
--
--
}
