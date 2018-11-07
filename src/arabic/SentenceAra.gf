--# -path=.:abstract:common:prelude

concrete SentenceAra of Sentence = CatAra ** open
  ResAra,
  Prelude,
  ResAra,
  TenseX,
  ParamX,
  CommonX in {


  flags optimize=all_subs ; coding=utf8 ;

  lin
{-
    PredVP np vp =
      { s = \\t,p,o =>
          case o of {
            Verbal =>
              case vp.comp.a.isPron of {
                False => vp.s ! t ! p ! Verbal ! np.a ++ np.s ! Nom ++ vp.comp.s ! Acc ;
                True  => vp.s ! t ! p ! Verbal ! np.a ++ vp.comp.s ! Acc ++ np.s ! Nom
              };
            Nominal =>
              np.s ! Nom ++ vp.s ! t ! p ! Nominal ! np.a ++ vp.comp.s ! Acc
          }
      };
-}
    PredVP np vp =
      { s =\\t,p,o =>
          let {
            pgn =
              case <o,np.a.isPron> of {
                <Verbal, False> => verbalAgr np.a.pgn;
                _               => np.a.pgn
              };
            gn = pgn2gn pgn;
            kataba  = vp.s ! pgn ! VPPerf ;
            yaktubu = vp.s ! pgn ! VPImpf Ind ;
            yaktuba = vp.s ! pgn ! VPImpf Cnj ;
            yaktub  = vp.s ! pgn ! VPImpf Jus ;
            vStr : ParamX.Tense -> Polarity -> Str =
              \tn,pl -> case<vp.isPred,tn,pl> of {
              <False, Pres, Pos> => yaktubu ;
              <False, Pres, Neg> => "لَا" ++ yaktubu ;
              <True, Pres, Pos> => "" ;      --no verb "to be" in present
              <True, Pres, Neg> => "لَيسَ" ;--same here, just add negation particle
              <_, Past, Pos> => kataba ;
              <_, Past, Neg> => "لَمْ" ++ yaktub ;
              <_, _Fut,  Pos> => "سَ" ++ yaktubu ;
              <_, _Fut,  Neg> => "لَنْ" ++ yaktuba
              };
            pred : ParamX.Tense -> Polarity -> Str =
              \tn,pl -> case <vp.isPred,tn,pl>  of {
              <True, Pres, Pos> => vp.pred.s ! gn ! Nom; --xabar marfooc
              _ => vp.pred.s ! gn ! Acc --xabar kaana wa laysa manSoob
              };

          } in
          case o of {
            Verbal =>
              --case <False, np.a.isPron> of { ---- AR workaround 18/12/2008
          case <vp.obj.a.isPron, np.a.isPron> of {
        {- IL: I don't think we should do prodrop here. vStr drops the copula in present tense,
               so there's hardly anything left for a predicative clause: e.g.
                  PredVP (UsePron i_Pron) (UseComp (CompCN (UseN car_N))) "I am a car"
               would be linearised just as "car", if we have both prodrop and copula drop.
               Leaving it up to someone who knows Arabic to decide what is better.
        Original here:
                <True,True>  => (vStr t p) ++ vp.obj.s ++ vp.s2 ++ (pred t p) ;
                -- ya2kuluhu
                <False,True> => (vStr t p) ++ vp.obj.s ++ vp.s2 ++ (pred t p); -}
                -- ya2kuluhu al-waladu, yakuluhu al-2awlaadu
                <False> => (vStr t p) ++ np.s ! Nom ++ vp.obj.s ++ vp.s2 ++ (pred t p);
                <True>  => (vStr t p) ++ vp.obj.s ++ np.s ! Nom ++ vp.s2 ++ (pred t p)
              };
            Nominal =>
              np.s ! Nom ++ (vStr t p) ++ vp.obj.s ++ vp.s2 ++ (pred t p)
          }
      };

--    PredSCVP sc vp = mkClause sc.s (agrP3 Sg) vp ;

    ImpVP vp = {
      s = \\p,g,n =>
        case p of {
          Pos => vp.s ! (Per2 g n) ! VPImp  ++ vp.obj.s  ++ vp.s2 ;
          Neg => "لا" ++ vp.s ! (Per2 g n) ! (VPImpf Jus) ++ vp.obj.s ++ vp.s2
        }
      };

--
--    SlashV2 np v2 =
--      mkClause (np.s ! Nom) np.a (predV v2) ** {c2 = v2.c2} ;
--
--    SlashVVV2 np vv v2 =
--      mkClause (np.s ! Nom) np.a
--        (insertObj (\\a => infVP vv.isAux (predV v2) a) (predVV vv))  **
--        {c2 = v2.c2} ;

-- ClSlash

    SlashVP np vps = PredVP np vps ** { c2 = vps.c2 } ;
    AdvSlash slash adv = slash ** { s2 = slash.s2 ++ adv.s } ;
    SlashPrep cl prep = cl ** {c2 = prep} ;

--  SlashVS np vs sslash = TODO


--    EmbedS  s  = {s = conjThat ++ s.s} ;
--    EmbedQS qs = {s = qs.s ! QIndir} ;
--    EmbedVP vp = {s = infVP False vp (agrP3 Sg)} ; --- agr
--

    UseCl t p cl =
      {s = t.s ++ p.s ++ 
         case <t.t,t.a> of { --- IL guessed tenses
             <(Pres|Cond),Simul>  => cl.s ! Pres ! p.p ! Nominal ;
             <Fut        ,_    >  => cl.s ! Fut ! p.p ! Nominal ;
             <_          ,_    >  => cl.s ! Past ! p.p ! Nominal
           }
      };

    UseQCl t p qcl =
      {s = \\q => t.s ++ p.s ++ 
         case <t.t,t.a> of { --- IL guessed tenses
           <(Pres|Cond),Simul>  => qcl.s ! Pres ! p.p ! q ;
           <Fut        ,_    >  => qcl.s ! Fut ! p.p ! q ;
           <_          ,_    >  => qcl.s ! Past ! p.p ! q
         }
      };

    UseRCl t p cl = {s = \\agr,c => t.s ++ p.s ++ cl.s ! t.t ! p.p ! agr ! c} ;

}
