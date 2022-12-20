--# -path=.:../abstract:../common:../prelude

-- Adam Slaski, 2009 <adam.slaski@gmail.com>

concrete VerbLit of Verb = CatLit ** open ResLit, Prelude, MorphoVerbLit in {

  flags optimize=all_subs ;  coding=utf8 ;

lin
    UseV v = defVP v;

    PassV2  v = setCopula (defVP (castv2 v)) True; 
     
  -- : V2 -> VPSlash ;  -- love (it)
    SlashV2a v = (defVP (castv2 v)) ** {cplCase=v.cplCase}; 

  -- : V3 -> NP -> VPSlash ;  -- give it (to her)
    Slash2V3 v3 np = setSlash (defVP (castv3 v3)) 
        (\\p,gn =>
          v3.cplCase.s ++ np.dep ! (npcase !<p,v3.cplCase.cas>) ) 
        v3.cplCase2; 

  -- : V3 -> NP -> VPSlash ;  -- give (it) to her
    Slash3V3 v3 np =  
      case np.nomType of {
        Reg => (setSlash (defVP (castv3 v3))
                (\\p,gn => 
                  v3.cplCase2.s ++ np.dep ! (npcase !<p,v3.cplCase2.cas>) )) 
               v3.cplCase; 
        _ => (setPronSlash (defVP (castv3 v3))
                (\\p,gn => 
                  v3.cplCase2.s ++ np.dep ! (npcase !<p,v3.cplCase2.cas>) )) 
               v3.cplCase
      };

--     ComplSlash : VPSlash -> NP -> VP ; -- love it
    ComplSlash vps np = 
      case np.nomType of {
        Reg => setPostCompl vps (\\p,gn =>
          vps.postCompl!p!gn ++ vps.cplCase.s ++ np.dep !(npcase !<p,vps.cplCase.cas>)) ;
        _ => setPreCompl vps (\\p,gn =>
          vps.cplCase.s ++ np.dep !(npcase !<p,vps.cplCase.cas>) ++ vps.preCompl!p!gn)
      };

--     AdvVP    : VP -> Adv -> VP ;        -- sleep here
    AdvVP vp adv = setAdv vp adv;

-- Not useful for Lithuanian bet to correct to AdvVP for conversion from English
--     AdVVP    : AdV -> VP -> VP ;        -- always sleep
    AdVVP adV vp = setAdv vp (adV ** {advType = OtherT}) ;

--     ReflVP   : VPSlash -> VP ;         -- love himself 
    ReflVP vps = setPostCompl vps 
      (\\p,gn => vps.postCompl!p!gn ++ vps.cplCase.s ++ reflPronForms!vps.cplCase.cas);

--     CompAP   : AP  -> Comp ;            -- (be) small
    CompAP ap = { s = \\gn => ap.s ! (cast_aform!<gn,Nom>) };

    CompCN cn = { s = \\gn => cn.s ! numGenNum gn ! Nom }; --- AR 7/12/2010

--     CompNP   : NP  -> Comp ;            -- (be) a man
    CompNP np = { s = \\gn => np.dep !InsC };

--     CompAdv  : Adv -> Comp ;            -- (be) here
    CompAdv adv = { s = \\_ => adv.s };

--     UseComp  : Comp -> VP ;            -- be warm
--    UseComp c = setCopula (setPostCompl (defVP {forms = \\_=>\\_=>[]; 
    UseComp c = setCopula (setPostCompl (defVP mkCopulaVerb)
        (\\_,gn => c.s!gn))
        True;
    
--     ComplVV  : VV  -> VP -> VP ;  -- want to run
    ComplVV vv vp = setPostCompl (defVP vv) 
        (\\p,gn => vp.verb.forms !Unfronted!VInf ++ vp.postCompl !p!gn);
    
--     ComplVQ  : VQ  -> QS -> VP ;  -- wonder who runs
    ComplVQ vq qs = setPostCompl (defVP vq) (\\p,gn => "," ++ qs.s);
    
--      ComplVS  : VS  -> S  -> VP ;  -- say that she runs
    ComplVS vs s = setPostCompl (defVP vs) (\\p,gn => [", kad"] ++ s.s);
 
--      ComplVA  : VA  -> AP  -> VP ;  -- become red
    ComplVA va a = setPostCompl (defVP (castva va)) (\\_,gn => va.cplCase.s ++ 
        case va.cplCase.adv of { False => a.s!(cast_aform!<gn,va.cplCase.cas>); True => a.adv } );

--     SlashV2V : V2V -> VP -> VPSlash ;  -- beg (her) to go
    SlashV2V v vp = setSlash (defVP (castv2 v))
        (\\p,gn => vp.verb.forms !Unfronted!VInf ++ vp.postCompl !p!gn)
        v.cplCase;

--     SlashV2S : V2S -> S  -> VPSlash ;  -- answer (to him) that it is good
    SlashV2S v s = setSlash (defVP (castv2 v))
        (\\_,_ => [", kad"] ++ s.s)
        v.cplCase;

--     SlashV2Q : V2Q -> QS -> VPSlash ;  -- ask (him) who came
    SlashV2Q v qs = setSlash (defVP (castv2 v))
        (\\_,_ => "," ++ qs.s)
        v.cplCase;
    
--     SlashVV    : VV  -> VPSlash -> VPSlash ;       -- want to buy
    SlashVV v vps = setSlash (defVP v)
        (\\p,gn => vps.verb.forms !Unfronted!VInf ++ vps.postCompl !p!gn)
        vps.cplCase;

--     SlashV2VNP : V2V -> NP -> VPSlash -> VPSlash ; -- beg me to buy
    SlashV2VNP v np vps = setSlash (defVP (castv2 v))
        (\\p,gn =>
            np.dep !(npcase !<p,v.cplCase.cas>) ++ 
            vps.verb.forms !Unfronted!VInf ++ vps.postCompl !p!gn)
        vps.cplCase;
        
--     SlashV2A : V2A -> AP -> VPSlash ;  -- paint (it) red
    SlashV2A va a = setSlash (defVP (castv2a va))
        (\\_,gn => va.cplCase.s ++ case va.cplCase.adv of { False => a.s!(cast_aform!<gn,va.cplCase.cas>); True => a.adv })
        va.cplCase2;
    
    
oper 
    castv2 : (Verb ** { cplCase:Complement }) -> Verb = \v2 -> {forms=v2.forms;asp=v2.asp;refl=v2.refl; passPastPart=v2.passPastPart ; actPastPart=v2.actPastPart ; actPastFreqPart=v2.actPastFreqPart ; actPresPart=v2.actPresPart ; actFutPart=v2.actFutPart };
    
    castv3 : (Verb ** { cplCase,cplCase2:Complement }) -> Verb = \v2 -> {forms=v2.forms;asp=v2.asp;refl=v2.refl; passPastPart=v2.passPastPart ; actPastPart=v2.actPastPart ; actPastFreqPart=v2.actPastFreqPart ; actPresPart=v2.actPresPart ; actFutPart=v2.actFutPart};
  
    castva : (Verb ** { cplCase:{cas:Case; s:Str}}) -> Verb = \v2 -> {forms=v2.forms;asp=v2.asp;refl=v2.refl; passPastPart=v2.passPastPart ; actPastPart=v2.actPastPart ; actPastFreqPart=v2.actPastFreqPart ; actPresPart=v2.actPresPart ; actFutPart=v2.actFutPart};
  
    castv2a : (Verb ** { cplCase:{cas:Case; s:Str}; cplCase2:Complement}) -> Verb = \v2 -> {forms=v2.forms;asp=v2.asp;refl=v2.refl; passPastPart=v2.passPastPart ; actPastPart=v2.actPastPart ; actPastFreqPart=v2.actPastFreqPart ; actPresPart=v2.actPresPart ; actFutPart=v2.actFutPart};
    
  defVP : Verb -> VerbPhrase = \v -> { 
        adv  = "";
        preCompl  = \\_,_ => [];
        postCompl   = \\_,_ => [];
        verb = v;
        withCopula = False;
        exp = False
    };
   
  setAdv : VerbPhrase -> CatLit.Adv -> VerbPhrase 
    = \vp,a -> case a.advType of {
      AdjT => {
        adv  = vp.adv ++ a.s;
        preCompl  = vp.preCompl;
        postCompl = vp.postCompl;
        verb = vp.verb;
        withCopula = vp.withCopula;
        exp = vp.exp -- adding adverb is not an expansion
      } ;
      PronT => {
        adv  = vp.adv;
        preCompl  = \\p,gn => a.s ++ vp.preCompl ! p ! gn;
        postCompl   = vp.postCompl;
        verb = vp.verb;
        withCopula = vp.withCopula;
        exp = vp.exp -- adding adverb is not an expansion
      } ;
      _ => {
        adv  = vp.adv;
        preCompl  = vp.preCompl;
        postCompl = \\p,gn => vp.postCompl ! p ! gn ++ a.s;
        verb = vp.verb;
        withCopula = vp.withCopula;
        exp = vp.exp -- adding adverb is not an expansion
      }     
    };

  setPreCompl : VerbPhrase -> (Polarity => GenNum => Str) -> VerbPhrase 
    = \vp,s -> {
        adv  = vp.adv;
        preCompl  = s;
        postCompl   = vp.postCompl;
        verb = vp.verb;
        withCopula = vp.withCopula;
        exp = vp.exp -- adding adverb is not an expansion
    };

  setPostCompl : VerbPhrase -> (Polarity => GenNum => Str) -> VerbPhrase 
    = \vp,s -> {
        adv  = vp.adv;
        preCompl  = vp.preCompl;
        postCompl  = s;
        verb = vp.verb;
        withCopula = vp.withCopula;
        exp = vp.exp
    };


  setSlash : VerbPhrase -> (Polarity => GenNum => Str) -> Complement -> VerbPhraseSlash 
    = \vp,s,cpl -> {
        adv  = vp.adv;
        preCompl  = vp.preCompl;
        postCompl   = s;
        verb = vp.verb;
        withCopula = vp.withCopula;
        exp = True;
        cplCase = cpl
    };

  setPronSlash : VerbPhrase -> (Polarity => GenNum => Str) -> Complement -> VerbPhraseSlash 
    = \vp,s,cpl -> {
        adv  = vp.adv;
        preCompl  = s;
        postCompl = vp.postCompl;
        verb = vp.verb;
        withCopula = vp.withCopula;
        exp = True;
        cplCase = cpl
    };

  setCopula : VerbPhrase -> Bool -> VerbPhrase 
    = \vp,b -> {
        adv  = vp.adv;
        preCompl  = vp.preCompl;
        postCompl   = vp.postCompl;
        verb = vp.verb;
        withCopula = b;
        exp = True
    };
    
} ;

