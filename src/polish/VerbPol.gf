--# -path=.:../abstract:../common:../prelude

-- Adam Slaski, 2009 <adam.slaski@gmail.com>

concrete VerbPol of Verb = CatPol ** open ResPol, Prelude in {

  flags optimize=all_subs ;  coding=utf8 ;

lin
    UseV v = defVP v;

    PassV2  v = setImienne (defVP (castv2 v)) True; 
     
    SlashV2a v = (defVP (castv2 v)) ** {c=v.c; postfix=\\_,_=>""}; 

    Slash2V3 v3 np = setSlash (defVP (castv3 v3)) 
        (\\p,gn =>
          v3.c.s ++ np.dep ! (npcase !<p,v3.c.c>) ) 
        v3.c2; 

    Slash3V3 v3 np = (setSlash (defVP (castv3 v3)) 
        (\\p,gn => 
          v3.c2.s ++ np.dep ! (npcase !<p,v3.c2.c>) )) 
        v3.c; 

--     ComplSlash : VPSlash -> NP -> VP ; -- love it
    ComplSlash vps np = setSufix2 vps (\\p,gn =>
          vps.sufix!p!gn ++ vps.c.s ++ np.dep !(npcase !<p,vps.c.c>) ++ vps.postfix!p!gn); 

--     AdvVP    : VP -> Adv -> VP ;        -- sleep here
    AdvVP vp adv = setPrefix vp (vp.prefix ++ adv.s);

--     AdVVP    : AdV -> VP -> VP ;        -- always sleep
    AdVVP adV vp = setPrefix vp (vp.prefix ++ adV.s);

--     ReflVP   : VPSlash -> VP ;         -- love himself 
    ReflVP vps = setSufix vps 
      (\\p,gn => vps.sufix!p!gn ++ vps.c.s ++ siebie ! (extract_case! vps.c.c) ++ vps.postfix!p!gn);

--     CompAP   : AP  -> Comp ;            -- (be) small
    CompAP ap = { s = \\gn => ap.s ! AF gn Nom };

    CompCN cn = { s = \\gn => cn.s ! numGenNum gn ! Nom }; --- AR 7/12/2010

--     CompNP   : NP  -> Comp ;            -- (be) a man
    CompNP np = { s = \\gn => np.dep !InstrC };

--     CompAdv  : Adv -> Comp ;            -- (be) here
    CompAdv adv = { s = \\_ => adv.s };
    
--     UseComp  : Comp -> VP ;            -- be warm
    UseComp c = setImienne (setSufix (defVP {si = \\_=>[]; sp = \\_=>[]; 
        asp = Imperfective; refl = ""; ppartp,pparti= record2table empty11forms
        })
        (\\_,gn => c.s!gn))
        True;
    
--     ComplVV  : VV  -> VP -> VP ;  -- want to run
    ComplVV vv vp = setSufix (defVP vv) 
        (\\p,gn => vp.prefix ++ vp.verb.si !VInfM ++  vp.sufix !p!gn);
    
--     ComplVQ  : VQ  -> QS -> VP ;  -- wonder who runs
    ComplVQ vq qs = setSufix (defVP vq) (\\p,gn => "," ++ qs.s);
    
--      ComplVS  : VS  -> S  -> VP ;  -- say that she runs
    ComplVS vs s = setSufix (defVP vs) (\\p,gn => [", że"] ++ s.s);
 
--      ComplVA  : VA  -> AP  -> VP ;  -- become red
    ComplVA va a = setSufix (defVP (castva va)) (\\_,gn => va.c.s ++ 
        case va.c.adv of { False => a.s!(AF gn va.c.c); True => a.adv } );

--     SlashV2V : V2V -> VP -> VPSlash ;  -- beg (her) to go
    SlashV2V v vp = setPostfix (defVP (castv2 v))
        (\\p,gn => vp.prefix ++ vp.verb.si !VInfM ++  vp.sufix !p!gn)
        v.c;

--     SlashV2S : V2S -> S  -> VPSlash ;  -- answer (to him) that it is good
    SlashV2S v s = setPostfix (defVP (castv2 v))
        (\\_,_ => [", że"] ++ s.s)
        v.c;
--     SlashV2Q : V2Q -> QS -> VPSlash ;  -- ask (him) who came
    SlashV2Q v qs = setPostfix (defVP (castv2 v))
        (\\_,_ => "," ++ qs.s)
        v.c;
    
--     SlashVV    : VV  -> VPSlash -> VPSlash ;       -- want to buy
    SlashVV v vps = setPostfix (setSufix (defVP v)
        (\\p,gn => vps.prefix ++ vps.verb.si !VInfM ++ vps.sufix !p!gn)) --???? why !pg
        vps.postfix
        vps.c;

--     SlashV2VNP : V2V -> NP -> VPSlash -> VPSlash ; -- beg me to buy
    SlashV2VNP v np vps = setPostfix (setSufix (defVP (castv2 v))
        (\\p,gn =>
            np.dep !(npcase !<p,v.c.c>) ++ vps.prefix ++ 
            vps.verb.si !VInfM ++ vps.sufix !p!gn))
        vps.postfix
        vps.c;
        
--     SlashV2A : V2A -> AP -> VPSlash ;  -- paint (it) red
    SlashV2A va a = setPostfix (defVP (castv2a va))
        (\\_,gn => va.c.s ++ case va.c.adv of { False => a.s!(AF gn va.c.c); True => a.adv })
        va.c2;
    
    
oper 
    castv2 : (Verb ** { c:Complement }) -> Verb = \v2 -> {si=v2.si;sp=v2.sp;asp=v2.asp;refl=v2.refl; ppartp=v2.ppartp; pparti=v2.pparti};
    
    castv3 : (Verb ** { c,c2:Complement }) -> Verb = \v2 -> {si=v2.si;sp=v2.sp;asp=v2.asp;refl=v2.refl; ppartp=v2.ppartp; pparti=v2.pparti};
  
    castva : (Verb ** { c:{c:Case; s:Str}}) -> Verb = \v2 -> {si=v2.si;sp=v2.sp;asp=v2.asp;refl=v2.refl; ppartp=v2.ppartp; pparti=v2.pparti};
  
    castv2a : (Verb ** { c:{c:Case; s:Str}; c2:Complement}) -> Verb = \v2 -> {si=v2.si;sp=v2.sp;asp=v2.asp;refl=v2.refl; ppartp=v2.ppartp; pparti=v2.pparti};
    
  defVP : Verb -> VerbPhrase = \v -> { 
        prefix  = "";
        sufix   = \\p,gn => "";
        verb = v;
        imienne = False;
        exp = False
    };
   
  setPrefix : VerbPhrase -> Str -> VerbPhrase 
    = \vp,s -> {
        prefix  = s;
        sufix   = vp.sufix;
        postfix = vp.postfix;
        verb = vp.verb;
        imienne = vp.imienne;
        exp = vp.exp -- adding adverb is not an expansion
    };

  setSufix : VerbPhrase -> (Polarity => GenNum => Str) -> VerbPhrase 
    = \vp,s -> {
        prefix  = vp.prefix;
        sufix   = s;
        verb = vp.verb;
        imienne = vp.imienne;
        exp = True
    };
    
  setSufix2 : VerbPhraseSlash -> (Polarity => GenNum => Str) -> VerbPhrase 
    = \vp,s -> {
        prefix  = vp.prefix;
        sufix   = s;
        verb = vp.verb;
        imienne = vp.imienne;
        exp = True
    };
  
  setSlash : VerbPhrase -> (Polarity => GenNum => Str) -> Complement -> VerbPhraseSlash 
    = \vp,s,c -> {
        prefix  = vp.prefix;
        sufix   = s;
        postfix = \\_,_=>"";
        verb = vp.verb;
        imienne = vp.imienne;
        exp = True;
        c = c
    };
    
  setPostfix : VerbPhrase -> (Polarity => GenNum => Str) -> Complement -> VerbPhraseSlash
    = \vp,s,c -> {
        prefix  = vp.prefix;
        sufix   = vp.sufix;
        postfix = s;
        verb = vp.verb;
        imienne = vp.imienne;
        exp = True;
        c=c
    };
    
  setImienne : VerbPhrase -> Bool -> VerbPhrase 
    = \vp,b -> {
        prefix  = vp.prefix;
        sufix   = vp.sufix;
        postfix = vp.postfix;
        verb = vp.verb;
        imienne = b;
        exp = True
    };
    
} ;

