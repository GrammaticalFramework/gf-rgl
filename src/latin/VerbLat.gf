--1 Construction rules for latin verb phrases
concrete VerbLat of Verb = CatLat ** open (S=StructuralLat),ResLat,IrregLat,ExtraLat,Predef,Prelude in {

  flags optimize=all_subs ;

  lin
--2 Complementization rules

--  UseV : V -> VP
    UseV = predV ; -- dormire

--  ComplVV : VV -> VP -> VP ;  -- want to run        
    ComplVV v vp =
      vp ** {
	s = \\af,qf => v.act ! af ;
	compl = \\ag => vp.compl ! ag ++ vp.inf ! VInfActPres
      } ;

--  ComplVS : VS -> S -> VP ;  -- say that she runs
    ComplVS vs s  = -- insertObj ( dummyNP (S.that_Subj.s ++ s.s ! PreS)) Nom_Prep (predV v) ;
      vs ** {
	s = \\a,q => vs.act ! a ++ case q of { VQTrue => Prelude.BIND ++ "ne"; VQFalse => "" };
	pass = \\p,q => vs.pass ! p ++ case q of { VQTrue => Prelude.BIND ++ "ne"; VQFalse => "" };
	compl = \\ag => defaultSentence s ! SOV ; -- s.s ! QIndir ;
	adv = [] ;
	obj = []
      } ;
--  ComplVQ : VQ -> QS -> VP ;  -- wonder who runs
    ComplVQ vq qs  = -- insertObj (dummyNP (q.s ! QIndir)) Nom_Prep (predV v) ;
      vq ** {
	s = \\a,q => vq.act ! a ++ case q of { VQTrue => Prelude.BIND ++ "ne"; VQFalse => "" };
	pass = \\p,q => vq.pass ! p ++ case q of { VQTrue => Prelude.BIND ++ "ne"; VQFalse => "" };
	compl = \\ag => qs.s ! QIndir ;
	adv = [] ;
	obj = []
      } ;
    
--  ComplVA : VA -> AP -> VP ;  -- they become red
    ComplVA v ap = (predV v) ** { compl = ap.s } ;

--  SlashV2a : V2 -> VPSlash ;  -- love (it)
    SlashV2a v = lin VP (predV2 v) ;

--  Slash2V3 : V3  -> NP -> VPSlash ;  -- give it (to her)
    Slash2V3 v np = lin VP (insertObjc np (predV3 v ** { c = v.c2 } ) );
    
--  Slash3V3 : V3  -> NP -> VPSlash ;  -- give (it) to her
    Slash3V3 v np = lin VP ( insertObjc np ( predV3 v ** { c = v.c} ) ) ;

--  SlashV2V : V2V -> VP -> VPSlash ;  -- beg (her) to go
--    SlashV2V v vp = insertObjc (\\a => infVP v.isAux vp a) (predVc v) ;

--  SlashV2S : V2S -> S  -> VPSlash ;  -- answer (to him) that it is good
--    SlashV2S v s  = insertObjc (\\_ => conjThat ++ s.s) (predVc v) ;

--  SlashV2Q : V2Q -> QS -> VPSlash ;  -- ask (him) who came
    SlashV2Q v q  = lin VP (insertObjc (dummyNP (q.s ! QIndir)) (predV2 v) ) ;

--  SlashV2A : V2A -> AP -> VPSlash ;  -- paint (it) red
    SlashV2A v ap = lin VP ( (predV2 v) ** { adj = ap.s } ) ; 

--  ComplSlash : VPSlash -> NP -> VP ; -- love it
    ComplSlash vp np = -- VPSlash -> NP -> VP
      insertObj np vp.c vp ;

--  SlashVV    : VV  -> VPSlash -> VPSlash ;       -- want to buy
--    SlashVV vv vp = 
--      insertObj (\\a => infVP vv.isAux vp a) (predVV vv) **
--        {c2 = vp.c2} ;

--  SlashV2VNP : V2V -> NP -> VPSlash -> VPSlash ; -- beg me to buy
--    SlashV2VNP vv np vp = 
--      insertObjPre (\\_ => vv.c2 ++ (combineNounPhrase np) ! PronNonDrop ! Acc)
--        (insertObjc (\\a => infVP vv.isAux vp a) (predVc vv)) **
--          {c2 = vp.c2} ;

--2 Other ways of forming verb phrases

--  ReflVP   : VPSlash -> VP ;         -- love himself
--    ReflVP v = insertObjPre (\\a => v.c2 ++ reflPron ! a) v ;    

--  UseComp : Comp -> VP ;            -- be warm
    UseComp comp = 
      insertAdj comp.s (predV be_V) ;

--  PassV2   : V2 -> VP ;               -- be loved
    PassV2 v = predV (
      v ** {
	act = table { VAct anter tense number person =>
			case anter of {
			  VSim => v.pass ! VPass tense number person ;
			  VAnt => ""  --error "using participles is not implemented yet"
			}
	  } 
	} );

--  AdvVP    : VP -> Adv -> VP ;        -- sleep here
    AdvVP vp adv = insertAdv adv vp ;

--    ExtAdvVP vp adv = vp

--  AdVVP    : AdV -> VP -> VP ;        -- always sleep
    AdVVP adv vp = vp ** { adv = vp.adv ++ adv.s } ;

--  AdvVPSlash : VPSlash -> Adv -> VPSlash ;  -- use (it) here
    AdvVPSlash vp adv = vp ** { adv = (adv.s ! Posit) ++ vp.adv } ;
    
--  AdVVPSlash : AdV -> VPSlash -> VPSlash ;  -- always use (it)
    AdVVPSlash adv vp = vp ** { adv = vp.adv ++ adv.s } ;
    
--    VPSlashPrep : VP -> Prep -> VPSlash ;  -- live in (it)

    --2 Complements to copula

--  CompAP   : AP  -> Comp ;            -- (be) small
    CompAP ap = ap ;
    
--  CompNP   : NP  -> Comp ;            -- (be) the man
    CompNP np = {s = \\_ =>
		   (combineNounPhrase np) ! PronNonDrop ! APostN ! DPreN ! Nom ;
      } ;

--  CompAdv  : Adv -> Comp ;            -- (be) here
    CompAdv a = {s = \\_ => a.s ! Posit } ;

--  CompCN   : CN  -> Comp ;            -- (be) a man/men
    CompCN cn = {s = table { Ag g n c => cn.preap.s ! Ag cn.g n Nom ++ cn.s ! n ! Nom ++ cn.postap.s ! Ag cn.g n Nom} };


--  UseCopula : VP ;                    -- be
    UseCopula = predV be_V ;
}
