--# -path=.:../abstract:../common:../prelude

-- Adam Slaski, 2009 <adam.slaski@gmail.com>

concrete RelativeLit of Relative = CatLit ** open ResLit, MorphoLit in {

 flags optimize=all_subs ; coding=utf8 ;

 lin

-- ASL
-- In my opinion this is terribly medley of two phenomena. One of them is connected with funs RelCl and RelS.
-- The other with rest of the funs. Why don't separate them?


--     RelCl    : Cl -> RCl ;            -- such that John loves her
    RelCl cl = {
      s = \\gn, pol, ant, ten => ["taip , kaip"] ++ cl.s ! pol ! ant ! ten
    };
	-- this is only sometimes correct. there is no way to find a case for 'taki' ('such') so rp 'taki' was substituted with sentence adv 'tak'


--     RelVP    : RP -> VP -> RCl ;      -- who loves John
-- enormous memory usage !!! 
    RelVP rp vp = {
      s = \\gn => case rp.mgn of { 
        NoGenNum=>
          \\pol, anter, tense => 
            "," ++ rp.s !(cast_aform!<gn,Nom>) ++ vp.preCompl !pol !gn ++ vp.adv ++
            ((indicativeForm vp.verb pol) !<tense, anter, gn, P3>) ++ 
            vp.postCompl !pol !gn;
        JustGenNum x => 
          \\pol, anter, tense => 
            "," ++ rp.s !(cast_aform!<gn,Nom>) ++ vp.preCompl !pol !gn ++ vp.adv ++
            ((indicativeForm vp.verb pol) !<tense, anter, x, P3>) ++ 
            vp.postCompl !pol !x
         }  
    };

--     RelSlash : RP -> ClSlash -> RCl ; -- whom John loves
    RelSlash rp clslash = {
      s = \\gn, pol, anter, tense => 
        "," ++ clslash.cpl.s ++ rp.s !(cast_aform!<gn,(extract_case!(npcase!<pol,clslash.cpl.cas>))>) ++ clslash.s !pol !anter !tense;
    };

--     IdRP  : RP ;                      -- which
    IdRP = { s = (mkPronXis "kuris").s ; mgn = NoGenNum };
    
--     FunRP : Prep -> NP -> RP -> RP ;  -- the mother of whom
-- i have bad feelings about that. terrible overgeneratnig
-- policjant, (za którym ksiądz) kocha ... - wrong tree
-- should be policjant, ((za którym) (ksiądz) kocha)
    FunRP p n rp = { s = table { 
        AF num gen Nom  => p.s ++ rp.s!AF num gen (extract_case!p.cas) ++ n.nom;
        AF num gen VocP => p.s ++ rp.s!AF num gen (extract_case!p.cas) ++ n.voc;
        AF num gen c    => p.s ++ rp.s!AF num gen (extract_case!p.cas) ++ n.dep!
            (case c of { Gen => GenC; Dat => DatC; Ins => InsC; Acc => AccC; _=>LocC });
        neutral => p.s ++ rp.s!neutral ++ n.nom
      };
      mgn = JustGenNum n.gn
    };

}
