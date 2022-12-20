--# -path=.:../abstract:../common:../prelude

-- Adam Slaski, 2009 <adam.slaski@gmail.com>

concrete QuestionLit of Question = CatLit ** open ResLit, Prelude, MorphoLit in {

 flags optimize=all_subs ;

lin

--     QuestCl     : Cl -> QCl ;            -- does John walk
    QuestCl cl = { s = \\p,a,t=> "ar" ++ cl.s !p !a !t };
    
--     QuestVP     : IP -> VP -> QCl ;      -- who walks
    QuestVP ip vp = {
        s = \\pol,anter,tense => ip.nom ++ vp.preCompl  !pol !ip.gn ++ vp.adv ++
            ((indicativeForm vp.verb pol) !<tense, anter, ip.gn, ip.p>) ++ 
            vp.postCompl !pol !ip.gn
    };

--     QuestSlash  : IP -> ClSlash -> QCl ; -- whom does John love 
    QuestSlash ip cls = {
        s = \\pol,anter,tense => cls.cpl.s ++ ip.dep ! cls.cpl.cas     ++ cls.s !pol !anter !tense
    };

--     QuestIAdv   : IAdv -> Cl -> QCl ;    -- why does John walk
    QuestIAdv ia cl = { s = \\p,a,t=> ia.s ++ cl.s !p !a !t };
    
--     QuestIComp  : IComp -> NP -> QCl ;   -- where is John
    QuestIComp ic np = { 
        s = \\p,a,t => ic.s ++ 
          (mkFormWithCopula {forms = \\_=>\\_=>[]; asp = Dual; refl = Norefl; passPastPart=\\_=>""; actPastPart=\\_=>""; actPastFreqPart=\\_=>""; actPresPart=\\_=>""; actFutPart=\\_=>""} p !<t,a,np.gn,np.p>) ++ np.nom
    };
 
--     IdetCN    : IDet -> CN -> IP ;       -- which five songs
    IdetCN idet cn = {
        nom = idet.s !Nom  !cn.g ++ cn.s !idet.nb !(accom_case! <idet.numAgr,Nom, cn.g>);
        voc = idet.s !VocL !cn.g ++ cn.s !idet.nb !(accom_case! <idet.numAgr,Nom, cn.g>);
        dep = \\cc => let c = extract_case! cc in
          idet.s !c !cn.g ++ cn.s !idet.nb ! (accom_case! <idet.numAgr, c, cn.g>);
--        gn = (accom_gennum !<idet.numAgr, cn.g, idet.nb>);
        gn = cast_gennum !<cn.g, idet.nb>;
        p = P3;
        nomType = Reg -- check it
    };
--     IdetIP    : IDet       -> IP ;       -- which five
    IdetIP idet = {
        nom = idet.s !Nom !SingPlur Masc;
        voc = idet.s !VocL !SingPlur Masc;
        dep = \\cc => let c = extract_case! cc in
          idet.s !c !SingPlur Masc;
--        gn = (accom_gennum !<idet.numAgr, SingPlur Masc, idet.nb>);
        gn = cast_gennum !<SingPlur Masc, idet.nb>;
        p = P3; 
        nomType = Reg -- check it
        };

--     AdvIP     : IP -> Adv -> IP ;        -- who in Paris
    AdvIP ip adv = {
        nom = ip.nom ++ adv.s;
        voc = ip.voc ++ adv.s;
        dep = \\cc => ip.dep!cc ++ adv.s;
        gn = ip.gn;
        p = ip.p;
        nomType = Reg -- check it
    }; 

--     IdetQuant : IQuant -> Num -> IDet ;  -- which (five)
    IdetQuant iq n = {
--        s = \\c,g => iq.s! AF (cast_gennum!<g,n.nb>) (accom_case!<n.numAgr,c,g>) ++ n.s !c !g;
        s = \\c,g => iq.s! (cast_aform_exp!<g,n.nb,(accom_case!<n.numAgr,c,g>)>) ++ n.s !c !g;
        detType = NormalDet ; -- to be checked
        nb = n.nb;
        numAgr = n.numAgr
    };

--     PrepIP    : Prep -> IP -> IAdv ;     -- with whom
    PrepIP prep ip = { s = prep.s ++ ip.dep !prep.cas};

--     CompIAdv  : IAdv -> IComp ;          -- where (is it)
    CompIAdv ia = ia;

--     CompIP    : IP   -> IComp ;          -- who (is it)
    CompIP ip = { s = ip.dep ! InsC };

    AdvIAdv i a = ss (i.s ++ a.s) ;

}
