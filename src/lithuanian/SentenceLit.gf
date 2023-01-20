--# -path=.:../abstract:../common:../prelude

-- Adam Slaski, 2009 <adam.slaski@gmail.com>

concrete SentenceLit of Sentence = CatLit ** open Prelude, ResLit, MorphoLit in {

 flags optimize=all_subs ; coding=utf8 ;

lin
--     PredVP    : NP -> VP -> Cl ;         -- John walks
    PredVP np vp = 
       case np.nomType of {
           -- by default, suppress the subject personMarker
           PersMark => {
               s = \\pol,anter,tense =>
                   vp.preCompl !pol !np.gn ++ vp.adv ++
                   ((indicativeForm vp.verb pol) !<tense, anter, np.gn, np.p>) ++ 
                   vp.postCompl !pol !np.gn ;
           };
           _ => {
               s = \\pol,anter,tense =>
                   np.nom ++ vp.preCompl !pol !np.gn ++ vp.adv ++
                   ((indicativeForm vp.verb pol) !<tense, anter, np.gn, np.p>) ++ 
                   vp.postCompl !pol !np.gn ;
           }
       };

--     UseCl    : Temp -> Pol -> Cl  -> S ;
    UseCl temp pol cl = {
        s = temp.s ++ pol.s ++ cl.s !pol.p !temp.a !temp.t
    };

--     UseRCl   : Temp -> Pol -> RCl -> RS ;
    UseRCl temp pol rcl = {
        s = \\gn => temp.s ++ pol.s ++ rcl.s !gn !pol.p !temp.a !temp.t
    }; 

--     UseQCl    : Temp -> Pol -> QCl  -> QS ;
    UseQCl temp pol qcl = {
        s = temp.s ++ pol.s ++ qcl.s !pol.p !temp.a !temp.t
    };
    
--     UseSlash : Temp -> Pol -> ClSlash -> SSlash ;
    UseSlash temp pol cls = {
        s = temp.s ++ pol.s ++ cls.s !pol.p !temp.a !temp.t;
        cpl = cls.cpl
    };

--     SlashVP   : NP -> VPSlash -> ClSlash ;      -- (whom) he sees
    SlashVP np vps = {
        s = \\pol,anter,tense => case vps.exp of {
            True => 
              np.nom ++ vps.preCompl !pol !np.gn ++ vps.adv ++
              ((indicativeForm vps.verb pol) !<tense, anter, np.gn, np.p>) ++ 
              vps.postCompl !pol !np.gn;
            False => 
              vps.preCompl !pol !np.gn ++ vps.adv ++
              ((indicativeForm vps.verb pol) !<tense, anter, np.gn, np.p>) ++ 
              vps.postCompl !pol !np.gn ++ np.nom
          };
        cpl = vps.cplCase
    };
    
--     AdvSlash  : ClSlash -> Adv -> ClSlash ;     -- (whom) he sees today
    AdvSlash cls adv = {
        s = \\pol,anter,tense => adv.s ++ cls.s !pol !anter !tense;
        cpl = cls.cpl
    };
    
--     SlashVS   : NP -> VS -> SSlash -> ClSlash ; -- (whom) she says that he loves
    SlashVS np vs ssl = {
        s = \\pol,anter,tense => np.nom ++
            ((indicativeForm vs pol) !<tense, anter, np.gn, np.p>) ++
            [", kad"] ++ ssl.s;
        cpl = ssl.cpl
    };
    
--     ImpVP     : VP -> Imp ;              -- love yourselves
    ImpVP vp = {
        s = \\pol,num => vp.preCompl !pol !MascSg ++
            (imperativeForm vp.verb pol (cast_gennum!<SingPlur Masc, num>) P2) ++ 
            vp.postCompl !pol !MascSg 
    };
    
--     AdvS     : Adv -> S  -> S ;            -- today, I will go home
    AdvS adv s = { s = adv.s ++ s.s };
    ExtAdvS adv s = { s = adv.s ++ "," ++ s.s };
    
--     SlashPrep : Cl -> Prep -> ClSlash ;         -- (with whom) he walks 
    SlashPrep c p = { s=c.s; cpl=p };

--     EmbedS    : S  -> SC ;               -- that she goes
    EmbedS s = { s = [", kad"] ++ s.s } ;

--     EmbedQS   : QS -> SC ;               -- who goes
    EmbedQS s = { s = "," ++ s.s } ;

--     EmbedVP   : VP -> SC ;               -- to go
    EmbedVP vp = {
        s = vp.preCompl !Pos !MascSg ++
            (infinitiveForm vp.verb Pos MascSg) ++ 
            vp.postCompl !Pos !MascSg 
    };

--     RelS     : S -> RS -> S ;              -- she sleeps, which is good
    RelS s rs = ss (s.s ++ rs.s!Neut);
    
--     PredSCVP  : SC -> VP -> Cl ;         -- that she goes is good
    PredSCVP sc vp = {
        s = \\pol,anter,tense =>
            sc.s ++ vp.preCompl !pol !Neut ++ vp.adv ++
            ((indicativeForm vp.verb pol) !<tense, anter, Neut, P3>) ++ 
            vp.postCompl !pol !Neut 
    };
}
