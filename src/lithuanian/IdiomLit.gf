--# -path=.:../abstract:../common:../prelude

-- Adam Slaski, 2009 <adam.slaski@gmail.com>

concrete IdiomLit of Idiom = CatLit ** open Prelude, ResLit, MorphoLit in {

 flags optimize=all_subs ;  coding=utf8 ;

 lin
    
--     ImpersCl  : VP -> Cl ;        -- it is hot
    ImpersCl vp = {
        s = \\pol,anter,tense =>
            vp.preCompl !pol !Neut ++ vp.adv ++
            ((indicativeForm vp.verb pol) !<tense, anter, Neut, P3>) ++ 
            vp.postCompl !pol !Neut 
    };    

--     ImpPl1    : VP -> Utt ;       -- let's go
    ImpPl1 vp = {
        s = vp.preCompl !Pos !MascPl ++ vp.adv ++
            (imperativeForm vp.verb Pos MascPl P1) ++ 
            vp.postCompl !Pos !MascPl 
    };
    
--     GenericCl : VP -> Cl ;        -- one sleeps
    GenericCl vp = {
        s = \\pol,anter,tense =>
            "ktoś" ++ vp.preCompl !pol !MascPl ++ vp.adv ++
            ((indicativeForm vp.verb pol) !<tense, anter, MascSg, P3>) ++ 
            vp.postCompl !pol !MascSg 
    };

--     CleftNP   : NP  -> RS -> Cl ; -- it is I who did it
    CleftNP  np rs = {s=\\pol,_,_ => "to" ++ (case pol of {Neg=>"nie";Pos=>""}) ++ np.nom ++ rs.s!np.gn };

--     CleftAdv  : Adv -> S  -> Cl ; -- it is here she slept
    CleftAdv adv s = {s=\\_,_,_ => adv.s ++ s.s };

--     ExistNP   : NP -> Cl ;        -- there is a house
    ExistNP np = {s=\\pol,anter,tense => case pol of { 
        Pos=> yraOp ! <np.gn, np.p, tense, anter> ++ np.nom;
        Neg=> neraOp!<tense,anter> ++ np.dep!GenC } };
    
--     ExistIP   : IP -> QCl ;       -- which houses are there
    ExistIP ip = {s=\\pol,anter,tense => case pol of {
        Pos=>ip.nom ++ yraOp ! <ip.gn, ip.p, tense, anter>;
        Neg=>ip.dep!GenC ++ neraOp!<tense,anter>} };

--     ProgrVP   : VP -> VP ;        -- be sleeping
    ProgrVP vp = {
        preCompl=vp.preCompl; adv=vp.adv; postCompl=vp.postCompl;
        withCopula = vp.withCopula; exp=vp.exp;
        verb= { forms = vp.verb.forms;
            refl=vp.verb.refl;
            asp=vp.verb.asp;
            passPastPart=vp.verb.passPastPart;
            actPastPart=vp.verb.actPastPart;
            actPastFreqPart=vp.verb.actPastFreqPart;
            actPresPart=vp.verb.actPresPart;
            actFutPart=vp.verb.actFutPart;
             }
        };
} ;
