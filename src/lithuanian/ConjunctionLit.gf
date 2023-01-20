--# -path=.:../abstract:../common:../../prelude

-- Adam Slaski, 2009 <adam.slaski@gmail.com>

concrete ConjunctionLit of Conjunction = 
  CatLit ** open ResLit, Coordination, Prelude in {

  flags optimize=all_subs ;  coding=utf8 ;

  lin

    ConjS conj list = {
        s = conj.sent1 ++ list.s1 ++ conj.sent2 ++ list.s2;
    };

    ConjAdv conj list = {
        s = conj.s1 ++ list.s1 ++ conj.s2 ++ list.s2;
        advType = list.advType
    };

-- le fem n'est pas vérifié                                                    
    ConjNP conj list ={
        nom = conj.s1 ++ list.np1.nom ++ conj.s2 ++ list.np2.nom;
        voc = conj.s1 ++ list.np1.voc ++ conj.s2 ++ list.np2.voc;
        dep = \\c => conj.s1 ++ list.np1.dep !c ++ conj.s2 ++ list.np2.dep !c;
        gn  = case <list.np1.gn,list.np2.gn> of {
            <(MascSg|MascPl), _> => MascPl;
            <_, (MascSg|MascPl)> => MascPl;
            <_,_>                => MascPl
        };
        p = case <list.np1.p,list.np2.p> of {
            <P1, _> => P1;
            <_, P1> => P1;
            <P2, _> => P2;
            <_, P2> => P2;
            <_,_>   => P3
        };
        nomType = case <list.np1.nomType,list.np2.nomType> of {
            <Reg,_> => Reg;
            <_,Reg>   => Reg;
            <PersMark,PersMark> => PersMark;
            <_,_>   => Pro
        }
    };

    ConjAP conj list = {
        adv = conj.s1 ++ list.ap1.adv ++ conj.s2 ++ list.ap2.adv;
        s = \\af=>conj.s1 ++ list.ap1.s!af ++ conj.s2 ++ list.ap2.s!af;
	isPost = list.ap2.isPost ---
    };
    ConjRS = conjunctDistrTable GenNum;




-- ---- These fun's are generated from the list cat's.
-- 
    BaseS   = twoSS ;
    ConsS   = consrSS comma ;

    BaseAdv x y = twoSS x y ** { advType = conjAdvType x.advType y.advType } ;
    ConsAdv xs x = consrSS comma xs x ** { advType = conjAdvType xs.advType x.advType } ;

    BaseRS  = twoTable GenNum ;
    ConsRS  = consrTable GenNum comma;

    BaseNP np1 np2 = { np1=np1; np2=np2 };
-- Fem non contrôlé
    ConsNP np npl = { np2=npl.np2; 
        np1 = {
            nom = np.nom ++ "," ++ npl.np1.nom;
            voc = np.voc ++ "," ++ npl.np1.voc;
            dep = \\c => np.dep !c ++ "," ++ npl.np1.dep !c;
            gn  = case <np.gn,npl.np1.gn> of {
                <(MascSg|MascPl), (MascSg|MascPl)> => MascPl;
                <_,_>                          => MascPl
            };
            p = case <np.p,npl.np1.p> of {
                <P1, _> => P1;
                <_, P1> => P1;
                <P2, _> => P2;
                <_, P2> => P2;
                <_,_>   => P3
            }            ;
            nomType = case <np.nomType,npl.np1.nomType> of {
                <Reg,_> => Reg;
                <_,Reg>   => Reg;
                <PersMark,PersMark> => PersMark;
                <_,_>   => Pro
        }
        }
    };
    
    BaseAP  ap1 ap2 =  { ap1=ap1; ap2=ap2 ; isPost = ap2.isPost};
    ConsAP  ap apl = { ap2=apl.ap2; ap1={
        s = \\af=> ap.s!af ++ "," ++ apl.ap1.s!af;
        adv = ap.adv ++ "," ++ apl.ap1.adv ; isPost = apl.isPost
    }  ; isPost = apl.isPost};
  
  lincat

    [S]   = {s1,s2 : Str} ;
    [Adv] = {s1,s2 : Str ; advType : AdvType} ;
    [NP]  = {np1,np2 : NounPhrase} ;
    [AP]  = {ap1,ap2 : AdjPhrase ; isPost : Bool} ;
    [RS]  = {s1,s2 : GenNum => Str} ;

}
