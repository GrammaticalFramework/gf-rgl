--# -path=.:../abstract:../common:../prelude

-- Adam Slaski, 2009, 2010 <adam.slaski@gmail.com>

concrete NounLit of Noun = CatLit ** open ResLit, Prelude, MorphoLit, Predef in {

  flags optimize=all_subs ;

  lin
-- CN = { s : Number => Case => Str; g : Gender };
-- Determiner : Type = { s : Case => Gender => Str; n: Number; a:Case => Case };
-- NounPhrase : Type = { s : PronForm => Str; n : Number; g: Gender; p : Person };

    DetCN det noun = {
      nom = (det.s ! Nom  ! noun.g) ++ (noun.s ! det.nb ! (accom_case! <det.numAgr,Nom,noun.g>));
      voc = (det.s ! VocL ! noun.g) ++ (noun.s ! det.nb ! (accom_case! <det.numAgr,VocL,noun.g>));
      dep = \\cc => let c = extract_case! cc in 
        (det.s ! c ! noun.g) ++ (noun.s ! det.nb ! (accom_case! <det.numAgr,c,noun.g>));
      gn = cast_gennum!<noun.g, det.nb>;
      p = P3;
      nomType = noun.nomType 
    } ;

--     DetNP   : Det -> NP ;  -- these five
-- unfortunately as def and indefart linearize to [] DetNP leads to placeing 
-- [] nominale phrases everywhere
-- if you want to parse with this grammar better comment this function
-- Masc only...

{-
    DetNP det = 
    {
      nom = (det.s ! Nom  ! SingPlur Masc);
      voc = (det.s ! VocL ! SingPlur Masc);
      dep = \\cc => let c = extract_case! cc in 
        (det.s ! c ! SingPlur Masc);
      gn = cast_gennum!<SingPlur Masc, det.nb>;
      p = P3;  
      isPron = True -- On ne sait pas...
    } ;
-}
    DetNP det = {
        nom = case det.detType of {
          EmptyIndef => a_Det.s ! Nom ! SingPlur Masc ++ det.s ! Nom ! SingPlur Masc;
          EmptyDef => a_Det.s ! Nom ! SingPlur Masc ++ det.s ! Nom  ! SingPlur Masc;
          _ => det.s ! Nom ! SingPlur Masc
        } ;
        voc = case det.detType of {
          EmptyIndef => a_Det.s ! VocL ! SingPlur Masc ++ det.s ! VocL ! SingPlur Masc;
          EmptyDef => the_Det.s ! VocL ! SingPlur Masc ++ det.s ! VocL ! SingPlur Masc;
          _ => det.s ! VocL ! SingPlur Masc
        } ;
        dep = case det.detType of {
          EmptyIndef => \\cc => let c = extract_case ! cc in (a_Det.s ! c ! SingPlur Masc ++ det.s ! c ! SingPlur Masc);
          EmptyDef => \\cc => let c = extract_case ! cc in (the_Det.s ! c ! SingPlur Masc ++ det.s ! c ! SingPlur Masc);
          _ => \\cc => let c = extract_case ! cc in (det.s ! c ! SingPlur Masc)
        } ;
        gn = cast_gennum!<SingPlur Masc, det.nb>;
        p = P3;  
        nomType = Pro -- On ne sait pas...
      } ;


-- surface structures of NP formed with MassNP, DefArt and IndefArt are identical
    MassNP piwo = {
      nom = piwo.s! Sg ! Nom;
      voc = piwo.s! Sg ! VocL;
      dep = \\cc => piwo.s ! Sg ! (extract_case! cc) ;
      gn = cast_gennum! <piwo.g, Sg>;
      p = P3 ;
      nomType = Reg
    } ;

    UsePron p = p;

    AdjCN adj noun = {
      s = \\n,c => (adj.s !(cast_aform_exp!<noun.g,n,c>)) ++ (noun.s ! n ! c) ;
      g = noun.g;
      nomType = Reg
    };

    
--     AdvCN   : CN -> Adv -> CN ;   -- house on the hill
{-
    AdvCN cn a = {
      s = \\n,c =>   (cn.s ! n ! c) ++ a.s;
      g = cn.g    
    };
    -}
    AdvCN cn a = 
      case a.advType of {
        OtherT => {
          s = \\n,c =>   (cn.s ! n ! c) ++ a.s;
          g = cn.g ;
--          p = cn.p;
          nomType = cn.nomType  -- Pronoun NP has limitations...
        };
        _ => {
          s = \\n,c => a.s ++ (cn.s ! n ! c);
          g = cn.g ;
--          p = cn.p;
          nomType = cn.nomType -- Pronoun NP has limitations...
        }
     } ;
    
--     AdvNP   : NP -> Adv -> NP ;    -- Paris today
    AdvNP np a = 
      case a.advType of {
        OtherT => {
          nom = np.nom ++ a.s;
          voc = np.voc ++ a.s;
          dep = \\c => np.dep!c ++ a.s;
          gn = np.gn;
          p = np.p;
          nomType = np.nomType -- Pronoun NP has limitations...
        };
        _ => {
          nom = a.s ++ np.nom;
          voc = a.s ++ np.voc;
          dep = \\c => a.s ++ np.dep!c;
          gn = np.gn;
          p = np.p;
          nomType = np.nomType -- Pronoun NP has limitations...
        }
     } ;

-- surface structures of NP formed with MassNP, DefArt and IndefArt are identical
--    DefArt =   {s = \\_=>[] ; sp = (mkPronXis "šis").sp }; 
--    IndefArt = {s = \\_=>[] ; sp = (mkPronXs "kažkoks").sp };

    UseN  sb = {
      s = \\n,c => sb.s ! SF n c; 
      g = sb.g;
      nomType = Reg      
    } ;

  -- : Pron -> Quant ;    -- my (house)
    PossPron p = {
      s = p.possForms ;
      detType = NormalDet;
      nomType = Pro
    };
        
    NumSg = { s = \\_,_ => ""; numAgr = AgrComb; nb = Sg; hasCard = False };
    NumPl = { s = \\_,_ => ""; numAgr = AgrComb; nb = Pl; hasCard = False };

  -- : Quant -> Num -> Det ;  -- these five    
    DetQuant q num = {
      s = \\c,g => q.s ! (cast_aform_exp!<g,num.nb,(accom_case!<num.numAgr,c,g>)>) ++ num.s !c !g;
      detType = q.detType; 
      nb = num.nb;
      numAgr = num.numAgr
    };
    
  -- : Quant -> Num -> Ord -> Det ;  -- these five best
    DetQuantOrd q num ord = {
      s = \\c,g => q.s ! (cast_aform_exp!<g,num.nb,(accom_case!<num.numAgr,c,g>)>)
        ++ num.s !c !g
        ++ ord.s ! (cast_aform_exp!<g,num.nb,(accom_case!<num.numAgr,c,g>)>) ;
      detType = q.detType; 
      nb = num.nb;
      numAgr = AgrComb
    };
    
    OrdSuperl a = {
        s = mkAtable a.super
    };

--          : N2 -> NP -> CN ;    -- mother of the king - karaliaus motina (but may be with various cases)
      ComplN2 n2 np = 
        case n2.cplCase.cas of {  
          GenC => -- case isNil n2.cpl.prep of {
--            True => 
            {
              s = \\n,c => n2.s ! SF n c ++ n2.cplCase.s ++ np.dep ! n2.cplCase.cas;
              g = n2.g;
              nomType = n2.nomType
--            };
--            _ => {
--              s = \\n,c => n2.cpl.prep ++ np.dep ! n2.cpl.cas ++ n2.s ! SF n c;
--              g = n2.g;
--              isPron = n2.isPron
--            }
          };
          _ => {
            s = \\n,c => n2.s ! SF n c ++ n2.cplCase.s ++ np.dep ! n2.cplCase.cas;
            g = n2.g;
            nomType = n2.nomType
          }
        };
    
    ComplN3 n3 np = {
      s =
        \\sf => n3.s ! sf ++ n3.cplCase.s ++ np.dep ! n3.cplCase.cas ;
      cplCase = n3.cplCase2;
      g = n3.g;
      nomType = n3.nomType
    };

    UseN2 n2 = {
      s = \\n,c => n2.s ! SF n c;
      g = n2.g;
      nomType = Reg
    };
    
    Use2N3 n3 = {
      s = n3.s;
      g = n3.g;
      cplCase = n3.cplCase;
      nomType = Reg
    };

    Use3N3 n3 = {
      s = n3.s;
      g = n3.g;
      cplCase = n3.cplCase2;
      nomType = Reg
    };

    
    RelNP np rs = {
      nom = np.nom ++ rs.s ! np.gn  ++ finalComma;
      voc = np.voc ++ rs.s ! np.gn ++ finalComma;
      dep = \\cc => np.dep !cc ++ rs.s ! np.gn  ++ finalComma;
      gn = np.gn;  
      p = np.p ;
      nomType = Reg
    };

    RelCN cn rs = {
      s = \\n,c => cn.s ! n ! c ++ "," ++ rs.s ! (cast_gennum!<cn.g,n>) ++ finalComma;
      g = cn.g ;
      nomType = Reg
    } ;
    
--     PPartNP : NP -> V2  -> NP ;    -- the man seen
    PPartNP np v2 = {
      nom = np.nom ++ (mkAtable (table2record v2.passPastPart)) ! (cast_aform!<np.gn,Nom>);
      voc = np.voc ++ (mkAtable (table2record v2.passPastPart)) ! (cast_aform!<np.gn,VocL>);
      dep = \\cc => np.dep !cc ++ (mkAtable (table2record v2.passPastPart)) ! (cast_aform!<np.gn,(extract_case!cc)>) ;
      gn = np.gn;
      p = np.p;
      nomType = Reg
    };
    

--     NumNumeral : Numeral -> Card ;  -- fifty-one
    NumNumeral n = { s=n.s; numAgr=n.numAgr; nb=n.nb };
    
--     NumDigits  : Digits  -> Card ;  -- 51
    NumDigits n =  { s=\\_,_ => n.s; numAgr=n.numAgr; nb=n.nb };
    NumDecimal n =  { s=\\_,_ => n.s; numAgr=n.numAgr; nb=n.nb };
    
--     NumCard : Card -> Num ;
    NumCard c = c ** { hasCard = True };

--     OrdDigits  : Digits  -> Ord ;  -- 51st
    OrdDigits n = { s=\\_=>n.o };

--     OrdNumeral : Numeral -> Ord ;  -- fifty-first
    OrdNumeral n = { s=n.o };
    
--     AdNum : AdN -> Card -> Card ;   -- almost 51
    AdNum ad c = { s = \\x,y=>ad.s ++ c.s!x!y; numAgr=c.numAgr; nb=c.nb };


--     PredetNP : Predet -> NP -> NP; -- only the man 
    PredetNP p np = case p.adj of {
      False => { 
        voc = p.np.voc ++ np.dep!GenC; 
        nom = p.np.nom ++ np.dep!GenC; 
        dep = \\c=> p.np.dep!c ++ np.dep!GenC; 
        n   = p.np.n; 
        gn  = p.np.gn; p=p.np.p; nomType = np.nomType };
      True => {
        voc = p.s!(cast_aform!<np.gn,VocL>) ++ np.dep!GenC; 
        nom = p.s!(cast_aform!<np.gn,Nom>) ++ np.dep!GenC; 
        dep = \\c=> p.s!(cast_aform!<np.gn,(extract_case!c)>) ++ np.dep!c; 
        n   =np.n; 
        gn  =np.gn; p=np.p; nomType = np.nomType }
      };

    UsePN n = n;
    
--     ApposCN : CN -> NP -> CN ;    -- city Paris (, numbers x and y)
    ApposCN cn np = {
        s= \\n,c=> cn.s!n!c ++ np.nom;
        g= cn.g;
        nomType = cn.nomType
    };


  -- : CN -> NP -> CN ;     -- house of Paris, house of mine
  PossNP cn np = {
          s = \\n,c => np.dep ! GenC ++ cn.s ! n ! c;
          g = cn.g;
          p = cn.p;
          nomType = cn.nomType -- Pronoun NP has limitations...
        } ;

  -- : CN -> NP -> CN ;     -- glass of wine - стакан чаю (чая)
  PartNP cn np = {
--          nom = cn.nom ++ np.dep ! GenC;
--          voc = cn.voc ++ np.dep ! GenC;
          s = \\n,c => cn.s ! n ! c ++ np.dep ! GenC;
          g = cn.g;
          p = cn.p;
          nomType = cn.nomType -- Pronoun NP has limitations...
        };

--     SentCN  : CN -> SC  -> CN ;   -- question where she sleeps
    SentCN cn sc = {
--        s= \\n,c=> cn.s!n!c ++ sc.s!cn.g!n ++ finalComma;
        s= \\n,c=> cn.s!n!c ++ sc.s ++ finalComma;
        g= cn.g;
        nomType = cn.nomType
    };

    DetDAP d = d ;


  --  : Quant ;       -- the (house), the (houses)
      DefArt = {
      s = \\_=>[];
      detType=EmptyDef ;
      nb = Sg;
      numAgr = AgrComb
    } ;
  -- : Quant ;       -- a (house), (houses)
    IndefArt = {
      s = \\_=>[];
      detType=EmptyIndef ;
      nb = Sg;
      numAgr = AgrComb
    } ;

}
