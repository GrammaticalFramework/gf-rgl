incomplete concrete NounBantu of Noun = 
  -- HL: Structural added for part_Prep, possess_Prep; contains Cat
  CatBantu ** open Structural, ResBantu, Prelude in {

flags optimize=all_subs ; coding = utf8 ;

lin
  -- Det -> CN -> NP
  -- e.g. 'the man'
  
  DetCN det cn =let g = cn.g ; n = det.n in {
    s = \\c => det.s!Sub ++ cn.s ! det.n !npcase2case c ++ det.s!Obj g ++ cn.s2!det.n;
    a =Ag cn.g det.n P3 ;
    } ;

  
  -- PN -> NP
  -- e.g. 'John'
  UsePN pn = {s = \\c => pn.s !npcase2case c ; a = Ag pn.g  Sg P3 } ;
  -- Pron -> NP
  -- e.g. 'he'
  UsePron pron = let agr = verbAgr pron.a;
                     n=agr.n; g=agr.g 
    in {s = table {
          NCase c => pron.s!Pers ;
          NPAcc => pron.s!Pers  ;
          NPNomPoss => pron.s!Poss n g };
        a = Ag agr.g agr.n agr.p;                             
    } ; 
  -- Predet -> NP -> NP
  -- e.g. 'only the man'
    
  PredetNP pred np = 
    let agr = predetAgr np.a  in {
      s = \\c =>   np.s ! NCase Nom ++ pred.s ! agr.g   ;
      a =Ag agr.g Pl P3 ;
    } ; 

   {-} PPartNP np v2 = {
      s = \\c => np.s ! c ++ v2.s ! VPPart ;
      a = np.a
      } ;
 -}
  RelNP  np rs = {
    s = \\c => np.s ! NCase Nom ++ frontComma ++ rs.s ! np.a ++ finalComma ;
    a = np.a
    } ;
  -- NP -> Adv -> NP
  -- e.g. 'Paris today'
  AdvNP np adv = {
    s = \\c => np.s ! NCase Nom ++ adv.s ;
    a = np.a
    } ;

  ExtAdvNP np adv = {
    s = \\c => np.s ! NCase Nom ++ embedInCommas adv.s ;
    a = np.a
    } ;

  DetQuant quant num = { s = table{ Sub => [];
                                    Obj g=>quant.s ! num.n! g    ++ num.s !g }; 
                         n  = num.n  } ;

  DetQuantOrd quant num ord ={ s = table{ Sub => []; 
                            Obj g=>quant.s ! num.n! g  ++  num.s! g  ++ ord.s ! g }; 
                          n  = num.n  } ;
  DetNP det =  { s = \\c => det.s!Obj G1 ; 
         a = agrP3  G1 det.n } ;  
  
  PossPron pron = { s = \\n,g => pron.s!Poss n g } ;
  
  NumSg = {s = \\_ => []; n = Sg };--; hasCard = False} ;
  NumPl = {s = \\_ => []; n = Pl };--; hasCard = False} ;
--b    NoOrd = {s = []} ;

  NumCard n = n  ;--** {hasCard = True} ;

  NumDigits n = {s = n.s ! NCard ; n = n.n} ;
  OrdDigits n = { s =  n.s ! NOrd} ;

  NumNumeral numeral = {s = numeral.s ! NCard; n = numeral.n} ;
  OrdNumeral numeral = {s = numeral.s ! NOrd} ;

  AdNum adn num = {s = \\g => adn.s ++ num.s!g ; n = num.n} ;

    OrdSuperl a ={s = \\g =>  a.s! AAdj g Sg ++ superVery} ;-- find how to include plular

 OrdNumeralSuperl n a = {s = \\g => n.s ! NOrd !g ++ a.s !AAdj g Sg } ;--what PL
   --  DefArt = {  s  = \\n,g => []} ; --what PL
  IndefArt, DefArt = {  s  = \\n,g =>[] } ;
     --IndefArt = {s = \\ n,g => artIndef } ;
    
  -- CN -> NP
  MassNP cn = let g = cn.g ; n = Sg | Pl in {
    s = \\c => cn.s ! n! npcase2case c;
    a = Ag g n P3 ;
    } ;
  UseN n = { s = n.s ; s2 = \\_ => [] ; g = n.g} ; --n
  UseN2 n = { s = n.s ; s2 = \\_ => [] ; g = n.g} ;--n ;
  UseN3 n = { s = n.s ; s2 = \\_ => [] ; g = n.g} ; --n ;

  Use2N3 f = {
    s = \\n,c => f.s ! n ! Nom ;
    s2 = \\_ => [] ;
    g = f.g ;
    c2 = f.c2
    } ;

  Use3N3 f = {
    s = \\n,c => f.s ! n ! Nom ;
    s2 = \\_ => [] ;
    g = f.g ;
    c2 = f.c3
    } ;

  ComplN2 n2 np = {s = \\n,c => n2.s ! n ! Nom ++ n2.c2.s!n!n2.g ++ np.s ! NCase Nom ;
                   s2 = \\_ => [] ;
                   g = n2.g 
    }; 
  ComplN3 n3 np = {
    s = \\n,c => n3.s ! n ! Nom ++ n3.c2.s!n!n3.g ++ np.s ! NCase Nom;
    g = n3.g ;
    c2 = n3.c3
    } ;

  AdjCN ap cn = {s = cn.s ; g = cn.g;
                 s2 = \\n =>cn.s2! n ++ ap.s ! cn.g ! n} ;
     
  RelCN cn rs = {
    s = \\n,c => cn.s ! n ! Nom ++ rs.s ! Ag cn.g n P3 ; s2 =\\n => []; --another persons
    g = cn.g
    } ; 
  -- AP -> CN -> CN
  -- e.g. 'big house'
  AdvCN cn ad = {s = \\n,c => cn.s ! n ! Nom ++ ad.s ;s2 =\\n => []; g = cn.g} ;

  SentCN cn sc = {s = \\n,c => cn.s ! n ! Nom ++ sc.s ; s2 =\\n => []; g = cn.g} ;

  -- ApposCN cn np = {s = \\n,c => cn.s ! n ! Nom ++ np.s ! NCase Nom ; s2 =\\n => []; g = cn.g} ;
 
  -- PossNP : CN -> NP -> CN
  -- e.g. 'house of Paris', 'house of mine'
  PossNP cn np =let agr = detAgr np.a in
    {s = \\n,c => cn.s ! n ! Nom ++ possess_Prep.s! n!cn.g  ++ np.s ! NPNomPoss; 
     s2 =\\n => []; g = cn.g} ;
  -- PartNP : CN -> NP -> CN
  -- e.g. 'glass of wine'
  PartNP cn np = {s = \\n,c => cn.s ! n ! Nom ++ part_Prep.s! n!cn.g  ++ np.s ! NPAcc ; s2 =\\n => []; g = cn.g} ;
    
  -- CountNP : Det -> NP -> NP
  -- e.g. 'three of them', 'some of the boys'
  CountNP det np = let  agr = verbAgr np.a 
    in {
      s = \\c => det.s!Obj agr.g ++ part_Prep.s!agr.n!agr.g ++ np.s!c ;--NPAcc was removed
      a = Ag agr.g agr.n agr.p
    } ;

   
  AdjDAP det ap = { s = table{ Sub => [] ; 
                               Obj g =>det.s!Obj g ++ ap.s!g !det.n}; 
                    n = det.n  };
      
  DetDAP d = { s=d.s; n=d.n };
    
  ApposCN cn np = let agr = complAgr np.a in 
    {s = \\n,c => np.s ! NCase Nom --++ possess_Prep.s!n!agr.g 
       ++ cn.s !n ! Nom   ; s2 =\\n => ""; g = cn.g} ;
  
}
