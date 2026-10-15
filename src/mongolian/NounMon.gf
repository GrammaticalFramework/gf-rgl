concrete NounMon of Noun = CatMon ** open ResMon, Prelude, ParadigmsMon, ExtraMon in {

 flags optimize=all_subs ; coding=utf8 ;

lin
 DetCN det cn = {
    s = \\rc => case <det.isPre,det.isDef> of {
                <True,True> => case rc of {
				Acc => det.s ++ cn.s ! det.n ! NAcc Definite ;
				_ => det.s ++ cn.s ! det.n ! toNCase rc Definite } ;
				<True,False> => case rc of {
				Acc => det.s ++ cn.s ! det.n ! NAcc Indefinite ;
				_ => det.s ++ cn.s ! det.n ! toNCase rc Indefinite } ;
	            _ => cn.s ! det.n ! NNom ++ det.sp ! rc
                } ;
	n = det.n ;
	p = P3 ;
	isPron = False ;
	isDef = det.isDef
    } ;

 UsePron p = {
    s = \\rc => p.s ! PronCase rc ;
    n = p.n ;
	p = p.p ;
	isPron = True ;
	isDef = True
	} ;

 UsePN pn = {
    s = \\rc => pn.s ! rc ; 
    n = Sg ;
    p = P3 ;
    isPron = False ;
	isDef = True
	} ;

 PredetNP predet np = {
    s = \\rc => predet.s ++ np.s ! rc ;
    n = np.n ;
    p = np.p ;
    isPron = np.isPron ;
	isDef = predet.isDef
    } ;
	 
 PPartNP np v2 = {
    s = \\rc => v2.s ! VPART PPast Nom ++ np.s ! rc ;
    n = np.n ;
    p = np.p ;
    isPron = np.isPron ;
	isDef = np.isDef
    } ;
    
 RelNP np rs = np ** {
    s = \\rc => np.s ! rc ++ "," ++ rs.s ! ComplSubj
    } ;

 AdvNP np adv = {
    s = \\rc => adv.s ++ np.s ! rc ;
    n = np.n ;
    p = np.p ;
    isPron = False ;
	isDef = np.isDef
    } ;

 ExtAdvNP np adv = np ** {
    s = \\rc => np.s ! rc ++ "," ++ adv.s
    } ;
    
 DetQuant quant num = {
    s = quant.s ! num.n ++ num.s ;
    sp = \\rc => quant.sp ! num.n ! NNom ++ num.sp ! rc ;
    n = num.n ;
	isDef = quant.isDef ;
    isPre = True
    } ;
     
 DetQuantOrd quant num ord = {
    s = quant.s ! num.n ++ num.s ++ ord.s ;
    sp = \\_ => quant.sp ! num.n ! NNom ++ num.s ++ ord.s ;
    n = num.n ;
	isDef = quant.isDef ;
    isPre = True
    } ;
    
 DetNP det = {
    s = \\rc => det.sp ! rc ;
	n = det.n ;
    p = P3 ;
	isPron = False ;
	 isDef = det.isDef
	} ;

 DetDAP det = {
    s = det.sp ;
    n = det.n ;
    isDef = det.isDef
    } ;

 AdjDAP dap ap = dap ** {
    s = \\rc => ap.s ++ dap.s ! rc
    } ;

 CountNP det np = {
    s = \\rc => det.s ++ np.s ! Abl ;
    n = np.n ; p = np.p ; isPron = False ; isDef = det.isDef
    } ;

 UseN,UseN2 = \noun -> {
    s = \\n,nc => noun.s ! SF n nc
    } ;
    
 PossPron p = {
    s = \\_ => p.s ! PronCase Gen ;
    sp = \\_,nc => p.s ! PronPoss (toRCase nc Definite) ; 
	isDef = True 
    } ;
   
 NumSg = {s = [] ; sp = \\_ => [] ; n = Sg} ;
 NumPl = {s = [] ; sp = \\_ => [] ; n = Pl} ;

 NumCard n = n ** {n = Sg} ;

 NumDigits,NumNumeral,NumDecimal = \numeral -> {
    s = numeral.s ! NCard ;
    sp = \\_ => numeral.s ! NCard ;
    n = numeral.n 
    } ;
  
 OrdDigits, OrdNumeral = \d -> {s = d.s ! NOrd } ;
 
 AdNum a card = {
    s = a.s ++ card.s ;
    sp = \\rc => a.s ++ card.sp ! rc ;
    n = card.n 
    } ;
      
 OrdSuperl a = {
    s = (variants {"хамгийн"|"туйлын"} ++ a.s)
    } ;

-- Mongolian shows a complex system of marking definiteness. Demonstrative, anaphoric and possessive 
-- determiners are used to indicate definiteness. The definite noun phrases are obligatorily accusative 
-- case marked as direct objects. (http://www.ilg.uni-stuttgart.de/projekte/C2/publications/Guntsetseg.pdf)
 DefArt = {
    s = \\n => case n of {
        Sg => "энэ" ; 
        Pl => "эдгээр" 
        } ;
    sp = \\n,nc => case n of {
        Sg => case nc of {
		   NNom => "энэ" ;
           NInst => "үүгээр" ;
		   _ => (regN "үүн").s ! SF Sg nc
		   } ;
        Pl => (regN "эдгээр").s ! SF Sg nc 
		} ;
	isDef = True
    } ;
-- the numeral neg for ‘one’ in the function as an indefinite article (Givon 1981) 
 IndefArt = {
    s = \\_ => [] ;
    sp = \\n,nc => case n of {
	    Sg => (regN "аль нэг").s ! SF Sg nc ;
		Pl => (regN "нэжгээд").s ! SF Sg nc
		} ;
	isDef = False
    } ;
  
 MassNP cn = {
    s = \\rc => cn.s ! Sg ! (toNCase rc Definite) ;
    n = Sg ;
    p = P3 ;
    isPron = False ;
	isDef = True
    } ;

 PossNP cn np = {
    s = \\n,nc => np.s ! Gen ++ cn.s ! n ! nc
    } ;

 PartNP cn np = {
    s = \\n,nc => np.s ! Gen ++ cn.s ! n ! nc
    } ;

 QuantityNP dec mu = {
    s = \\rc => case mu.isPre of {
      True => mu.s ++ dec.s ! NCard ;
      False => dec.s ! NCard ++ mu.s
      } ;
    n = dec.n ; p = P3 ; isPron = False ; isDef = False
    } ;

 Use2N3 n3 = {
    s = n3.s ;
    c2 = n3.c2
    } ;
	  
 Use3N3 n3 = {
    s = n3.s ;
    c2 = n3.c3
    } ;
	  
 ComplN2 n2 compl = {
    s = \\n,nc => appCompl n2.c2 compl.s ++ n2.s ! SF n nc
    } ;

 ComplN3 n3 compl = {
    s  = \\sf => appCompl n3.c2 compl.s ++ n3.s ! sf ;
    c2 = n3.c3
    } ;
  
 AdjCN ap cn = {
    s = \\n,nc => ap.s ++ cn.s ! n ! nc
    } ;
    
 RelCN cn rs = {
    s = \\n,nc => rs.s ! Attributive ++ cn.s ! n ! nc 
    } ;
    
 AdvCN cn adv = {
    s = \\n,nc => adv.s ++ cn.s ! n ! nc
	} ;
    
 SentCN cn s = {
    s = \\n,nc => s.s ++ cn.s ! n ! nc 
    } ;
    
 ApposCN cn np = {
    s = \\n,nc => np.s ! Nom ++ cn.s ! n ! nc
    } ;
    
}

