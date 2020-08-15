--# -path=.:../abstract:../../prelude:../common

instance ParadigmsGus of ParadigmsBantu = DiffGus, CommonBantu ** open  (Predef=Predef),Prelude,MorphoGus, CatGus in {


oper
  --Cgender : Type ; 
   omo_aba  : Cgender ;--%
  omo_eme   : Cgender ;
  e_ci      : Cgender ;
  eri_ama   : Cgender ;
  ege_ebi   : Cgender ;
  oro_ci    : Cgender ;
  aka_ebi   : Cgender ; --%
  abo_ama   : Cgender ;
  oko_ama   : Cgender ;
  ama_ama   : Cgender ;
  aa        : Cgender ;

  Cgender =  MorphoGus.Cgender ; 
  Number =  MorphoGus.Number ;
  Case   =  MorphoGus.NPCase ;
   omo_aba   = G1 ;
  omo_eme   = G2 ;
  e_ci      = G3 ;
  eri_ama   = G4 ;
  ege_ebi   = G5 ;
  oro_ci    = G6 ;
  aka_ebi   = G7 ; 
  abo_ama   = G8 ;
  oko_ama   = G9 ;
  ama_ama    =G11;
  aa        = G10 ;
  
  nominative = npNom ;
  locative = npLoc ;

  --npNumber np = (nounAgr  np.a).n ;



  regN = MorphoGus.regN ; 
  iregN = MorphoGus.iregN ;


  mkPrepof : Number => Cgender => Str = 
    table Number {  Sg => table {
                    G1| G2  => "bwo" ;
                    G3 => "ya";
                    G4 => "ria";
                    G5 => "kia"; --
                    G6 => "rwa";
                    G7 => "ka";
                    G8 => "bwa";
                    G9 => "kwa";
                    G10 => "a";
                    G11 => "aa"
                   }; 
                                 
                   Pl => table { G1 => "ba" ;
                    G2  => "ya" ;
                    G3|G6  => "chia";
                    G4  |G8|G9|G10 => "a";
                    G5  => "bi"; --
                    G11 => "aa";
                    G7 => "bia"} } ;
                       

 
    verb2snoun : Verb ->  Cgender -> Noun = \v,g->    
    let wp = "mu" + init(v.s ! VGen) +"ji" ;
        wpl = "wa" + init(v.s ! VGen) +"ji" in 
    iregN wp wpl g ;
 

  regA : Str -> A = \s -> lin A (MorphoGus.regA s) ;
  cregA : Str -> A = \s -> lin A (MorphoGus.cregA s) ;
   iregA : (fat,fatter : Str) -> A =\a,b -> lin A (MorphoGus.iregA a b);
  {-mkA = overload {
    mkA : Str -> A = \a -> lin A (regA a)|lin A (cregA a) ;
    mkA : (fat,fatter : Str) -> A =\a,b -> lin A (iregA a b);
    } ;

  prepA2 : A -> Prep -> A2 ;

mkA2 = overload {
    mkA2 : A -> Prep -> A2   = prepA2 ;
    mkA2 : A -> Str -> A2    = \a,p -> prepA2 a (mkPrep p False) ;
    mkA2 : Str -> Prep -> A2 = \a,p -> prepA2 (regA a) p;
    mkA2 : Str -> Str -> A2  = \a,p -> prepA2 (regA a) (mkPrep p False);
  } ;
  
-}
  mkV = overload {
    mkV : (cry : Str) -> V=\v-> lin V (regV v ) ; -- regular, incl. cry-cries, kiss-kisses etc
   mkV : Str -> Str ->Str ->Str ->Str ->V =\v,v1,v2,v3,v4 -> lin V (iregV v v1 v2 v3 v4 ) ;  -- fix compound, e.g. under+take
  };
  iregV =MorphoGus.iregV;
  regV=MorphoGus.regV ;
  mkV2 = overload {
        mkV2  : Str -> V2 = \s -> dirV2 (regV s) ;   
        mkV2  : V -> V2 = dirV2 ;
        mkV2  : V -> Prep -> V2 = prepV2  ;
  };


  regPN    : Str ->Cgender -> PN ;          
   nounPN : N -> PN ;



} 
