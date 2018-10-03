--1 Differences between Bantu languages

interface DiffBantu = open CommonBantu, Prelude in {
  flags coding=utf8 ;

-- HL: everything depending on Gender is not common, so it must not 
--     be in CommonBantu, but here in DiffBantu.
oper 
  Gender : PType ;
  firstGender : Gender ; -- G1
  secondGender : Gender ; -- G2

  Noun : Type  = {s : Number => Case => Str ; g : Gender};
  CNoun : Type = {s : Number => Case => Str ; g : Gender; s2 : Number => Str};
  AAgr : Type  = {g : Gender ; n : Number} ;

param 
  Agr =  Ag Gender Number Person ;

oper
--  AGRE = {g : Gender ; n : Number  ; p : Person} ;
  Agre : Type = {g : Gender ; n : Number ;  p : Person} ;
  agre : Gender -> Number -> Person -> Agre = \g,n,p -> {g = g ; n = n ; p = p} ;

  agrFeatures : Agr -> Agre = \a -> case a of {Ag g n p => {g = g ; n = n ; p = p}} ;
  getGender : Agr -> Gender = \a -> case a of {Ag g _ _ => g};
  getNumber : Agr -> Number = \a -> case a of {Ag _ n _ => n};
  getPerson : Agr -> Person = \a -> case a of {Ag _ _ p => p};

  clitAgr : Agr -> {n : Number ; p : Person} = \a -> case a of {
    Ag _ n p => {n = n; p = p} 
    } ;
  complAgr : Agr -> {g : Gender ; n : Number} = \a -> case a of {
    Ag g n _ => {g = g ; n = n} 
    } ;
  predetAgr : Agr -> {g : Gender} = \a -> case a of {
    Ag g _ _ => {g = g} 
    } ;
  verbAgr : Agr -> {g : Gender ; n : Number ; p : Person} = \a -> case a of {
    Ag g n p => {g = g ; n = n  ; p = p} 
    } ; -- verbAgr = agrFeatures, why both? HL

  detAgr : Agr -> {g : Gender ; p : Person} = \a -> case a of {
    Ag g _ p => {g = g; p = p} 
    } ;

  agrG1 : Number -> Person -> Agr = \n,p ->
    Ag firstGender n p ;
  dapagr : Gender -> Person -> Agr = \g,p ->
    Ag g Sg p ;
  agrP3 : Gender -> Number -> Agr = \g,n ->
    Ag g n P3 ;

  aagr : Gender -> Number -> AAgr = \g,n ->
    {g = g ; n = n} ;

  ---- Conjunction Agreements----

  conjAgr : Number -> Agr -> Agr -> Agr = \n,xa,ya -> 
    let 
      x = agrFeatures xa ; y = agrFeatures ya
    in 
    Ag (conjGender x.g y.g) (conjNumber (conjNumber x.n y.n) n) 
       (conjPPerson x.p y.p) ;

  conjGender : Gender -> Gender -> Gender ;

param
 -- AForm = AAdj Gender Number | AComp Gender Number ;
  PronForm= Pers | Poss Number Gender;
  DetForm = Sub | Obj Gender ;

-- HL: the above is material removed from CommonBantu and adapted --

oper
  conjThan  : Str ; --one of them in bantu
  conjThat  : Str ;
  superVery : Str ; -- one of bantu
  
  reflPron : Agr => Str ; -- second of bantu.

param
  VForm ;
  DForm ;
  AForm;
oper
  ProunSgprefix : Gender -> Str ; -- added, HL
  ProunPlprefix : Gender -> Str ; -- added, HL
  Cardoneprefix  : Gender ->  Str;
 Cardtwoprefix  : Gender ->  Str;
 Allpredetprefix : Gender ->  Str;
 PrefixPlNom : Gender ->  Str;
 mkprefix,Ordprefix : Gender ->  Str;
 Cardprefix : Gender ->  Str ;
 Mostpredetprefix : Gender ->  Str;
 Adjpprefix : Gender -> Number ->   Str;
 VowelAdjprefix: Gender -> Number ->   Str;
  ConsonantAdjprefix: Gender -> Number ->   Str;
}

