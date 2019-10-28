--1 Bantu auxiliary operations.

interface ResBantu = DiffBantu ** open CommonBantu in {

flags 
  optimize=all ;
  coding=utf8 ;

--2 Constants uniformly defined in terms of language-dependent constants

oper
  npNom : NPCase = NCase Nom ;
  npLoc : NPCase = NCase Loc ;
  npcase2case : NPCase -> Case = \nc -> case nc of {NCase c => c ; _ => Nom} ;
 

 mkIP : Str -> Number -> {s : Str ; n : Number} = \who,n ->
      {
        s = who ;
        n = n
      } ;

  mkNP : (i,my : Str) ->  Cgender -> Number ->Bool-> Person ->  
    {s : NPCase => Str ; a : Agr;isPron:Bool} = \i,my,g,n,b,p -> 
    { s = table {
        NCase Nom => i ;
        _ => my 
        } ;
      a = Ag g n p ;
      isPron=b;
    };

  regNP : Str -> Cgender -> Number ->Bool-> {s : NPCase => Str ; a : Agr; isPron:Bool} = \that,g, n,b-> 
    mkNP that that g n b P3  ;

  mkPron: (i, mine : Str) ->  Cgender -> Number -> Person ->
   {s: PronForm => Str ; a : Agr} = \i,mine, g,n,p -> 
        { s = table { 
            Pers => i;
            Poss n g => case <n,g> of {
              <Sg ,_> => ProunSgprefix g + mine ; 
              <Pl,_> => ProunPlprefix g + mine}       
            } ;
          a = Ag g n p } ; 
  QClause ={s : Polarity => Tense => Anteriority => QForm => Str} ;
  Compvv,Clause : Type;
  
  Compvv,Clause = {s : Polarity => Tense => Anteriority => Str};
  Verb : Type ;
  Verb = { s :VForm =>  Str;
          progV:Str;
          imp : Polarity => ImpForm => Str;
         s1 : Polarity => Tense => Anteriority =>  Agr=> Str };
  NounPhrase  = {s : NPCase => Str ; a : Agr; isPron : Bool} ;
  Preposition={s: Number =>  Cgender => Str; isFused: Bool} ;
  Comp = {s : Agr => Str} ;
  Compl : Type = {s : Str } ;
  VerbPhrase : Type = {  
            s: Agr => Polarity => Tense => Anteriority => Str; --s1 will be used for progresive tense
            compl  : Agr => Str;
            progV:Str;
            imp : Polarity => ImpForm => Str;
            inf: Str};
 SlashVP : Type = VerbPhrase ** {c2 : Preposition} ;
 
 mkVPSlash : Preposition ->  VerbPhrase -> VerbPhrase ** {c2 : Preposition} = \c,vp -> vp ** {c2 = c} ;

  insertComplement : (Agr => Str) -> VerbPhrase -> VerbPhrase= \co,vp -> { 
                  s = vp.s ;
                       progV=vp.progV;
                  compl = \\agr => vp.compl ! agr ++  co ! agr ; 
                  imp =\\po,imf => vp.imp!po!ImpF (getNum imf) (getbool imf);
                  inf= ""};
mkQuestion : {s : Str} -> Clause -> QClause = \wh,cl -> {
      s = \\t,a,p =>
        let
          cls = cl.s ! t ! a ! p ;
          why = wh.s
        in table {
          QDir   => why ++ cls  ;
          QIndir => why ++ cls 
        }
      } ;

      insertObjNP : Preposition -> NounPhrase -> VerbPhrase -> VerbPhrase = \co, np,vp -> 
     {
      s = vp.s;
       progV=vp.progV;
        compl = \\agr => co.s!(nounAgr agr).n !(nounAgr agr).g  ++ vp.compl! agr++ np.s!npNom;
       imp =\\po,imf => vp.imp!po!ImpF (getNum imf) (getbool imf); inf= ""};

    

insertObj: (Agr => Str) -> VerbPhrase -> VerbPhrase = \obj,vp -> {
      s = vp.s ;
      progV=vp.progV;
     imp =\\po,imf => vp.imp!po!ImpF (getNum imf) (getbool imf);
      compl = \\agr => vp.compl ! agr ++ obj ! agr ; 
      inf= vp.inf};

  getNum : ImpForm->  Number = \gn ->
   case gn of { ImpF Sg _ => Sg ; _ => Pl } ;
  getbool : ImpForm->  Bool = \gn ->
   case gn of { ImpF _  False=> False ; _ => True } ;
    cBind : Str = Predef.BIND ;
insertObjPre : (Agr => Str) -> VerbPhrase -> VerbPhrase = \obj,vp -> {
      s = vp.s ;
      progV=vp.progV;
      compl = \\agr => obj ! agr ++ vp.compl ! agr ;
      imp =\\po,imf => vp.imp!po!ImpF (getNum imf) (getbool imf); inf= ""  } ;

insertObjc : (Agr => Str) -> SlashVP -> SlashVP = \obj,vp ->insertObj obj vp ** {c2 = vp.c2} ;



insertAdV : Str -> VerbPhrase -> VerbPhrase = \adv,vp -> {
      s = vp.s ;
      progV=vp.progV;
      compl = \\agr => vp.compl ! agr ++ adv ;
      imp =\\po,imf => vp.imp!po!ImpF (getNum imf) (getbool imf);inf= "" 
         } ;

  --insertPass : Str -> VerbPhrase -> VerbPhrase = \aux,vp -> {
  --    s =  aux ++ vp.s ;
   --   compl = \\agr => aux ++ vp.compl;
   --    isaux=False;    } ; 

   finalComma : Str = pre {"," | "." => []; "" => SOFT_BIND ++ ","; _ => []} ;
  frontComma : Str = SOFT_BIND ++ "," ; 
}

