--# -path=.:../abstract:../common:../../prelude

concrete ConjunctionRus of Conjunction =
  CatRus ** open ResRus, Coordination, Prelude in {

  flags optimize=all_subs ;  coding=utf8 ;

  lincat
    [Adv] = {s1,s2 : Str} ;
    [IAdv] = {s1,s2 : Str} ;
    [AdV] = {s1,s2 : Str} ;
    [AP] = {s1,s2 : AdjTable ;
      short1,short2 : AgrTable ;
      isPost : Bool;
      preferShort : ShortFormPreference
      } ;
    [DAP] = {s1,s2 : DetTable ;
      c : Case ;
      size : NumSize
      } ;
    [NP] = {s1,s2 : Case => Str ;
      -- prep1,prep2 : Case => Str ;
      pron : Bool ;
      a : Agr
      } ;
    [CN] = {s1,s2 : Number => Case => Str ;
      g : Gender ;
      mayben : MaybeNumber ;
      anim : Animacy
      } ;
    [S] = {s1,s2 : Mood => Str} ;
    [RS] = {s1,s2 : AdjTable} ;

  lin
    -- : Adv -> Adv -> ListAdv ;     -- here, there
    BaseAdv = twoSS ;

    -- : Adv -> ListAdv -> ListAdv ; -- here, there, everywhere
    ConsAdv = consrSS comma ;

    -- : AdV -> AdV -> ListAdV ;     -- always, sometimes
    BaseAdV = twoSS ;
    -- : AdV -> ListAdV -> ListAdV ; -- always, sometimes, never
    ConsAdV = consrSS comma ;

    -- : IAdv -> IAdv -> ListIAdv ;     -- where, when
    BaseIAdv = twoSS ;
    -- : IAdv -> ListIAdv -> ListIAdv ; -- where, when, why
    ConsIAdv = consrSS comma ;

    -- : AP -> AP -> ListAP ;       -- red, white
    BaseAP x y = twoTable3 GenNum Animacy Case x y ** {
      short1 = x.short ;
      short2 = y.short ;
      isPost = orB x.isPost y.isPost;
      preferShort = selectAPForm x.preferShort y.preferShort
      } ;

    -- ConsAP : AP -> ListAP -> ListAP ;   -- red, white, blue
    ConsAP x xs = consrTable3 GenNum Animacy Case comma x xs ** {
      short1 = \\ag=> x.short ! ag ++ comma ++ xs.short1 ! ag ;
      short2 = xs.short2 ;
      isPost = orB x.isPost xs.isPost ;
      preferShort = selectAPForm x.preferShort xs.preferShort
      } ;

    -- : DAP -> DAP -> ListDAP ;       --
    BaseDAP x y = twoTable3 Gender Animacy Case x y ** {
      c = y.c ;
      size = conjSize x.size y.size ;  -- different genders -> plural?
      } ;

    -- : DAP -> ListDAP -> ListDAP ;   --
    ConsDAP x xs = consrTable3 Gender Animacy Case comma x xs ** {
      g = xs.g ;  --?
      c = xs.c ;  -- ?
      size = xs.size  -- different genders -> plural?
      } ;

    -- : Conj -> ListDAP -> Det ;   -- his or her
    ConjDet conj xs = {
      s=\\g,anim,cas => conj.s1 ++ xs.s1 ! g ! anim ! cas ++ conj.s2 ++ xs.s2 ! g ! anim ! cas ;
      type=NormalDet ; -- hopefully ok to drop empty cases
      g=xs.g ;
      c=xs.c ;
      size=xs.size
      } ;

    -- : S -> S -> ListS ;      -- John walks, Mary runs
    BaseS = twoTable Mood ;

    -- : S -> ListS -> ListS ;  -- John walks, Mary runs, Bill swims
    ConsS = consrTable Mood comma ;

    -- : RS -> RS -> ListRS ;       -- who walks, whom I know
    BaseRS x y = twoTable3 GenNum Animacy Case x y ** {c = y.c} ;

    -- : RS -> ListRS -> ListRS ;   -- who walks, whom I know, who is here
    ConsRS xs x = consrTable3 GenNum Animacy Case comma xs x ** {c = xs.c} ;

    -- : Conj -> ListAdv -> Adv ;   -- here or there
    ConjAdv = conjunctDistrSS ;
    -- : Conj -> ListIAdv -> IAdv ;   -- where or why
    ConjIAdv = conjunctDistrSS ;
    -- : Conj -> ListAdV -> AdV ;   -- always or sometimes;
    ConjAdV conj xs = {
      s = conj.s1 ++ xs.s1 ++ conj.s2 ++ xs.s2 ;
      p = Pos
      } ;

    -- : Conj -> ListAP -> AP ;     -- cold and warm
    ConjAP conj xs = conjunctDistrTable3 GenNum Animacy Case conj xs ** {
      short = \\ag => conj.s1 ++ xs.short1 ! ag ++ conj.s2 ++ xs.short2 ! ag ;
      isPost = xs.isPost;
      preferShort = xs.preferShort
      } ;

    ConjS conj ss = conjunctDistrTable Mood conj ss ;

    -- : Conj -> ListRS -> RS ;     -- who walks and whose mother runs
    ConjRS conj ss = conjunctDistrTable3 GenNum Animacy Case conj ss ** {
      c = ss.c
      } ;

    -- : CN -> CN -> ListCN ;      -- man, woman
    BaseCN x y = {
      s1 = x.s ;
      s2 = y.s ;
      g = conjGender x.g y.g ;
      mayben = JustPl ;
      anim = conjAnim x.anim y.anim
      } ;

    -- : CN -> ListCN -> ListCN ;  -- man, woman, child
    ConsCN x xs = consrTable2 Number Case comma x xs ** {
      g = conjGender x.g xs.g ;
      mayben = JustPl ;
      anim = conjAnim x.anim xs.anim
      } ;

    -- : Conj -> ListCN -> CN ;     -- man and woman
    ConjCN conj xs = {
      s = \\n,cas => conj.s1 ++ xs.s1 ! n ! cas ++ conj.s2 ++ xs.s2 ! n ! cas ;
      g = xs.g ;
      mayben = JustPl ;
      anim = xs.anim ;
      rel = (guessAdjectiveForms "");
      rt = GenType ;
    } ;

    -- : NP -> NP -> ListNP ;      -- John, Mary
    BaseNP x y = {
      s1 = x.s ;
      s2 = y.s ;
    --  prep1 = x.prep ;
    --  prep2 = y.prep ;
      pron = y.pron ; --???
      a = y.a
    } ;

    -- : NP -> ListNP -> ListNP ;  -- John, Mary, Bill
    ConsNP x xs = {
      s1 = \\c => x.s ! c ++ comma ++ xs.s1 ! c ;
      s2 = xs.s2 ;
      --prep1 = \\c => x.prep ! c ++ comma ++ xs.prep1 ! c ;
      --prep2 = xs.prep2 ;
      a = conjAgr x.a xs.a ;
      pron = xs.pron ;
      anim = conjAnim x.anim xs.anim
    } ;

    -- : Conj -> ListNP -> NP ;     -- she or we
    ConjNP conj xs = {
      s = \\c => conj.s1 ++ xs.s1 ! c ++ conj.s2 ++ xs.s2 ! c ;
      a = case conj.n of {
            Sg => xs.a ;
            Pl => case xs.a of {Ag gn p => Ag GPl p}
          } ;
      pron = xs.pron ;
      anim = xs.anim
    } ;

  oper
    conjGender : Gender -> Gender -> Gender = \_,m -> m ;
    conjAnim : Animacy -> Animacy -> Animacy = \m,n -> case <m,n> of {
      <Inanimate,Inanimate> => Inanimate ;
      _ => Animate
    } ;
    conjNumber : Number -> Number -> Number = \m,n -> case <m,n> of {
      <Sg,Sg> => Sg ;
      _ => Pl
    } ;
    conjSize : NumSize -> NumSize -> NumSize = \m,n -> n ;   -- TODO: check latest win?
    conjGenNum : GenNum -> GenNum -> GenNum = \m,n -> case <m,n> of {
      <GSg Fem,GSg Fem> => FSg ;
      <GSg Masc,GSg Masc> => MSg ;
      <GSg Neut,GSg Neut> => NSg ;
      <GSg _,GSg _> => GPl;  -- TODO: Is this true for animate only or in general?
      _ => GPl
    } ;
    conjAgr : Agr -> Agr -> Agr
      = \a1,a2 ->
        let a1rec = case a1 of {Ag gn p => {gn=gn; p=p} } in
        let a2rec = case a2 of {Ag gn p => {gn=gn; p=p} } in
        Ag (conjGenNum a1rec.gn a2rec.gn) (conjPerson a1rec.p a2rec.p);
    selectAPForm : ShortFormPreference -> ShortFormPreference -> ShortFormPreference
      = \sfp1,sfp2 ->
      case <sfp1,sfp2> of {
        <PrefShort, PrefShort> => PrefShort ;
        _ => PreferFull
        } ;
}
