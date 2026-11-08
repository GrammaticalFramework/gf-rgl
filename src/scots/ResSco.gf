resource ResSco = ResEng - [getCompar,getSuperl,artIndef,auxBe,posneg,mkVerbForms,
                            nonAuxVerbForms,auxVerbForms,infVP,
                            reflPron,possPron,predV,predVc,predVV,mkClause,vfn] ** open Prelude in {

oper
  artIndef = pre {
    "eu" | "Eu" | "unique" | "univers" | "unit" | "urin" | "utop" |
    "uise" => "a" ;
    "u" | "U" | "a" | "e" | "i" | "o" | "A" | "E" | "I" | "O" => "an" ;
    "SMS" | "sms" => "an" ;
    _ => "a"
    } ;

  getCompar : Case -> Adjective -> Str = \c,a -> case a.isMost of {
    True => "mair" ++ a.s ! AAdj Posit c ;
    False => a.s ! AAdj Compar c
    } ;
  getSuperl : Case -> Adjective -> Str = \c,a -> case a.isMost of {
    True => "maist" ++ a.s ! AAdj Posit c ;
    False => a.s ! AAdj Superl c
    } ;

  auxBe : Aux = {
    pres = \\b,a => case <b,a> of {
      <Pos,AgP1 Sg> => "am" ;
      <Neg,AgP1 Sg> => "amna" ;
      _ => agrVerb (posneg b "is")  (posneg b "are") a
      } ;
    contr = \\b,a => case <b,a> of {
      <Pos,AgP1 Sg> => cBind "m" ;
      <Neg,AgP1 Sg> => cBind "m not" ; --- am not I
      _ => agrVerb (posneg b (cBind "s"))  (posneg b (cBind "re")) a
      } ;
    past = \\b,a => case a of {          --# notpresent
      AgP1 Sg | AgP3Sg _ => posneg b "wis" ; --# notpresent
      _                  => posneg b "wir"   --# notpresent
      } ; --# notpresent
    inf  = "be" ;
    ppart = "been" ;
    prpart = "bein"
    } ;

  posneg : Polarity -> Str -> Str = \p,s -> case p of {
    Pos => s ;
    Neg => s + "na"
    } ;

  vfn : Bool -> Str -> Str -> Str -> {aux, fin, adv, inf : Str} =
    \contr,x,y,z -> case contr of {
      True  => {aux = y ; adv = [] ; fin = [] ; inf = z} ;
      False => {aux = x ; adv = "no" ; fin = [] ; inf = z}
      } ;

  reflPron : Agr => Str = table {
    AgP1 Sg      => "mysel" ;
    AgP2 Sg      => "yersel" ;
    AgP3Sg Masc  => "himsel" ;
    AgP3Sg Fem   => "hersel" ;
    AgP3Sg Neutr => "itsel" ;
    AgP1 Pl      => "oursels" ;
    AgP2 Pl      => "yersels" ;
    AgP3Pl _     => "thairsels"
    } ;

  possPron : Agr => Str = table {
    AgP1 Sg      => "my" ;
    AgP2 Sg      => "yer" ;
    AgP3Sg Masc  => "his" ;
    AgP3Sg Fem   => "her" ;
    AgP3Sg Neutr => "its" ;
    AgP1 Pl      => "our" ;
    AgP2 Pl      => "yer" ;
    AgP3Pl _     => "thair"
    } ;

  predV : Verb -> VP = \verb -> {
    p = verb.p ;
    prp = verb.s ! VPresPart ;
    ptp = verb.s ! VPPart ;
    inf = verb.s ! VInf ;
    ad = \\_ => [] ;
    ext = [] ;
    isSimple = True ;
    s2 = \\a => if_then_Str verb.isRefl (reflPron ! a) [] ;
    isAux = False ;
    auxForms = {contr,past,pres = \\_,_ => nonExist} ;
    nonAuxForms = {
      pres = \\agr => presVerb verb agr ;
      past = verb.s ! VPast  --# notpresent
      }
    } ;

  predVc : (Verb ** {c2 : Str}) -> SlashVP = \verb ->
    predV verb ** {c2 = verb.c2 ; gapInMiddle = True; missingAdv = False} ;

  predVV : {s : VVForm => Str ; p : Str ; typ : VVType} -> VP = \verb ->
    let verbs = verb.s
    in case verb.typ of {
      VVAux => predAux {
        pres,contr = table {
          Pos => \\_ => verbs ! VVF VPres ;
          Neg => \\_ => verbs ! VVPresNeg
          } ;
        past = table {                        --# notpresent
          Pos => \\_ => verbs ! VVF VPast ;   --# notpresent
          Neg => \\_ => verbs ! VVPastNeg     --# notpresent
          } ;                                 --# notpresent
        p = verb.p ;
        inf = verbs ! VVF VInf ;
        ppart = verbs ! VVF VPPart ;
        prpart = verbs ! VVF VPresPart
        } ;
      _ => predV {s = \\vf => verbs ! VVF vf ; p = verb.p ; isRefl = False}
      } ;

  mkVerbForms : Agr -> VP -> VerbForms = \agr,vp -> case vp.isAux of {
    True =>
      let aux : Aux = vp.auxForms ** {
            inf = vp.inf ;
            ppart = vp.ptp ;
            prpart = vp.prp } ;
      in auxVerbForms aux ;
    False =>
      let fin : Str = vp.nonAuxForms.pres ! agr ;
          inf : Str = vp.inf ;
          part : Str = vp.ptp ;
      in nonAuxVerbForms fin inf part
                                       vp.nonAuxForms.past      --# notpresent
    } ;

  nonAuxVerbForms : (fin,inf,part : Str) ->
                    (past : Str) -> --# notpresent
                    VerbForms = \fin,inf,part
                                ,past --# notpresent
                                ->
    \\tns,ant,pol,ord,agr =>
      case <tns,ant,pol,ord> of {
        <Pres,Simul,CPos,ODir _>      => vff fin [] ;
        <Pres,Simul,CPos,OQuest>      => vf (scoDoes agr) inf ;
        <Pres,Anter,CPos,ODir True>   => vf (haveContr agr) part ;                    --# notpresent
        <Pres,Anter,CPos,_>           => vf (scoHave agr) part ;                      --# notpresent
        <Pres,Anter,CNeg c,ODir True> => vfn c (haveContr agr) (scoHavent agr) part ; --# notpresent
        <Pres,Anter,CNeg c,_>         => vfn c (scoHave agr) (scoHavent agr) part ;   --# notpresent
        <Past,Simul,CPos,ODir _>      => vff past [] ;                                --# notpresent
        <Past,Simul,CPos,OQuest>      => vf "did" inf ;                               --# notpresent
        <Past,Simul,CNeg c,_>         => vfn c "did" "didna" inf ;                    --# notpresent
        <Past,Anter,CPos,ODir True>   => vf (cBind "d") part ;                        --# notpresent
        <Past,Anter,CPos,_>           => vf "haed" part ;                             --# notpresent
        <Past,Anter,CNeg c,ODir True> => vfn c (cBind "d") (cBind "d no") part ;      --# notpresent
        <Past,Anter,CNeg c,_>         => vfn c "haed" "haedna" part ;                 --# notpresent
        <Fut,Simul,CPos,ODir True>    => vf (cBind "ll") inf ; --# notpresent
        <Fut,Simul,CPos,_>            => vf "will" inf ; --# notpresent
        <Fut,Simul,CNeg c,ODir True>  => vfn c (cBind "ll") (cBind "ll no") inf ; --# notpresent
        <Fut,Simul,CNeg c,_>          => vfn c "will" "winna" inf ; --# notpresent
        <Fut,Anter,CPos,ODir True>    => vf (cBind "ll") ("hae" ++ part) ; --# notpresent
        <Fut,Anter,CPos,_>            => vf "will" ("hae" ++ part) ; --# notpresent
        <Fut,Anter,CNeg c,ODir True>  => vfn c (cBind "ll") (cBind "ll no") ("hae" ++ part) ; --# notpresent
        <Fut,Anter,CNeg c,_>          => vfn c "will" "winna" ("hae" ++ part) ; --# notpresent
        <Cond,Simul,CPos,ODir True>   => vf (cBind "d") inf ; --# notpresent
        <Cond,Simul,CPos,_>           => vf "wad" inf ; --# notpresent
        <Cond,Simul,CNeg c,ODir True> => vfn c (cBind "d") (cBind "d no") inf ; --# notpresent
        <Cond,Simul,CNeg c,_>         => vfn c "wad" "wadna" inf ; --# notpresent
        <Cond,Anter,CPos,ODir True>   => vf (cBind "d") ("hae" ++ part) ; --# notpresent
        <Cond,Anter,CPos,_>           => vf "wad" ("hae" ++ part) ; --# notpresent
        <Cond,Anter,CNeg c,ODir True> => vfn c (cBind "d") (cBind "d no") ("hae" ++ part) ; --# notpresent
        <Cond,Anter,CNeg c,_>         => vfn c "wad" "wadna" ("hae" ++ part) ; --# notpresent
        <Pres,Simul,CNeg c,_>         => vfn c (scoDoes agr) (scoDoesnt agr) inf
        } ;

  auxVerbForms : Aux -> VerbForms = \verb ->
    \\t,ant,cb,ord,agr =>
      let b = case cb of {CPos => Pos ; _ => Neg} ;
          inf = verb.inf ;
          fin = verb.pres ! b ! agr ;
          finp = verb.pres ! Pos ! agr ;
          cfin = verb.contr ! b ! agr ;
          cfinp = verb.contr ! Pos ! agr ;
          part = verb.ppart
      in case <t,ant,cb,ord> of {
        <Pres,Anter,CPos,ODir True>   => vf (haveContr agr) part ;
        <Pres,Anter,CPos,_>           => vf (scoHave agr) part ;
        <Pres,Anter,CNeg c,ODir True> => vfn c (haveContr agr) (scoHavent agr) part ;
        <Pres,Anter,CNeg c,_>         => vfn c (scoHave agr) (scoHavent agr) part ;
        <Past,Anter,CPos,ODir True>   => vf (cBind "d") part ;
        <Past,Anter,CPos,_>           => vf "haed" part ;
        <Past,Anter,CNeg c,ODir True> => vfn c (cBind "d") (cBind "d no") part ;
        <Past,Anter,CNeg c,_>         => vfn c "haed" "haedna" part ;
        <Fut,Simul,CPos,ODir True>    => vf (cBind "ll") inf ;
        <Fut,Simul,CPos,_>            => vf "will" inf ;
        <Fut,Simul,CNeg c,ODir True>  => vfn c (cBind "ll") (cBind "ll no") inf ;
        <Fut,Simul,CNeg c,_>          => vfn c "will" "winna" inf ;
        <Fut,Anter,CPos,ODir True>    => vf (cBind "ll") ("hae" ++ part) ;
        <Fut,Anter,CPos,_>            => vf "will" ("hae" ++ part) ;
        <Fut,Anter,CNeg c,ODir True>  => vfn c (cBind "ll") (cBind "ll no") ("hae" ++ part) ;
        <Fut,Anter,CNeg c,_>          => vfn c "will" "winna" ("hae" ++ part) ;
        <Cond,Simul,CPos,ODir True>   => vf (cBind "d") inf ;
        <Cond,Simul,CPos,_>           => vf "wad" inf ;
        <Cond,Simul,CNeg c,ODir True> => vfn c (cBind "d") (cBind "d no") inf ;
        <Cond,Simul,CNeg c,_>         => vfn c "wad" "wadna" inf ;
        <Cond,Anter,CPos,ODir True>   => vf (cBind "d") ("hae" ++ part) ;
        <Cond,Anter,CPos,_>           => vf "wad" ("hae" ++ part) ;
        <Cond,Anter,CNeg c,ODir True> => vfn c (cBind "d") (cBind "d no") ("hae" ++ part) ;
        <Cond,Anter,CNeg c,_>         => vfn c "wad" "wadna" ("hae" ++ part) ;
        <Past,Simul,CPos,_>           => vf (verb.past ! b ! agr) [] ;                                  --# notpresent
        <Past,Simul,CNeg c,_>         => vfn c (verb.past ! Pos ! agr) (verb.past ! Neg ! agr) [] ;     --# notpresent
        <Pres,Simul,CPos,ODir True>   => vf cfin [] ;
        <Pres,Simul,CPos,_>           => vf fin [] ;
        <Pres,Simul,CNeg c,ODir True> => vfn c cfinp fin [] ;
        <Pres,Simul,CNeg c,_>         => vfn c finp fin []
        } ;

  infVP : VVType -> VP -> Bool -> Anteriority -> CPolarity -> Agr -> Str =
    \typ,vp,ad_pos,ant,cb,a ->
      case cb of {CPos => [] ; _ => "no"} ++
      case ant of {
        Simul => case typ of {
          VVAux => vp.ad ! a ++ vp.inf ;
          VVInf => case ad_pos of {
            True  => vp.ad ! a ++ "tae" ++ vp.inf ;
            False => "tae" ++ vp.ad ! a ++ vp.inf
            } ;
          _ => vp.ad ! a ++ vp.prp
          } ;
        Anter => case typ of {
          VVAux => "hae" ++ vp.ad ! a ++ vp.ptp ;
          VVInf => case ad_pos of {
            True  => vp.ad ! a ++ "tae" ++ "hae" ++ vp.ptp ;
            False => "tae" ++ "hae" ++ vp.ad ! a ++ vp.ptp
            } ;
          _ => "haein" ++ vp.ad ! a ++ vp.ptp
          }
        } ++ vp.p ++ vp.s2 ! a ++ vp.ext ;

  scoHave : Agr -> Str = agrVerb "haes" "hae" ;
  scoHavent : Agr -> Str = agrVerb "haesna" "haena" ;
  scoDoes : Agr -> Str = agrVerb "dis" "dae" ;
  scoDoesnt : Agr -> Str = agrVerb "disna" "dinna" ;

  mkClause : Str -> Agr -> VP -> Clause = \subj,agr,vp -> {
    s = \\t,a,b,o =>
      let verb = mkVerbForms agr vp ! t ! a ! b ! o ! agr ;
          compl = vp.s2 ! agr ++ vp.ext
      in case o of {
        ODir _ => subj ++ verb.aux ++ verb.adv ++ vp.ad ! agr ++
                  verb.fin ++ verb.inf ++ vp.p ++ compl ;
        OQuest => verb.aux ++ subj ++ verb.adv ++ vp.ad ! agr ++
                  verb.fin ++ verb.inf ++ vp.p ++ compl
        }
    } ;

}
