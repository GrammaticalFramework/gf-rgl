--# -coding=utf8
concrete ExtraBul of ExtraBulAbs = CatBul ** 
  open ResBul, MorphoFunsBul, Coordination, Prelude, Predef in {
  flags coding=utf8 ;


  lin
    PossIndefPron p = {
      s = \\_,aform => p.gen ! (indefAForm aform) ;
      nonEmpty = True;
      spec = Indef;
      p = Pos
      } ;
      
    ReflQuant = {
      s = \\_,aform => reflPron ! aform ;
      nonEmpty = True;
      spec = Indef;
      p = Pos
    } ;

    ReflIndefQuant = {
      s = \\_,aform => reflPron ! (indefAForm aform) ;
      nonEmpty = True;
      spec = Indef;
      p = Pos
    } ;

    EmptyRelSlash slash = {
      s = \\t,a,p,agr => slash.c2.s ++ whichRP ! agr.gn ++ slash.s ! agr ! t ! a ! p ! Main
      } ;

    i8fem_Pron  = mkPron "аз" "мой" "моя" "моят" "моя" "моята" "мое" "моето" "мои" "моите" (GSg Fem)  PronP1 ;
    i8neut_Pron = mkPron "аз" "мой" "моя" "моят" "моя" "моята" "мое" "моето" "мои" "моите" (GSg Neut) PronP1 ;
    
    whatSg8fem_IP  = mkIP "каква" "каква" (GSg Fem) ;
    whatSg8neut_IP = mkIP "какво" "какво" (GSg Neut) ;

    whoSg8fem_IP  = mkIP "коя" "кого" (GSg Fem) ;
    whoSg8neut_IP = mkIP "кое" "кого" (GSg Neut) ;
    
    youSg8fem_Pron  = mkPron "ти" "твой" "твоя" "твоят" "твоя" "твоята" "твое" "твоето" "твои" "твоите" (GSg Fem)  PronP2 ;
    youSg8neut_Pron = mkPron "ти" "твой" "твоя" "твоят" "твоя" "твоята" "твое" "твоето" "твои" "твоите" (GSg Neut) PronP2 ;

    onePl_Num = {s = table {
                       CFMasc Indef _ | CFFem Indef | CFNeut Indef            => "едни" ;
                       CFMasc Def _ | CFMascDefNom _ | CFFem Def | CFNeut Def => "едните"
                     } ;
                 nn = NCountable;
                 nonEmpty = True
                } ;

    UttImpSg8fem  pol imp = {s = pol.s ++ imp.s ! pol.p ! GSg Fem} ;
    UttImpSg8neut pol imp = {s = pol.s ++ imp.s ! pol.p ! GSg Fem} ;
    
    IAdvAdv adv = {s = \\qf => (mkIAdv "колко").s ! qf ++ adv.s} ;

  lincat
    VPI   = {s : Agr => Str} ;
    [VPI] = {s : Agr => Ints 4 => Str} ;

  lin
    BaseVPI x y = {s  = \\a=>table {4 => y.s!a;    _ => x.s!a}} ;
    ConsVPI x xs = {s  = \\a=>table {4 => xs.s!a!4; t => x.s!a++linCoord bindComma!t++xs.s!a!t}};

    MkVPI vp = {s = daComplex Simul Pos vp ! Perf} ;
    ConjVPI conj vpi = {
      s = \\a =>  linCoord []!conj.sep ++ vpi.s!a!conj.sep ++ conj.s ++ vpi.s!a!4
      } ;
    ComplVPIVV vv vpi = 
      insertObj (\\a => vpi.s ! a) Pos (predV vv) ;

  lincat
    VPS   = {s : Agr => Str} ;
    [VPS] = {s : Agr => Ints 4 => Str} ;

  lin
    BaseVPS x y  = {s  = \\a=>table {4 => y.s!a;    _ => x.s!a}} ;
    ConsVPS x xs = {s  = \\a=>table {4 => xs.s!a!4; t => x.s!a++linCoord bindComma!t++xs.s!a!t}};

    PredVPS np vps = {s = np.s ! RSubj ++ vps.s ! personAgr np.gn np.p} ;

    MkVPS t p vp = {
      s = \\a => 
            let verb  = vpTenses vp ! t.t ! t.a ! p.p ! a ! False ! Perf ;
                compl = vp.compl ! a
          in t.s ++ p.s ++ verb ++ compl
      } ;
      
    ConjVPS conj vps = {
      s = \\a => linCoord []!conj.sep ++ vps.s!a!conj.sep ++ conj.s ++ vps.s!a!4
      } ;

    PassVPSlash vp = insertObj (\\a => vp.ad.s ++ vp.s ! Perf ! VPassive (aform a.gn Indef (RObj Acc)) ++
                                       vp.compl1 ! a ++ vp.compl2 ! a) Pos (predV verbBe) ;

    PassAgentVPSlash vp np =
      insertObj (\\_ => "от" ++ np.s ! RObj CPrep) Pos
                     (insertObj (\\a => vp.ad.s ++ vp.s ! Perf ! VPassive (aform a.gn Indef (RObj Acc)) ++
                                        vp.compl1 ! a ++ vp.compl2 ! a) Pos (predV verbBe)) ;

} 
