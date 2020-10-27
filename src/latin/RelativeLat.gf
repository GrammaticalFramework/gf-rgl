concrete RelativeLat of Relative = CatLat ** open ResLat in {
--
--  flags optimize=all_subs ;
--
  lin
--
--    RelCl cl = {
--      s = \\t,a,p,_ => "such" ++ "that" ++ cl.s ! t ! a ! p ! ODir ; 
--      c = Nom
--      } ;
--
    RelVP rp vp = { s = \\g,n => mkClause (emptyNP ** { s = \\_,c => rp.s ! (Ag g n c) ; g = g ; n = n } ) vp };
--      s = \\t,ant,b,ag => 
--        let 
--          agr = case rp.a of {
--            RNoAg => ag ;
--            RAg a => a
--            } ;
--          cl = mkClause (rp.s ! RC (fromAgr agr).g Nom) agr vp
--        in
--        cl.s ! t ! ant ! b ! ODir ;
--      c = Nom
--      } ;
--
---- Pied piping: "at which we are looking". Stranding and empty
---- relative are defined in $ExtraLat.gf$ ("that we are looking at", 
---- "we are looking at").
    --
    -- RelSlash : RP -> ClSlash -> RCl ;
    RelSlash rp slash = { s = \\g,n => slash ** { adv = rp.s ! Ag g n Gen } } ; -- abuse adverbs again
--      s = \\t,a,p,agr => 
--          slash.c2 ++ rp.s ! RPrep (fromAgr agr).g ++ slash.s ! t ! a ! p ! ODir ;
--      c = Acc
--
    FunRP p np rp = {
      s = \\a => case a of { Ag g n c => rp.s ! a ++ p.s ++ (combineNounPhrase np) ! PronNonDrop ! APostN ! DPreN ! Acc };
      -- s = \\c => (combineNounPhrase np) ! PronNonDrop ! Acc ++ p.s ++ rp.s ! RPrep (fromAgr np.a).g ;
      -- a = RAg np.a
      } ;
--
    IdRP = {
      s = table {
	Ag Masc           Sg (Nom | Voc) => "qui" ;
	Ag Fem            Sg (Nom | Voc) => "quae" ;
	Ag Neutr          Sg (Nom | Voc) => "quod" ;
	Ag _              Sg Gen => "cuius" ;
	Ag _              Sg Dat => "cui" ;
	Ag Masc           Sg Acc => "quem" ;
	Ag Fem            Sg Acc => "quam" ;
	Ag Neutr          Sg Acc => "quod" ;
	Ag (Masc | Neutr) Sg Abl => "quo" ;
	Ag Fem            Sg Abl => "qua" ;
	Ag Masc           Pl (Nom | Voc) => "qui" ;
	Ag (Fem  | Neutr) Pl (Nom | Voc) => "quae" ;
	Ag (Masc | Neutr) Pl Gen => "quorum" ;
	Ag Fem            Pl Gen => "quarum" ;
	Ag _              Pl Dat => "quibus" ;
	Ag Masc           Pl Acc => "quos" ;
	Ag Fem            Pl Acc => "quas" ;
	Ag Neutr          Pl Acc => "quae" ;
	Ag _              Pl Abl => "cui"
	}
      }
      ;
--      let varr : Str -> Str = \x -> variants {x ; "that"} --- for bwc
--      in {
--      s = table {
--        RC _ Gen => "whose" ; 
--        RC Neutr _  => varr "which" ;
--        RC _ Acc    => varr "whom" ;
--        RC _ Nom    => varr "who" ;
--        RPrep Neutr => "which" ;
--        RPrep _     => "whom"
--        } ;
--      a = RNoAg
--      } ;
--
}
