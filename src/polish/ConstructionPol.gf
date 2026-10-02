--# -path=.:../abstract:../common:../prelude

-- Idiomatic constructions and calendar expressions for Polish.
concrete ConstructionPol of Construction = CatPol **
  open ResPol, MorphoPol, Prelude, (G = GrammarPol), (P = ParadigmsPol),
       (S = StructuralPol) in {

flags coding=utf8 ;

lincat
  Timeunit = N ;
  Hour = {s : Str} ;
  Weekday = N ;
  Month = N ;
  Monthday, Year = {s : Str} ;
  Language = {s : Str} ;

lin
  ready_VP = G.UseComp (G.CompAP (G.PositA (mkA (guess_model "gotowy")))) ;

  -- Polish expresses age with mieć: "ma trzydzieści lat".
  has_age_VP card = G.ComplSlash (G.SlashV2a S.have_V2) (ageNP card) ;

  n_units_AP card unit adj = {
    s = \\af => card.s ! Nom ! unit.g ++ unit.s ! Pl ! Gen ++
                  (mkAtable adj.pos) ! af;
    adv = card.s ! Nom ! unit.g ++ unit.s ! Pl ! Gen ++ adj.advpos;
    isPost = False
    } ;

  n_units_of_NP card unit np = {
    nom = card.s ! Nom ! unit.g ++ unit.s ! Pl ! Gen ++ np.dep ! GenNoPrep;
    voc = card.s ! VocP ! unit.g ++ unit.s ! Pl ! Gen ++ np.dep ! GenNoPrep;
    dep = \\cc => let c = extract_case ! cc in
      card.s ! c ! unit.g ++ unit.s ! Pl ! Gen ++ np.dep ! GenNoPrep;
    gn = accom_gennum ! <card.a,unit.g,card.n>;
    p = P3
    } ;

  cup_of_CN np = containerCN (P.mkN "filiżanka" Fem) np ;

  weekdayPunctualAdv w = {s = "w" ++ w.s ! SF Sg Acc} ;
  weekdayHabitualAdv w = {s = "w" ++ w.s ! SF Pl Acc} ;
  yearAdv y = {s = "w" ++ y.s ++ "roku"} ;
  intYear i = {s = i.s} ;

  weekdayN w = w ;
  monthN m = m ;

  monday_Weekday = P.mkN "poniedziałek" (Masc Inanimate) ;
  tuesday_Weekday = P.mkN "wtorek" (Masc Inanimate) ;
  wednesday_Weekday = P.mkN "środa" Fem ;
  thursday_Weekday = P.mkN "czwartek" (Masc Inanimate) ;
  friday_Weekday = P.mkN "piątek" (Masc Inanimate) ;
  saturday_Weekday = P.mkN "sobota" Fem ;
  sunday_Weekday = P.mkN "niedziela" Fem ;

  january_Month = P.mkN "styczeń" (Masc Inanimate) ;
  february_Month = P.mkN "luty" (Masc Inanimate) ;
  march_Month = P.mkN "marzec" (Masc Inanimate) ;
  april_Month = P.mkN "kwiecień" (Masc Inanimate) ;
  may_Month = P.mkN "maj" (Masc Inanimate) ;
  june_Month = P.mkN "czerwiec" (Masc Inanimate) ;
  july_Month = P.mkN "lipiec" (Masc Inanimate) ;
  august_Month = P.mkN "sierpień" (Masc Inanimate) ;
  september_Month = P.mkN "wrzesień" (Masc Inanimate) ;
  october_Month = P.mkN "październik" (Masc Inanimate) ;
  november_Month = P.mkN "listopad" (Masc Inanimate) ;
  december_Month = P.mkN "grudzień" (Masc Inanimate) ;

oper
  yearN : N = P.mkN "rok" (Masc Inanimate) ;

  ageNP : Card -> NP ;
  ageNP card = lin NP {
    nom = ageForm card Nom;
    voc = ageForm card VocP;
    dep = \\cc => ageForm card (extract_case ! cc);
    gn = NeutSg;
    p = P3
    } ;

  ageForm : Card -> Case -> Str ;
  ageForm card c = card.s ! c ! (Masc Inanimate) ++ case <card.n,card.a> of {
    <Sg,NoA> => case c of {
      Gen => "roku"; Dat => "rokowi"; Instr => "rokiem";
      Loc => "roku"; VocP => "roku"; _ => "rok"
      };
    <_,DwaA> => case c of {
      Gen|Loc => "lat"; Dat => "latom"; Instr => "latami"; _ => "lata"
      };
    <_,_> => "lat"
    } ;

  cardinalUnitNP : Card -> N -> NP ;
  cardinalUnitNP card unit = lin NP {
    nom = card.s ! Nom ! unit.g ++ unit.s ! SF card.n
            (accom_case ! <card.a,Nom,unit.g>);
    voc = card.s ! VocP ! unit.g ++ unit.s ! SF card.n
            (accom_case ! <card.a,VocP,unit.g>);
    dep = \\cc => let c = extract_case ! cc in
      card.s ! c ! unit.g ++ unit.s ! SF card.n
        (accom_case ! <card.a,c,unit.g>);
    gn = accom_gennum ! <card.a,unit.g,card.n>;
    p = P3
    } ;

  containerCN : N -> NP -> CN ;
  containerCN container np = lin CN {
    s = \\n,c => container.s ! SF n c ++ np.dep ! GenNoPrep;
    g = container.g
    } ;
}
