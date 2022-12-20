--# -path=.:../prelude:../common
--# -coding=utf8

-- L.Boizou, 2022 <lboizou@gmail.com>

resource MorphoAdjectiveLit = ResLit ** open Prelude, (Predef=Predef) in {

     flags  coding=utf8; 
  
  oper guessAdjModel : Str -> AdjForms = \form ->
    case form of {
      stem + "as" => adj1aModel stem;
      stem + "is" => adj2aModel stem;
      stem + "ys" => adj2bModel stem;
--      stem + "is" => adj2cModel stem;
      stem + "us" => adj3aModel stem;
      _ => adj0aModel form
--      _ => Predef.error (form ++ "not matched as adjective in AdjectiveMorphoPol.guess_model_lit)")
    };

  oper adj0aModel : Str -> AdjForms 
    = \stem -> -- mini
    {
      msnom, msacc, msgen, msins, msdat, msloc, mpnom, mpacc, mpgen, mpins, mpdat, mploc,
      fsnom, fsacc, fsgen, fsins, fsdat, fsloc, fpnom, fpacc, fpgen, fpins, fpdat, fploc, nnom = stem    
    };

--  oper model1lit : Str -> AdjForms
  oper adj1aModel : Str -> AdjForms
    = \stem -> -- ger(as)
    {
      msnom=stem + "as"; msacc=stem + "ą"; msgen=stem + "o"; msins=stem + "u"; msdat=stem + "am"; msloc=stem + "ame"; 
      mpnom=stem + "i"; mpacc=stem + "us"; mpgen=stem + "ų"; mpins=stem + "ais"; mpdat=stem + "iems"; mploc=stem + "uose";
      fsnom=stem + "a"; fsacc=stem + "ą"; fsgen=stem + "os"; fsins=stem + "a"; fsdat=stem + "ai"; fsloc=stem + "oje";
      fpnom=stem + "os"; fpacc=stem + "as"; fpgen=stem + "ų"; fpins=stem + "omis"; fpdat=stem + "oms"; fploc=stem + "ose"; 
      nnom=stem + "a";    
    };
  
--  oper model3lit : Str -> AdjForms 
  oper adj2aModel : Str -> AdjForms 
    = \stem -> -- didel(is) 
    {
      msnom=stem + "is"; msacc=stem + "į"; msgen=(soften stem) + "io"; msins=(soften stem) + "iu"; 
      msdat=(soften stem) + "iam"; msloc=(soften stem) + "iame"; 
      mpnom=stem + "i"; mpacc=(soften stem) + "ius"; mpgen=(soften stem) + "ių"; 
      mpins=(soften stem) + "iais"; mpdat=stem + "iems"; mploc=(soften stem) + "iuose";
      fsnom=stem + "ė"; fsacc=stem + "ę"; fsgen=stem + "ės"; fsins=stem + "e"; fsdat=stem + "ei"; fsloc=stem + "ėje";
      fpnom=stem + "ės"; fpacc=stem + "es"; fpgen=(soften stem) + "ių"; fpins=stem + "ėmis"; fpdat=stem + "ėms"; fploc=stem + "ėse"; 
      nnom=stem + "i";    
    };

--  oper model3_2lit : Str -> AdjForms 
  oper adj2bModel : Str -> AdjForms 
    = \stem -> -- kair(ys) 
    {
      msnom=stem + "ys"; msacc=stem + "į"; msgen=(soften stem) + "io"; msins=(soften stem) + "iu"; 
      msdat=(soften stem) + "iam"; msloc=(soften stem) + "iame"; 
      mpnom=stem + "i"; mpacc=(soften stem) + "ius"; mpgen=(soften stem) + "ių"; 
      mpins=(soften stem) + "iais"; mpdat=stem + "iems"; mploc=(soften stem) + "iuose";
      fsnom=stem + "ė"; fsacc=stem + "ę"; fsgen=stem + "ės"; fsins=stem + "e"; fsdat=stem + "ei"; fsloc=stem + "ėje";
      fpnom=stem + "ės"; fpacc=stem + "es"; fpgen=(soften stem) + "ių"; fpins=stem + "ėmis"; fpdat=stem + "ėms"; fploc=stem + "ėse"; 
      nnom=stem + "i";
    };

--  oper model4lit : Str -> AdjForms 
  oper adj2cModel : Str -> AdjForms 
    = \stem -> -- rankin(is)
    {
      msnom=stem + "is"; msacc=stem + "į"; msgen=(soften stem) + "io"; msins=(soften stem) + "iu"; 
      msdat=(soften stem) + "iam"; msloc=(soften stem) + "iame"; 
      mpnom=(soften stem) + "iai"; mpacc=(soften stem) + "ius"; mpgen=(soften stem) + "ių"; 
      mpins=(soften stem) + "iais"; mpdat=(soften stem) + "iams"; mploc=(soften stem) + "iuose";
      fsnom=stem + "ė"; fsacc=stem + "ę"; fsgen=stem + "ės"; fsins=stem + "e"; fsdat=stem + "ei"; fsloc=stem + "ėje";
      fpnom=stem + "ės"; fpacc=stem + "es"; fpgen=(soften stem) + "ių"; fpins=stem + "ėmis"; fpdat=stem + "ėms"; fploc=stem + "ėse"; 
      nnom=stem + "i";
    };
  
--  oper model4lit : Str -> AdjForms 
  oper adj3aModel : Str -> AdjForms 
    = \stem -> -- skan(us)
    {
      msnom=stem + "us"; msacc=stem + "ų"; msgen=stem + "aus"; 
      msins=(soften stem) + "iu"; msdat=(soften stem) + "iam"; msloc=(soften stem) + "iame"; 
      mpnom=stem + "ūs"; mpacc=(soften stem) + "ius"; mpgen=(soften stem) + "ių"; 
      mpins=(soften stem) + "iais"; mpdat=stem + "iems"; mploc=(soften stem) + "iuose";
      fsnom=stem + "i"; fsacc=(soften stem) + "ią"; fsgen=(soften stem) + "ios"; 
      fsins=(soften stem) + "ia"; fsdat=(soften stem) + "iai"; fsloc=(soften stem) + "ioje";
      fpnom=(soften stem) + "ios"; fpacc=(soften stem) + "ias"; fpgen=(soften stem) + "ių"; 
      fpins=(soften stem) + "iomis"; fpdat=(soften stem) + "ioms"; fploc=(soften stem) + "iose"; 
      nnom=stem + "u";
    };

}
