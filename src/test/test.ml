let () =  Alcotest.run "clam" [
  "display",         Display.tests;
  "substitute",      Substitute.tests;
  "prom",            Prom.tests;
  "is",              Is.tests;
  "is!",             Is.tests_not;
  "isa",             Isa.tests;
  "isa!",            Isa.tests_not;
  "join",            Join.tests;
  "meet",            Meet.tests;
  "search_elem",     SearchElem.tests;
  "search_attr",     SearchAttr.tests;
  "search_app",      SearchApp.tests;
  "search_app_type", SearchAppType.tests;
  "full",            Full.tests;
]
