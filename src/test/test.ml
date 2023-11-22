let () =  Alcotest.run "clam" [
  "display", Display.tests;
  "app"    , App.tests;
  "prom"   , Prom.tests;
  "is"     , Is.tests;
  "is!"    , Is.tests_not;
  "isa"    , Isa.tests;
  "join"   , Join.tests;
  "meet"   , Meet.tests;
  "elem"   , Elem.tests;
  "attr"   , Attr.tests;
  "full"   , Full.tests;
]
