let () =  Alcotest.run "clam" [
  "display", Display.tests;
  "meet"   , Meet.tests;
  "isa"    , Isa.tests;
  "app"    , App.tests;
  "elem"   , Elem.tests;
  "attr"   , Attr.tests;
  "full"   , Full.tests;
]
