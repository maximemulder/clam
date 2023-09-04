let () =  Alcotest.run "clam" [
  "display", Display.tests;
  "meet"   , Meet.tests;
  "sub"    , Sub.tests;
  "elem"   , Elem.tests;
  "attr"   , Attr.tests;
  "full"   , Full.tests;
]
