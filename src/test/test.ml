let () =  Alcotest.run "clam" [
  "display",    Display.tests;
  "substitute", Substitute.tests;
  "promote",    Promote.tests;
  "is",         Is.tests;
  "is!",        Is.tests_not;
  "isa",        Isa.tests;
  "isa!",       Isa.tests_not;
  "join",       Join.tests;
  "meet",       Meet.tests;
  "full",       Full.tests;
]
