let () =  Alcotest.run "clam" [
  "display",        Display.tests;
  "app",            App.tests;
  "prom",           Prom.tests;
  "is",             Is.tests;
  "is!",            Is.tests_not;
  "isa",            Isa.tests;
  "isa!",           Isa.tests_not;
  "join",           Join.tests;
  "meet",           Meet.tests;
  "infer_app",      InferApp.tests;
  "infer_app_type", InferAppType.tests;
  "infer_elem",     InferElem.tests;
  "infer_attr",     InferAttr.tests;
  "full",           Full.tests;
]
