CLASS FooClass02:

   METHOD PRIVATE VOID Foo1(
   ):
      DEFINE VARIABLE lcFoo AS CHARACTER NO-UNDO.
      DEFINE VARIABLE liFoo AS INTEGER NO-UNDO.

      // #### These work ###
      DEFINE VARIABLE loFooClass AS Fooclass02 NO-UNDO. // Case of variable declaration doesn't match case of compiled class FooClass02

      THIS-OBJECT:Foo2("1":U, lcFoo). // INPUT-OUTPUT missing
      THIS-OBJECT:Foo2(1, liFoo). // INPUT-OUTPUT missing

      FooClass02:StaticFoo2("1":U, lcFoo). // INPUT-OUTPUT missing
      FooClass02:StaticFoo2(1, liFoo). // INPUT-OUTPUT missing

      THIS-OBJECT:Foo2("1":U, INPUT-OUTPUT lcFoo). // No issue, ok
      THIS-OBJECT:Foo2(1, INPUT-OUTPUT liFoo). // No issue, ok

      // #### These dont work ###
      THIS-OBJECT:Foo2(?, lcFoo). // No Issue shown, should be INPUT-OUTPUT missing
      THIS-OBJECT:Foo2(?, liFoo). // No Issue shown, should be INPUT-OUTPUT missing

      Fooclass02:StaticFoo2("1":U, INPUT-OUTPUT lcFoo). // No Issue shown, should be Class name casing
      Fooclass02:StaticFoo2(1, INPUT-OUTPUT  liFoo). // No Issue shown, should be Class name casing

      Fooclass02:StaticFoo2("1":U, lcFoo). // No Issue shown, should be both Class name casing and INPUT-OUTPUT missing
      Fooclass02:StaticFoo2(1, liFoo). // No Issue shown, should be both Class name casing and INPUT-OUTPUT missing
   END METHOD.

   METHOD PRIVATE VOID Foo2(
      pcFoo1 AS CHARACTER,
      INPUT-OUTPUT pcFoo2 AS CHARACTER
   ):
      IF pcFoo1 <> ? THEN DO:
         pcFoo2 = "Foo":U.
      END.
   END METHOD.

   METHOD PRIVATE VOID Foo2(
      piFoo1 AS INTEGER,
      INPUT-OUTPUT piFoo2 AS INTEGER
   ):
      IF piFoo1 <> ? THEN DO:
         piFoo2 = 1.
      END.
   END METHOD.

   METHOD PRIVATE STATIC VOID StaticFoo2(
      pcFoo1 AS CHARACTER,
      INPUT-OUTPUT pcFoo2 AS CHARACTER
   ):
      IF pcFoo1 <> ? THEN DO:
         pcFoo2 = "Foo":U.
      END.
   END METHOD.

   METHOD PRIVATE STATIC VOID StaticFoo2(
      piFoo1 AS INTEGER,
      INPUT-OUTPUT piFoo2 AS INTEGER
   ):
      IF piFoo1 <> ? THEN DO:
         piFoo2 = 1.
      END.
   END METHOD.

END CLASS.
