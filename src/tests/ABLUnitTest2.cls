ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING OpenEdge.Core.Assert.

CLASS ABLUnitTest2:

@Test.
METHOD PUBLIC VOID test2():
  def var xx as handle no-undo.
  run testProfiler.p persistent set xx.
  run test2 in xx.
  delete procedure xx.
 END METHOD.
 
END CLASS.