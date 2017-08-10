ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING OpenEdge.Core.Assert.

CLASS ABLUnitTest1:

@Test.
METHOD PUBLIC VOID test1():
  def var xx as handle no-undo.
  run testProfiler.p persistent set xx.
  run test1 in xx.
  delete procedure xx.
 END METHOD.
 
END CLASS.