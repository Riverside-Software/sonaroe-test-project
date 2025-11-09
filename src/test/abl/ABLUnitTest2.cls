routine-level on error undo, throw.

using OpenEdge.Core.Assert.

class ABLUnitTest2:

  @Test.
  method public void test2():
    def var xx as handle no-undo.
    run testProfiler.p persistent set xx.
    run test2 in xx.
    delete procedure xx.
  end method.

end class.