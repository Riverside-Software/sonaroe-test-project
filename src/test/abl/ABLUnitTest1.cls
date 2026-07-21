routine-level on error undo, throw.

using OpenEdge.Core.Assert.

class ABLUnitTest1:

  @Test.
  method public void test1():
    define variable xx as handle no-undo.
    define variable zzz as rssw.StaticLoad.
    run testProfiler.p persistent set xx.
    run test1 in xx.
    delete procedure xx.
    zzz = rssw.StaticLoad:getInstance().
    zzz:getBar().
  end method.

end class.