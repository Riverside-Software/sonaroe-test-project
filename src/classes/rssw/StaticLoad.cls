block-level on error undo, throw.

class rssw.StaticLoad:
  define variable lIsInitialized as logical no-undo.

  constructor public StaticLoad():
    // Nothing
  end constructor.

  method public void LoadEntity():
    EnsureInitialized().
    message "StaticLoad".
  end method.

  method private void EnsureInitialized():
    if not lIsInitialized then
    do:
      message "StaticLoad".
      lIsInitialized = true.
    end.
  end method.
end class.