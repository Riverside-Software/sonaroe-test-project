block-level on error undo, throw.

class rssw.StaticLoad:
  define private static variable instance as rssw.StaticLoad no-undo.

  constructor private StaticLoad():
    // Nothing
  end constructor.

  method public static rssw.StaticLoad getInstance():
    if (instance eq ?) then do:
      instance = new rssw.StaticLoad().
    end.
    return instance.
  end method.

  method public char getFoo():
    return "foo".
  end method.

  method public char getBar():
    return "bar".
  end method.

end class.