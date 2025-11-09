class rssw.SimpleObjectChild2 inherits rssw.SimpleObjectChild1:
  define public property anotherObj as rssw.SimpleObject get. set.

  constructor SimpleObjectChild2(xx as rssw.SimpleObject):
    this-object:anotherObj = xx.
  end constructor.

end class.
