 
 /*------------------------------------------------------------------------
    File        : Class1
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike
    Created     : Fri Apr 12 11:11:45 CEST 2013
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING Consultingwerk.InterfaceSample.Interface2.
USING Consultingwerk.InterfaceSample.Interface3.



CLASS Consultingwerk.interfaceSample.class2 IMPLEMENTS Interface2, Interface3: 

    /*------------------------------------------------------------------------------
         Purpose: This should produce no lint warning, as the method is in Interface1
         Notes:   Interface1 is not directly implemented by the class, but it's the 
                  SUPER-Interface of Interface2
     ------------------------------------------------------------------------------*/
	@Progress.Lang.Generated.
	METHOD PUBLIC VOID MethodFromInterface1 ():
		MESSAGE "Foo".
		THIS-OBJECT:MethodFromInterface2().
		MESSAGE "Bar".
	END METHOD.

    /*------------------------------------------------------------------------------
         Purpose: This should produce no lint warning, as the method is in Interface3
         Notes:
     ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID MethodFromInterface2 ():
        DEF VAR zz AS INT NO-UNDO.
		FOR EACH Customer WHERE Name BEGINS "A" NO-LOCK:
		  zz = zz + 1.
		END.
		MESSAGE zz.
	END METHOD.

    /*------------------------------------------------------------------------------
         Purpose: This should produce no lint warning, as the method is in Interface3
         Notes:
     ------------------------------------------------------------------------------*/
	METHOD PUBLIC VOID MethodFromInterface3 ():
		
	END METHOD.

	/*------------------------------------------------------------------------------
    	 Purpose: This should produce a lint warning, as the method is in NO interface
    	 Notes:
     ------------------------------------------------------------------------------*/
	METHOD PUBLIC VOID MethodInNoInterface(  ):
		
	END METHOD.

    /*------------------------------------------------------------------------------
         Purpose: This should produce no lint warning, as the method is not PUBLIC
         Notes:
     ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID MethodInNoInterface2 (  ):
        if true then do:
          message "123".
        end.
    END METHOD.

    /*------------------------------------------------------------------------------
         Purpose: This should produce no lint warning, as the method is STATIC
         Notes:
     ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC VOID MethodInNoInterface3 (  ):
        message "gilles".
    END METHOD.

    METHOD PUBLIC STATIC VOID MethodInNoInterface4 (  ):
        message "gilles".
    END METHOD.

    METHOD PUBLIC STATIC VOID MethodInNoInterface5 (  ):
        message "gilles".
    END METHOD.

END CLASS.