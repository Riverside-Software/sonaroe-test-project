/***************************************************************************\
*****************************************************************************
**
**     Program: asstate.p
**    Descript: To test, enter an Invalid Two-Character State Abbrev.
**
*****************************************************************************
\***************************************************************************/

TRIGGER PROCEDURE FOR Assign OF Customer.State.
  
IF Customer.Country = "USA" AND NOT (CAN-FIND(State OF Customer)) 
THEN DO:
   MESSAGE "Illegal State name for the U.S.A." 
	   VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
   RETURN ERROR.     
END.














