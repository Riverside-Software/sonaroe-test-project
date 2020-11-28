/***************************************************************************\
*****************************************************************************
**
**     Program: delcust.p
**    Descript:
**
*****************************************************************************
\***************************************************************************/

TRIGGER Procedure FOR Delete OF Customer.

/* Variable Definitions */

DEFINE VARIABLE answer AS LOGICAL.

/* Customer record cannot be deleted if outstanding invoices are found */

FIND FIRST invoice OF customer NO-ERROR.
IF AVAILABLE invoice THEN DO:
   IF invoice.amount <= invoice.totalpaid + invoice.adjustment THEN DO:
      MESSAGE "Invoice OK, looking for Orders..."
         VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
      FIND FIRST order OF customer NO-ERROR.
      IF NOT AVAILABLE order THEN DO:
         RETURN.
      END.   
      ELSE DO:
       MESSAGE "Open orders exist for Customer " customer.custnum  
               ".  Cannot delete."
               VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
       RETURN ERROR.     
      END.
   END.
   ELSE DO:
      MESSAGE "Outstanding Unpaid Invoice Exists, Cannot Delete"
              VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
      RETURN ERROR.
   END.
END.   
ELSE DO:
   FIND FIRST order OF customer NO-ERROR.
   IF NOT AVAILABLE order THEN DO:
      RETURN.
   END.   
   ELSE DO:
      MESSAGE "Open orders exist for Customer " customer.custnum  
              ".  Cannot delete."
              VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
      RETURN ERROR.
   END.
END.      
