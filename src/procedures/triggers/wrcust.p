/***************************************************************************\
*****************************************************************************
**
**     Program: wrcust.p
**    Descript:
**
*****************************************************************************
\***************************************************************************/

TRIGGER PROCEDURE FOR Write OF Customer OLD BUFFER oldCustomer.

/* Variable Definitions */

DEFINE VARIABLE i AS INTEGER INITIAL 0.
DEFINE VARIABLE Outstanding AS INTEGER INITIAL 0.

/* Check to see if the user changed the Customer Number */

IF Customer.CustNum <> oldCustomer.CustNum AND oldCustomer.CustNum <> 0
THEN DO:

   /* If user changed the Customer Number, find related orders and change  */
   /* their customer numbers.                                              */
   
   FOR EACH order OF oldCustomer:
      Order.CustNum = Customer.CustNum.
      i = i + 1.
   END.
   IF i > 0 THEN
     MESSAGE i "orders changed to reflect the new customer number!" 
	     VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
END.

/* Ensure that the Credit Limit value is always Greater than the sum of this
 * Customer's Outstanding Balance 
 */

FOR EACH Order OF Customer:
   FOR EACH OrderLine OF Order where order.shipdate EQ ?:
      Outstanding = Outstanding + OrderLine.ExtendedPrice.
   END.
END.
FOR EACH Invoice OF Customer:
   Outstanding = Outstanding + ( Amount - ( TotalPaid + Adjustment )).
END.

IF Customer.CreditLimit < Outstanding THEN DO:
   MESSAGE "This Customer has an outstanding balance of: " Outstanding 
           ".  The Credit Limit MUST exceed this amount!" 
           VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
   RETURN ERROR.
END. 
