/***************************************************************************\
*****************************************************************************
**
**     Program: delinv.p
**    Descript:
**
*****************************************************************************
\***************************************************************************/

TRIGGER PROCEDURE FOR Delete OF Invoice.

/* Invoices cannot be deleted if the Invoice amount exceeds Total-Paid + Adjustment */

IF Invoice.Amount > Invoice.TotalPaid + Invoice.Adjustment 
THEN DO:
  MESSAGE "The Invoice Amount cannot be greater than TotalPaid + Adjustment" 
	  VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
  RETURN ERROR.
END.
