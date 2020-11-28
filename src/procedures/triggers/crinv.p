/***************************************************************************\
*****************************************************************************
**
**     Program: crinv.p
**    Descript:
**
*****************************************************************************
\***************************************************************************/

TRIGGER PROCEDURE FOR Create OF Invoice.

/* Automatically increment Invoice Number using Next-Inv-Num Sequence */

ASSIGN invoice.invoicenum = NEXT-VALUE(NEXTINVNUM).
