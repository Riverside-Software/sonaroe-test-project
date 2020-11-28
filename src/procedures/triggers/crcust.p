/***************************************************************************\
*****************************************************************************
**
**     Program: crcust.p
**    Descript:
**
*****************************************************************************
\***************************************************************************/

TRIGGER PROCEDURE FOR Create OF Customer.

/* Automatically Increment Customer Number using NextCustNum Sequence */

ASSIGN Customer.CustNum = NEXT-VALUE(NextCustNum).
   
