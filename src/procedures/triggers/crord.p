/***************************************************************************\
*****************************************************************************
**
**     Program: crord.p
**    Descript:
**
*****************************************************************************
\***************************************************************************/

TRIGGER PROCEDURE FOR Create OF Order.

/* Automatically Increment Order-Number using Next-Ord-Num Sequence */

ASSIGN  order.ordernum =  NEXT-VALUE(NextOrdNum) 

/* Set Order Date to TODAY,  Promise Date to 2 weeks from TODAY */

order.orderdate = TODAY
order.promisedate = TODAY + 14.
