/***************************************************************************\
*****************************************************************************
**
**     Program: crbin.p
**    Descript:
**
*****************************************************************************
\***************************************************************************/

TRIGGER PROCEDURE FOR Create OF Bin.

/* Automatically Increment Customer Number using NextCustNum Sequence */

ASSIGN Bin.BinNum = NEXT-VALUE(NextBinNum).
   
