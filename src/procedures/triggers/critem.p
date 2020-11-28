/***************************************************************************\
*****************************************************************************
**
**     Program: critem.p
**    Descript:
**
*****************************************************************************
\***************************************************************************/
 
TRIGGER PROCEDURE FOR Create OF Item.

/* Automatically assign a unique item number using NextItemNum Sequence */

ASSIGN Item.ItemNum = NEXT-VALUE(NextItemNum).
