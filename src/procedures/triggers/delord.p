/***************************************************************************\
*****************************************************************************
**
**     Program: delord.p
**    Descript:
**
*****************************************************************************
\***************************************************************************/

TRIGGER PROCEDURE FOR Delete OF Order.

/* When Orders are deleted, associated Order detail lines (OrderLine) 
 * are also deleted.
 */

MESSAGE "Deleting Order" OrderNum VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
FOR EACH OrderLine OF Order:
   DELETE OrderLine.
END.










