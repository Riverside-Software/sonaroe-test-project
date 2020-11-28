/***************************************************************************\
*****************************************************************************
**
**     Program: delord.p
**    Descript:
**
*****************************************************************************
\***************************************************************************/

TRIGGER PROCEDURE FOR Delete OF OrderLine.

/* Trigger provides an information message when orderlines are deleted */

MESSAGE "Deleting Order Line:" Linenum "Order Num:" OrderNum
        VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.

