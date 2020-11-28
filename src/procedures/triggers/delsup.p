TRIGGER PROCEDURE FOR DELETE OF Supplier.

FIND FIRST purchaseorder OF supplier WHERE postatus <> "Received" 
NO-LOCK NO-ERROR.

IF AVAIL purchaseorder THEN DO:

     MESSAGE "Supplier can not be deleted."
     "There is at least one PO that has not been received."
     VIEW-AS ALERT-BOX ERROR BUTTONS OK.
     RETURN ERROR. 
     
END. /*if avail purchaseorder*/
 
ELSE DO:
   /*delete received po*/
   
     FOR EACH purchaseorder OF Supplier:

          FOR EACH poline OF purchaseorder:
              DELETE poline.
          END. /*for each poline*/

          DELETE purchaseorder.
          
     END. /*for each purchaseorder of supplier*/
 
END. /*else do*/
