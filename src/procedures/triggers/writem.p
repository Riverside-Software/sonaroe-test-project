/***************************************************************************\
*****************************************************************************
**
**     Program: writem.p
**    Descript:
**
*****************************************************************************
\***************************************************************************/

TRIGGER PROCEDURE FOR Write OF Item.

/* 
 * Generate Po if there is not enough qty 
 */

IF Item.MinQty >
   ((Item.OnHand - Item.Allocated) + item.onorder) THEN DO:
  
   FIND FIRST supplieritemxref where supplieritemxref.itemnum =
   item.itemnum NO-LOCK NO-ERROR.
   
   IF AVAIL supplieritemxref THEN DO:
        
        FIND supplier WHERE supplier.supplieridnum =
            supplieritemxref.supplieridnum NO-LOCK NO-ERROR.       
       
        IF AVAIL supplier THEN DO:
             CREATE purchaseorder.
             ASSIGN 
                purchaseorder.DateEntered = today
                purchaseorder.POStatus = "Ordered"
                purchaseorder.SupplierIDNum = supplieritemxref.supplieridnum.
              
             CREATE poline.
             ASSIGN
             poline.ponum = purchaseorder.ponum
             poline.linenum = 1
             poline.Discount = supplier.discount
             poline.Itemnum = item.itemnum
             poline.Price = item.price
             poline.qty = item.reorder
             poline.ExtendedPrice = (item.price * poline.qty) * (1 - supplier.discount).
             item.onorder = item.onorder + item.reorder.
  
     /******
             MESSAGE "Purchase Order: " purchaseorder.ponum " for " Item.ItemName
             ", Item Number: " Item.ItemNum " has been generated."
             VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
     *******/
        END. /* If avail suplier*/
     
  END. /*if avail supplieritemxref*/
  
END.
