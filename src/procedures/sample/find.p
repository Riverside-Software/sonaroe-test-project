{ sample/find.i customer }
{ sample/find.i item }
find first
warehouse.

for each customer where customer.name begins 'c' by customer.address desc:
  display customer.name.
end.

define variable qry1 as handle no-undo.
create query qry1.
open query qry1 for each invoice where invoice.invoicedate >= date(1, 1, 2020)
                                   and invoice.invoicedate <= date(31, 1, 2020)
                                   and shipCharge LT 10
                                   and invoice.custnum = 54
                                 use-index invoiceNum by amount.




define temp-table ttCustomer no-undo like customer
   index ttCustPK as primary unique custnum.
