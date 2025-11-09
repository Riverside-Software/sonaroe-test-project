define buffer bb for customer.
do transaction:
  find first bb.
  find first customer exclusive-lock.
  find first item exclusive-lock.
  disp customer.custnum item.itemnum.
end.

&GLOBAL-DEFINE PPVAR1 100
