define variable hPrc as handle.
define variable hPrc2 as handle.
define variable custNum as int.
define variable invNum as int.
define variable cntry as int.


define input parameter ipSelect as char.

if ipSelect eq '' then do:
  if dynamic-function('getTaxAmnt', input custNum, input invNum, input cntry) > int(ipSelect) then do:
    return dynamic-function('getTaxAmnt2' in hPrc, input custNum).
    if valid-handle(hPrc2) then delete procedure hPc2.
  end.
end.
else
  return false.




if can-find (first customer where customer.custnum = custNum) then do:
  find first customer where customer.custnum = custNum.
                      exclusive-lock no-error.
end.


if can-find (first customer where customer.custnum = custNum) 
   and can-find (first invoice where invoice.invnum = invNum) then false.
else true.



