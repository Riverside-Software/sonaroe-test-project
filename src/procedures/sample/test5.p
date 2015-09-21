def var zz as int no-undo.

procedure foo :
 def input param prm1 as int.
 def output param prm2 as int no-undo.

 /* This is a very long line. This is a very long line. This is a very long line. This is a very long line. This is a very long line. This is a very long line. This is a very long line. This is a very long line. This is a very long line. This is a very long line. This is a very long line. This is a very long line. */
 for each customer where customer.custnum eq prm1 no-lock:
    prm2 = custnum.   
 end.
 
end procedure.

message "Text1".

do transaction:
  find first

  item 
  
  
  exclusive-lock.
  disp item.itemnum.
  if false then do transaction:
    message "foo".
    message "bar".
  end.
    
end.

&GLOBAL-DEFINE MYDEF 10


do transaction:
  find first customer exclusive-lock.
  disp customer.custnum.
end.

DEF VAR cls1 AS Consultingwerk.InterfaceSample.Class1.
cls1 = new Consultingwerk.InterfaceSample.Class1().
cls1:MethodFromInterface1().

return '0'.
