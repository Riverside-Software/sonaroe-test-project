DEFINE VARIABLE qry as HANDLE NO-UNDO.
DEFINE VARIABLE inputParam AS CHARACTER NO-UNDO.
qry:QUERY-PREPARE("for each customer where name = '" + inputParam + "'"). 

find first customer.
qry:QUERY-PREPARE("for each product where product.name = begins '" + customer.name + "'").

