{ sample/find.i customer }
{ sample/find.i item }
find first
warehouse.

for each customer where customer.name begins 'c' by customer.address desc:
  display customer.name.
end.

