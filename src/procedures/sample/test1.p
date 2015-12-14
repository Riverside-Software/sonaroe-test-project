def var zz as int no-undo.

/* Some comments
Et des caractères accentués
<b>Vérification échappement code HMTL</b>
&lt;b&gt;Deuxième vérification&lt;/b&gt;
*/
procedure foo :
 def input param prm1 as int.
 def output param prm2 as int no-undo.

 for each customer where customer.custnum eq prm1 no-lock:
    prm2 = custnum.   
 end.
 
end procedure.

define shared temp-table tt1 no-undo
 field a as char
 field b as char.
define shared buffer b1 for tt1.
define shared dataset ds1 for b1.

{ sample/inc/test.i} find first item exclusive-lock.
disp item.itemnum.

/* Comment level 1 /*

Nested comment 1 */ Still level 1
 RUN VALUE(foobar) Should be commented
*/


RUN VALUE("Hello !").

/* One more comment /* Nested 1 */ Comment */
