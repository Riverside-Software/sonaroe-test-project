/** Some comments */

find first customer.
if customer.custnum < 0 then do:
  message "msg".
end.

DEF NEW SHARED VAR var1        AS INTEGER NO-UNDO.
DEF NEW SHARED VAR var2        AS INTEGER NO-UNDO.
DEF NEW SHARED VAR var3        AS INTEGER NO-UNDO.
DEF NEW SHARED VAR var4        AS INTEGER NO-UNDO.
DEF NEW SHARED VAR var5        AS INTEGER NO-UNDO.
DEF NEW SHARED VAR var6        AS INTEGER NO-UNDO.
DEF NEW SHARED VAR var7        AS INTEGER NO-UNDO.
DEF NEW SHARED VAR var8        AS INTEGER NO-UNDO.
DEF NEW SHARED VAR var9        AS INTEGER NO-UNDO.
DEF NEW SHARED VAR var10       AS INTEGER NO-UNDO.
DEF NEW SHARED VAR var11       AS INTEGER NO-UNDO.
DEF NEW SHARED VAR var12       AS INTEGER NO-UNDO.
DEF NEW SHARED VAR var13       AS INTEGER NO-UNDO.
DEF NEW SHARED VAR var14       AS INTEGER NO-UNDO.
DEF NEW SHARED VAR var15       AS INTEGER NO-UNDO.

define new shared temp-table tt1 no-undo
 field a as char
 field b as char.
define new shared buffer b1 for tt1.
define new shared dataset ds1 for b1.

for each item :
  run proc1 (input item.itemnum).
end.

procedure proc1:
  DEF INPUT PARAMETER prm1 as INTEGER.

  def var zz as char no-undo.
  
  if (prm1 mod 2) = 0 then do:
    display item.itemname
     item.catpage.
  end.  
end procedure.

return '0'.
