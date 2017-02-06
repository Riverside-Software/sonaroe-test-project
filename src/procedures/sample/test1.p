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
    prm2 = custnum + 'ABC'.
 end.
 
end procedure.

define shared temp-table tt1 no-undo
 field a as char
 field b as char.
define shared buffer b1 for tt1.
define shared dataset ds1 for b1.

{ sample/inc/test.i} find first item exclusive-lock.
disp item.itemnum.

/* Backslash rule */
message "C:\Temp\hello.txt".

/* Comment level 1 /*

Nested comment 1 */ Still level 1
 RUN VALUE(foobar) Should be commented
*/


RUN VALUE("Hello !").

/* One more comment /* Nested 1 */ Comment */
def var obj as progress.lang.object.
DEFINE VARIABLE foobar AS CHARACTER NO-UNDO.
obj = DYNAMIC-NEW foobar ().

DEFINE FRAME DEFAULT-FRAME 
    "Content of sample.txt" VIEW-AS TEXT
          SIZE 25 BY .62 AT ROW 1.48 COL 24 WIDGET-ID 4
          FONT 6
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 73 BY 29.86 WIDGET-ID 100.
DEFINE VARIABLE CtrlFrame AS WIDGET-HANDLE NO-UNDO.
define variable obj2 as com-handle.
CREATE CONTROL-FRAME CtrlFrame ASSIGN
       FRAME           = FRAME DEFAULT-FRAME:HANDLE
       ROW             = 21.95
       COLUMN          = 3
       HEIGHT          = 1.76
       WIDTH           = 7
       WIDGET-ID       = 28
       HIDDEN          = yes
       SENSITIVE       = yes.
obj2:MyProperty:Yes = 2000.

System.ComponentModel.BrowsableAttribute:No.
