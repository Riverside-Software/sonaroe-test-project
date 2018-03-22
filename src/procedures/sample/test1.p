def var zz as int no-undo.

/* Some comments
And accents é à ê ù
<b>HTML escape ?</b>
&lt;b&gt;Deuxième vérification&lt;/b&gt;
Other alphabets: юллюм ρεκυε सार्वजनिक 史発暮 ヱツヤ彼 🍺🍻🍽🍷😫🥂🥂😵🥃🥃😴
*/

procedure foo :
 def input param prm1 as int.
 def output param prm2 as int no-undo.

 /* This is a very long line. This is a very long line. This is a very long line. This is a very long line. This is a very long line. This is a very long line. This is a very long line. This is a very long line. This is a very long line. This is a very long line. This is a very long line. This is a very long line. */
 for each customer where customer.custnum eq prm1 no-lock:
    prm2 = custnum.   
 end.
 
end procedure.

define new shared temp-table tt1 no-undo
 field a as char
 field b as char.
define new shared buffer b1 for tt1.
define new shared dataset ds1 for b1.

{ sample/inc/test.i}
find first item exclusive-lock.
disp item.itemnum.

/* Backslash rule */
message "C:\Temp\hello.txt".

/* Comment level 1 /*

Nested comment 1 */ Still level 1
 RUN VALUE(xxx) Should be commented
*/


/* One more comment /* Nested 1 */ Comment */
def var obj as progress.lang.object.
DEFINE VARIABLE foobar AS CHARACTER NO-UNDO initial "Progress.Lang.Object".
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
obj2:MyProperty:Yes = 2000 no-error.

System.ComponentModel.BrowsableAttribute:No.

&IF OPSYS = "WIN32" &THEN
 MESSAGE "Win32".
&ENDIF
&IF OPSYS = "UNIX" &THEN
 MESSAGE "Unix".
&ENDIF

return '0'.
