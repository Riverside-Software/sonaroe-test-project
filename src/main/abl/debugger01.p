DEFINE TEMP-TABLE tt1 NO-UNDO
  FIELD fld1 AS CHARACTER.
DEFINE TEMP-TABLE tt2 NO-UNDO
  FIELD fld1 AS CHARACTER
  FIELD fld2 AS INT64
  FIELD fld3 AS RAW.
DEFINE TEMP-TABLE tt3 NO-UNDO
  FIELD fld4 AS CHARACTER
  FIELD fld5 AS DECIMAL.

DEFINE DATASET ds1 FOR tt1, tt2, tt3.

DEF VAR c1  AS CHAR NO-UNDO.
DEF VAR c2  AS CHAR NO-UNDO.
DEF VAR i1  AS INT  NO-UNDO.
DEF VAR i2  AS INT  NO-UNDO. 
DEF VAR i3 AS INT EXTENT 10 NO-UNDO.
// Should be caught by SQ secrets sensor
define variable sqToken as character initial 'squ_30b8185f4f3d94e6c3ab4aa5df39da190533b0ac'.
var CefSharp.DevTools.Emulation.UserAgentMetadata webView1.
message webView1:Architecture.

i1 = 1.
i2 = 10000000.
c1 = "Test Unicode ắ Γ é à". // Unicode
c2 = FILL('--', 20).

CREATE tt1.
ASSIGN tt1.fld1 = "foobar".
CREATE tt1.
ASSIGN tt1.fld1 = "foobar2".

MESSAGE THIS-PROCEDURE SESSION.
i3[1] = 111.
i3[2] = 222.
i3[4] = 444.

RUN debugger02.p (INPUT 1, INPUT 2, OUTPUT c1).
RUN sample/coverage2.p.

QUIT.
