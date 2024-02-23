def input parameter ipIn1 as int no-undo.
def input parameter ipIn2 as int no-undo.
def output parameter opOut1 as char no-undo.

define variable hdl1 as handle no-undo.
define variable oProxy as OpenEdge.Net.URI no-undo.
define variable oSimple1 as rssw.SimpleObject no-undo.
define variable oSimple2 as rssw.SimpleObject no-undo.
define variable oSimple3 as rssw.SimpleObject no-undo.
define variable hdlArr as handle extent 5 no-undo.
define variable objArr as rssw.SimpleObject extent 5 no-undo.

define buffer Benefits for Benefits.
find first Benefits no-lock no-error.

hdl1 = this-procedure.

hdlArr[1] = this-procedure.
hdlArr[2] = file-information.
hdlArr[3] = session.
objArr[1] = new rssw.SimpleObject().
objArr[2] = new rssw.SimpleObject().
objArr[3] = new rssw.SimpleObjectChild2(objArr[1]).
objArr[4] = new rssw.SimpleObjectChild2(objArr[3]).
oSimple3 = new rssw.SimpleObjectChild1().
objArr[5] = oSimple3.
oSimple2 = objArr[4].


run internalProc1.
message "Next statement in an include file" view-as alert-box.
{ debugger02.i World }

oProxy = new OpenEdge.Net.URI('http', "localhost", 1000).
message oProxy:toString().
oSimple1 = new rssw.SimpleObject().
oSimple1:method1(1).

opOut1 = string(ipIn1).
opOut1 = opOut1 + " ".
opOut1 = opOut1 + string(ipIn2).

return.

procedure internalProc1:
    define variable mmm as int initial 10 no-undo.
    
    bell.

    run internalProc2.
end procedure.

procedure internalProc2:
    bell.
end procedure.
