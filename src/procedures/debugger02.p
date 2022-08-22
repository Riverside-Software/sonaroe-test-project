def input parameter ipIn1 as int.
def input parameter ipIn2 as int.
def output parameter opOut1 as char.

// define variable zzz as rssw.MyClass.
define variable xxx as int.
define variable oProxy as OpenEdge.Net.URI.
define variable oSimple as rssw.SimpleObject.

define buffer Benefits for Benefits.

run internalProc1.
message "Next statement in an include file" view-as alert-box.
{ debugger02.i World }

// zzz = new rssw.MyClass(3, "4").
// zzz:method1(8).
oProxy = new OpenEdge.Net.URI('http', "localhost", 1000).
message oProxy:toString().
oSimple = new rssw.SimpleObject().
oSimple:method1(1).

opOut1 = string(ipIn1).
opOut1 = opOut1 + " ".
opOut1 = opOut1 + string(ipIn2).
for each Benefits no-lock:
   display ipIn1 DependentCare HealthCare MedicalSpending.
end.

return.

procedure internalProc1:
    define variable mmm as int initial 10.
    
    bell.

    run internalProc2.
end procedure.

procedure internalProc2:
    bell.
end.
