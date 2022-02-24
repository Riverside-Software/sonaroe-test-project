// RUN statement passes five input parameters, as well as two compile-time arguments
DEFINE VARIABLE var1 AS CHARACTER INITIAL "prm2".
DEFINE VARIABLE var2 AS CHARACTER INITIAL "prm4".
RUN procedure.p ("param1", "param2", "param3", "param4", "param5") var1 var2.
RUN procedure.p ("param1", "param2", "param4", dynamic-function("param5"))).

