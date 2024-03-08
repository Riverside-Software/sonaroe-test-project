DEFINE VARIABLE lcVar1 AS LONGCHAR NO-UNDO.
DEFINE VARIABLE cVar2  AS CHARACTER NO-UNDO.

MESSAGE STRING(lcVar1) VIEW-AS ALERT-BOX. // Fail if lcVar longer than 31999 bytes
ASSIGN cVar2 = "Prefix " + lcVar1. // Also fail if lcVar longer than 31999 bytes

DEFINE VARIABLE iVar1 AS INTEGER NO-UNDO.
DEFINE VARIABLE iVar2 AS INT64   NO-UNDO.
ASSIGN iVar1 = NEXT-VALUE(NextCustNum). // Sequences return 64 bits int
ASSIGN iVar1 = 1.2 * iVar2. // Total can be longer than 2^32 

{ sample/inc/test.i }
