&GLOBAL-DEFINE COV1 TRUE

MESSAGE "xxx".
MESSAGE "xxx".
MESSAGE "xxx".
MESSAGE "xxx".
MESSAGE "xxx".
MESSAGE "xxx".
MESSAGE "xxx".

IF TRUE THEN
  MESSAGE "xxx".
IF FALSE THEN
  MESSAGE "xxx".

{ sample/inc/coverage.i }

define temp-table myTT no-undo
 field fld1 as character
 field fld2 as character
 field fld3 as character
 index idx1 is primary unique fld1
 index idx2 fld2
 index idx3 fld3
 index idx4 fld3 fld2.

define temp-table myTT2 no-undo like myTT use-index fld1.
define temp-table myTT3 no-undo like myTT use-index idx1.

{&_proparse_prolint-nowarn(invalid-useindex)}
define temp-table myTT4 no-undo like myTT use-index fld1.

define temp-table myTT5 no-undo like myTT
 use-index fld1
 use-index idx2 as primary
 use-index fld3.

define var xx as int no-undo.

RETURN.
