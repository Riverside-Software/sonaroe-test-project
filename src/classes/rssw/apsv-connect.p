define input parameter ipLogin as character no-undo.
define input parameter ipPassword as character no-undo.
define input parameter ipExtra as character no-undo.

message "Connect: " + session:server-connection-id + " -- " + session:server-connection-context.
return ''.
