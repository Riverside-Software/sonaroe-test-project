subscribe to 'preCompileHook' anywhere.
subscribe to 'postCompileHook' anywhere.

procedure preCompileHook:
  define input  parameter ipSrcFile as character no-undo.
  define input  parameter ipRCode   as character no-undo.
  define input  parameter ipXref    as character no-undo.
  define output parameter opCancel  as integer   no-undo.

  log-manager:write-message ("Pre-compilation hook").
  opCancel = 0. // Don't cancel compilation
  // Return 1 to log a warning, and 2 to log an error
end.

procedure postCompileHook:
  define input  parameter ipSrcFile as character no-undo.
  define input  parameter ipRCode   as character no-undo.
  define input  parameter ipXref    as character no-undo.

end procedure.
