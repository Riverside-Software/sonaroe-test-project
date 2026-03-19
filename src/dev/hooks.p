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
end procedure.

procedure postCompileHook:
  define input  parameter ipSrcFile as character no-undo.
  define input  parameter ipRCode   as character no-undo.
  define input  parameter ipXref    as character no-undo.

  // Also possible to use logInfo or logError
  run logWarning in source-procedure (substitute("&1|&2|&3|&4", 1234 /* Error number */, 2 /* Line number */, search(ipSrcFile), "Warning message")).

end procedure.
