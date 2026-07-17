subscribe to 'preCompileHook' anywhere.
subscribe to 'postCompileHook' anywhere.

procedure preCompileHook:
  define input  parameter ipSrcFile  as character no-undo.
  define input  parameter ipBuildDir as character no-undo.
  define input  parameter ipXref     as character no-undo.
  define output parameter opCancel   as integer   no-undo.

  log-manager:write-message ("Pre-compilation hook").

  // 0 -> No effect
  // 1 -> Cancel with warning (deprecated)
  // 2 -> Cancel with error (deprecated)
  // 3 -> Handled by hook, compiled successfully
  // 4 -> Handled by hook, compilation error
  // run hookWarning in source-procedure (789, 2, search(ipSrcFile), "My message"). /* Err num, line number, absolute file name, message */
  opCancel = 0. // Don't cancel compilation

end procedure.

procedure postCompileHook:
  define input  parameter ipSrcFile as character no-undo.
  define input  parameter ipBuildDir   as character no-undo.
  define input  parameter ipXref    as character no-undo.

  // Also possible to use hookWarning or hookError
  // run hookWarning in source-procedure (789, 2, search(ipSrcFile), "My message"). /* Err num, line number, absolute file name, message */

end procedure.
