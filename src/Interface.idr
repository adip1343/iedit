module Interface

import Types
import Utils

%include C "ceditor.h"
%link C "ceditor.o"

-- ffi interface for call to C functions
namespace raw
	-- enter:s raw mode
	export
	enterRawMode : IO ()
	enterRawMode = foreign FFI_C "enterRawMode" (IO ())

	runCommand : String -> IO (Raw String)
	runCommand cmd = foreign FFI_C "runCommand" (String -> IO (Raw String)) cmd

	readChar : IO Char
	readChar = foreign FFI_C "readChar" (IO Char)

	readCharBlocking : IO Char
	readCharBlocking = foreign FFI_C "readCharBlocking" (IO Char)

	-- @TODO rewrite with read calls
	editorReadKey : IO Int
	editorReadKey = foreign FFI_C "editorReadKey" (IO Int)

	-- @TODO buffer writes
	writeBuffer : String -> Int -> IO Int
	writeBuffer = foreign FFI_C "writeBuffer" (String -> Int -> IO Int)

	getWindowRows : IO Int
	getWindowRows = foreign FFI_C "getWindowRows" (IO Int)

	getWindowCols : IO Int
	getWindowCols = foreign FFI_C "getWindowCols" (IO Int)

-- wrapping C calls for error handling
export
writeBuffer : String -> IO (Either () ())
writeBuffer buf = do
	ret <- raw.writeBuffer  buf (prim_lenString buf)
	case ret /= -1 of
		True =>	pure $ Right ()
		False => pure $ Left ()

export
editorReadKey : IO (Either () Key)
editorReadKey = do
	ret <- raw.editorReadKey
	case ret /= -1 of
		True => pure $ Right (intToKey ret)
		False => pure $ Left ()

-- @TODO : reduce calls to C functions
export
getWindowSize : IO (Either () (Int, Int))
getWindowSize = do
	rows <- getWindowRows
	case rows == 0 of
		True => pure (Left ())
		False => do
			cols <- getWindowCols
			case cols == 0 of
				True => pure (Left ())
				False => pure (Right (rows, cols))

export
runCommand : String -> IO String
runCommand cmd = do
	MkRaw result <- raw.runCommand ("idris --client \"" ++ cmd ++"\"") 
	pure result