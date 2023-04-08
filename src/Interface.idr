module Interface

import Types
import Utils

%include C "ceditor.h"
%link C "ceditor.o"

-- ffi interface for call to C functions
namespace raw
	-- enters raw mode
	export
	enterRawMode : IO ()
	enterRawMode = foreign FFI_C "enterRawMode" (IO ())

	export
	readChar : IO Char
	readChar = foreign FFI_C "readChar" (IO Char)

	export
	readCharBlocking : IO Char
	readCharBlocking = foreign FFI_C "readCharBlocking" (IO Char)

	-- @TODO rewrite with read calls
	export
	editorReadKey : IO Int
	editorReadKey = foreign FFI_C "editorReadKey" (IO Int)

	-- @TODO buffer writes
	export
	writeBuffer : String -> Int -> IO Int
	writeBuffer = foreign FFI_C "writeBuffer" (String -> Int -> IO Int)

	export
	getWindowRows : IO Int
	getWindowRows = foreign FFI_C "getWindowRows" (IO Int)

	export
	getWindowCols : IO Int
	getWindowCols = foreign FFI_C "getWindowCols" (IO Int)


-- namespace ripe
-- 	export
-- 	writeBuffer : String -> IO (Either () ())
-- 	writeBuffer buf = do
-- 		ret <- raw.writeBuffer  buf (prim_lenString buf)
-- 		case ret /= -1 of
-- 			True =>	pure $ Right ()
-- 			False => pure $ Left ()

-- 	export
-- 	editorReadKey : IO (Either () Int)
-- 	editorReadKey = do
-- 		ret <- raw.editorReadKey
-- 		case ret /= -1 of
-- 			True => pure (Right ret)
-- 			False => pure (Left ())

-- export
-- editorDrawRows : EditorState -> (j : Int) ->  IO (Either () ())
-- editorDrawRows  e j = let MkEditor (cx, cy) (row, col) (offx, offy) nRows rows = e in
-- 	let i = j + offy in 
-- 		let Just toDraw = if i < nRows then (index' (cast i) rows) else (Just "~") in  
-- 			do
-- 				case compare j (row-1) of
-- 					LT => do
-- 						ripe.writeBuffer (substr (cast offx) (cast col) toDraw) 
-- 						ripe.writeBuffer (clearLineRightOfCursor ++ "\r\n")
-- 						editorDrawRows e (j+1)
-- 					EQ => do
-- 						ripe.writeBuffer (substr (cast offx) (cast col) toDraw)
-- 						ripe.writeBuffer clearLineRightOfCursor
-- 						editorDrawRows e (j+1)
-- 					GT => pure (Right ())

-- export
-- editorReadFile : (fileName : String) -> IO (Either () (List String))
-- editorReadFile fileName = do
-- 	Right text <- readFile fileName | Left fileError => pure (Left ())
-- 	pure (Right (lines text))

-- export
-- editorGetFileName : IO (Either () String)
-- editorGetFileName = do
-- 	args <- getArgs
-- 	case length args < 2 of
-- 		True => pure (Left ())
-- 		False => let Just fileName = (index' (cast 1) args) in pure (Right fileName)
	

-- -- @TODO : reduce calls to C functions
-- export
-- getWindowSize : IO (Either () (Int, Int))
-- getWindowSize = do
-- 	rows <- getWindowRows
-- 	case rows == 0 of
-- 		True => pure (Left ())
-- 		False => do
-- 			cols <- getWindowCols
-- 			case cols == 0 of
-- 				True => pure (Left ())
-- 				False => pure (Right (rows, cols))