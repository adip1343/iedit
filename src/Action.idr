module Action

import Types
import Interface
import Utils
import Update 

-- draws rows to screen
export
editorDrawRows : EditorState -> (j : Int) ->  IO (Either () ())
editorDrawRows  e j = let MkEditor (cx, cy) rx (row, col) (offx, offy) nRows _ _ rows = e in
	let i = j + offy in 
		let Just toDraw = if i < nRows then (index' (cast i) rows) else (Just emptyErow) in  
			do
				case j < row of
					True => do
						writeBuffer (substr (cast offx) (cast col) (render toDraw)) 
						writeBuffer (clearLineRightOfCursor ++ "\r\n")
						editorDrawRows e (j+1)
					False => pure (Right ())

-- draws status bar in bottom line
export
editorDrawStatusBar : EditorState -> IO (Either () ())
editorDrawStatusBar e 
	= let MkEditor (cx, cy) rx (row, col) (offx, offy) nRows inSync fileName rows = e in
		let msg = ((if fileName == "" then "[No Name]" else fileName) ++ 
			(if inSync then "" else "(modified)") ++
			(" - " ++ (show nRows) ++ " lines")) ++
			("            Ln " ++ (show (cy+1)) ++ ", " ++ "Col " ++ (show (rx+1))) in
			do
				writeBuffer invertColors
				writeBuffer (pad col msg)
				writeBuffer (unsetInvertColors)
				pure (Right ())


-- get name of file from command line args
export
editorGetFileName : IO (Either () String)
editorGetFileName = do
	args <- getArgs
	case length args < 2 of
		True => pure (Left ())
		False => let Just fileName = (index' (cast 1) args) in pure (Right fileName)

-- read file content into list of strings
export
editorReadFile : (fileName : String) -> IO (Either FileError (List Erow))
editorReadFile fileName = do
	Right text <- readFile fileName | Left fileError => pure (Left fileError)
	pure (Right (map updateErow (lines text)))

-- write file content to disk
export
editorWriteFile : (fileName : String) -> (rows : List Erow) -> IO (Either () ())
editorWriteFile fileName rows 
	= let toWrite =  unlines (map chars rows) in
		do
			Right () <- writeFile fileName toWrite | Left fileError => pure (Left ())
			pure (Right ())