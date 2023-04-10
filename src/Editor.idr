import Control.ST
import Control.ST.ImplicitCall
import Types
import Interface
import Utils
import Update 

-- draws rows to screen
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

editorDrawStatusBar : EditorState -> IO (Either () ())
editorDrawStatusBar e 
	= let MkEditor (cx, cy) rx (row, col) (offx, offy) nRows _ fileName rows = e in
		let msg = ((if fileName == "" then "[No Name]" else fileName) ++ 
			(" - " ++ (show nRows) ++ " lines")) ++
			("            Ln " ++ (show (cy+1)) ++ ", " ++ "Col " ++ (show (rx+1))) in
			do
				writeBuffer invertColors
				writeBuffer (pad col msg)
				writeBuffer (unsetInvertColors)
				pure (Right ())


-- get name of file from command line args
editorGetFileName : IO (Either () String)
editorGetFileName = do
	args <- getArgs
	case length args < 2 of
		True => pure (Left ())
		False => let Just fileName = (index' (cast 1) args) in pure (Right fileName)

-- read file content into list of strings
editorReadFile : (fileName : String) -> IO (Either () (List Erow))
editorReadFile fileName = do
	Right text <- readFile fileName | Left fileError => pure (Left ())
	pure (Right (map updateErow (lines text)))

-- write file content to disk
editorWriteFile : (fileName : String) -> (rows : List Erow) -> IO (Either () ())
editorWriteFile fileName rows 
	= let toWrite =  unlines (map chars rows) in
		do
			Right () <- writeFile fileName toWrite | Left fileError => pure (Left ())
			pure (Right ())

interface EditorIO (m : Type -> Type) where
	Editor : Type
	init : ST m Var [add Editor]
	loadFile : (editor : Var) -> ST m (Either () ()) [editor ::: Editor]
	saveFile : (editor : Var) -> ST m (Either () ()) [editor ::: Editor]
	handleKeypress : (editor : Var) -> (key : Key) -> ST m (Either () ()) [editor ::: Editor]
	refreshScreen : (editor : Var) -> ST m (Either () ()) [editor ::: Editor]
	close : (editor : Var) -> ST m () [Remove editor Editor]
	readKey : (editor : Var) -> ST m (Either () Key) [editor ::: Editor]

implementation EditorIO IO where
	
	Editor = State EditorState
	
	loadFile editor = do
		Right fileName <- lift $ editorGetFileName | Left noFile => pure (Right ())
		Right fileLines <- lift $ editorReadFile fileName | Left fileError => pure (Left ())
		update editor (set_fileName fileName)
		update editor (set_numRows (cast (length fileLines)))
		update editor (set_rows fileLines)
		pure (Right ())
	
	saveFile editor = do
		MkEditor _ _ _ _ _ _ fileName rows <- read editor
		Right () <- lift $ editorWriteFile fileName rows | Left fileError => pure (Left ())
		pure(Right ())

	init = do
		lift enterRawMode 
		editor <- new initialEditorState
		Right (r, c) <- lift getWindowSize | Left () => pure editor
		e <- read editor
		update editor (set_screen (r-1, c))
		Right () <- loadFile editor | Left () => pure editor
		pure editor

	readKey editor = do
		Right key <- lift editorReadKey | Left () => pure (Left ())
		pure (Right key)
	
	-- other keys
	handleKeypress editor (CharKey key) = do
		update editor (editorInsertChar (CharKey key))
		update editor editorRecalculateNumRows
		pure (Right ())

	-- Ctrl-Q
	handleKeypress editor CtrlQ = do
		lift $ writeBuffer clearScreen
		lift $ writeBuffer (escapes.moveCursor (0, 0))
		pure (Left ())

	-- Ctrl-S
	handleKeypress editor CtrlS = saveFile editor

	-- movement keys
	handleKeypress editor key = do
		update editor (editorCursorMovement key)
		pure (Right ()) 

	refreshScreen editor = do
		update editor editorRenderx
		update editor editorScroll
		lift $ writeBuffer hideCursor	
		lift $ writeBuffer (escapes.moveCursor (0, 0))	 
		lift $ editorDrawRows !(read editor) 0
		lift $ editorDrawStatusBar !(read editor)
		MkEditor (cx, cy) rx _ (offx, offy) numRows _ _ rows <- read editor
		lift $ writeBuffer (escapes.moveCursor (rx - offx, cy - offy))
		lift $ writeBuffer showCursor

	close editor = delete editor

-- event loop listening to keypress and updating
listenEvent : (EditorIO m, ConsoleIO m) => 
				(editor : Var) -> ST m () [editor ::: Editor {m}]
listenEvent editor = do
		Right key <- readKey editor | Left () => pure ()
		Right userContinues <- handleKeypress editor key | Left () => pure ()
		refreshScreen editor
		listenEvent editor

main : IO ()
main = run (do 
	editor <- init
	refreshScreen editor
	listenEvent editor
	close editor)