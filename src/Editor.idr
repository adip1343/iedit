import Control.ST
import Control.ST.ImplicitCall

%include C "ceditor.h"
%link C "ceditor.o"

record EditorState where
	constructor MkEditor
	cursor : (Int, Int)
	screen : (Int, Int)
	offset : (Int, Int)
	numRows : Int
	rows : List String

initialEditorState : EditorState
initialEditorState = MkEditor (0, 0) (0, 0) (0, 0) 0 []


namespace raw
	-- enters raw mode
	enterRawMode : IO ()
	enterRawMode = foreign FFI_C "enterRawMode" (IO ())

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

namespace utils
	ctrl : Char -> Int
	ctrl key = prim__andInt 0x1f (prim__charToInt key)

	times : Int -> String -> String
	times 0 _ = ""
	times i str = str ++ (times (i-1) str)

	clip : (x : Int) -> (min : Int) -> (max: Int) -> Int
	clip x mn mx = case compare mn x of
		GT => mn 
		EQ => mn
		LT => case compare x mx of 
				GT => mx
				EQ => mx
				LT => x

	moveCursor : (Int, Int) -> EditorState -> EditorState
	moveCursor (dx, dy) e 
		= let MkEditor (cx, cy) (row, col) (offx, offy) numRows rows = e in 
			set_cursor ((clip (cx+dx) 0 (col-1)), (clip (cy+dy) 0 numRows)) e

	editorScroll : EditorState -> EditorState
	editorScroll e = let MkEditor (cx, cy) (row, col) (offx, offy) numRows rows = e in
						let updatedOffy = min cy offy in
							set_offset (offx, max updatedOffy (cy - row + 1)) e


namespace escapes
	clearScreen : String
	clearScreen = "\ESC[2J"

	moveCursor : (Int, Int) -> String
	moveCursor (x, y) = ("\ESC["++ (show (y+1)) ++ ";" ++ (show (x+1)) ++ "H")

	hideCursor : String
	hideCursor = "\ESC[?25l"

	showCursor : String
	showCursor = "\ESC[?25h"

	clearLineRightOfCursor : String
	clearLineRightOfCursor = "\ESC[K"

namespace ripe

	writeBuffer : String -> IO (Either () ())
	writeBuffer buf = do
		ret <- raw.writeBuffer  buf (prim_lenString buf)
		case ret /= -1 of
			True =>	pure $ Right ()
			False => pure $ Left ()

	editorReadKey : IO (Either () Int)
	editorReadKey = do
		ret <- raw.editorReadKey
		case ret /= -1 of
			True => pure (Right ret)
			False => pure (Left ())
	
	-- 
	editorDrawRows : EditorState -> (j : Int) ->  IO (Either () ())
	editorDrawRows  e j 
	= let MkEditor (cx, cy) (row, col) (offx, offy) nRows rows = e in
		let i = j + offy in 
	  		let Just toDraw = if i < nRows then (index' (cast i) rows) else (Just "~") in  
				do
					case compare j (row-1) of
						LT => do
							ripe.writeBuffer (substr (cast 0) (cast col) toDraw) 
							ripe.writeBuffer (clearLineRightOfCursor ++ "\r\n")
							editorDrawRows e (j+1)
						EQ => do
							ripe.writeBuffer (substr (cast 0) (cast col) toDraw)
							ripe.writeBuffer clearLineRightOfCursor
							editorDrawRows e (j+1)
						GT => pure (Right ())

	editorReadFile : (fileName : String) -> IO (Either () (List String))
	editorReadFile fileName = do
		Right text <- readFile fileName | Left fileError => pure (Left ())
		pure (Right (lines text))
	
	editorGetFileName : IO (Either () String)
	editorGetFileName = do
		args <- getArgs
		case length args < 2 of
			True => pure (Left ())
			False => let Just fileName = (index' (cast 1) args) in pure (Right fileName)
		

	-- @TODO : reduce calls to C functions
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


interface EditorIO (m : Type -> Type) where
	Editor : Type
	init : ST m Var [add Editor]
	loadFile : (editor : Var) -> ST m (Either () ()) [editor ::: Editor]
	handleKeypress : (editor : Var) -> (key : Int) ->ST m (Either () ()) [editor ::: Editor]
	refreshScreen : (editor : Var) -> ST m (Either () ()) [editor ::: Editor]
	remove : (editor : Var) -> ST m () [Remove editor Editor]
	readKey : (editor : Var) -> ST m (Either () Int) [editor ::: Editor]

implementation EditorIO IO where
	
	Editor = State EditorState
	
	loadFile editor = do
		Right fileName <- lift $ editorGetFileName | Left noFile => pure (Right ())
		Right fileLines <- lift $ editorReadFile fileName | Left fileError => pure (Left ())
		update editor (set_numRows (cast (length fileLines)))
		update editor (set_rows fileLines)
		pure (Right ())

	init = do
		lift enterRawMode 
		editor <- new initialEditorState
		Right (r, c) <- lift $ ripe.getWindowSize | Left () => pure editor
		e <- read editor
		update editor (set_screen (r, c))
		Right () <- loadFile editor | Left () => pure editor
		pure editor

	readKey editor = do
		Right key <- lift ripe.editorReadKey | Left () => pure (Left ())
		pure (Right key)
	
	-- Ctrl-Q
	handleKeypress editor 17 = do
		lift $ ripe.writeBuffer clearScreen
		lift $ ripe.writeBuffer (escapes.moveCursor (0, 0))
		pure (Left ())

	-- arrowLeft 
	handleKeypress editor 1000 = do
		update editor (utils.moveCursor (-1, 0))
		pure (Right ())
	
	-- arrowRight
	handleKeypress editor 1001 = do
		update editor (utils.moveCursor (1, 0))
		pure (Right ())
	
	-- arrowUp
	handleKeypress editor 1002 = do
		update editor (utils.moveCursor (0, -1))
		pure (Right ())

	-- arrowDown
	handleKeypress editor 1003 = do
		update editor (utils.moveCursor (0, 1))
		pure (Right ())

	-- -- delKey
	-- handleKeypress editor 1004 = ?hole
	
	-- homeKey
	-- handleKeypress editor 1005 = ?hole
	
	-- -- endKey
	-- handleKeypress editor 1006 = ?hole
	
	-- -- pageUp
	-- handleKeypress editor 1007 = ?hole
	
	-- -- pageDown
	-- handleKeypress editor 1007 = ?hole

	-- -- backSpace
	-- handleKeypress editor 127 = ?hole
	
	-- other keys
	handleKeypress editor c = pure (Right ()) 

	refreshScreen editor = do
		update editor editorScroll
		lift $ ripe.writeBuffer hideCursor	
		lift $ ripe.writeBuffer (escapes.moveCursor (0, 0))	 
		lift $ ripe.editorDrawRows !(read editor) 0
		MkEditor (cx, cy) (row, col) (offx, offy) numRows rows <- read editor
		lift $ ripe.writeBuffer (escapes.moveCursor (cx, cy - offy))
		lift $ ripe.writeBuffer showCursor
	
	remove editor = delete editor

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
	remove editor)