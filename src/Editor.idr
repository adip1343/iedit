import Control.ST
import Control.ST.ImplicitCall

%include C "ceditor.h"
%link C "ceditor.o"

record EditorState where
	constructor MkEditor
	cursor : (Int, Int)
	screen : (Int, Int)
	numRows : Int
	erow : String

initialEditorState : EditorState
initialEditorState = MkEditor (0, 0) (0, 0) 0 ""

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

	moveCursor : EditorState -> (Int, Int) -> EditorState
	moveCursor e (dx, dy) 
		= let (x, y) = cursor e in 
			let (rows, cols) = screen e in
				set_cursor ((clip (x+dx) 0 (cols-1)), (clip (y+dy) 0 (rows-1))) e

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

	editorDrawRows : Int -> IO (Either () ())
	editorDrawRows 1 = do
		ripe.writeBuffer ("~" ++ clearLineRightOfCursor)
		pure (Right ()) 
	editorDrawRows i = do
		ripe.writeBuffer ("~" ++ clearLineRightOfCursor ++ "\r\n")
		editorDrawRows (i-1)

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
	handleKeypress : (editor : Var) -> (key : Int) ->ST m (Either () ()) [editor ::: Editor]
	refreshScreen : (editor : Var) -> ST m (Either () ()) [editor ::: Editor]
	remove : (editor : Var) -> ST m () [Remove editor Editor]
	readKey : (editor : Var) -> ST m (Either () Int) [editor ::: Editor]

implementation EditorIO IO where
	
	Editor = State EditorState

	init = do
		lift enterRawMode 
		editor <- new initialEditorState
		Right (rows, cols) <- lift $ ripe.getWindowSize | Left () => pure editor
		e <- read editor
		write editor (set_screen (rows, cols) e)
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
		e <- read editor
		write editor (utils.moveCursor e (-1, 0))
		pure (Right ())
	
	-- arrowRight
	handleKeypress editor 1001 = do
		e <- read editor
		write editor (utils.moveCursor e (1, 0))
		pure (Right ())
	
	-- arrowUp
	handleKeypress editor 1002 = do
		e <- read editor
		write editor (utils.moveCursor e (0, -1))
		pure (Right ())

	-- arrowDown
	handleKeypress editor 1003 = do
		e <- read editor
		write editor (utils.moveCursor e (0, 1))
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
		e <- read editor
		lift $ ripe.writeBuffer hideCursor	
		lift $ ripe.writeBuffer (escapes.moveCursor (0, 0))	 
		lift $ ripe.editorDrawRows $ fst (screen e)
		lift $ ripe.writeBuffer (escapes.moveCursor (cursor e))
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
	listenEvent editor
	remove editor)

-- main : IO ()
-- main  = do 
-- 	Right (rows, cols) <- getWindowSize | Left () => pure ()
-- 	printLn $ show (rows, cols)