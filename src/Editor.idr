import Control.ST
import Control.ST.ImplicitCall

%include C "ceditor.h"
%link C "ceditor.o"

namespace raw
	-- enters raw mode
	enterRawMode : IO ()
	enterRawMode = foreign FFI_C "enterRawMode" (IO ())

	readChar : IO Char
	readChar = foreign FFI_C "readChar" (IO Char)

	readCharBlocking : IO Char
	readCharBlocking = foreign FFI_C "readCharBlocking" (IO Char)

	-- @TODO buffer writes
	writeBuffer : String -> Int -> IO Int
	writeBuffer = foreign FFI_C "writeBuffer" (String -> Int -> IO Int)

	getWindowRows : IO Int
	getWindowRows = foreign FFI_C "getWindowRows" (IO Int)

	getWindowCols : IO Int
	getWindowCols = foreign FFI_C "getWindowCols" (IO Int)

namespace utils
	ctrl : Char -> Char
	ctrl key = prim__andChar key (prim__intToChar 0x1f)

	times : Int -> String -> String
	times 0 _ = ""
	times i str = str ++ (times (i-1) str)


namespace escapes
	clearScreen : String
	clearScreen = "\ESC[2J"

	moveCursor : (row : Int) -> (col : Int) -> String
	moveCursor row col = ("\ESC["++ (show row) ++ ";" ++ (show col) ++ "H")

	hideCursor : String
	hideCursor = "\ESC[?25l"

	showCursor : String
	showCursor = "\ESC[?25h"

	clearLineRightOfCursor : String
	clearLineRightOfCursor = "\ESC[K"



namespace ripe
	readChar : IO (Either () Char)
	readChar = do
		c <- raw.readChar
		if c /= '\0'
			then pure (Right c)
			else pure (Left ())

	readCharBlocking : IO (Either () Char)
	readCharBlocking = do
		c <- raw.readCharBlocking
		if c /= '\0'
			then pure (Right c)
			else pure (Left ())

	writeBuffer : String -> IO (Either () ())
	writeBuffer buf = do
		ret <- raw.writeBuffer  buf (prim_lenString buf)
		case ret /= -1 of
			True =>	pure $ Right ()
			False => pure $ Left ()

	editorReadKey : IO (Either () Char)
	editorReadKey = ripe.readCharBlocking

	-- return True if user doesn't quits Ctrl Q
	editorHandleKeypress : IO (Either () ())
	editorHandleKeypress = do
		Right c <- editorReadKey | Left () => pure (Left ())	--some error
		case c /= ctrl 'q' of
			True => pure (Right ())
			False => do
				ripe.writeBuffer clearScreen	-- clear screen
				ripe.writeBuffer (moveCursor 1 1)	-- move cursor to top left
				pure (Left ())	-- Right False ?

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

editorRefreshScreen : ST IO (Either () ()) [] 
editorRefreshScreen = do
	Right (rows, cols) <- lift ripe.getWindowSize | Left () =>  returning (Left ()) (pure ())
	lift $ ripe.writeBuffer hideCursor	
	lift $ ripe.writeBuffer (moveCursor 1 1)	 
	lift $ ripe.editorDrawRows rows
	lift $ ripe.writeBuffer (moveCursor 1 1)
	lift $ ripe.writeBuffer showCursor	

handlerLoop : ST IO () []
handlerLoop = do
	Right userContinues <- lift ripe.editorHandleKeypress | Left () =>   returning () (pure ())
	editorRefreshScreen
	handlerLoop

main' : ST IO () []
main' = do
	lift $ raw.enterRawMode
	handlerLoop

main : IO ()
main = run main'

-- SOME RESPECTABLE GARBAGE CAN BE FOUND HERE --
{- 
record Editor where
	constructor MkEditor
	rows : Int
	cols : Int
	cursorX : Int
	cursorY : Int

editorInitState : Editor
editorInitState = MkEditor 
	{rows = 0} 
	{cols = 0}
	{cursorX = 0}
	{cursorY = 0}

mutual
	printCharAndReadInput : Char -> ST IO () []
	printCharAndReadInput c = do
		lift $ putStrLn $ show (cast {to=Int} c) ++ "\r"  
		--else lift $ putStrLn $ show c ++ "\r"
		if c /= 'q'
			then readInput
			else returning () (pure ())

	readInput : ST IO () []
	readInput = do
		Right c <- lift $ readCharBlocking | Left () => returning () (pure ())
		if c /= '\0'
			then printCharAndReadInput c --lift $ putStrLn $ show (cast {to=Int} c) ++ "\r"
			else readInput 


-- EditorState : Type
-- EditorState = WaitingKeyPress 
-- 			| 

-}
