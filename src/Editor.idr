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

	writeBuffer : String -> Int -> IO Int
	writeBuffer = foreign FFI_C "writeBuffer" (String -> Int -> IO Int)

	ctrl : Char -> Char
	ctrl key = prim__andChar key (prim__intToChar 0x1f)

	getWindowRows : IO Int
	getWindowRows = foreign FFI_C "getWindowRows" (IO Int)

	getWindowCols : IO Int
	getWindowCols = foreign FFI_C "getWindowCols" (IO Int)


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
				ripe.writeBuffer "\x1b[2J"	-- clear screen
				ripe.writeBuffer "\x1b[H"	-- move cursor to top left
				pure (Left ())	-- Right False ?

	editorDrawRows : Int -> IO (Either () ())
	editorDrawRows 0 = pure (Right ())
	editorDrawRows i = do
		ripe.writeBuffer "~\r\n"
		editorDrawRows (i-1)

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

record Editor where
	constructor MkEditor
	rows : Int
	cols : Int

editorInitState : Editor
editorInitState = MkEditor 
	{rows = 0} 
	{cols = 0}

editorRefreshScreen : ST IO (Either () ()) [editor ::: State Editor] 
editorRefreshScreen = do
	lift $ ripe.writeBuffer "\x1b[2J"	-- clear screen
	lift $ ripe.writeBuffer "\x1b[H"	-- move cursor to top left
	lift $ ripe.editorDrawRows 24
	lift $ ripe.writeBuffer "\x1b[H"	-- move cursor to top left

handlerLoop : ST IO () [editor ::: State Editor]
handlerLoop = do
	Right userContinues <- lift ripe.editorHandleKeypress | Left () =>   returning () (pure ())
	editorRefreshScreen
	handlerLoop


main' : ST IO () []
main' = do
	lift $ raw.enterRawMode
	editor <- new editorInitState
	{-Right ()-}
	handlerLoop
	delete editor

main : IO ()
main = run main'

-- SOME RESPECTABLE GARBAGE CAN BE FOUND HERE --
{- 
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
-}