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

	writeBuffer : String -> IO Int
	writeBuffer buf = foreign FFI_C "writeBuffer" (String -> Int -> IO Int) buf (prim_lenString buf)

	ctrl : Char -> Char
	ctrl key = prim__andChar key (prim__intToChar 0x1f)

	
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

	editorReadKey : IO (Either () Char)
	editorReadKey = ripe.readCharBlocking

	-- return True if user doesn't quits Ctrl Q
	editorHandleKeypress : IO (Either () ())
	editorHandleKeypress = do
		Right c <- editorReadKey | Left () => pure (Left ())	--some error
		case c /= ctrl 'q' of
			True => pure (Right ())
			False => pure (Left ())	-- Right False ?

handlerLoop : ST IO () []
handlerLoop = do
	Right userContinues <- lift ripe.editorHandleKeypress | Left () =>   returning () (pure ())
	?editorRefreshScreen
	handlerLoop


main' : ST IO () []
main' = do
	lift $ raw.enterRawMode
	{-Right ()-}
	handlerLoop

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