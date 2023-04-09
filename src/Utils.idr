module Utils

import Types

namespace utils
	ctrl : Char -> Int
	ctrl key = prim__andInt 0x1f (prim__charToInt key)

	times : Int -> String -> String
	times 0 _ = ""
	times i str = str ++ (times (i-1) str)

	export
	clip : (x : Int) -> (min : Int) -> (max: Int) -> Int
	clip x mn mx = case compare mn x of
		GT => mn 
		EQ => mn
		LT => case compare x mx of 
				GT => mx
				EQ => mx
				LT => x
	export
	length : Maybe String -> Int
	length (Just s) = cast (length s)
	length Nothing = 0

	export
	tabStop : Int
	tabStop = 4

	tabToSpaces' : (row : List Char) -> (cx : Int) -> (rx : Int) -> List Char
	tabToSpaces' [] _ _ = []
	tabToSpaces' (x :: xs) cx rx = case x == '\t' of
		True => let t = tabStop - (mod rx tabStop) in
			(replicate (cast t) ' ') ++ (tabToSpaces' xs (cx+1) (rx+t))
		False =>  x :: (tabToSpaces' xs (cx+1) (rx+1))

	tabToSpaces : String -> String
	tabToSpaces s = pack (tabToSpaces' (unpack s) 0 0)

	export
	updateErow : String -> Erow
	updateErow s = MkErow s (tabToSpaces s)

	mapCxToRx' : (row : List Char) -> (cx : Int) -> (j : Int) -> (rx : Int) -> Int
	mapCxToRx' [] cx j rx = rx
	mapCxToRx' (x :: xs) cx j rx = case j == cx of 
		True => rx 
		False => case x == '\t' of
			True => mapCxToRx' xs cx (j+1) (rx + tabStop - (mod rx tabStop))
			False => mapCxToRx' xs cx (j+1) (rx + 1)

	export
	mapCxToRx : (row : Erow) -> (cx : Int) -> Int
	mapCxToRx row cx = let (MkErow cs rs) = row in 
		mapCxToRx' (unpack cs) cx 0 0


namespace escapes
	export
	clearScreen : String
	clearScreen = "\ESC[2J"

	export
	moveCursor : (Int, Int) -> String
	moveCursor (x, y) = ("\ESC["++ (show (y+1)) ++ ";" ++ (show (x+1)) ++ "H")

	export
	hideCursor : String
	hideCursor = "\ESC[?25l"

	export
	showCursor : String
	showCursor = "\ESC[?25h"

	export
	clearLineRightOfCursor : String
	clearLineRightOfCursor = "\ESC[K"