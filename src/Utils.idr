module Utils

import Types

namespace utils
	export
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
	
	pad' : (len : Int) -> List Char -> List Char
	pad' len [] 		= replicate (cast len) ' '
	pad' 0 _ 			= []
	pad' len (x :: xs) 	= x :: (pad' (len-1) xs)  

	export
	pad : (len : Int) -> String -> String
	pad len s = pack (pad' len (unpack s))

	export
	insertAt : String -> (i : Int) -> (c : Char) -> String 
	insertAt s i c = (substr (cast 0) (cast i) s) ++ (singleton c) ++ (substr (cast i) (length s) s) 

	export
	removeAt : String -> (i : Int) -> String
	removeAt s i = (substr (cast 0) (cast i) s) ++ (substr (cast (i+1)) (length s) s)

	removeFirst : List a -> List a
	removeFirst [] = []
	removeFirst (x :: xs) = xs

	export
	updateAt : List a -> (i : Int) -> a -> List a
	updateAt xs i x = let (xl, xr) = splitAt (cast i) xs in
		xl ++ [x] ++ removeFirst xr

	-- insert character and update rendered row
	export
	insertChar : (row :Erow) -> (i : Int) -> (c : Char) -> Erow
	insertChar row i c = let MkErow chars render = row in
		updateErow (insertAt chars i c)

	-- remove character and update rendered row
	export
	removeChar : (row :Erow) -> (i : Int) -> Erow
	removeChar row i = let MkErow chars render = row in
		updateErow (removeAt chars i)

	-- append i and i-1 row and return position of last character in first row
	export
	appendRows : (rows : List Erow) -> (i : Int) -> (Int, List Erow)
	appendRows [] _ = (0, [])
	appendRows (row1 :: row2 :: rows) 1 = ((cast (length (chars row1))), (updateErow ((chars row1) ++ (chars row2))) :: rows)
	appendRows (row :: rows) i = let (j, rows') = appendRows rows (i-1) in (j, row :: rows')  

	splitAt : Int -> String -> (String, String)
	splitAt	i row = let (s1, s2) = splitAt (cast i) (unpack row) in (pack s1, pack s2)

	export
	splitRow : (rows : List Erow) -> (i : Int) -> (j : Int) -> List Erow
	splitRow [] _ _ = []
	splitRow (row :: rows) 0 j = let (row1, row2) = splitAt j (chars row) in 
		(updateErow row1) :: (updateErow row2) :: rows
	splitRow (row :: rows) i j = row :: (splitRow rows (i-1) j)

	export
	isPrint : Char -> Bool
	isPrint _ = True
	isPrint '\t' = True
	isPrint '\ESC' = False
	isPrint c = (not . isControl) c

	identifierChar : Char -> Bool
	identifierChar '.' = True
	identifierChar '\'' = True
	identifierChar '_' = True
	identifierChar c = isAlphaNum c

	identifierStart : Char -> Bool
	identifierStart '_' = True
	identifierStart c = isAlpha c

	validId : String -> Either () String 
	validId "" = Left ()
	validId s = case identifierStart (strHead s) of 
		True => Right s 
		False => Left ()
	
	export
	getIdentifierUnderCursor : Int -> Erow -> Either () String
	getIdentifierUnderCursor i row = let (left, right) = splitAt i (chars row) in
		let (rightId, _) = span identifierChar right in
			let (leftId, _) = span identifierChar (reverse left) in
				validId ((reverse leftId) ++ rightId)


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

	export
	invertColors : String
	invertColors = "\ESC[7m"

	export
	unsetInvertColors : String
	unsetInvertColors = "\ESC[m"