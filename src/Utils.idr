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
	moveCursor : (Int, Int) -> EditorState -> EditorState
	moveCursor (dx, dy) e 
		= let MkEditor (cx, cy) (row, col) (offx, offy) numRows rows = e in
			case (index' (cast cy) rows) of
				Just line => set_cursor ((clip (cx+dx) 0 (cast (length line))) , (clip (cy+dy) 0 numRows)) e 
				Nothing => set_cursor (cx , (clip (cy+dy) 0 numRows)) e

	export
	editorScroll : EditorState -> EditorState
	editorScroll e = let MkEditor (cx, cy) (row, col) (offx, offy) numRows rows = e in
						let (updatedOffx, updatedOffy) = (min cx offx, min cy offy) in		-- scroll up to cursor
							set_offset (max updatedOffx (cx - col + 1), max updatedOffy (cy - row + 1)) e


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

	clearLineRightOfCursor : String
	clearLineRightOfCursor = "\ESC[K"