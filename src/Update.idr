module Update

import Types
import Utils

export
editorRenderx : EditorState -> EditorState
editorRenderx e 
	= let MkEditor (cx, cy) rx _ _ numRows _ _ rows = e in
		case index' (cast cy) rows of
			Just line => let updatedRx = mapCxToRx line cx in
				set_renderx updatedRx e 
			Nothing => set_renderx 0 e

export
editorScroll : EditorState -> EditorState
editorScroll e 
	= let MkEditor (cx, cy) rx (row, col) (offx, offy) numRows _ _ _ = e in
		let (updatedOffx, updatedOffy) = (min rx offx, min cy offy) in		-- scroll up to cursor
			set_offset (max updatedOffx (rx - col + 1), max updatedOffy (cy - row + 1)) e		

export
editorCursorMovement : Key -> EditorState -> EditorState
editorCursorMovement ArrowLeft e 
	= let MkEditor (cx, cy) rx _ _ numRows _ _ rows = e in
		case cx == 0 of
			-- moving to last char of row above
			True => case cy == 0 of
				-- already in first row do nothing
				True => e
				-- move to last char of row above
				False => let Just line = index' (cast (cy-1)) rows in 
					set_cursor ((cast (length (chars line))), (cy-1)) e
			-- moving one char left
			False => set_cursor (cx - 1, cy) e			 

editorCursorMovement ArrowRight e 
	= let MkEditor (cx, cy) rx _ _ numRows _ _ rows = e in
		case (index' (cast cy) rows) of
			-- row contains text
			Just line => case cx == cast (length (chars line)) of
				-- already at last char of row, move to first char of row below
				True => set_cursor (0, cy + 1) e
				-- move one char right
				False => set_cursor (cx + 1, cy) e
			-- in emptry row do nothing
			Nothing => e

editorCursorMovement ArrowUp e
	= let MkEditor (cx, cy) rx _ _ numRows _ _ rows = e in
		case cy == 0 of
			-- aready at top row 
			True => e
			-- move one row up and snap cursor to end of line
			False => let Just line = index' (cast (cy-1)) rows in
				set_cursor (min cx (cast (length (chars line))), cy-1) e

editorCursorMovement ArrowDown e
	= let MkEditor (cx, cy) rx _ _ numRows _ _ rows = e in
		case cy == numRows of
			-- already at bottom row
			True => e
			-- move one row below and snap cursor to end of line
			False => case index' (cast (cy+1)) rows of
				Just line 	=> set_cursor (min cx (cast (length (chars line))), cy+1) e
				Nothing		=> set_cursor (0, cy+1) e

editorCursorMovement (CharKey key) e
	= let MkEditor (cx, cy) rx _ _ numRows _ _ rows = e in
		e

-- editorCursorMovement Home e
-- 	= let MkEditor (cx, cy) (row, col) (offx, offy) numRows rows = e in
-- 		-- move to start of row
-- 		set_cursor (0, cy) e

-- editorCursorMovement End e
-- 	= let MkEditor (cx, cy) (row, col) (offx, offy) numRows rows = e in
-- 		let rowlen = length (index' (cast cy) rows) in
-- 			-- move to end of row
-- 			set_cursor (rowlen, cy) e