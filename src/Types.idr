module Types

public export
record Erow where
	constructor MkErow
	chars : String
	render : String

export
emptyErow : Erow
emptyErow = MkErow "~" "~"

public export
record EditorState where
	constructor MkEditor
	cursor : (Int, Int)
	renderx : Int
	screen : (Int, Int)
	offset : (Int, Int)
	numRows : Int
	status : String
	fileName : String
	rows : List Erow

export
initialEditorState : EditorState
initialEditorState = MkEditor (0, 0) 0 (0, 0) (0, 0) 0 "" "" []

public export
data Key = CtrlQ 
		| Home 
		| End 
		| PageUp 
		| PageDown 
		| ArrowUp
		| ArrowDown 
		| ArrowLeft 
		| ArrowRight 
		| Delete 
		| Space
		| CharKey Int

export
intToKey : Int -> Key
intToKey 17 	= CtrlQ
intToKey 1000 	= ArrowLeft
intToKey 1001 	= ArrowRight
intToKey 1002 	= ArrowUp
intToKey 1003 	= ArrowDown
-- intToKey 1004 	= Delete
-- intToKey 1005 	= Home
-- intToKey 1006 	= End
-- intToKey 1007	= PageUp
-- intToKey 1008 	= PageDown
-- intToKey 127	= Space
intToKey key	= CharKey key

