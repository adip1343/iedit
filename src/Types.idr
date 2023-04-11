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
	inSync : Bool		-- File is same as in disk
	fileName : String
	status : String
	rows : List Erow

export
initialEditorState : EditorState
initialEditorState = MkEditor (0, 0) 0 (0, 0) (0, 0) 0 True "" "" []

public export
data Key = CtrlQ
		| CtrlS
		| CtrlT
		| Home 
		| End 
		| PageUp 
		| PageDown 
		| ArrowUp
		| ArrowDown 
		| ArrowLeft 
		| ArrowRight 
		| Delete 
		| BackSpace
		| Enter
		| CharKey Int

export
intToKey : Int -> Key
intToKey 13		= Enter
intToKey 17 	= CtrlQ
intToKey 19		= CtrlS
intToKey 20		= CtrlT
intToKey 1000 	= ArrowLeft
intToKey 1001 	= ArrowRight
intToKey 1002 	= ArrowUp
intToKey 1003 	= ArrowDown
-- intToKey 1004 	= Delete
-- intToKey 1005 	= Home
-- intToKey 1006 	= End
-- intToKey 1007	= PageUp
-- intToKey 1008 	= PageDown
intToKey 127	= BackSpace
intToKey key	= CharKey key

-- Settings
export
tabStop : Int
tabStop = 4