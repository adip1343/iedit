module Types

export
record EditorState where
	constructor MkEditor
	cursor : (Int, Int)
	screen : (Int, Int)
	offset : (Int, Int)
	numRows : Int
	rows : List String

export
initialEditorState : EditorState
initialEditorState = MkEditor (0, 0) (0, 0) (0, 0) 0 []

export
data Key = CtrlQ | Home | End | PageUp | PageDown | ArrowUp| ArrowDown | ArrowLeft | ArrowRight | Delete 