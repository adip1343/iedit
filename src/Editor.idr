import Control.ST
import Control.ST.ImplicitCall
import Types
import Interface
import Utils
import Update
import Action 

interface EditorIO (m : Type -> Type) where
	Editor : Type
	init : ST m (Either () Var) [addIfRight Editor]
	loadFile : (editor : Var) -> ST m (Either () ()) [editor ::: Editor]
	saveFile : (editor : Var) -> ST m (Either () ()) [editor ::: Editor]
	handleKeypress : (editor : Var) -> (key : Key) -> ST m (Either () ()) [editor ::: Editor]
	refreshScreen : (editor : Var) -> ST m (Either () ()) [editor ::: Editor]
	close : (editor : Var) -> ST m () [Remove editor Editor]
	readKey : (editor : Var) -> ST m (Either () Key) [editor ::: Editor]

implementation EditorIO IO where
	
	Editor = State EditorState
	
	loadFile editor = do
		Right fileName <- lift $ editorGetFileName | Left noFile => pure (Left ())
		Right fileLines <- lift $ editorReadFile fileName | Left fileError => pure (Left ())
		update editor (set_fileName fileName)
		update editor (set_numRows (cast (length fileLines)))
		update editor (set_rows fileLines)
		pure (Right ())
	
	saveFile editor = do
		MkEditor _ _ _ _ _ _ fileName _ rows <- read editor
		Right () <- lift $ editorWriteFile fileName rows | Left fileError => pure (Left ())
		update editor (set_inSync True)
		pure(Right ())

	init = do 
		Right (r, c) <- lift getWindowSize | Left () => pure (Left ())
		editor <- new initialEditorState
		update editor (set_screen (r-2, c))
		Right () <- loadFile editor | Left () => do
			delete editor
			pure (Left ())
		lift enterRawMode
		pure (Right editor)

	readKey editor = do
		Right key <- lift editorReadKey | Left () => pure (Left ())
		pure (Right key)
	
	-- other keys
	handleKeypress editor (CharKey key) = do
		case isPrint (cast key) of
			-- Insert Printable Characters
			True => do
				update editor (editorInsertChar (CharKey key))
				update editor (set_inSync False)
				update editor editorRecalculateNumRows
				pure (Right ())
			False => pure (Right ())

	-- Ctrl-Q
	handleKeypress editor CtrlQ = do
		lift $ writeBuffer clearScreen
		lift $ writeBuffer (escapes.moveCursor (0, 0))
		pure (Left ())

	-- Ctrl-S
	handleKeypress editor CtrlS = saveFile editor

	-- Ctrl T
	handleKeypress editor CtrlT = do
		saveFile editor
		e <- read editor
		lift $ runCommand (loadFile e)
		case (editorGetIdentifierUnderCursor e) of
			Right identifer => do
				type <- lift $ runCommand (getType identifer)
				update editor (set_status (getFinalLine type))
				pure (Right ())
			Left () => pure (Right ()) 

	--backSpace 
	handleKeypress editor BackSpace = do
		update editor editorRemoveChar
		update editor (set_inSync False)
		update editor editorRecalculateNumRows
		pure (Right ())

	--Enter
	handleKeypress editor Enter = do
		update editor (editorInsertChar Enter)
		update editor (set_inSync False)
		update editor editorRecalculateNumRows
		pure (Right ())

	-- movement keys
	handleKeypress editor key = do
		update editor (editorCursorMovement key)
		pure (Right ()) 

	refreshScreen editor = do
		update editor editorRenderx
		update editor editorScroll
		lift $ writeBuffer hideCursor	
		lift $ writeBuffer (escapes.moveCursor (0, 0))	 
		lift $ editorDrawRows !(read editor) 0
		lift $ editorDrawStatusBar !(read editor)
		lift $ editorDrawOutputBar !(read editor)
		MkEditor (cx, cy) rx _ (offx, offy) numRows _ _ _ rows <- read editor
		lift $ writeBuffer (escapes.moveCursor (rx - offx, cy - offy))
		lift $ writeBuffer showCursor

	close editor = delete editor

-- event loop listening to keypress and updating
listenEvent : (EditorIO m, ConsoleIO m) => 
				(editor : Var) -> ST m () [editor ::: Editor {m}]
listenEvent editor = do
		Right key <- readKey editor | Left () => pure ()
		Right userContinues <- handleKeypress editor key | Left () => pure ()
		refreshScreen editor
		listenEvent editor


main : IO ()
main = run (do 
	Right editor <- init | Left () => pure ()
	refreshScreen editor
	listenEvent editor
	close editor)