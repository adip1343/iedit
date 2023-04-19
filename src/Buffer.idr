import Control.ST
import Control.ST.ImplicitCall
import Interface

data BufferState = Flushed | Filled  

-- data AppendOK : BufferState -> Type where
--      AppendFlushed : AppendOK Flushed
--      AppendFilled : AppendOK Filled

-- interface BufferIO (m : Type -> Type) where
-- 	Buffer : BufferState -> Type
-- 	init : ST m Var [add (Buffer Flushed)]
-- 	append : (buffer : Var) -> (str : String) -> {auto prf : AppendOK st} -> 
-- 		ST m () [buffer ::: Buffer st :-> Buffer Filled]
-- 	flush : (buffer : Var) -> 
-- 		ST m (Either () ()) [buffer ::: Buffer Filled :-> Buffer Flushed]
-- 	close : (buffer : Var) -> ST m () [Remove buffer (Buffer Flushed)]

export
interface BufferIO (m : Type -> Type) where
	Buffer : Type
	initBuffer : ST m Var [add Buffer]
	append : (buffer : Var) -> (str : String) -> 
		ST m () [buffer ::: Buffer]
	flush : (buffer : Var) -> 
		ST m (Either () ()) [buffer ::: Buffer]
	closeBuffer : (buffer : Var) -> ST m () [Remove buffer Buffer]

export
implementation BufferIO IO where
	Buffer = State String

	initBuffer = do
		buffer <- new ""
		pure buffer

	append buffer str = update buffer (\buf => buf ++ str)

	flush buffer = do
		lift $ writeBuffer !(read buffer)
		write buffer ""
		pure (Right ())

	closeBuffer buffer = delete buffer


-- operate : (BufferIO m, ConsoleIO m) => (buffer : Var) -> ST m () [buffer ::: Buffer{m}]
-- operate buffer = do
-- 	append buffer "AB"
-- 	append buffer "CD"
-- 	flush buffer
-- 	pure ()

-- main : IO ()
-- main = run(do 
-- 	buffer <- init
-- 	operate buffer
-- 	close buffer
-- )