import           Codegen
import qualified Data.ByteString as BS
import           Exp
import           ExpS
import qualified LLVM.Context
import qualified LLVM.Module

main :: IO ()
main = do
  let f x = (x + 1)^10
  expr <- recoverSharing (f Var)
  let code = codegen expr
  LLVM.Context.withContext $ \context ->
    LLVM.Module.withModuleFromAST context code $ \mod' -> do
      asm <- LLVM.Module.moduleLLVMAssembly mod'
      BS.putStr asm
