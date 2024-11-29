{-# LANGUAGE OverloadedStrings #-}
module Main where
import MyLib
import qualified LLVM.AST
import qualified LLVM.AST.Constant
import qualified LLVM.AST.Float
import qualified LLVM.AST.Type
import qualified LLVM.CodeGenOpt
import qualified LLVM.CodeModel
import qualified LLVM.Context
import qualified LLVM.IRBuilder.Instruction
import qualified LLVM.IRBuilder.Module
import qualified LLVM.IRBuilder.Monad
-- import qualified LLVM.Internal.OrcJIT.CompileLayer
import qualified LLVM.Linking
import qualified LLVM.Module
import qualified LLVM.OrcJIT
-- import qualified LLVM.Pretty
import qualified LLVM.Relocation
import qualified LLVM.Target
import qualified Data.IntMap as IntMap
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC8
import qualified Data.ByteString.Short as SBS
import Control.Monad.IO.Class

codegen :: ExpS -> LLVM.AST.Module
codegen expr = LLVM.IRBuilder.Module.buildModule "dsl.ll" $ do
  let xparam :: LLVM.IRBuilder.Module.ParameterName
      xparam = LLVM.IRBuilder.Module.ParameterName "x"
  _ <- LLVM.IRBuilder.Module.function "f" [(LLVM.AST.Type.double, xparam)] LLVM.AST.Type.double $ \[arg] -> do
    result <- goExp (IntMap.singleton 0 arg) expr
    LLVM.IRBuilder.Instruction.ret result
  pure ()
  where
    goValue :: IntMap.IntMap LLVM.AST.Operand -> Value -> LLVM.AST.Operand
    goValue _ (ConstV x) = LLVM.AST.ConstantOperand $ LLVM.AST.Constant.Float $ LLVM.AST.Float.Double x
    goValue env (VarV i) = env IntMap.! i
    goSimpleExp :: (LLVM.IRBuilder.Monad.MonadIRBuilder m, LLVM.IRBuilder.Module.MonadModuleBuilder m) => IntMap.IntMap LLVM.AST.Operand -> SimpleExp -> m LLVM.AST.Operand
    -- goSimpleExp :: IntMap.IntMap LLVM.AST.Operand -> SimpleExp -> LLVM.IRBuilder.Module.IRBuilderT LLVM.IRBuilder.Module.ModuleBuilder LLVM.AST.Operand
    goSimpleExp env (AddS x y) = LLVM.IRBuilder.Instruction.fadd (goValue env x) (goValue env y)
    goSimpleExp env (SubS x y) = LLVM.IRBuilder.Instruction.fsub (goValue env x) (goValue env y)
    goSimpleExp env (MulS x y) = LLVM.IRBuilder.Instruction.fmul (goValue env x) (goValue env y)
    goSimpleExp env (DivS x y) = LLVM.IRBuilder.Instruction.fdiv (goValue env x) (goValue env y)
    goExp :: (LLVM.IRBuilder.Monad.MonadIRBuilder m, LLVM.IRBuilder.Module.MonadModuleBuilder m) => IntMap.IntMap LLVM.AST.Operand -> ExpS -> m LLVM.AST.Operand
    -- goExp :: IntMap.IntMap LLVM.AST.Operand -> ExpS -> LLVM.IRBuilder.Module.IRBuilderT LLVM.IRBuilder.Module.ModuleBuilder LLVM.AST.Operand
    goExp env (Let i x body) = do
      v <- goSimpleExp env x `LLVM.IRBuilder.Monad.named` SBS.toShort (BSC8.pack ("x" ++ show i))
      goExp (IntMap.insert i v env) body
    goExp env (Value v) = pure $ goValue env v


withSimpleJIT :: ExpS -> ((Double -> Double) -> a) -> IO ()
withSimpleJIT expr f = do
  LLVM.Context.withContext $ \context -> do
    LLVM.Linking.loadLibraryPermanently Nothing
    LLVM.Module.withModuleFromAST context (codegen expr) $ \mod' -> do
      LLVM.Target.withHostTargetMachine LLVM.Relocation.PIC LLVM.CodeModel.JITDefault LLVM.CodeGenOpt.Default $ \targetMachine -> do
        LLVM.OrcJIT.withExecutionSession $ \executionSession -> do
          dylib <- LLVM.OrcJIT.createJITDylib executionSession "myDylib"
          LLVM.OrcJIT.withClonedThreadSafeModule mod' $ \threadSafeModule -> do
            objectLayer <- LLVM.OrcJIT.createRTDyldObjectLinkingLayer executionSession
            compileLayer <- LLVM.OrcJIT.createIRCompileLayer executionSession objectLayer targetMachine
            LLVM.OrcJIT.addDynamicLibrarySearchGeneratorForCurrentProcess compileLayer dylib
            LLVM.OrcJIT.addModule threadSafeModule dylib compileLayer
            asm <- LLVM.Module.moduleLLVMAssembly mod'
            liftIO $ BS.putStr asm

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  let f x = (x + 1)^10
  expr <- recoverSharing (f Var)
  withSimpleJIT expr (\f' -> ())
