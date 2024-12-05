{-# LANGUAGE OverloadedStrings #-}
import           Codegen
import           Control.DeepSeq (NFData, force)
import           Control.Exception (evaluate)
import qualified Data.ByteString as BS
import           Exp
import           ExpS
import           Foreign.Ptr (FunPtr, castPtrToFunPtr, wordPtrToPtr)
import qualified LLVM.CodeGenOpt
import qualified LLVM.CodeModel
import qualified LLVM.Context
import qualified LLVM.Linking
import qualified LLVM.Module
import qualified LLVM.OrcJIT as JIT
import qualified LLVM.Passes
import qualified LLVM.Relocation
import qualified LLVM.Target

foreign import ccall unsafe "dynamic"
  mkDoubleFun :: FunPtr (Double -> Double) -> (Double -> Double)

withSimpleJIT :: NFData a => ExpS Double -> ((Double -> Double) -> a) -> IO (Maybe a)
withSimpleJIT expr doFun = do
  LLVM.Context.withContext $ \context -> do
    _ <- LLVM.Linking.loadLibraryPermanently Nothing
    LLVM.Module.withModuleFromAST context (codegen expr) $ \mod' -> do
      LLVM.Target.withHostTargetMachine LLVM.Relocation.PIC LLVM.CodeModel.JITDefault LLVM.CodeGenOpt.Default $ \targetMachine -> do
        asm <- LLVM.Module.moduleLLVMAssembly mod'
        putStrLn "*** Before optimization ***"
        BS.putStr asm
        putStrLn "***************************"

        let passSetSpec = LLVM.Passes.PassSetSpec
                          { LLVM.Passes.passes = [LLVM.Passes.CuratedPassSet 2]
                          , LLVM.Passes.targetMachine = Nothing -- Just targetMachine
                          }
        LLVM.Passes.runPasses passSetSpec mod'

        asm' <- LLVM.Module.moduleLLVMAssembly mod'
        putStrLn "*** After optimization ***"
        BS.putStr asm'
        putStrLn "**************************"

        tasm <- LLVM.Module.moduleTargetAssembly targetMachine mod'
        putStrLn "*** Target assembly ***"
        BS.putStr tasm
        putStrLn "***********************"

        JIT.withExecutionSession $ \executionSession -> do
          dylib <- JIT.createJITDylib executionSession "myDylib"
          JIT.withClonedThreadSafeModule mod' $ \threadSafeModule -> do
            objectLayer <- JIT.createRTDyldObjectLinkingLayer executionSession
            compileLayer <- JIT.createIRCompileLayer executionSession objectLayer targetMachine
            JIT.addDynamicLibrarySearchGeneratorForCurrentProcess compileLayer dylib
            JIT.addModule threadSafeModule dylib compileLayer

            sym <- JIT.lookupSymbol executionSession compileLayer dylib "f"
            case sym of
              Left (JIT.JITSymbolError err) -> do
                print err
                pure Nothing
              Right (JIT.JITSymbol fnAddr _jitSymbolFlags) -> do
                let fn = mkDoubleFun . castPtrToFunPtr $ wordPtrToPtr fnAddr
                Just <$> evaluate (force $ doFun fn)

main :: IO ()
main = do
  let f x = (x + 1)^10 * (x + 1) + cos x
  expr <- recoverSharing (f Var)
  result <- withSimpleJIT expr (\f' -> f' 1.0)
  case result of
    Nothing      -> pure ()
    Just result' -> print result'
