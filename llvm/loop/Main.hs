{-# LANGUAGE OverloadedStrings #-}
import           Control.DeepSeq (NFData, force)
import           Control.Exception (evaluate)
import qualified Data.ByteString as BS
import           Data.Int
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Storable.Mutable as VSM
import           Exp
import           ExpS
import           Foreign.Ptr (FunPtr, Ptr, castPtrToFunPtr, wordPtrToPtr)
import qualified LLVM.CodeGenOpt
import qualified LLVM.CodeModel
import qualified LLVM.Context
import qualified LLVM.Linking
import qualified LLVM.Module
import qualified LLVM.OrcJIT as JIT
import qualified LLVM.Passes
import qualified LLVM.Relocation
import qualified LLVM.Target
import           LoopCodegen
import           System.IO.Unsafe

foreign import ccall unsafe "dynamic"
  mkDoubleArrayFun :: FunPtr (Int32 -> Ptr Double -> Ptr Double -> IO ()) -> (Int32 -> Ptr Double -> Ptr Double -> IO ())

withArrayJIT :: NFData a => ExpS Double -> ((VS.Vector Double -> VS.Vector Double) -> IO a) -> IO (Maybe a)
withArrayJIT expr doFun = do
  LLVM.Context.withContext $ \context -> do
    _ <- LLVM.Linking.loadLibraryPermanently Nothing
    LLVM.Module.withModuleFromAST context (codegenNoAlias expr) $ \mod' -> do
      LLVM.Target.withHostTargetMachine LLVM.Relocation.PIC LLVM.CodeModel.JITDefault LLVM.CodeGenOpt.Default $ \targetMachine -> do
        asm <- LLVM.Module.moduleLLVMAssembly mod'
        putStrLn "*** Before optimization ***"
        BS.putStr asm
        putStrLn "***************************"

        let passSetSpec = LLVM.Passes.PassSetSpec
                          { LLVM.Passes.passes = [LLVM.Passes.CuratedPassSet 2]
                          , LLVM.Passes.targetMachine = Just targetMachine
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
                let ptrFn = mkDoubleArrayFun . castPtrToFunPtr $ wordPtrToPtr fnAddr
                    vecFn !inputVec = unsafePerformIO $ do
                      let !n = VS.length inputVec
                      resultVec <- VSM.unsafeNew n
                      VS.unsafeWith inputVec $ \inputPtr ->
                        VSM.unsafeWith resultVec $ \resultPtr ->
                          ptrFn (fromIntegral n) resultPtr inputPtr
                      VS.unsafeFreeze resultVec
                result <- doFun vecFn
                Just <$> evaluate (force result)

main :: IO ()
main = do
  let f x = (x + 1)^10 * (x + 1)
  expr <- recoverSharing (f Var)
  _ <- withArrayJIT expr $ \vf -> do
    print $ vf (VS.fromList [1..20])
  pure ()
