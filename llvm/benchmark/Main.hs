{-# LANGUAGE OverloadedStrings #-}
import qualified Codegen
import           Control.DeepSeq (NFData, force)
import           Control.Exception (evaluate)
import           Criterion.Main
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
import qualified LoopCodegen
import           System.IO.Unsafe

foreign import ccall unsafe "dynamic"
  mkDoubleFun :: FunPtr (Double -> Double) -> (Double -> Double)

withSimpleJIT :: NFData a => ExpS Double -> ((Double -> Double) -> IO a) -> IO (Maybe a)
withSimpleJIT expr doFun = do
  LLVM.Context.withContext $ \context -> do
    _ <- LLVM.Linking.loadLibraryPermanently Nothing
    LLVM.Module.withModuleFromAST context (Codegen.codegen expr) $ \mod' -> do
      LLVM.Target.withHostTargetMachine LLVM.Relocation.PIC LLVM.CodeModel.JITDefault LLVM.CodeGenOpt.Default $ \targetMachine -> do
        JIT.withExecutionSession $ \executionSession -> do
          dylib <- JIT.createJITDylib executionSession "myDylib"
          JIT.withClonedThreadSafeModule mod' $ \threadSafeModule -> do
            objectLayer <- JIT.createRTDyldObjectLinkingLayer executionSession
            compileLayer <- JIT.createIRCompileLayer executionSession objectLayer targetMachine
            JIT.addDynamicLibrarySearchGeneratorForCurrentProcess compileLayer dylib
            JIT.addModule threadSafeModule dylib compileLayer

            let passSetSpec = LLVM.Passes.PassSetSpec
                              { LLVM.Passes.passes = [LLVM.Passes.CuratedPassSet 2]
                              , LLVM.Passes.targetMachine = Nothing -- Just targetMachine
                              }
            LLVM.Passes.runPasses passSetSpec mod'

            sym <- JIT.lookupSymbol executionSession compileLayer dylib "f"
            case sym of
              Left (JIT.JITSymbolError err) -> do
                print err
                pure Nothing
              Right (JIT.JITSymbol fnAddr _jitSymbolFlags) -> do
                let fn = mkDoubleFun . castPtrToFunPtr $ wordPtrToPtr fnAddr
                result <- doFun fn
                Just <$> evaluate (force result)

foreign import ccall unsafe "dynamic"
  mkDoubleArrayFun :: FunPtr (Int32 -> Ptr Double -> Ptr Double -> IO ()) -> (Int32 -> Ptr Double -> Ptr Double -> IO ())

withArrayJIT :: NFData a => ExpS Double -> ((VS.Vector Double -> VS.Vector Double) -> IO a) -> IO (Maybe a)
withArrayJIT expr doFun = do
  LLVM.Context.withContext $ \context -> do
    _ <- LLVM.Linking.loadLibraryPermanently Nothing
    LLVM.Module.withModuleFromAST context (LoopCodegen.codegenNoAlias expr) $ \mod' -> do
      LLVM.Target.withHostTargetMachine LLVM.Relocation.PIC LLVM.CodeModel.JITDefault LLVM.CodeGenOpt.Default $ \targetMachine -> do
        JIT.withExecutionSession $ \executionSession -> do
          dylib <- JIT.createJITDylib executionSession "myDylib"
          JIT.withClonedThreadSafeModule mod' $ \threadSafeModule -> do
            objectLayer <- JIT.createRTDyldObjectLinkingLayer executionSession
            compileLayer <- JIT.createIRCompileLayer executionSession objectLayer targetMachine
            JIT.addDynamicLibrarySearchGeneratorForCurrentProcess compileLayer dylib
            JIT.addModule threadSafeModule dylib compileLayer

            let passSetSpec = LLVM.Passes.PassSetSpec
                              { LLVM.Passes.passes = [LLVM.Passes.CuratedPassSet 2]
                              , LLVM.Passes.targetMachine = Just targetMachine
                              }
            LLVM.Passes.runPasses passSetSpec mod'

            sym <- JIT.lookupSymbol executionSession compileLayer dylib "f"
            case sym of
              Left (JIT.JITSymbolError err) -> do
                print err
                pure Nothing
              Right (JIT.JITSymbol fnAddr _jitSymbolFlags) -> do
                let ptrFn = mkDoubleArrayFun . castPtrToFunPtr $ wordPtrToPtr fnAddr
                    vecFn !inputVec = unsafePerformIO $ do
                      let !n = VS.length inputVec
                      resultVec <- VSM.new n
                      VS.unsafeWith inputVec $ \inputPtr ->
                        VSM.unsafeWith resultVec $ \resultPtr ->
                          ptrFn (fromIntegral n) resultPtr inputPtr
                      VS.unsafeFreeze resultVec
                result <- doFun vecFn
                Just <$> evaluate (force result)

f :: Num a => a -> a
f x = (x + 1)^10
{-# SPECIALIZE f :: Double -> Double #-}

g :: Num a => a -> a
g x = let y1 = x + 1
          y2 = y1 * y1
          y4 = y2 * y2
          y8 = y4 * y4
      in y8 * y2
{-# SPECIALIZE g :: Double -> Double #-}

main :: IO ()
main = do
  expr <- recoverSharing (f Var)
  let !input = VS.fromList [0..10000]
  _ <- withSimpleJIT expr $ \simpleF ->
    withArrayJIT expr $ \arrayF ->
      defaultMain
        [ bench "Haskell/vector" $ whnf (VS.map f) input
        , bench "Haskell unrolled/vector" $ whnf (VS.map g) input
        , bench "JIT/vector" $ whnf (VS.map simpleF) input
        , bench "JIT/array" $ whnf arrayF input
        ]
  pure ()
