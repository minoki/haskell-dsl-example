{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
module LoopCodegen (codegen, codegenNoAlias) where
import qualified Data.ByteString.Char8 as BS.Char8
import qualified Data.ByteString.Short as SBS
import qualified Data.IntMap as IntMap
import           Exp
import           ExpS
import qualified LLVM.AST as AST (Definition (GlobalDefinition), Module,
                                  Operand (ConstantOperand, LocalReference))
import qualified LLVM.AST.Constant as AST (Constant (Float))
import qualified LLVM.AST.Float as AST (SomeFloat (Double))
import qualified LLVM.AST.Global as AST (Global (basicBlocks, name, parameters, returnType),
                                         Parameter (Parameter),
                                         functionDefaults)
import qualified LLVM.AST.IntegerPredicate as AST (IntegerPredicate (SLT))
import qualified LLVM.AST.ParameterAttribute as AST (ParameterAttribute (NoAlias))
import qualified LLVM.AST.Type as AST (Type (ArrayType, FunctionType, argumentTypes, isVarArg, resultType),
                                       double, i32, ptr, void)
import qualified LLVM.IRBuilder.Constant as IR (int32)
import qualified LLVM.IRBuilder.Instruction as IR (add, br, call, condBr, fadd,
                                                   fdiv, fmul, fneg, fsub, gep,
                                                   icmp, load, phi, retVoid,
                                                   store)
import qualified LLVM.IRBuilder.Module as IR (MonadModuleBuilder,
                                              ParameterName (ParameterName),
                                              buildModule, emitDefn, extern,
                                              function)
import qualified LLVM.IRBuilder.Monad as IR (MonadIRBuilder, block,
                                             emptyIRBuilder, fresh, named,
                                             runIRBuilderT)

doubleFnType :: AST.Type
doubleFnType = AST.FunctionType
  { AST.resultType = AST.double
  , AST.argumentTypes = [AST.double]
  , AST.isVarArg = False
  }

codegen :: ExpS Double -> AST.Module
codegen expr = IR.buildModule "dsl.ll" $ do
  absFn <- IR.extern "llvm.fabs.f64" [AST.double] AST.double
  expFn <- IR.extern "llvm.exp.f64" [AST.double] AST.double
  logFn <- IR.extern "llvm.log.f64" [AST.double] AST.double
  sinFn <- IR.extern "llvm.sin.f64" [AST.double] AST.double
  cosFn <- IR.extern "llvm.cos.f64" [AST.double] AST.double

  let goValue :: IntMap.IntMap AST.Operand -> Value Double -> AST.Operand
      goValue _ (ConstV x) = AST.ConstantOperand $ AST.Float $ AST.Double x
      goValue env (VarV i) = env IntMap.! i
      goSimpleExp :: (IR.MonadIRBuilder m, IR.MonadModuleBuilder m) => IntMap.IntMap AST.Operand -> SimpleExp Double -> m AST.Operand
      -- goSimpleExp :: IntMap.IntMap AST.Operand -> SimpleExp Double -> LLVM.IRBuilder.Module.IRBuilderT LLVM.IRBuilder.Module.ModuleBuilder AST.Operand
      goSimpleExp env (UnaryS Negate x) = IR.fneg (goValue env x)
      goSimpleExp env (UnaryS Abs x) = IR.call doubleFnType absFn [(goValue env x, [])]
      goSimpleExp env (UnaryS Exp x) = IR.call doubleFnType expFn [(goValue env x, [])]
      goSimpleExp env (UnaryS Log x) = IR.call doubleFnType logFn [(goValue env x, [])]
      goSimpleExp env (UnaryS Sin x) = IR.call doubleFnType sinFn [(goValue env x, [])]
      goSimpleExp env (UnaryS Cos x) = IR.call doubleFnType cosFn [(goValue env x, [])]
      goSimpleExp env (AddS x y) = IR.fadd (goValue env x) (goValue env y)
      goSimpleExp env (SubS x y) = IR.fsub (goValue env x) (goValue env y)
      goSimpleExp env (MulS x y) = IR.fmul (goValue env x) (goValue env y)
      goSimpleExp env (DivS x y) = IR.fdiv (goValue env x) (goValue env y)
      goExp :: (IR.MonadIRBuilder m, IR.MonadModuleBuilder m) => IntMap.IntMap AST.Operand -> ExpS Double -> m AST.Operand
      -- goExp :: IntMap.IntMap AST.Operand -> ExpS Double -> LLVM.IRBuilder.Module.IRBuilderT LLVM.IRBuilder.Module.ModuleBuilder AST.Operand
      goExp env (Let i x body) = do
        v <- goSimpleExp env x `IR.named` SBS.toShort (BS.Char8.pack ("x" ++ show i))
        goExp (IntMap.insert i v env) body
      goExp env (Value v) = pure $ goValue env v

  let sizeName :: IR.ParameterName
      sizeName = IR.ParameterName "size"
  let resultArrayName :: IR.ParameterName
      resultArrayName = IR.ParameterName "resultArray"
  let inputArrayName :: IR.ParameterName
      inputArrayName = IR.ParameterName "inputArray"
  _ <- IR.function "f" [(AST.i32, sizeName), (AST.ptr, resultArrayName), (AST.ptr, inputArrayName)] AST.void $ \[size, resultArray, inputArray] -> mdo
    prologue <- IR.block
    IR.br loop

    loop <- IR.block `IR.named` "loop"
    counter <- IR.phi [(IR.int32 0, prologue), (nextCounter, loopBody)] `IR.named` "counter"
    lt <- IR.icmp AST.SLT counter size `IR.named` "lt"
    IR.condBr lt loopBody epilogue

    loopBody <- IR.block `IR.named` "loopBody"
    xPtr <- IR.gep (AST.ArrayType 0 AST.double) inputArray [IR.int32 0, counter] `IR.named` "xPtr"
    x <- IR.load AST.double xPtr 0 `IR.named` "x"
    result <- goExp (IntMap.singleton 0 x) expr `IR.named` "result"
    resultPtr <- IR.gep (AST.ArrayType 0 AST.double) resultArray [IR.int32 0, counter] `IR.named` "resultPtr"
    IR.store resultPtr 0 result
    nextCounter <- IR.add counter (IR.int32 1) `IR.named` "nextCounter"
    IR.br loop

    epilogue <- IR.block `IR.named` "epilogue"
    IR.retVoid

  pure ()

codegenNoAlias :: ExpS Double -> AST.Module
codegenNoAlias expr = IR.buildModule "dsl.ll" $ do
  absFn <- IR.extern "llvm.fabs.f64" [AST.double] AST.double
  expFn <- IR.extern "llvm.exp.f64" [AST.double] AST.double
  logFn <- IR.extern "llvm.log.f64" [AST.double] AST.double
  sinFn <- IR.extern "llvm.sin.f64" [AST.double] AST.double
  cosFn <- IR.extern "llvm.cos.f64" [AST.double] AST.double

  let goValue :: IntMap.IntMap AST.Operand -> Value Double -> AST.Operand
      goValue _ (ConstV x) = AST.ConstantOperand $ AST.Float $ AST.Double x
      goValue env (VarV i) = env IntMap.! i
      goSimpleExp :: (IR.MonadIRBuilder m, IR.MonadModuleBuilder m) => IntMap.IntMap AST.Operand -> SimpleExp Double -> m AST.Operand
      -- goSimpleExp :: IntMap.IntMap AST.Operand -> SimpleExp Double -> LLVM.IRBuilder.Module.IRBuilderT LLVM.IRBuilder.Module.ModuleBuilder AST.Operand
      goSimpleExp env (UnaryS Negate x) = IR.fneg (goValue env x)
      goSimpleExp env (UnaryS Abs x) = IR.call doubleFnType absFn [(goValue env x, [])]
      goSimpleExp env (UnaryS Exp x) = IR.call doubleFnType expFn [(goValue env x, [])]
      goSimpleExp env (UnaryS Log x) = IR.call doubleFnType logFn [(goValue env x, [])]
      goSimpleExp env (UnaryS Sin x) = IR.call doubleFnType sinFn [(goValue env x, [])]
      goSimpleExp env (UnaryS Cos x) = IR.call doubleFnType cosFn [(goValue env x, [])]
      goSimpleExp env (AddS x y) = IR.fadd (goValue env x) (goValue env y)
      goSimpleExp env (SubS x y) = IR.fsub (goValue env x) (goValue env y)
      goSimpleExp env (MulS x y) = IR.fmul (goValue env x) (goValue env y)
      goSimpleExp env (DivS x y) = IR.fdiv (goValue env x) (goValue env y)
      goExp :: (IR.MonadIRBuilder m, IR.MonadModuleBuilder m) => IntMap.IntMap AST.Operand -> ExpS Double -> m AST.Operand
      -- goExp :: IntMap.IntMap AST.Operand -> ExpS Double -> LLVM.IRBuilder.Module.IRBuilderT LLVM.IRBuilder.Module.ModuleBuilder AST.Operand
      goExp env (Let i x body) = do
        v <- goSimpleExp env x `IR.named` SBS.toShort (BS.Char8.pack ("x" ++ show i))
        goExp (IntMap.insert i v env) body
      goExp env (Value v) = pure $ goValue env v

  let functionBody size resultArray inputArray = mdo
        prologue <- IR.block
        IR.br loop

        loop <- IR.block `IR.named` "loop"
        counter <- IR.phi [(IR.int32 0, prologue), (nextCounter, loopBody)] `IR.named` "counter"
        lt <- IR.icmp AST.SLT counter size `IR.named` "lt"
        IR.condBr lt loopBody epilogue

        loopBody <- IR.block `IR.named` "loopBody"
        xPtr <- IR.gep (AST.ArrayType 0 AST.double) inputArray [IR.int32 0, counter] `IR.named` "xPtr"
        x <- IR.load AST.double xPtr 0 `IR.named` "x"
        result <- goExp (IntMap.singleton 0 x) expr `IR.named` "result"
        resultPtr <- IR.gep (AST.ArrayType 0 AST.double) resultArray [IR.int32 0, counter] `IR.named` "resultPtr"
        IR.store resultPtr 0 result
        nextCounter <- IR.add counter (IR.int32 1) `IR.named` "nextCounter"
        IR.br loop

        epilogue <- IR.block `IR.named` "epilogue"
        IR.retVoid

  ((sizeName, resultArrayName, inputArrayName), blocks) <- IR.runIRBuilderT IR.emptyIRBuilder $ do
    sizeName <- IR.fresh `IR.named` "size"
    resultArrayName <- IR.fresh `IR.named` "resultArray"
    inputArrayName <- IR.fresh `IR.named` "inputArray"
    functionBody (AST.LocalReference AST.i32 sizeName) (AST.LocalReference AST.ptr resultArrayName) (AST.LocalReference AST.ptr inputArrayName)
    pure (sizeName, resultArrayName, inputArrayName)

  let def = AST.GlobalDefinition AST.functionDefaults
            { AST.name = "f"
            , AST.parameters = ([AST.Parameter AST.i32 sizeName [], AST.Parameter AST.ptr resultArrayName [AST.NoAlias], AST.Parameter AST.ptr inputArrayName []], False)
            , AST.returnType = AST.void
            , AST.basicBlocks = blocks
            }
  IR.emitDefn def

  pure ()
