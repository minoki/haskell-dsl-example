{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
module Codegen (codegen) where
import qualified Data.ByteString.Char8 as BS.Char8
import qualified Data.ByteString.Short as SBS
import qualified Data.IntMap as IntMap
import           Exp
import           ExpS
import qualified LLVM.AST as AST (Module, Operand (ConstantOperand))
import qualified LLVM.AST.Constant as AST (Constant (Float))
import qualified LLVM.AST.Float as AST (SomeFloat (Double))
import qualified LLVM.AST.Type as AST (Type (FunctionType, argumentTypes, isVarArg, resultType),
                                       double)
import qualified LLVM.IRBuilder.Instruction as IR (call, fadd, fdiv, fmul, fneg,
                                                   fsub, ret)
import qualified LLVM.IRBuilder.Module as IR (MonadModuleBuilder,
                                              ParameterName (ParameterName),
                                              buildModule, extern, function)
import qualified LLVM.IRBuilder.Monad as IR (MonadIRBuilder, named)

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

  let xparam :: IR.ParameterName
      xparam = IR.ParameterName "x"
  _ <- IR.function "f" [(AST.double, xparam)] AST.double $ \[arg] -> do
    result <- goExp (IntMap.singleton 0 arg) expr
    IR.ret result
  pure ()
