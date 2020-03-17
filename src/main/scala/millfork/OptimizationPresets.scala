package millfork

import millfork.assembly.AssemblyOptimization
import millfork.assembly.mos.AssemblyLine
import millfork.assembly.mos.opt._
import millfork.node.opt.{UnreachableCode, UnusedFunctions, UnusedGlobalVariables, UnusedLocalVariables}

/**
  * @author Karol Stasiak
  */
object OptimizationPresets {
  val NodeOpt = List(
    UnreachableCode,
    UnusedFunctions,
    UnusedLocalVariables,
    UnusedGlobalVariables,
  )
  val NodeOpt0 = List(
    UnusedFunctions,
    UnusedGlobalVariables,
  )
  val AssOpt: List[AssemblyOptimization[AssemblyLine]] = List[AssemblyOptimization[AssemblyLine]](
    UnusedLabelRemoval,
    AlwaysGoodOptimizations.NonetAddition,
    AlwaysGoodOptimizations.NonetBitOp,
    AlwaysGoodOptimizations.PointlessSignCheck,
    AlwaysGoodOptimizations.PoinlessLoadBeforeAnotherLoad,
    AlwaysGoodOptimizations.PointlessLoadAfterLoadOrStore,
    LaterOptimizations.PointessLoadingForShifting,
    AlwaysGoodOptimizations.SimplifiableBitOpsSequence,
    AlwaysGoodOptimizations.SimplifiableIndexChanging,
    AlwaysGoodOptimizations.IdempotentDuplicateRemoval,
    AlwaysGoodOptimizations.BranchInPlaceRemoval,
    UnusedLabelRemoval,
    AlwaysGoodOptimizations.OptimizableMasking,
    AlwaysGoodOptimizations.UnconditionalJumpRemoval,
    UnusedLabelRemoval,
    AlwaysGoodOptimizations.RearrangeMath,
    LaterOptimizations.PointlessLoadAfterStore,
    AlwaysGoodOptimizations.PoinlessLoadBeforeAnotherLoad,
    AlwaysGoodOptimizations.PointlessOperationAfterLoad,
    AlwaysGoodOptimizations.PointlessLoadBeforeTransfer,
    VariableToRegisterOptimization,
    TwoVariablesToIndexRegistersOptimization,
    AlwaysGoodOptimizations.RearrangableLoadFromTheSameLocation,
    AlwaysGoodOptimizations.PoinlessLoadBeforeAnotherLoad,
    AlwaysGoodOptimizations.CommonIndexSubexpressionElimination,
    AlwaysGoodOptimizations.PointlessOperationPairRemoval,
    AlwaysGoodOptimizations.PointlessOperationPairRemoval2,
    AlwaysGoodOptimizations.PoinlessLoadBeforeAnotherLoad,
    LaterOptimizations.PointlessLoadAfterStore,
    AlwaysGoodOptimizations.PointlessOperationAfterLoad,
    AlwaysGoodOptimizations.IdempotentDuplicateRemoval,
    LoopUnrolling.LoopUnrolling,
    AlwaysGoodOptimizations.ConstantPointer,
    AlwaysGoodOptimizations.ConstantIndexPropagation,
    AlwaysGoodOptimizations.PointlessLoadBeforeReturn,
    AlwaysGoodOptimizations.PoinlessFlagChange,
    AlwaysGoodOptimizations.FlagFlowAnalysis,
    AlwaysGoodOptimizations.ConstantFlowAnalysis,
    AlwaysGoodOptimizations.PointlessMath,
    AlwaysGoodOptimizations.PointlessOperationFromFlow,
    AlwaysGoodOptimizations.SimplifiableComparison,
    VariableToRegisterOptimization,
    TwoVariablesToIndexRegistersOptimization,
    ChangeIndexRegisterOptimizationPreferringX2Y,
    VariableToRegisterOptimization,
    TwoVariablesToIndexRegistersOptimization,
    ChangeIndexRegisterOptimizationPreferringY2X,
    VariableToRegisterOptimization,
    TwoVariablesToIndexRegistersOptimization,
    AlwaysGoodOptimizations.ConstantFlowAnalysis,
    LaterOptimizations.DoubleLoadToDifferentRegisters,
    LaterOptimizations.DoubleLoadToTheSameRegister,
    LaterOptimizations.DoubleLoadToDifferentRegisters,
    LaterOptimizations.DoubleLoadToTheSameRegister,
    LaterOptimizations.DoubleLoadToDifferentRegisters,
    LaterOptimizations.DoubleLoadToTheSameRegister,
    LaterOptimizations.DoubleLoadToTwoRegistersWhenOneWillBeTrashed,
    EmptyMemoryStoreRemoval,
    EmptyParameterStoreRemoval,
    AlwaysGoodOptimizations.PointlessOperationFromFlow,
    AlwaysGoodOptimizations.PoinlessLoadBeforeAnotherLoad,
    AlwaysGoodOptimizations.IdempotentDuplicateRemoval,
    AlwaysGoodOptimizations.ConstantPointer,
    AlwaysGoodOptimizations.ConstantIndexPropagation,
    AlwaysGoodOptimizations.ConstantFlowAnalysis,
    AlwaysGoodOptimizations.PointlessRegisterTransfers,
    AlwaysGoodOptimizations.PointlessRegisterTransfersBeforeCompare,
    AlwaysGoodOptimizations.PointlessRegisterTransfersBeforeReturn,
    AlwaysGoodOptimizations.PointlessRegisterTransfersBeforeStore,
    AlwaysGoodOptimizations.PointlessStashingToIndexOverShortSafeBranch,
    AlwaysGoodOptimizations.LoopInvariantRegister,
    AlwaysGoodOptimizations.PointlessStackStashing,
    AlwaysGoodOptimizations.PointlessStashingForLaterStore,
    AlwaysGoodOptimizations.PointlessStashingForLaterLoad,
    AlwaysGoodOptimizations.LoadingOfJustWrittenValue,
    AlwaysGoodOptimizations.PointlessStackStore,
    AlwaysGoodOptimizations.RearrangeMath,
    AlwaysGoodOptimizations.LoadingOfJustWrittenValue,
    EmptyMemoryStoreRemoval,
    EmptyParameterStoreRemoval,
    AlwaysGoodOptimizations.PointlessLoadBeforeReturn,
    LaterOptimizations.PointessLoadingForShifting,
    AlwaysGoodOptimizations.SimplifiableBitOpsSequence,
    AlwaysGoodOptimizations.SimplifiableBitOpsSequence,
    AlwaysGoodOptimizations.SimplifiableBitOpsSequence,
    AlwaysGoodOptimizations.SimplifiableBitOpsSequence,

    LaterOptimizations.LoadingAfterShifting,
    EmptyMemoryStoreRemoval,
    EmptyParameterStoreRemoval,
    AlwaysGoodOptimizations.PoinlessStoreBeforeStore,
    LaterOptimizations.PointlessLoadAfterStore,
    AlwaysGoodOptimizations.PoinlessLoadBeforeAnotherLoad,
    AlwaysGoodOptimizations.RearrangableLoadFromTheSameLocation,
    AlwaysGoodOptimizations.ShiftingJustWrittenValue,
    AlwaysGoodOptimizations.PointlessAccumulatorShifting,
    AlwaysGoodOptimizations.ReverseFlowAnalysis,

    LaterOptimizations.LoadingAfterShifting,
    EmptyMemoryStoreRemoval,
    EmptyParameterStoreRemoval,
    AlwaysGoodOptimizations.PoinlessStoreBeforeStore,
    LaterOptimizations.PointlessLoadAfterStore,
    AlwaysGoodOptimizations.PoinlessLoadBeforeAnotherLoad,
    AlwaysGoodOptimizations.UnconditionalJumpRemoval,
    AlwaysGoodOptimizations.AlwaysTakenJumpRemoval,
    AlwaysGoodOptimizations.UnusedLabelRemoval,

    AlwaysGoodOptimizations.ConstantInlinedShifting,
    LaterOptimizations.LoadingAfterShifting,
    AlwaysGoodOptimizations.PointlessAccumulatorShifting,
    EmptyMemoryStoreRemoval,
    EmptyParameterStoreRemoval,
    AlwaysGoodOptimizations.PoinlessStoreBeforeStore,
    AlwaysGoodOptimizations.ConstantPointer,
    LaterOptimizations.PointlessLoadAfterStore,
    AlwaysGoodOptimizations.PoinlessLoadBeforeAnotherLoad,
    AlwaysGoodOptimizations.TailCallOptimization,
    AlwaysGoodOptimizations.UnusedCodeRemoval,
    AlwaysGoodOptimizations.ReverseFlowAnalysis,
    AlwaysGoodOptimizations.ModificationOfJustWrittenValue,
    AlwaysGoodOptimizations.ConstantInlinedShifting,
    AlwaysGoodOptimizations.ShiftingJustWrittenValue,
    AlwaysGoodOptimizations.PointlessAccumulatorShifting,
    AlwaysGoodOptimizations.ReverseFlowAnalysis,
    AlwaysGoodOptimizations.PointlessMathFromFlow,
    AlwaysGoodOptimizations.PointlessMathFromFlow,
    AlwaysGoodOptimizations.PointlessMathFromFlow,
    AlwaysGoodOptimizations.ReplacingArithmeticsWithBitOps,
    AlwaysGoodOptimizations.PointlessMathFromFlow,
    AlwaysGoodOptimizations.PointlessMathFromFlow,
    AlwaysGoodOptimizations.PointlessMathFromFlow,
    AlwaysGoodOptimizations.PointlessMathFromFlow,
    AlwaysGoodOptimizations.PointlessMathFromFlow,
    LaterOptimizations.CommutativeInPlaceModifications,
    AlwaysGoodOptimizations.LoadingOfJustWrittenValue,
    AlwaysGoodOptimizations.PointlessStackStore,
    AlwaysGoodOptimizations.OptimizeZeroComparisons,
    AlwaysGoodOptimizations.SimplifiableCondition,
    AlwaysGoodOptimizations.ConstantPointer,
    AlwaysGoodOptimizations.IncrementingIndexRegistersAfterTransfer,
    AlwaysGoodOptimizations.MathOperationOnTwoIdenticalMemoryOperands,
    LaterOptimizations.UseZeropageAddressingMode,
    AlwaysGoodOptimizations.UnconditionalJumpRemoval,
    AlwaysGoodOptimizations.AlwaysTakenJumpRemoval,
    AlwaysGoodOptimizations.UnusedLabelRemoval,

    LaterOptimizations.DontUseIndexRegisters,
    LaterOptimizations.UseXInsteadOfStack,
    LaterOptimizations.UseYInsteadOfStack,
    LaterOptimizations.IndexSwitchingOptimization,
    LaterOptimizations.LoadingBranchesOptimization,
    LaterOptimizations.IncreaseWithLimit,
    SingleAssignmentVariableOptimization,
    LocalVariableReadOptimization,
    AlwaysGoodOptimizations.PointlessStackStore,
    AlwaysGoodOptimizations.SimplifiableStackOperation,
    LaterOptimizations.UseBit,
    LaterOptimizations.ReplaceableLoad,
  )

  val Good: List[AssemblyOptimization[AssemblyLine]] = List[AssemblyOptimization[AssemblyLine]](
    UnusedLabelRemoval,
    AlwaysGoodOptimizations.Adc0Optimization,
    AlwaysGoodOptimizations.AlwaysTakenJumpRemoval,
    AlwaysGoodOptimizations.BitPackingUnpacking,
    AlwaysGoodOptimizations.BranchInPlaceRemoval,
    AlwaysGoodOptimizations.CarryFlagConversion,
    DangerousOptimizations.ConstantIndexOffsetPropagation,
    AlwaysGoodOptimizations.ConstantInlinedShifting,
    AlwaysGoodOptimizations.CommonBranchBodyOptimization,
    AlwaysGoodOptimizations.CommonExpressionInConditional,
    AlwaysGoodOptimizations.CommonIndexSubexpressionElimination,
    AlwaysGoodOptimizations.ConstantPointer,
    AlwaysGoodOptimizations.ConstantFlowAnalysis,
    AlwaysGoodOptimizations.ConstantIndexPropagation,
    AlwaysGoodOptimizations.DoubleJumpSimplification,
    EmptyMemoryStoreRemoval,
    EmptyParameterStoreRemoval,
    AlwaysGoodOptimizations.FlagFlowAnalysis,
    AlwaysGoodOptimizations.IdempotentDuplicateRemoval,
    AlwaysGoodOptimizations.ImpossibleBranchRemoval,
    AlwaysGoodOptimizations.IncrementingIndexRegistersAfterTransfer,
    AlwaysGoodOptimizations.IndexComparisonOptimization,
    AlwaysGoodOptimizations.IndexSequenceOptimization,
    AlwaysGoodOptimizations.InefficientStashingToRegister,
    AlwaysGoodOptimizations.LoadingOfJustWrittenValue,
    AlwaysGoodOptimizations.LoopInvariantRegister,
    LoopUnrolling.LoopUnrolling,
    AlwaysGoodOptimizations.MathOperationOnTwoIdenticalMemoryOperands,
    AlwaysGoodOptimizations.ModificationOfJustWrittenValue,
    AlwaysGoodOptimizations.NonetAddition,
    AlwaysGoodOptimizations.NonetBitOp,
    AlwaysGoodOptimizations.OperationsAroundShifting,
    AlwaysGoodOptimizations.OptimizableMasking,
    AlwaysGoodOptimizations.OptimizeZeroComparisons,
    AlwaysGoodOptimizations.PoinlessFlagChange,
    AlwaysGoodOptimizations.PointlessAccumulatorShifting,
    AlwaysGoodOptimizations.PointlessLoadAfterLoadOrStore,
    AlwaysGoodOptimizations.PoinlessLoadBeforeAnotherLoad,
    AlwaysGoodOptimizations.PointlessLoadBeforeReturn,
    AlwaysGoodOptimizations.PointlessLoadBeforeTransfer,
    AlwaysGoodOptimizations.PointlessMath,
    AlwaysGoodOptimizations.PointlessMathFromFlow,
    AlwaysGoodOptimizations.PointlessOperationAfterLoad,
    AlwaysGoodOptimizations.PointlessOperationFromFlow,
    AlwaysGoodOptimizations.PointlessOperationPairRemoval,
    AlwaysGoodOptimizations.PointlessOperationPairRemoval2,
    AlwaysGoodOptimizations.PointlessRegisterTransfers,
    AlwaysGoodOptimizations.PointlessRegisterTransfersBeforeCompare,
    AlwaysGoodOptimizations.PointlessRegisterTransfersBeforeReturn,
    AlwaysGoodOptimizations.PointlessSignCheck,
    AlwaysGoodOptimizations.PointlessStackStashing,
    AlwaysGoodOptimizations.PointlessStackStore,
    AlwaysGoodOptimizations.PointlessStashingForLaterLoad,
    AlwaysGoodOptimizations.PointlessStashingForLaterStore,
    AlwaysGoodOptimizations.PointlessStashingToIndexOverShortSafeBranch,
    AlwaysGoodOptimizations.PoinlessStoreBeforeStore,
    AlwaysGoodOptimizations.PointlessStoreToTheSameVariable,
    AlwaysGoodOptimizations.RearrangableLoadFromTheSameLocation,
    AlwaysGoodOptimizations.RearrangeMath,
    AlwaysGoodOptimizations.RemoveNops,
    AlwaysGoodOptimizations.ReplacingArithmeticsWithBitOps,
    AlwaysGoodOptimizations.ReverseFlowAnalysis,
    AlwaysGoodOptimizations.ShiftingJustWrittenValue,
    AlwaysGoodOptimizations.SimplifiableBitOpsSequence,
    AlwaysGoodOptimizations.SimplifiableComparison,
    AlwaysGoodOptimizations.SimplifiableCondition,
    AlwaysGoodOptimizations.SimplifiableIndexChanging,
    AlwaysGoodOptimizations.SimplifiableStackOperation,
    AlwaysGoodOptimizations.SmarterShiftingOfWords,
    AlwaysGoodOptimizations.SmarterShiftingBytes,
    AlwaysGoodOptimizations.UnconditionalJumpRemoval,
    UnusedLabelRemoval,
    AlwaysGoodOptimizations.TailCallOptimization,
    AlwaysGoodOptimizations.UnusedCodeRemoval,
    AlwaysGoodOptimizations.UnusedLabelRemoval,
    UseAccumulatorInsteadOfXRegister,
    UseAccumulatorInsteadOfYRegister,
    VariableToRegisterOptimization,
    TwoVariablesToIndexRegistersOptimization,
  )

  val QuickPreset: List[AssemblyOptimization[AssemblyLine]] = List[AssemblyOptimization[AssemblyLine]](
    UnusedLabelRemoval,
    AlwaysGoodOptimizations.Adc0Optimization,
    AlwaysGoodOptimizations.BranchInPlaceRemoval,
    AlwaysGoodOptimizations.CommonBranchBodyOptimization,
    AlwaysGoodOptimizations.CommonExpressionInConditional,
    AlwaysGoodOptimizations.CommonIndexSubexpressionElimination,
    AlwaysGoodOptimizations.IndexSequenceOptimization,
    AlwaysGoodOptimizations.PoinlessStoreBeforeStore,
    AlwaysGoodOptimizations.PointlessLoadAfterLoadOrStore,
    AlwaysGoodOptimizations.PointlessLoadBeforeTransfer,
    AlwaysGoodOptimizations.PointlessOperationFromFlow,
    AlwaysGoodOptimizations.ReverseFlowAnalysis,
    AlwaysGoodOptimizations.SimplifiableCondition,
    LaterOptimizations.DontUseIndexRegisters,
    VariableToRegisterOptimization,
    TwoVariablesToIndexRegistersOptimization,
    AlwaysGoodOptimizations.PointlessLoadAfterLoadOrStore,
    LaterOptimizations.DoubleLoadToTheSameRegister
  )
}
