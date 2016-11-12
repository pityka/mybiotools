package hivheritability

sealed trait WhichKernelToTest
case object OnlyHuman extends WhichKernelToTest
case object OnlyHIV extends WhichKernelToTest
case object HumanAndHIV extends WhichKernelToTest
case object HumanAndHIVAndCross extends WhichKernelToTest
case object NoLRT extends WhichKernelToTest