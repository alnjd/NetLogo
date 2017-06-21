// (C) Uri Wilensky. https://github.com/NetLogo/NetLogo

package org.nlogo.parse

import
  org.nlogo.core.{ Expression, Syntax, TypeNames },
    Syntax.compatible

import
  ParseResult.fail

object RichSyntax {
  def apply(name: String, syntax: Syntax, variadic: Boolean): RichSyntax =
    new RichSyntax(syntax, variadic, Nil, name)

  sealed trait ArgumentType {
    def matches(t: Int): Boolean
    def intersection(t: Int): Int
    def goal: Int
  }
  case object NoMoreArguments extends ArgumentType {
    def matches(t: Int): Boolean = false
    def intersection(t: Int): Int = 0
    def goal = 0
  }
  case class Argument(tpe: Int) extends ArgumentType {
    def matches(t: Int): Boolean = compatible(t, tpe)
    def intersection(t: Int): Int = t & tpe
    def goal = tpe
  }
  case class MaybeArgument(tpe: Int) extends ArgumentType {
    def matches(t: Int): Boolean = compatible(t, tpe)
    def intersection(t: Int): Int = t & tpe
    def goal = tpe
  }
  case class OneOfArgument(tpeA: Int, tpeB: Int) extends ArgumentType {
    def matches(t: Int): Boolean =
      compatible(t, tpeA) || compatible(t, tpeB)
    def intersection(t: Int): Int = {
      val compatibleA = compatible(t, tpeA)
      val compatibleB = compatible(t, tpeB)

      if (compatibleA && compatibleB) t & (tpeA | tpeB)
      else if (compatibleA)           t & tpeA
      else                            t & tpeB
    }
    def goal = tpeA | tpeB
  }

  private def removeRepeatableModifier(i: Int): Int =
    i & (~ Syntax.RepeatableType)

  sealed trait ArgumentRecognizer {
    def withArgument(arg: Expression): ArgumentRecognizer
    def withArguments(arg: Seq[Expression]): ArgumentRecognizer
    def recognizedArgument: ArgumentType
  }

  case object FinishedRecognizer extends ArgumentRecognizer {
    def withArgument(arg: Expression) = this
    def withArguments(arg: Seq[Expression]): ArgumentRecognizer = this
    def recognizedArgument: ArgumentType = NoMoreArguments
  }

  case class NormalRecognizer(_tpe: Int, next: ArgumentRecognizer) extends ArgumentRecognizer {
    val tpe = removeRepeatableModifier(_tpe)

    def withArgument(arg: Expression): ArgumentRecognizer =
      // TODO: check that arg satisfies types
      next

    def withArguments(args: Seq[Expression]): ArgumentRecognizer =
      if (args.isEmpty) this
      // TODO: check that args.head satisfies types
      else next.withArguments(args.tail)

    def recognizedArgument: ArgumentType =
      if ((tpe & Syntax.OptionalType) != 0)
        MaybeArgument(tpe & ~ Syntax.OptionalType)
      else
        Argument(tpe)
  }

  // this is only used in a *variadic* context.
  // Syntaxes with defaultOption, etc. will be turned into NormalRecognizers
  case class FinalVariadicRecognizer(_tpe: Int) extends ArgumentRecognizer {
    val tpe = removeRepeatableModifier(_tpe)
    def withArgument(arg: Expression): ArgumentRecognizer = this
    def withArguments(args: Seq[Expression]): ArgumentRecognizer = this
    def recognizedArgument: ArgumentType = MaybeArgument(tpe)
  }

  case class NonFinalVariadicRecognizer(_tpe: Int, next: ArgumentRecognizer) extends ArgumentRecognizer {
    val tpe = removeRepeatableModifier(_tpe)

    def withArgument(arg: Expression): ArgumentRecognizer =
      if (compatible(arg.reportedType, tpe))
        this
      else
        next.withArgument(arg)

    def withArguments(args: Seq[Expression]): ArgumentRecognizer =
      if (args.isEmpty) this
      else if (compatible(args.head.reportedType, tpe)) withArguments(args.tail)
      else next.withArguments(args)

    def recognizedArgument: ArgumentType =
      next.recognizedArgument match {
        case NoMoreArguments => Argument(tpe)
        case Argument(otherTpe) => OneOfArgument(tpe, otherTpe)
        case MaybeArgument(otherTpe) => OneOfArgument(tpe, otherTpe)
        case OneOfArgument(otherTpe, _) => OneOfArgument(tpe, otherTpe) // this shouldn't happen
      }
  }


  // syntaxes with defaults and variadics are bimodal.
  //
  // Mode 1 (not directly enclosed by parens = non-variadic):
  // Mode 2 (directly enclosed by parens = varidic):
  //   This has two sub-modes:
  //   a. The repeatable argument is the last argument on the right
  //   b. The repeatable argument is not the last argument on the right
  //
  // In Mode 1, the number of arguments is entirely deterministic.
  // The repeatable is extruded as necessary to reach the default argument count
  //  (this is often only once, but can be 2 or more times)
  // This is a linear state machine
  //
  // In mode 2a, the number of arguments prior to the repeatable argument is entirely
  // deterministic, then it is followed by a list of arguments which can be repeated any
  // number of times. If minimumOption is supplied, there must be at least minimum arguments.
  // This is a linear state machine, until it reaches the repeatable argument, at which point it
  // simply verifies.
  //
  // In mode 2a, only the arguments following the repeatable argument are entirely deterministic.
  // Once the first argument has been seen which cannot be repeatable, the rest of the types
  // are entirely determined.
}

import RichSyntax._

class RichSyntax(syntax: Syntax, variadic: Boolean, arguments: List[(Expression, Int)], primName: String) {
  lazy val allArgs = syntax.left +: syntax.right

  val argCount = arguments.length

  private def removeRepeatableModifier(i: Int): Int =
    i & (~ Syntax.RepeatableType)

  private def isRepeatable(i: Int): Boolean =
    (i & Syntax.RepeatableType) != 0

  def transformToRecognizerNormal(tpes: List[Int], countRemaining: Int): ArgumentRecognizer = {
    if (tpes.isEmpty) FinishedRecognizer
    else if (isRepeatable(tpes.head) && countRemaining > tpes.length)
      NormalRecognizer(tpes.head, transformToRecognizerNormal(tpes, countRemaining - 1))
    else
      NormalRecognizer(tpes.head, transformToRecognizerNormal(tpes.tail, countRemaining - 1))
  }

  def transformToRecognizerVariadic(tpes: List[Int]): ArgumentRecognizer = {
    if (tpes.isEmpty) FinishedRecognizer
    else if (isRepeatable(tpes.head) && tpes.length == 1)
      FinalVariadicRecognizer(tpes.head)
    else if (isRepeatable(tpes.head))
      NonFinalVariadicRecognizer(tpes.head, transformToRecognizerVariadic(tpes.tail))
    else
      NormalRecognizer(tpes.head, transformToRecognizerVariadic(tpes.tail))
  }

  def nextArgumentType: ArgumentType = {
    if (syntax.isInfix && arguments.length < allArgs.length)
      Argument(allArgs(arguments.length))
    else {
      val rec =
        if (variadic) transformToRecognizerVariadic(syntax.right)
        else          transformToRecognizerNormal(syntax.right, syntax.rightDefault)
      rec.withArguments(arguments.map(_._1)).recognizedArgument
    }
  }

  def withArgument(arg: Expression): ParseResult[RichSyntax] = {
    val assignedType = nextArgumentType match {
      case NoMoreArguments => fail(s"additional unwanted argument: $arg", arg.sourceLocation)
      case other           =>
        if (other.matches(arg.reportedType))
          SuccessfulParse(other.intersection(arg.reportedType))
        else {
          val displayedReportedType = {
            if ((other.goal & Syntax.ReferenceType) == 0 && (arg.reportedType & ~Syntax.ReferenceType) != 0)
              arg.reportedType & ~ Syntax.ReferenceType
            else
              arg.reportedType
          }
          fail(
            s"$primName expected this input to be ${TypeNames.aName(other.goal)}, but got ${TypeNames.aName(displayedReportedType)} instead", arg.sourceLocation)
        }
    }
    assignedType.map { tpe =>
      new RichSyntax(syntax, variadic, arguments :+ (arg -> tpe), primName)
    }
  }

  def typedArguments: Seq[(Expression, Int)] = arguments
}