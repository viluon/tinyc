package me.viluon.tinyc
package ir

import bindings.AST

import cats.Monad
import cats.data.StateT
import cats.syntax.all._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/**
 * Lower the AST into IR.
 *
 * The computation happens in the [[Lowering.Converter]] monad,
 * starting from [[Lowering.lowerTopLevel]].
 */
object Lowering {
  // converter needs
  // - fresh register store
  // - environment
  case class ConvState(
                        regCtr: Int,
                        env: Map[String, IRRegister],
                        ir: List[BasicBlock] = Nil,
                        currBlock: BasicBlock = IRNode.Block(Nil, Nil, Continuation.Halt())
                      )

  type Converter[A] = StateT[Either[String, *], ConvState, A]

  private type BasicBlock = IRNode.Block[IRRegister]
  private type Expr = IRNode.IRExpression[IRRegister]

  def lowerTopLevel(ast: AST): Converter[IRProgram[IRRegister]] = ast match {
    case b: AST.Block => for {
      _ <- b.stmts.traverse_(lower)
      _ <- finalizeBlock
      state <- get
    } yield IRProgram(state.ir.head)
    case _ => crash(s"top level must be a block, which $ast certainly isn't")
  }

  private def get: Converter[ConvState] = StateT.get
  private def put(n: ConvState): Converter[Unit] = StateT.set(n)
  private def modify(f: ConvState => ConvState): Converter[Unit] = StateT.modify(f)
  private def pure[A](x: A): Converter[A] = cats.Monad[Converter].pure(x)

  private def crash[A](msg: String): Converter[A] = StateT.liftF(Left(msg))
  private def fresh: Converter[IRRegister] = for {
    state <- get
    ConvState(regCtr, _, _, _) = state
    _ <- put(state.copy(regCtr = regCtr + 1))
  } yield IRRegister.IntReg(regCtr)

  private def emit(instr: IRNode.IRExpression[IRRegister]): Converter[Unit] = {
    import shapeless.lens

    val currBlockBody = lens[ConvState].currBlock.body
    modify(currBlockBody.modify(_)(body => instr :: body))
  }

  private def bind(name: String, reg: IRRegister): Converter[Unit] = modify { s =>
    s.copy(env = s.env + (name -> reg))
  }

  private def lookup(name: String): Converter[IRRegister] = for {
    s <- get
    reg <- s.env.get(name) match {
      case Some(reg) => pure(reg)
      case None => crash(s"compiler bug: could not find $name in ${s.env}")
    }
  } yield reg

  private def finalizeBlock: Converter[Unit] = modify { s =>
    val body = shapeless.lens[BasicBlock].body
    s.copy(ir = s.ir :+ body.modify(s.currBlock)(_.reverse), currBlock = IRNode.Block(Nil, Nil, Continuation.Halt()))
  }

  private def setContinuation(continuation: Continuation[IRRegister]): Converter[Unit] = modify(
    shapeless.lens[ConvState].currBlock.cont.set(_)(continuation)
  )

  /*
  here's the (new) idea:
  - each block has a signature, which is a unique ID and a parameter list
  - during lowering, we fill in a map from IDs to signatures and actual blocks
  - we also maintain predecessor and successor maps to ease variable bridging
    (variables need to be forwarded through parameters but that's triggered by a pull
    which cascades signature changes up to the definition site)
  - everything refers to IDs to break up loops
   */

  private def lower(ast: AST): Converter[IRRegister] = ast match {
    case int: AST.Integer => for {
      reg <- fresh
      _ <- emit(IRNode.KInt(reg, int.v))
    } yield reg
    case _: AST.Double => ???
    case _: AST.Char => ???
    case _: AST.String => ???
    case id: AST.Identifier => lookup(id.name)
    case _: AST.Type => ???
    case _: AST.PointerType => ???
    case _: AST.ArrayType => ???
    case _: AST.NamedType => ???
    case seq: AST.Sequence => seq.body.traverse(lower).map(_ => IRRegister.NoReg())
    case b: AST.Block => b.stmts.traverse(lower).map(_ => IRRegister.NoReg())
    case vr: AST.VarDecl => for {
      // TODO this can't handle &x
      res <- lower(vr.value)
      _ <- bind(vr.ident.name, res)
    } yield res
    case fn: AST.FunDecl => lower(fn.body) // TODO
    case _: AST.StructDecl => ???
    case _: AST.FunPtrDecl => ???
    case br: AST.If => for {
      cond <- lower(br.condition)
      consequent <- lowerBlock(br.consequent)
      alternative <- lowerBlock(br.alternative)
      _ <- setContinuation(Continuation.Branch(cond, consequent, alternative))
      _ <- finalizeBlock
    } yield ???
    case _: AST.Switch => ???
    case _: AST.While => ???
    case _: AST.DoWhile => ???
    case _: AST.For => ???
    case _: AST.Break => ???
    case _: AST.Continue => ???
    case ret: AST.Return => lower(ret.value) // TODO
    case bin: AST.BinaryOp => for {
      a <- lower(bin.left)
      b <- lower(bin.right)
      operation <- bin.op.name() match {
        case "+" => pure(BinaryOperator.Add() -> false)
        case "-" => pure(BinaryOperator.Sub() -> false)
        case "*" => pure(BinaryOperator.Mul() -> false)
        case "/" => pure(BinaryOperator.Div() -> false)
        case "<" => pure(BinaryOperator.LessThan() -> false)
        case "<=" => pure(BinaryOperator.LessOrEqual() -> false)
        // FIXME flipping the arguments may pose problems with floats
        case ">" => pure(BinaryOperator.LessThan() -> true)
        case ">=" => pure(BinaryOperator.LessOrEqual() -> true)
        case invalid => crash(s"unknown binary operator $invalid")
      }
      (op, flip) = operation
      reg <- fresh
      (l, r) = if (flip) (b, a) else (a, b)
      _ <- emit(IRNode.BinOp(reg, op, l, r))
    } yield reg
    case _: AST.Assignment => ???
    case _: AST.UnaryOp => ???
    case _: AST.UnaryPostOp => ???
    case _: AST.Address => ???
    case _: AST.Deref => ???
    case _: AST.Index => ???
    case _: AST.Member => ???
    case _: AST.MemberPtr => ???
    case _: AST.Call => ???
    case _: AST.Cast => ???
    case _: AST.Write => ???
    case _: AST.Read => ???
  }

  private def asBlock(nodes: List[IRNode[IRRegister]]): Converter[IRNode[IRRegister]] = for {
    _ <- pure(())
    nodes <- expressionify(nodes)
  } yield IRNode.Block(
    Nil /*TODO: let's refactor lower() into a Writer that keeps track of dependencies*/ ,
    List(nodes), // FIXME
    Continuation.Halt()
  )

  implicit class ListWindowOps[A](val xs: List[A]) extends AnyVal {
    import cats.syntax.applicative._
    import cats.syntax.either._
    import cats.syntax.flatMap._
    import cats.syntax.functor._
    import cats.syntax.traverse._

    /**
     * Partition [[xs]] into sublists, each terminated by an element.
     * The non-terminated tail is returned separately.
     */
    def splitWithTerminators[B, C, M[_]: Monad](p: A => M[Either[B, C]]): M[(List[(List[B], C)], List[B])] = {
      val iter = xs.iterator

      def go(bs: ArrayBuffer[B]): M[Either[List[B], (List[B], C)]] =
        iter.nextOption().toRight(bs.toList).map(x => p(x).flatMap {
          case Left(b) => bs += b; go(bs)
          case Right(c) => ((bs.toList, c).asRight: Either[List[B], (List[B], C)]).pure
        }).sequence.map(_.flatten)

      def run(buf: mutable.ArrayBuffer[(List[B], C)]): M[(List[(List[B], C)], List[B])] =
        go(ArrayBuffer()).flatMap {
          case Right(win) => buf += win; run(buf)
          case Left(rest) => (buf.toList, rest).pure
        }

      run(ArrayBuffer())
    }
  }

  private def buildBlocks(nodes: List[IRNode[IRRegister]]): Converter[(List[BasicBlock], List[Expr])] = {
    import cats.syntax.either._
    type T = Either[Expr, Continuation[IRRegister]]
    for {
      winsAndTail <- nodes.splitWithTerminators {
        case expr: IRNode.IRExpression[_] => pure(expr.asLeft: T)
        case IRNode.Block(signature, body, cont, callingConvention) => pure[T](cont.asRight)
      }
      (windows, tail) = winsAndTail
    } yield {
      val unlinkedBlocks = windows.map {
        case (exprs, cont) => IRNode.Block(Nil /*TODO*/, exprs, cont)
      }
      val linkedBlocks = unlinkedBlocks.reverse.foldLeft(List[BasicBlock]()) {
        case (Nil, block) => List(block)
        case (acc@succ :: _, block) => block.copy(cont = Continuation.Unconditional(succ)) :: acc
      }
      (linkedBlocks, tail)
    }
  }

  private def expressionify(nodes: List[IRNode[IRRegister]]): Converter[Expr] = {
    ???
  }
}
