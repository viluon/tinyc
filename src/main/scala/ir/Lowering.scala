package me.viluon.tinyc
package ir

import bindings.AST

import cats.Monad
import cats.data.StateT
import cats.syntax.traverse.toTraverseOps

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
  case class ConvState(regCtr: Int, env: Map[String, Int])
  type Converter[A] = StateT[Either[String, *], ConvState, A]

  private type BasicBlock = IRNode.Block[IRRegister]
  private type Expr = IRNode.IRExpression[IRRegister]

  def lowerTopLevel(ast: AST): Converter[IRProgram[IRRegister]] = ast match {
    case b: AST.Block =>
      b.stmts.traverse(lower).map(
        _.map(_.asInstanceOf[BasicBlock])
          .map(_.copy(callingConvention = CallingConvention.Function()))
      ).map(IRProgram(_))
    case _ => crash(s"top level must be a block, which $ast certainly isn't")
  }

  private def get: Converter[ConvState] = StateT.get
  private def put(n: ConvState): Converter[Unit] = StateT.set(n)
  private def modify(f: ConvState => ConvState): Converter[Unit] = StateT.modify(f)
  private def pure[A](x: A): Converter[A] = cats.Monad[Converter].pure(x)

  private def crash[A](msg: String): Converter[A] = StateT.liftF(Left(msg))
  private def fresh: Converter[IRRegister] = for {
    state <- get
    ConvState(regCtr, _) = state
    _ <- put(state.copy(regCtr = regCtr + 1))
  } yield IRRegister(regCtr)

  private def bind(name: String, reg: IRRegister): Converter[Unit] = modify { s =>
    s.copy(env = s.env + (name -> reg.n))
  }

  private def lookup(name: String): Converter[IRRegister] = for {
    s <- get
    reg <- s.env.get(name) match {
      case Some(n) => pure(IRRegister(n))
      case None => crash(s"compiler bug: could not find $name in ${s.env}")
    }
  } yield reg

  private def lower(ast: AST): Converter[IRNode[IRRegister]] = ast match {
    case int: AST.Integer => pure(IRNode.KInt(int.v))
    case _: AST.Double => ???
    case _: AST.Char => ???
    case _: AST.String => ???
    case id: AST.Identifier => lookup(id.name).map(IRNode.Load(_))
    case _: AST.Type => ???
    case _: AST.PointerType => ???
    case _: AST.ArrayType => ???
    case _: AST.NamedType => ???
    case seq: AST.Sequence => seq.body.traverse(lower).flatMap(asBlock)
    case b: AST.Block => b.stmts.traverse(lower).flatMap(asBlock)
    case vr: AST.VarDecl => for {
      // TODO this can't handle &x
      reg <- fresh
      res <- lower(vr.value)
      _ <- bind(vr.ident.name, reg)
    } yield res
    case fn: AST.FunDecl => lower(fn.body) // TODO
    case _: AST.StructDecl => ???
    case _: AST.FunPtrDecl => ???
    case _: AST.If => ???
    case _: AST.Switch => ???
    case _: AST.While => ???
    case _: AST.DoWhile => ???
    case _: AST.For => ???
    case _: AST.Break => ???
    case _: AST.Continue => ???
    case ret: AST.Return => lower(ret.value) // TODO
    case bin: AST.BinaryOp => for {
      // FIXME ugliness
      l <- lower(bin.left)
      l <- expressionify(List(l))
      r <- lower(bin.right)
      r <- expressionify(List(r))
      op <- bin.op.name() match {
        case "+" => pure(BinaryOperator.Add())
        case "-" => pure(BinaryOperator.Sub())
        case "*" => pure(BinaryOperator.Mul())
        case "/" => pure(BinaryOperator.Div())
        case invalid => crash(s"unknown binary operator $invalid")
      }
    } yield IRNode.BinOp(op, l, r)
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
      val iter = xs.iterator // smelly
      // yeah :\ but I need to remember the position in xs and this is an easy way to do that
      // feel free to make it pure instead :P (without ListBuffer and the iterator)
      // no I meant you
      // (:

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
        case (acc@(succ :: _), block) => block.copy(cont = Continuation.Unconditional(succ)) :: acc
      }
      (linkedBlocks, tail)
    }
  }

  private def expressionify(nodes: List[IRNode[IRRegister]]): Converter[Expr] = {
    ???
  }
}
