package me.viluon.tinyc
package ir

import bindings.AST
import ir.IRNode.BasicBlockID
import ir.IRType.IRInt

import cats.data.StateT
import cats.syntax.all._

/**
 * Lower the AST into IR.
 *
 * The computation happens in the [[Lowering.IRGen]] monad,
 * starting from [[Lowering.lowerTopLevel]].
 */
object Lowering {
  // converter needs
  // - fresh register store
  // - environment

  /*
    here's the (new) idea:
    - each block has a signature, which is a unique ID and a parameter list
    - during lowering, we fill in a map from IDs to signatures and actual blocks
    - we also maintain predecessor and successor maps to ease variable bridging
      (variables need to be forwarded through parameters but that's triggered by a pull
      which cascades signature changes up to the definition site)
    - everything refers to IDs to break up loops
    - additionally, we keep track of which block (by ID) defined what variable
      - wait, this should be an IRRegister thing, not a variable thing...
      - so Map[IRRegister, BasicBlockID]
  */

  case class IRGenState(
                         regCtr: Int,
                         blockCtr: Int,
                         blocks: Map[BasicBlockID, BasicBlock],
                         definedIn: Map[IRRegister, BasicBlockID],
                         predecessors: Map[BasicBlockID, List[BasicBlockID]],
                         successors: Map[BasicBlockID, List[BasicBlockID]],
                       )

  type IRGen[A] = StateT[Either[String, *], IRGenState, A]
  type BlockLocal[A] = StateT[IRGen, BlockLocal.State, A]

  type Cont = Continuation[IRRegister, BasicBlockID]
  type BasicBlock = IRNode.Block[IRRegister]
  type Expr = IRNode.IRExpression[IRRegister]

  def lowerTopLevel(ast: AST): IRGen[IRProgram[IRRegister]] = ast match {
    case b: AST.Block => for {
      blockIds <- b.stmts.traverse {
        case fn: AST.FunDecl => lowerBlock(fn.body.stmts, fn.params.map(p => p._1.name() -> p._2), CallingConvention.Function())
        case invalid =>
          throw new IllegalStateException(s"$invalid (of ${invalid.getClass}) is unsupported at the top level")
      }
      state <- get
    } yield IRProgram(blockIds.head, state.blocks)
    case _ => crash(s"top level must be a block, which $ast certainly isn't")
  }

  private def get: IRGen[IRGenState] = StateT.get
  private def put(n: IRGenState): IRGen[Unit] = StateT.set(n)
  private def modify(f: IRGenState => IRGenState): IRGen[Unit] = StateT.modify(f)
  private def pure[A](x: A): IRGen[A] = cats.Monad[IRGen].pure(x)
  private def crash[A](msg: String): IRGen[A] = StateT.liftF(Left(msg))

  private def fresh: IRGen[IRRegister] = for {
    state <- get
    regCtr = state.regCtr
    _ <- put(state.copy(regCtr = regCtr + 1))
  } yield IRRegister.IntReg(regCtr)

  private def freshBlockId: IRGen[BasicBlockID] = for {
    state <- get
    blockCtr = state.blockCtr
    _ <- put(state.copy(blockCtr = blockCtr + 1))
  } yield BasicBlockID(blockCtr)

  private def submitBlock(blockID: BasicBlockID, block: BasicBlock): IRGen[Unit] = for {
    state <- get
    blocks = state.blocks
    _ <- put(state.copy(blocks = blocks + (blockID -> block)))
  } yield ()

  object BlockLocal {
    /**
     * Lowering state local to a basic block.
     */
    case class State(env: Map[String, IRRegister], body: List[Expr])

    private def get: BlockLocal[State] = StateT.get
    private def put(n: State): BlockLocal[Unit] = StateT.set(n)
    private def modify(f: State => State): BlockLocal[Unit] = StateT.modify(f)
    private def pure[A](x: A): BlockLocal[A] = cats.Monad[BlockLocal].pure(x)
    private def crash[A](msg: String): BlockLocal[A] = StateT.liftF(Lowering.crash(msg))
    private def fresh: BlockLocal[IRRegister] = StateT.liftF(Lowering.fresh)

    private def emit(instr: Expr): BlockLocal[Unit] = modify(
      shapeless.lens[State].body.modify(_)(body => instr :: body)
    )

    private def bind(name: String, reg: IRRegister): BlockLocal[Unit] = modify { s =>
      s.copy(env = s.env + (name -> reg))
    }

    private def lookup(name: String): BlockLocal[IRRegister] = for {
      s <- get
      reg <- s.env.get(name) match {
        case Some(reg) => pure(reg)
        case None => crash(s"compiler bug: could not find $name in ${s.env}")
      }
    } yield reg

    def lower(ast: AST): BlockLocal[IRRegister] = ast match {
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
      case br: AST.If => ???
//        for {
//          cond <- lower(br.condition)
//          consequent <- lowerBlock(br.consequent)
//          alternative <- lowerBlock(br.alternative)
//          _ <- setContinuation(Continuation.Branch(cond, consequent, alternative))
//          _ <- finalizeBlock
//        } yield ???
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
  }

  private def lowerBlock(
                          stmts: List[AST],
                          params: List[(String, Type)],
                          callConv: CallingConvention = CallingConvention.Unrestricted()
                        ): IRGen[BasicBlockID] = for {
    blockId <- freshBlockId
    local /* get a local monad going */ = stmts.traverse_(BlockLocal.lower)
    paramRegs = params.zipWithIndex.map {
      case ((name, typ), index) => name -> IRRegister.Param(index, typ)
    }
    state <- local.runS(BlockLocal.State(paramRegs.toMap, Nil))
    // TODO (one step at a time) we need to deal with continuations properly
    _ <- submitBlock(blockId, IRNode.Block(params.map(_ => IRInt()), state.body, Continuation.Halt(), callConv))
  } yield blockId
}
