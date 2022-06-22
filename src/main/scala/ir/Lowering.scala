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

  /*
  FIXME: outstanding issues:
    - we can't just do seq.body.traverse(lower) and the like – the BB *has to* end with a continuation
      and it can't contain control flow inside
      consider
        if A then
          B;
        else
          C;
        D;
      the result should be
        A-B
         \ \
          C-D
      so we'd like to start lowering A, lower B and C in the meantime, then end A and switch to lowering D
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
        case fn: AST.FunDecl => for {
          id <- freshBlockID
          params = fn.params.map {
            case (symbol, typ) => symbol.name() -> IRType.from(typ)
          }
          _ <- lowerBlock(id, fn.body.stmts, params, CallingConvention.Function())
        } yield id
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

  private def freshBlockID: IRGen[BasicBlockID] = for {
    state <- get
    blockCtr = state.blockCtr
    _ <- put(state.copy(blockCtr = blockCtr + 1))
  } yield BasicBlockID(blockCtr)

  private def submitBlock(blockID: BasicBlockID, block: BasicBlock): IRGen[Unit] = for {
    state <- get
    blocks = state.blocks
    _ <- put(state.copy(blocks = blocks + (blockID -> block)))
  } yield ()

  private def mapBlock(id: BasicBlockID, fn: BasicBlock => IRGen[BasicBlock]): IRGen[Unit] = for {
    s <- get
    block <- fn(s.blocks(id))
    _ <- put(s.copy(blocks = s.blocks + (id -> block)))
  } yield ()

  private def mapContinuation(id: BasicBlockID, fn: Cont => IRGen[Cont]): IRGen[Unit] =
    mapBlock(id, block => fn(block.cont).map(k => block.copy(cont = k)))

  /**
   * The set of immediate predecessors of a basic block.
   * A basic block may be its own parent.
   */
  private def parentsOf(blockID: BasicBlockID): IRGen[Set[BasicBlockID]] = for {
    s <- get
    parents = s.blocks.filter(_._2.cont.callees.contains(blockID))
  } yield parents.keySet

  private def lookupOrForward(blockID: BasicBlockID, name: String, visited: Set[BasicBlockID]): IRGen[Option[IRRegister]] = for {
    s <- get
    block = s.blocks(blockID)
    _ = println(s"looking for $name in $block")
    mbReg <- block.env.get(name) match {
      case Some(reg) => pure(Some(reg))
      case None => forwardVariable(blockID, name, visited)
    }
  } yield mbReg

  private def forwardVariable(blockID: BasicBlockID, name: String, visited: Set[BasicBlockID] = Set()): IRGen[Option[IRRegister]] = for {
    // find the variable in all predecessors
    // TODO use the parents here, forward the variable through parameter lists
    //  (adding it to envs), error if not found (empty unvisited parents)
    // TODO this should check that the calling convention of the block allows such extensions (functions typically don't)
    parents <- parentsOf(blockID)
    p = parents.removedAll(visited)
    // look the name up in each parent
    locations <- p.toList.traverse(id => lookupOrForward(id, name, visited + id).map(_.map(id -> _)))
    // short-circuit if any of the lookups failed
    mbLocations = if (locations.nonEmpty) locations.sequence else None
    mbReg <- mbLocations.map(variableLocations =>
      for {
        // extend the arg list at each callsite with the register containing the requested variable
        _ <- variableLocations.traverse_ {
          case (id, reg) => mapContinuation(id, cont => cont.mapTargets {
            case Continuation.Target(callee, args)
              if callee == blockID => pure(Continuation.Target(callee, args :+ reg))
            case t => pure(t)
          })
        }
        // extend the parameter list of this block by one
        typ = IRType.IRInt() // FIXME/TODO this needs to take type info into account!
        _ <- mapBlock(blockID, block => pure(
          block.copy(params = block.params :+ typ)
        ))
        s <- get
      } yield IRRegister.Param(s.blocks(blockID).params.size - 1, typ)
    ).sequence
  } yield mbReg


  object BlockLocal {
    /**
     * Lowering state local to a basic block.
     */
    case class State(blockID: BasicBlockID, callConv: CallingConvention, env: Map[String, IRRegister], body: List[Expr])

    private def get: BlockLocal[State] = StateT.get
    private def put(n: State): BlockLocal[Unit] = StateT.set(n)
    private def modify(f: State => State): BlockLocal[Unit] = StateT.modify(f)
    def pure[A](x: A): BlockLocal[A] = cats.Monad[BlockLocal].pure(x)
    private def crash[A](msg: String): BlockLocal[A] = StateT.liftF(Lowering.crash(msg))
    private def fresh: BlockLocal[IRRegister] = StateT.liftF(Lowering.fresh)
    private def freshBlockID: BlockLocal[BasicBlockID] = StateT.liftF(Lowering.freshBlockID)

    private def emit(instr: Expr): BlockLocal[Unit] = modify(
      shapeless.lens[State].body.modify(_)(body => instr :: body)
    )

    private def bind(name: String, reg: IRRegister): BlockLocal[Unit] = modify { s =>
      println(s"binding $name to $reg in ${s.blockID}")
      s.copy(env = s.env + (name -> reg))
    }

    /**
     * End the current basic block with a continuation and start a fresh one.
     *
     * @param f The monadic producer of the continuation along with a calling convention for the fresh basic block,
     *          called with the ID of the fresh block.
     */
    def endWith[A](f: BasicBlockID => BlockLocal[(Cont, CallingConvention, A)]): BlockLocal[A] = for {
      nextID <- freshBlockID
      s <- get
      _ <- put(State(nextID, CallingConvention.Unrestricted(), Map(), Nil))
      r <- f(nextID)
      (cont, callConv, a) = r
      block = IRNode.Block(s.blockID, List(), s.env, s.body.reverse, cont, s.callConv)
      _ <- StateT.liftF(submitBlock(s.blockID, block))
      _ <- modify(shapeless.lens[State].callConv.set(_)(callConv))
    } yield a

    /**
     * Replace the continuations of the ultimate successors of the given basic block.
     * This function targets the leaves of the IR graph.
     *
     * @param id   The ID of the basic block to scan for successors
     * @param cont The continuation to replace found continuations with
     */
    private def redirect(id: BasicBlockID, cont: Cont): BlockLocal[Unit] = {
      def successors(i: BasicBlockID): BlockLocal[Set[BasicBlockID]] = for {
        s <- StateT.liftF(Lowering.get)
      } yield s.blocks(i).cont.callees

      case class DfsState(visited: Set[BasicBlockID], stack: List[BasicBlockID])
      DfsState(Set(), List(id)).tailRecM(dfs => dfs.stack match {
        case head :: tail =>
          for {
            succs <- successors(head)
            _ <- if (succs.isEmpty /*leaf*/ )
              StateT.liftF(Lowering.modify(
                shapeless.lens[IRGenState].blocks.modify(_)(blocks =>
                  // replace the continuation
                  // sadly, shapelens don't work across indices
                  blocks + (head -> blocks(head).copy(cont = cont))
                )
              )): BlockLocal[Unit]
            else pure(())
            newSuccessors = succs.removedAll(dfs.visited)
          } yield Left(DfsState(dfs.visited + head, newSuccessors.toList ++ tail))
        case Nil => pure(Right(()))
      })
    }

    private def lowerBlock(id: BasicBlockID, stmts: List[AST], params: List[(String, IRType)]): BlockLocal[Unit] = for {
      // we first need to make sure the progress so far is visible to other computations
      s <- get
      _ = println("lower push from " + s.blockID)
      block = IRNode.Block(
        s.blockID,
        List(),
        s.env,
        s.body.reverse,
        Continuation.Halt(), // FIXME we need to store successors here I'm afraid
        s.callConv
      )
      _ <- StateT.liftF(submitBlock(s.blockID, block))
      _ <- StateT.liftF(Lowering.lowerBlock(id, stmts, params))
      _ = println(s"lower pop ($id)")
      // TODO and here we need to read the changes and apply them to the running state
    } yield ()

    /**
     * The set of immediate predecessors of the current basic block.
     * A basic block may be its own parent.
     */
    private def parents: BlockLocal[Set[BasicBlockID]] = for {
      s <- get
      parents <- StateT.liftF(parentsOf(s.blockID))
    } yield parents

    private def lookup(name: String): BlockLocal[IRRegister] = for {
      s <- get
      blockID = s.blockID
      mbReg <- (s.env.get(name) match {
        case Some(reg) => pure(Some(reg))
        case None => StateT.liftF(forwardVariable(blockID, name))
      }): BlockLocal[Option[IRRegister]]
      reg <- mbReg match {
        case Some(reg) => pure(reg)
        case None =>
          val ns = collection.mutable.Map[Any, Int]()
          val nameOf = (x: Any) => "node_" + ns.getOrElseUpdate(x, ns.size)
          for {
            irGenState <- StateT.liftF(Lowering.get)
            _ = println(irGenState.blocks.map(_._2.toDot(nameOf).indent(2)).mkString("\ndigraph {\n", "\n", "}"))
            reg <- crash[IRRegister](s"could not find $name in parent environments of block $blockID")
          } yield reg
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
      case br: AST.If => for {
        cond <- lower(br.condition)
        consequent <- freshBlockID
        alternative <- br.alternative.traverse(alt => freshBlockID.map(_ -> alt))
        noParams = List[IRRegister]()
        next <- endWith(next => pure((
          Continuation.Branch(
            cond,
            Continuation.Target(consequent, noParams),
            alternative
              .map(alt => Continuation.Target(alt._1, noParams))
              .getOrElse(Continuation.Target(next, noParams))
          ),
          CallingConvention.Unrestricted(),
          next
        ))): BlockLocal[BasicBlockID]
        _ <- lowerBlock(consequent, List(br.consequent), List())
        _ <- alternative.traverse(alt => lowerBlock(alt._1, List(alt._2), List()))
        jmpToNext = Continuation.Unconditional(Continuation.Target(next, noParams))
        _ <- redirect(consequent, jmpToNext)
        _ <- alternative.traverse_(alt => redirect(alt._1, jmpToNext))
      } yield IRRegister.NoReg()
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
      case as: AST.Assignment => for {
        reg <- fresh
        target <- as.lvalue match {
          case id: AST.Identifier => pure(id.name)
          case _ => ???
        }
        rv <- lower(as.rvalue)
        _ <- emit(IRNode.Copy(reg, rv))
        _ <- bind(target, reg)
      } yield reg
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

  /**
   * Lower a sequence of statements, creating basic blocks as necessary.
   * This is the root of a new [[BlockLocal]] computation.
   *
   * @param blockID  The ID to bind the lowered block to.
   * @param stmts    The body of the high-level block.
   * @param params   The named parameters in scope, typically from function definitions.
   * @param callConv The calling convention for the root basic block.
   * @return The ID of the root basic block created by this lowering.
   */
  private def lowerBlock(
                          blockID: BasicBlockID,
                          stmts: List[AST],
                          params: List[(String, IRType)],
                          callConv: CallingConvention = CallingConvention.Unrestricted()
                        ): IRGen[Unit] = for {
    _ <- pure(())
    local /* get a local monad going */ = for {
      _ <- stmts.traverse_(BlockLocal.lower)
      // submit the last in-progress block
      _ <- BlockLocal.endWith[Unit](_ => BlockLocal.pure(
        (Continuation.Halt()/*TODO???*/, CallingConvention.Unrestricted(), ())
      ))
    } yield ()
    paramRegs = params.zipWithIndex.map {
      case ((name, typ), index) => name -> IRRegister.Param(index, typ)
    }
    _ <- local.runS(BlockLocal.State(blockID, CallingConvention.Unrestricted(), paramRegs.toMap, Nil))
  } yield ()
}
