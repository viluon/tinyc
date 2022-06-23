package me.viluon.tinyc
package ir

import bindings.AST
import ir.IRNode.{BasicBlockID, BlockOps}
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

  // BEWARE: Shapeless lenses don't show up when looking for references

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
      entry <- freshBlockID
      functionBlocks <- b.stmts.traverse {
        case fn: AST.FunDecl => for {
          id <- freshBlockID
          params = fn.params.map {
            case (symbol, typ) => symbol.name() -> IRType.from(typ)
          }
          _ <- lowerBlock(id, fn.body.stmts, params, CallingConvention.Function())
        } yield fn.name -> id
        case invalid =>
          throw new IllegalStateException(s"$invalid (of ${invalid.getClass}) is unsupported at the top level")
      }
      main <- functionBlocks.toMap.get("main") match {
        case Some(id) => pure(id)
        case None => crash(s"no main function found")
      }
      dummyReg <- fresh
      entryBlock = IRNode.Block(entry, Nil, Map(), List(IRNode.Call(dummyReg, main, List())), Continuation.Halt())
      _ <- submitBlock(entryBlock)
      state <- get
    } yield IRProgram(entry, state.blocks)
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

  private def submitBlock(block: BasicBlock): IRGen[Unit] = for {
    state <- get
    blocks = state.blocks
    _ <- put(state.copy(blocks = blocks + (block.id -> block)))
  } yield ()

  private def mapBlock(id: BasicBlockID, fn: BasicBlock => IRGen[BasicBlock]): IRGen[Unit] = for {
    s <- get
    block <- s.blocks.get(id) match {
      case Some(block) => fn(block)
      case None =>
        println(s)
        throw new IllegalStateException(s"block $id not found")
    }
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
    // TODO this needs a change to support calls, where
    //  for the callee of a call continuation we add the return block to the parents
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
        s <- get
        param = IRRegister.Param(s.blocks(blockID).params.size, typ)
        _ <- mapBlock(blockID, block => pure(
          block.copy(params = block.params :+ typ, env = block.env + (name -> param))
        ))
      } yield param
    ).sequence
  } yield mbReg


  object BlockLocal {
    /**
     * Lowering state local to a basic block.
     *
     * @param block The basic block being lowered.
     */
    case class State(block: BasicBlock)

    private def get: BlockLocal[BasicBlock] = for {
      s <- StateT.get: BlockLocal[State]
    } yield s.block
    private def put(n: State): BlockLocal[Unit] = for {
      block <- get
      _ <- StateT.liftF(Lowering.submitBlock(block))
      _ <- StateT.set(n): BlockLocal[Unit]
    } yield ()
    private def modify(f: State => State): BlockLocal[Unit] = StateT.modify(f)
    def pure[A](x: A): BlockLocal[A] = cats.Monad[BlockLocal].pure(x)
    private def crash[A](msg: String): BlockLocal[A] = StateT.liftF(Lowering.crash(msg))
    private def fresh: BlockLocal[IRRegister] = StateT.liftF(Lowering.fresh)
    private def freshBlockID: BlockLocal[BasicBlockID] = StateT.liftF(Lowering.freshBlockID)

    private def emit(instr: Expr): BlockLocal[Unit] = modify(
      shapeless.lens[State].block.body.modify(_)(body => instr :: body)
    )

    private def bind(name: String, reg: IRRegister): BlockLocal[Unit] = modify { s =>
      println(s"binding $name to $reg in ${s.block.id}")
      shapeless.lens[State].block.env.modify(s)(env => env + (name -> reg))
    }

    /**
     * End the current basic block with a continuation and start a fresh one.
     *
     * @param f The monadic producer of the continuation along with a calling convention for the fresh basic block,
     *          called with the ID of the fresh block.
     */
    def endWith[A](f: BasicBlockID => BlockLocal[(Cont, CallingConvention, A)]): BlockLocal[A] = for {
      block <- get
      // create a fresh block for further lowering
      nextID <- freshBlockID
      _ <- put(State(IRNode.Block(nextID, Nil, Map(), Nil, Continuation.Halt())))
      // kindly ask f to give us the desired continuation for the finished block
      // and the calling convention for the next block
      r <- f(nextID)
      (cont, callConv, a) = r
      // update the current block to have the desired continuation
      finishedBlock = block.copy(body = block.body.reverse, cont = cont)
      _ <- StateT.liftF(submitBlock(finishedBlock))
      // update the fresh block's calling convention as desired by f
      _ <- modify(shapeless.lens[State].block.callingConvention.set(_)(callConv))
    } yield a

    /**
     * Replace the continuations of the ultimate successors of the given basic block.
     * This function targets the leaves of the IR graph.
     *
     * @param id   The ID of the basic block to scan for successors
     * @param cont The continuation to replace found continuations with
     */
    private def redirect(id: BasicBlockID, cont: Cont): BlockLocal[Unit] = new GraphTraversal[BlockLocal, BasicBlockID, Unit] {
      override def successors(node: BasicBlockID): BlockLocal[Set[BasicBlockID]] = for {
        s <- StateT.liftF(Lowering.get)
      } yield s.blocks(node).cont.callees.toSet

      override def visit(node: BasicBlockID): BlockLocal[Option[Unit]] = pure(None)
      override def leaf(node: BasicBlockID): BlockLocal[Option[Unit]] = StateT.liftF(Lowering.modify(
        shapeless.lens[IRGenState].blocks.modify(_)(blocks =>
          // replace the continuation
          // sadly, shapelens don't work across indices
          blocks + (node -> blocks(node).copy(cont = cont))
        )
      )).map(_ => None)
    }.dfs(id).map(_ => ())

    private def lowerBlock(id: BasicBlockID, stmts: List[AST], params: List[(String, IRType)]): BlockLocal[Unit] = for {
      // we first need to make sure the progress so far is visible to other computations
      block <- get
      _ <- sync
      _ = println("lower push from " + block.id)
      _ <- StateT.liftF(Lowering.lowerBlock(id, stmts, params))
      _ = println(s"lower pop ($id)")
    } yield ()

    private def lookup(name: String): BlockLocal[IRRegister] = for {
      block <- get
      blockID = block.id
      _ = println(s"lookup $name in $blockID")
      mbReg <- (block.env.get(name) match {
        case Some(reg) => pure(Some(reg))
        case None => StateT.liftF(forwardVariable(blockID, name))
      }): BlockLocal[Option[IRRegister]]
      // very important: the global block map has been updated, we need to fetch the changes
      irGenState <- StateT.liftF(Lowering.get)
      _ <- put(State(irGenState.blocks(blockID)))
      reg <- mbReg match {
        case Some(reg) => pure(reg)
        case None =>
          val ns = collection.mutable.Map[Any, Int]()
          val nameOf = (x: Any) => "node_" + ns.getOrElseUpdate(x, ns.size)
          for {
            irGenState <- StateT.liftF(Lowering.get)
            _ = println(irGenState.blocks.map(_._2.toDot(nameOf).toString.indent(2)).mkString("\ndigraph {\n", "\n", "}"))
            reg <- crash[IRRegister](s"could not find $name in parent environments of block $blockID")
          } yield reg
      }
      _ = println(s"lookup $name in $blockID: $reg")
      _ = println(s"  original $block")
      block <- get
      _ = println(s"  updated $block")
    } yield reg

    private def debug: BlockLocal[Unit] = for {
      irGenState <- StateT.liftF(Lowering.get)
      _ = IRProgram(BasicBlockID(0), irGenState.blocks).display()
    } yield ()

    private def sync: BlockLocal[Unit] = for {
      block <- get
      _ <- put(State(block))
    } yield ()

    def lower(ast: AST): BlockLocal[IRRegister] = for {
      reg <- go(ast)
      _ <- sync
    } yield reg

    private def go(ast: AST): BlockLocal[IRRegister] = ast match {
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
      case seq: AST.Sequence => seq.body.traverse_(lower).map(_ => IRRegister.NoReg())
      case b: AST.Block => b.stmts.traverse_(lower).map(_ => IRRegister.NoReg())
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
        _ <- alternative.traverse_(alt => lowerBlock(alt._1, List(alt._2), List()))
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
      case ret: AST.Return => for {
        reg <- lower(ret.value)
        _ <- endWith[Unit](_ => pure((
          Continuation.Return(reg),
          CallingConvention.Unrestricted(),
          ()
        )))
      } yield IRRegister.NoReg()
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
        target <- as.lvalue match {
          case id: AST.Identifier => pure(id.name)
          case _ => ???
        }
        reg <- lower(as.rvalue)
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
    paramsInScope = params.zipWithIndex.map {
      case ((name, typ), index) => name -> IRRegister.Param(index, typ)
    }
    _ <- local.runS(BlockLocal.State(
      IRNode.Block(blockID, params.map(_._2), paramsInScope.toMap, Nil, Continuation.Halt())
    ))
  } yield ()
}
