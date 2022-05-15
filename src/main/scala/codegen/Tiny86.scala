package me.viluon.tinyc
package codegen

import ir.IRNode.{BinOp, Block, IRExpression, KInt}
import ir.{BinaryOperator, IRNode, IRProgram, IRRegister}

import cats.data.{StateT, Writer}
import cats.syntax.traverse.toTraverseOps

object Tiny86 extends Target {
  import tiny86._
  import tiny86.{Register => MachineReg}
  override type Code = Program

  // generator monad needs:
  // - fresh register store
  // - code vector log
  //
  // we can defer the choice of instruction arguments using lambdas.
  //
  // ideally, we'd detect shared nodes in the IR DAG,
  // allocate registers for them, and share these
  // registers.

  // I'm kinda split on IR registers vs just a DAG structure
  // not to mention I don't know how to generate either (see Lowering)

  // this codegen backend already assumes sequences of instructions
  // arranged in basic blocks

  // you can play here but please don't be mean to my code (I don't have a backup)

  //> ideally, we'd detect shared nodes in the IR DAG,
  //> allocate registers for them, and share these
  //> registers.
  // y
  // I don't think there's actually gonna be much duplication of blocks but idk
  //
  // I'm interested both in "get rich quick" codegen schemes and long-term plans with better infrastructure.
  // The trouble is that I need to do register allocation and instruction selection, these two are kinda intertwined.
  // Example: you run out of physical registers, so you need to spill some data into memory (or rather onto the stack).
  //          This changes the code that you're allocating registers and selecting instructions for. Sometimes you can
  //          spill more registers earlier to ease register allocation in a loop, etc.
  //
  // I feel like that's a last step
  // I'd want to keep everything high level assuming infinite registers, and then before doing codegen convert to actual
  // registers and select instruction overloads based on what's using registers/RAM
  // and you'd want to be careful picking which virtual registers are demoted to RAM based on usage
  //
  // that's kinda where we are here, this object takes the SSA IR and should produce something that uses machine registers
  //
  // I'd do something that can model the number of times an instruction runs, and the impl can be super dumb early on
  // then weight instructions based on accesses
  // I guess you don't wanna demote all but the top X registers since they're not necessarily gonna overlap

  // yeah, I'd like to keep frequency information around in the IR, so that it's available at codegen time.
  // I don't wanna keep it around in machine code I don't think.
  // Anyway, another issue: instruction selection should consider runs of instructions (or subDAGs of the IR, really).
  //                                                     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ how do I do this?
  //                                                                                   for (int i = 0; i < instructions.size; ++i) { if (instruction[i] == yeet && instructions[i + 1] == leet && instructions[i + 2].children[0] == deet) emitYeetLeetDeet() }
  // doing the "subDAGs of the IR" thing is the tree tiling technique I mentioned
  // let me pull up the slides

  // I think you're over-engineering
  // make a mini codegen
  // learn what you wanna improve on
  // iterate

  // good point
  // ... but still :p

  // I can't talk, setting up a full deferred renderer with HDR support and bloom before I've drawn a single triangle

  // btw, the guy teaching the class worked on PlayStation 3 and PS Vita compilers
  // hardcore C++ guy (but quite young) that I frequently get into arguments with
  // thanks for the brainstorming btw, this is fun
  // (I'm looking at the slides, wasn't paying attention)

  // neat|
  // ew, and lol|
  // how old?

  // ~38ish? he has small kids
  // "young" for a C++ guy? yes

  // also the caret behaviour goes weird when you type as well

  // well, it's not Selene...

  // if I go up and down between ^, it sticks the caret before "and" then jumps to the end
  // except if you're typing, sometimes it forgets to go to the end

  // I guess JetBrains haven't heard of CRDTs, huh

  // nerd

  // surely (instruction selection)'s a *last* step, or are there complex instructions?
  //
  // it's not the last step, clever instruction selection can free up registers
  // register allocation and instruction selection should run iteratively one after the other
  //                                               ^^^^^^^^^^^^^^^^^^^^^^ but how do you do that
  //                                                                      if you start instr. selection in the IR?

  // make an MVP first! you'll learn loads
  // yaknow, the (jankiest?) little bit of code that takes IR and poops out insrtuctions

  // yeah yeah, working on it. In fact it's almost done (fresh is still ??? here, needs refactor from Writer to State)

  // nice!
  // once you've done it, work out specifically what you want to add and decide why you can't do it with the MVP

  // ok
  // stay around, will ya?
  // btw, do you see type errors in Lowering?

  // yeah, nodes

  // until reaching a fixed point (or running out of fuel)
  // so I'm thinking straightforward instruction selection now, then I'll see what can be done
  // (maybe a later peephole opt. pass)

  // nice!
  // fyi I'm doing codey bits but I'll pop back every now and then

  // cool beans

  // IR can include "CISC" instructions that match the ISA

  // never! CISC is evil, the IR should be fairly high-level with few syntactic forms
  // to make pattern matching and rewriting as easy as possible

  // fine fine...

  private type GenLog = List[Instruction]
  private case class GenState(regCtr: Int = 0, allocation: Map[IRRegister, MachineReg] = Map())
  private type Generator[A] = StateT[Writer[GenLog, *], GenState, A]

  override def emit(program: IRProgram[IRRegister]): Code = gen(program)

  private def pure[A](x: A): Generator[A] = cats.Monad[Generator].pure(x)
  private def add(instr: Instruction): Generator[Unit] = StateT.liftF(Writer.tell(List(instr)))
  private def fresh: Generator[MachineReg] = ???
  private def lookup(register: IRRegister): Generator[MachineReg] = ???

  private def gen(program: IRProgram[IRRegister]): Code = {
    val (code, _) = program.fns.traverse(fn => for {
      _ <- pure(())
      IRNode.Block(signature, body, cont, callingConvention) = fn
      _ <- body.traverse(translate)
    } yield ()).run(GenState()).run

    val builder = new ProgramBuilder()
    (code :+ new HALT()).foreach(builder.add)
    builder.program()
  }

  private def translate(node: IRExpression[IRRegister]): Generator[MachineReg] = node match {
    case KInt(k) => for {
      reg <- fresh
      _ <- add(new MOV(reg, k.toLong))
    } yield reg
    case BinOp(op, left, right) =>
      import BinaryOperator._
      for {
        l <- lookup(left)
        reg <- fresh
        _ <- add(new MOV(reg, l))
        r <- lookup(right)
        _ <- add(op match {
          case Add() => new ADD(reg, r)
          case Sub() => new SUB(reg, r)
          case Mul() => new IMUL(reg, r) // FIXME needs to follow type info
          case Div() => new IDIV(reg, r) //  ditto
        })
      } yield reg
    case IRNode.Load(target) => ???
  }
}
