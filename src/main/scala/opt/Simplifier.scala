package me.viluon.tinyc
package opt

import analysis.ConstantAnalysis
import ir.{Continuation, IRNode, IRProgram, IRRegister}
import analysis.Lattice.FlatLat
import ir.IRNode.BasicBlockID

import cats.Id

import scala.util.Try
import cats.syntax.all._

object Simplifier {
  def optimise(ir: IRProgram[IRRegister]): Try[IRProgram[IRRegister]] = {
    val constants = new ConstantAnalysis(ir).fixpoint().map {
      case (id, state) => id -> state.map {
        case ((id, (expr, i)), env) => i -> env.map {
          case (reg, v) => reg -> v
        }
      }
    }

    println("Constant analysis:")
    println(constants)
    // FIXME disgusting mutation
    var idCtr = ir.blocks.keys.map(_.n).max + 1
    def freshID = {
      val r = BasicBlockID(idCtr)
      idCtr += 1
      r
    }

    // create new, optimised blocks through constant propagation
    val optimisations = ir.blocks.flatMap {
      case (originalID, block) if block.body.nonEmpty => (block.cont match {
        case Continuation.Branch(condition, consequent, alternative) =>
          val env = constants(originalID)(block.body.size - 1)
          env(condition) match {
            case FlatLat.Mid(x) =>
              val id = freshID
              Map(id -> block.copy(
                id = id,
                cont = Continuation.Unconditional(if (x == 0) alternative else consequent),
              ))
            case _ => Map()
          }
        case _ => Map()
      }).map(originalID -> _)
      case _ => Map() // TODO it'd be nice to have a simplifier monad with withFilter
                      //  that could short-circuit automatically
    }

    println("possible optimisations:")
    println(optimisations.mkString("\n"))

    val repl = ir.blocks.map {
      case (originalID, _) => originalID -> (optimisations.get(originalID) match {
        case Some((newID, _)) => newID
        case None => originalID
      })
    }

    println("replacements:")
    println(repl.map(_.toString()).toList.sorted.mkString("\n"))

    // replace references to existing blocks with the optimised ones
    // note that unreferenced blocks are not collected
    val newEntry = repl(ir.entry)
    val optimisedIR = IRProgram(newEntry, optimisations.values.toMap ++ ir.blocks.map {
      case (id, block) => id -> block.copy(
        cont = block.cont.mapTargets {
          case Continuation.Target(callee, args) =>
            println(s"redirecting continuation of $id from $callee to ${repl(callee)}")
            Id(Continuation.Target(repl(callee), args))
        },
        body = block.body.map {
          case IRNode.Call(target, callee, args) =>
            println(s"redirecting call in $id from $callee to ${repl(callee)}")
            IRNode.Call(target, repl(callee), args)
          case expr => expr
        },
      )
    })

    Try(optimisedIR)
  }
}
