package me.viluon.tinyc

import codegen.{Target, Tiny86}
import ir.IRNode.BasicBlockID
import ir.Lowering.{IRGenState, lowerTopLevel}

import tinyc.Frontend

import scala.util.Try

case class Pipeline(src: String,
                    fileName: String = "",
                    target: Target = Tiny86,
                    debugMode: Boolean = false
                   ) {
  import bindings.ASTBridge.ForeignASTBridgeOps

  def run: Try[target.Code] = for {
    ast <- Try(Option(new Frontend().parse(src)).get.wrapped)
    _ <- TypeAnalysis.analyse(ast.foreign).left.map(errs => new IllegalStateException(errs.mkString("\n"))).toTry
    ir <- lowerTopLevel(ast).run(
      IRGenState(0, 0, Map(), Map(), Map(), Map())
    ).map(_._2).left.map(new IllegalStateException(_)).toTry
    _ = println(ir)
    _ = if (debugMode) {
      ir.display()
      println("\n\n")

      val suspect = ir.blocks(BasicBlockID(2))
      println(suspect)
      println()

      val m = collection.mutable.Map[Any, Int]()
      println(suspect.toDot((x: Any) => "node_" + m.getOrElseUpdate(x, m.size)))

      println("\n\n")
    } else ()
    _ = println(ir.toDot)
    code = target.emit(ir)
  } yield code
}
