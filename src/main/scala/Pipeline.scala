package me.viluon.tinyc

import codegen.{Target, Tiny86}
import ir.Lowering.{IRGenState, lowerTopLevel}

import tinyc.Frontend

import scala.util.Try

case class Pipeline(src: String,
                    fileName: String = "",
                    target: Target = Tiny86
                   ) {
  import bindings.ASTBridge.ForeignASTBridgeOps

  def run: Try[target.Code] = for {
    ast <- Try(Option(new Frontend().parse(src)).get.wrapped)
    _ <- TypeAnalysis.analyse(ast.foreign).left.map(errs => new IllegalStateException(errs.mkString("\n"))).toTry
    ir <- lowerTopLevel(ast).run(IRGenState(0, 0, Map(), Map(), Map(), Map())).map(_._2).left.map(new IllegalStateException(_)).toTry
    _ = println(ir)
    _ = println(ir.toDot)
    code = target.emit(ir)
  } yield code
}
