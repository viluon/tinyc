package me.viluon.tinyc

import codegen.{Target, Tiny86}
import ir.Lowering.{ConvState, lowerTopLevel}

import org.intellij.lang.annotations.Language
import tinyc.Frontend

import scala.util.Try

case class Pipeline(@Language(value = "JAVA", prefix = "class Foo { ", suffix = " }") src: String,
                    fileName: String = "",
                    target: Target = Tiny86
                   ) {
  import bindings.ASTBridge.ForeignASTBridgeOps

  def run: Try[target.Code] = for {
    ast <- Try(Option(new Frontend().parse(src)).get.wrapped)
    _ <- TypeAnalysis.analyse(ast.foreign).left.map(errs => new IllegalStateException(errs.mkString("\n"))).toTry
    ir <- lowerTopLevel(ast).run(ConvState(0, Map())).map(_._2).left.map(new IllegalStateException(_)).toTry
    code = target.emit(ir)
  } yield code
}
