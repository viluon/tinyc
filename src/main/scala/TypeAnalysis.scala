package me.viluon.tinyc

import bindings.ASTTypeResolver.ASTTypeResolverExtensions

import cats.syntax.option._
import cats.data.{State, EitherT}
import bindings.AST

object TypeAnalysis {
  case class TypeAnalysisState(declarations: Map[tinyc.Symbol, Type])
  case class TypeError(msg: String, location: tinyc.SourceLocation)

  type Analysis[A] = EitherT[State[TypeAnalysisState, *], List[TypeError], A]

  private def assign(sym: tinyc.Symbol, t: Type): Analysis[Unit] = EitherT.liftF(State.modify(
    s => s.copy(declarations = s.declarations + (sym -> t))
  ))

  private def query(sym: tinyc.Symbol): Analysis[Option[Type]] = EitherT.liftF(State.inspect(
    s => s.declarations.get(sym)
  ))

  private def get: Analysis[TypeAnalysisState] = EitherT.liftF(State.get)

  @inline private def pure[A](a: A): Analysis[A] = EitherT.liftF(State.pure(a))
  @inline private def crash[A](msg: String, loc: tinyc.SourceLocation): Analysis[A] =
    EitherT.leftT(List(TypeError(msg, loc)))

  def analyse(ast: tinyc.AST): Either[List[TypeError], Map[tinyc.Symbol, Type]] =
    analyse(ast.wrapped).flatMap(_ => get).value.run(TypeAnalysisState(Map())).value._2.map(_.declarations)

  // "not being of a type" (spelled None) actually means having the unit type, misnamed void in tinyC
  def analyse(ast: AST): Analysis[Option[Type]] = ast match {
    case _: AST.Integer => pure(Type.Int.some)
    case _: AST.Double => pure(Type.Double.some)
    case _: AST.Char => pure(Type.Char.some)
    case _: AST.String => pure(Type.String.some)
    case id: AST.Identifier => for {
      mt <- query(id.symbol)
      t <- mt match {
        case None => crash(s"undefined $id", id.loc)
        case someT => pure(someT)
      }
    } yield t
    case _: AST.Type => ???
    case _: AST.PointerType => ???
    case _: AST.ArrayType => ???
    case _: AST.NamedType => ???
    case _: AST.Sequence => ???
    case block: AST.Block => ???
    case _: AST.VarDecl => ???
    case _: AST.FunDecl => ???
    case _: AST.StructDecl => ???
    case _: AST.FunPtrDecl => ???
    case _: AST.If => ???
    case _: AST.Switch => ???
    case _: AST.While => ???
    case _: AST.DoWhile => ???
    case _: AST.For => ???
    case _: AST.Break => ???
    case _: AST.Continue => ???
    case _: AST.Return => ???
    case _: AST.BinaryOp => ???
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
