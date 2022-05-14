package me.viluon.tinyc

import cats.implicits._
import com.monovore.decline._

import java.io.FileWriter
import java.nio.file.Path
import scala.io.Source
import scala.util.Using

object Main extends CommandApp(
  name = "tinycc",
  header = "viluon's TinyC compiler",
  main = {
    (
      Opts.argument[Path](metavar = "input"),
      Opts.argument[Path](metavar = "output")
    ).mapN { (input, output) =>
      (for {
        src <- Using(Source.fromFile(input.toFile))(_.mkString)
        code <- Pipeline(src, input.getFileName.toString).run
        _ <- Using(new FileWriter(output.toFile))(_ write code.toString)
      } yield ()).get
    }
  }
)
