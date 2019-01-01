package com.todesking.ojaml.ml0.compiler.scala

import java.nio.file.Path
import java.nio.file.Files

case class FileContent(path: Path, content: String)

object FileContent {
  def read(path: Path) = FileContent(path, new String(Files.readAllBytes(path)))
}
