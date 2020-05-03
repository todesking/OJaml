package com.todesking.ojaml.ml0.compiler.scala

import java.nio.file.Path
import java.nio.file.Files
import java.io.InputStream

case class Source(path: String, content: String)

object Source {
  def readFile(path: Path) = Source(path.toString, new String(Files.readAllBytes(path)))
  def readStream(path: String, in: InputStream) = {
    val sb = new StringBuilder()
    val scanner = new java.util.Scanner(in)
    while (scanner.hasNext()) {
      sb.append(scanner.nextLine())
      sb.append('\n')
    }
    Source(path, sb.toString())
  }
  def readResource(cl: ClassLoader, path: String) = {
    val in = cl.getResourceAsStream(path)
    if (in == null) throw new IllegalArgumentException(s"Resource not found: $path")
    try { readStream(path, in) } finally { in.close() }
  }
}
