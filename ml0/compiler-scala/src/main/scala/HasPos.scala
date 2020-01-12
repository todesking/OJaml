package com.todesking.ojaml.ml0.compiler.scala

trait HasPos {
  private[this] var _pos: Pos = null
  def fillPos(location: String, line: Int, col: Int): Unit =
    fillPos(Pos(location, line, col))
  def fillPos(pos: Pos): Unit =
    if (_pos == null) _pos = pos
  def pos: Pos = _pos
}
