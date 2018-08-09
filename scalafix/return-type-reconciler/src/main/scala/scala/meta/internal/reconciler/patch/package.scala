package scala.meta.internal.reconciler

import scala.meta.{Input, Position}
import scala.meta.internal.{semanticdb => s}

package object patch {

  type PositionDiff = Seq[(Int, Int)]

  implicit class PositionOps(pos: Position) {
    def range: s.Range = s.Range(
      startLine = pos.startLine,
      startCharacter = pos.startColumn,
      endLine = pos.endLine,
      endCharacter = pos.endColumn
    )
    def size: Int = pos.end - pos.start
  }

  implicit class RangeOps(range: s.Range) {
    def position(input: Input): Position = Position.Range(
      input = input,
      startLine = range.startLine,
      startColumn = range.startCharacter,
      endLine = range.endLine,
      endColumn = range.endCharacter
    )
  }

}
