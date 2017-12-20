package millfork.assembly

import millfork.env.Label

sealed trait Chunk {
  def linearize: List[AssemblyLine]
  def sizeInBytes: Int
}

case object EmptyChunk extends Chunk {
  override def linearize: Nil.type = Nil

  override def sizeInBytes = 0
}

case class LabelledChunk(label: String, chunk: Chunk) extends Chunk {
  override def linearize: List[AssemblyLine] = AssemblyLine.label(Label(label)).copy(elidable=false) :: chunk.linearize

  override def sizeInBytes: Int = chunk.sizeInBytes
}

case class SequenceChunk(chunks: List[Chunk]) extends Chunk {
  override def linearize: List[AssemblyLine] = chunks.flatMap(_.linearize)

  override def sizeInBytes: Int = chunks.map(_.sizeInBytes).sum
}

case class LinearChunk(lines: List[AssemblyLine]) extends Chunk {
  def linearize: List[AssemblyLine] = lines

  override def sizeInBytes: Int = lines.map(_.sizeInBytes).sum

}