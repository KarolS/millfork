package millfork.output

import millfork.{CompilationFlag, CompilationOptions}
import millfork.assembly.AbstractCode
import millfork.env.Environment

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/**
  * @author Karol Stasiak
  */
abstract class Deduplicate[T <: AbstractCode](env: Environment, options: CompilationOptions) {

  def apply(compiledFunctions: mutable.Map[String, CompiledFunction[T]]): Unit = {
    if (options.flag(CompilationFlag.OptimizeForSize)) {
      runStage(compiledFunctions, extractCommonCode)
    }
    runStage(compiledFunctions, deduplicateIdenticalFunctions)
    runStage(compiledFunctions, eliminateTailJumps)
  }

  def runStage(compiledFunctions: mutable.Map[String, CompiledFunction[T]],
               function: (String, Map[String, Either[String, List[T]]]) => Seq[(String, CompiledFunction[T])]): Unit = {
    bySegment(compiledFunctions).foreach {
      case (segmentName, segContents) =>
        function(segmentName, segContents).foreach {
          case (fname, cf) => compiledFunctions(fname) = cf
        }
    }
  }

  def extractCommonCode(segmentName: String, segContents: Map[String, Either[String, List[T]]]): Seq[(String, CompiledFunction[T])] = {
    var result = ListBuffer[(String, CompiledFunction[T])]()
    val chunks = segContents.flatMap{
      case (_, Left(_)) => Nil
      case (functionName, Right(code)) =>
        if (options.flag(CompilationFlag.OptimizeForSize)) {
          getExtractableSnippets(functionName, code)
        } else Nil
    }.flatMap { chunk =>
        for {
          start <- chunk.code.indices
          end <- start + 1 to chunk.code.length
        } yield CodeChunk(chunk.functionName, chunk.offset + start, chunk.offset + end)(chunk.code.slice(start, end))
    }.filter(_.codeSizeInBytes > 3).groupBy(_.code).filter{
      case (code, _) =>
        if (isBadExtractedCodeHead(code.head)) false
        else if (isBadExtractedCodeLast(code.last)) false
        else true
    }.mapValues(_.toSeq).filter {
      case (_, instances) =>
        val chunkSize = instances.head.codeSizeInBytes
        val extractedProcedureSize = chunkSize + 1
        val savedInCallers = (chunkSize - 3) * instances.length
        val maxPossibleProfit = savedInCallers - extractedProcedureSize
        // (instances.length >=2) println(s"Instances: ${instances.length}, max profit: $maxPossibleProfit:  $instances")
        maxPossibleProfit > 0 && instances.length >= 2 // TODO
    }.flatMap(_._2).toSeq
    //println(s"Chunks: ${chunks.length} $chunks")
    val candidates: Seq[(Int, Map[List[T], Seq[CodeChunk[T]]])] = powerSet(chunks)((set, chunk) => !set.exists(_ & chunk)).filter(_.nonEmpty).filter(set => (for {
      x <- set
      y <- set
      if x != y
    } yield x & y).forall(_ == false)).toSeq.map(_.groupBy(_.code).filter(_._2.size >= 2).mapValues(_.toSeq)).filter(_.nonEmpty).map { map =>
      map.foldLeft(0) {
        (sum, entry) =>
          val chunkSize = entry._2.head.codeSizeInBytes
          val chunkCount = entry._2.size
          val extractedProcedureSize = chunkSize + 1
          val savedInCallers = (chunkSize - 3) * chunkCount
          sum + savedInCallers - extractedProcedureSize
      } -> map
    }.filter { set =>
      val allChunks = set._2.values.flatten
      (for {
        x <- allChunks
        y <- allChunks
        if x != y
      } yield x & y).forall(_ == false)
    }
//    candidates.sortBy(_._1).foreach {
//      case (profit, map) =>
//        if (profit > 0) {
//          println(s"Profit: $profit ${map.map { case (_, instances) => s"${instances.length}×${instances.head}" }.mkString(" ; ")}")
//        }
//    }
    if (candidates.nonEmpty) {
      val best = candidates.maxBy(_._1)
      //println(s"Best extraction candidate: $best")
      val allAffectedFunctions = best._2.values.flatten.map(_.functionName).toSet
      val toRemove = allAffectedFunctions.map(_ -> mutable.Set[Int]()).toMap
      val toReplace = allAffectedFunctions.map(_ -> mutable.Map[Int, String]()).toMap
      if (options.log.traceEnabled){
        options.log.debug(s"Extracted ${best._2.size} common code subroutines from ${allAffectedFunctions.size} functions, saving $best._1 bytes")
      }
      for((code, instances) <- best._2) {
        val newName = env.nextLabel("xc")
        result += newName -> NormalCompiledFunction(segmentName, createLabel(newName) :: tco(code :+ createReturn), hasFixedAddress = false)
        for(instance <- instances) {
          toReplace(instance.functionName)(instance.offset) = newName
          for (i <- instance.offset + 1 until instance.endOffset) {
            toRemove(instance.functionName) += i
          }
        }
      }
      for(functionName <- allAffectedFunctions) {
        result += functionName -> {
          val linesToRemove = toRemove(functionName)
          val linesToReplace = toReplace(functionName)
          val newCode = segContents(functionName).right.get.zipWithIndex.flatMap{
            case (line, i) =>
               if (linesToRemove(i)) None
               else if (linesToReplace.contains(i)) Some(createCall(linesToReplace(i)))
               else Some(line)
          }
          NormalCompiledFunction(segmentName, tco(newCode), hasFixedAddress = false)
        }
      }

    }
    result.toSeq
  }

  def deduplicateIdenticalFunctions(segmentName: String, segContents: Map[String, Either[String, List[T]]]): Seq[(String, CompiledFunction[T])] = {
    var result = ListBuffer[(String, CompiledFunction[T])]()
    val identicalFunctions = segContents.flatMap{
      case (name, code) => code.toOption.map(c => name -> actualCode(name, c))
    }.groupBy(_._2).values.toSeq.map(_.keySet).filter(set => set.size > 1)
    for(set <- identicalFunctions) {
      val representative = if (set("main")) "main" else set.head
      options.log.debug(s"Functions [${set.mkString(",")}] are identical")
      for (function <- set) {
        if (function != representative) {
          result += function -> RedirectedFunction(segmentName, representative, 0)
        } else {
          segContents(function) match {
            case Right(code) =>
              result += function -> NormalCompiledFunction(segmentName,
                set.toList.map(name => createLabel(name)) ++ actualCode(function, code),
                hasFixedAddress = false)
            case Left(_) =>
          }
        }
      }
    }
    result.toSeq
  }

  private def follow(segContents: Map[String, Either[String, List[T]]], to: String): Option[String] = {
    var result: String = to
    val visited = mutable.Set[String]()
    do {
      segContents.get(to) match {
        case Some(Left(next)) =>
          if (visited(next)) return None
          visited += result
          result = next
        case Some(Right(_)) =>
          return Some(result)
        case _ => return None
      }
    } while(true)
    None
  }

  def eliminateTailJumps(segmentName: String, segContents: Map[String, Either[String, List[T]]]): Seq[(String, CompiledFunction[T])] = {
    var result = ListBuffer[(String, CompiledFunction[T])]()
    val fallThroughList = segContents.flatMap {
      case (name, Right(code)) =>
        if (code.isEmpty) None
        else getJump(code.last)
          .filter(segContents.contains)
          .filter(_ != name)
          .filter(_ != "main")
          .flatMap(to => follow(segContents, to))
          .map(name -> _)
      case _ => None
    }
    val fallthroughPredecessors = fallThroughList.groupBy(_._2).mapValues(_.head._1) // TODO: be smarter than head
    fallthroughPredecessors.foreach {
      case (to, from) =>
        options.log.debug(s"Fallthrough from $from to $to")
        val init = segContents(from).right.get.init
        result += from -> NormalCompiledFunction(segmentName,
          init ++ segContents(to).right.get,
          hasFixedAddress = false
        )
        val initSize = init.map(_.sizeInBytes).sum
        if (initSize <= 2) {
          result += to -> RedirectedFunction(segmentName, from, initSize)
        } else {
          result += to -> NonexistentFunction()
        }
    }
    result.toSeq
  }

  def tco(code: List[T]): List[T]

  def isBadExtractedCodeHead(head: T): Boolean

  def isBadExtractedCodeLast(head: T): Boolean

  def getJump(line: T): Option[String]

  def createCall(functionName: String): T

  def createReturn(): T

  def createLabel(name: String): T

  def bySegment(compiledFunctions: mutable.Map[String, CompiledFunction[T]]): Map[String, Map[String, Either[String, List[T]]]] = {
    compiledFunctions.flatMap {
      case (name, NormalCompiledFunction(segment, code, false)) => Some((segment, name, Right(code))) // TODO
      case (name, RedirectedFunction(segment, target, 0)) => Some((segment, name, Left(target))) // TODO
      case _ => None
    }.groupBy(_._1).mapValues(_.map { case (_, name, code) => name -> code }.toMap)
  }

  def actualCode(functionName: String, functionCode: List[T]): List[T]

  def isExtractable(line: T): Boolean

  def getExtractableSnippets(functionName: String, code: List[T]): List[CodeChunk[T]] = {
    var cursor = 0
    var mutCode = code
    val result = mutable.ListBuffer[CodeChunk[T]]()
    while (true) {
      val (bad, rest1) = mutCode.span(l => !isExtractable(l))
      mutCode = rest1
      cursor += bad.length
      val (good, rest2) = mutCode.span(l => isExtractable(l))
      mutCode = rest2
      if (good.nonEmpty) {
        result += CodeChunk(functionName, cursor, cursor + good.length)(good)
        cursor += good.length
      } else {
        //println(s"Snippets in $functionName: $result")
        return result.toList
      }
    }
    null
  }

  def powerSet[A](t: Iterable[A])(f: (Set[A], A) => Boolean): Set[Set[A]] = {
    @annotation.tailrec
    def pwr(t: Iterable[A], ps: Set[Set[A]]): Set[Set[A]] =
      if (t.isEmpty) ps
      else pwr(t.tail, ps ++ (ps.filter(p => f(p, t.head)) map (_ + t.head)))
    pwr(t, Set(Set.empty[A]))
  }
}

case class CodeChunk[T <: AbstractCode](functionName: String, offset: Int, endOffset: Int)(val code: List[T]) {
  val codeSizeInBytes: Int = code.map(_.sizeInBytes).sum

  def &(that: CodeChunk[T]): Boolean =
    this.functionName == that.functionName &&
      this.offset <= that.endOffset &&
      that.offset <= this.endOffset

  override def toString: String = s"$functionName:$offset:${code.map(_.toString.trim).mkString(";")}($codeSizeInBytes bytes)"
}
