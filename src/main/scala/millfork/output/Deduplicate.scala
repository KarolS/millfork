package millfork.output

import millfork.{CompilationFlag, CompilationOptions, Prehashed}
import millfork.assembly.AbstractCode
import millfork.env.Environment

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/**
  * @author Karol Stasiak
  */
abstract class Deduplicate[T <: AbstractCode](env: Environment, options: CompilationOptions) {

  def apply(compiledFunctions: mutable.Map[String, CompiledFunction[T]]): Unit = {
    if (options.flag(CompilationFlag.SubroutineExtraction)) {
      while(runStage(compiledFunctions, extractCommonCode)){}
    }
    if (options.flag(CompilationFlag.FunctionDeduplication)) {
      runStage(compiledFunctions, deduplicateIdenticalFunctions)
    }
    if (options.flag(CompilationFlag.FunctionFallthrough)) {
      runStage(compiledFunctions, eliminateTailJumps)
      runStage(compiledFunctions, eliminateTailJumps)
      runStage(compiledFunctions, eliminateTailJumps)
      runStage(compiledFunctions, eliminateRemainingTrivialTailJumps)
      runStage(compiledFunctions, eliminateRemainingTrivialTailJumps)
      runStage(compiledFunctions, eliminateRemainingTrivialTailJumps)
    }
    fixDoubleRedirects(compiledFunctions)
//    println(compiledFunctions.map {
//      case (k, v) => k + " " + (v match {
//        case _: NormalCompiledFunction[_] => "NormalCompiledFunction"
//        case _ => v.toString
//      })
//    }.mkString(" ; "))
  }

  def removeChains(map: Map[String, String]): Map[String, String] = map.filterNot{case (_, to) => map.contains(to)}

  def runStage(compiledFunctions: mutable.Map[String, CompiledFunction[T]],
               function: (String, Map[String, Either[String, CodeAndAlignment[T]]]) => Seq[(String, CompiledFunction[T])]): Boolean = {
    var progress = false
    bySegment(compiledFunctions).foreach {
      case (segmentName, segContents) =>
        val segmentDelta = function(segmentName, segContents)
        progress |= segmentDelta.nonEmpty
        segmentDelta.foreach {
          case (fname, cf) => compiledFunctions(fname) = cf
        }
    }
    progress
  }

  def extractCommonCode(segmentName: String, segContents: Map[String, Either[String, CodeAndAlignment[T]]]): Seq[(String, CompiledFunction[T])] = {
    val minSnippetSize =
      if (options.flag(CompilationFlag.OptimizeForSize)) 4
      else if (options.flag(CompilationFlag.OptimizeForSonicSpeed)) 16
      else if (options.flag(CompilationFlag.OptimizeForSpeed)) 13
      else 10
    var result = ListBuffer[(String, CompiledFunction[T])]()
    val snippets: Seq[(List[T], CodeChunk[T])] = segContents.toSeq.flatMap {
      case (_, Left(_)) => Nil
      case (functionName, Right(CodeAndAlignment(code, _))) =>
        if (functionName.startsWith(".xc")) Nil
        else getExtractableSnippets(functionName, code).filter(_.codeSizeInBytes.>=(minSnippetSize)).map(code -> _)
    }
    val chunksWithThresholds: Seq[(CodeChunk[T], Int)] = snippets.flatMap { case (wholeCode, snippet) =>
      for {
        start <- snippet.code.indices
        end <- start + 1 to snippet.code.length
      } yield wholeCode -> CodeChunk(snippet.functionName, snippet.offset + start, snippet.offset + end)(snippet.code.slice(start, end))
    }.map(_._2).filter(_.codeSizeInBytes >= minSnippetSize).groupBy { chunk =>
      chunk.renumerateLabels(this, temporary = true)
    }.filter {
      case (code, _) =>
        if (isBadExtractedCodeHead(code.value.head)) false
        else if (isBadExtractedCodeLast(code.value.last)) false
        else true
    }.mapValues(_.toSeq).flatMap {
      case v@(_, instances) =>
        val chunkSize = instances.head.codeSizeInBytes
        val extractedProcedureSize = chunkSize + 1
        val savedInCallers = (chunkSize - 3) * instances.length
        val maxPossibleProfit = savedInCallers - extractedProcedureSize
        // (instances.length >=2) println(s"Instances: ${instances.length}, max profit: $maxPossibleProfit:  $instances")
        if (maxPossibleProfit > 0 && instances.length >= 2) {
          env.log.trace(s"Instances: ${instances.length}, max profit: $maxPossibleProfit:  $instances")
          instances.map(_ -> maxPossibleProfit)
        } else Nil // TODO
    }.toSeq
    if (chunksWithThresholds.isEmpty) return Nil
    var chunks = chunksWithThresholds.map(_._1)
    var threshold = 1
    while (chunks.length > 100 || chunks.length > 20 && !allChunksDisjoint(chunks)) {
      env.log.trace(s"Current threshold: $threshold current chunk count ${chunks.size}")
      threshold = chunksWithThresholds.filter(c => c._2 >= threshold).map(_._2).min + 1
      chunks = chunksWithThresholds.filter(c => c._2 >= threshold).map(_._1)
    }
    env.log.debug(s"Requiring $threshold profit trimmed the chunk candidate list from ${chunksWithThresholds.size} to ${chunks.size}")
    if (env.log.traceEnabled) {
      chunksWithThresholds.filter(c => c._2 >= threshold).foreach(c => env.log.trace(c.toString))
    }
    if (chunks.length < 20) env.log.debug(s"Chunks: ${chunks.length} $chunks") else env.log.debug(s"Chunks: ${chunks.length}")
    val powerset = if (allChunksDisjoint(chunks)) Seq(chunks) else powerSet(chunks)((set, chunk) => !set.exists(_ & chunk)).filter(_.nonEmpty).filter(set => allChunksDisjoint(set)).toSeq
    val candidates: Seq[(Int, Map[List[T], Seq[CodeChunk[T]]])] = powerset.map(_.groupBy(chunk => chunk.renumerateLabels(this, temporary = true).value).filter(_._2.size >= 2).mapValues(_.toSeq).view.force).filter(_.nonEmpty).map { map =>
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
      allChunksDisjoint(allChunks)
    }
//    candidates.sortBy(_._1).foreach {
//      case (profit, map) =>
//        if (profit > 0) {
//          env.log.trace(s"Profit: $profit ${map.map { case (_, instances) => s"${instances.length}×${instances.head}" }.mkString(" ; ")}")
//        }
//    }
    if (candidates.nonEmpty) {
      val best = candidates.maxBy(_._1)
      //env.log.debug(s"Best extraction candidate: $best")
      val allAffectedFunctions = best._2.values.flatten.map(_.functionName).toSet
      val toRemove = allAffectedFunctions.map(_ -> mutable.Set[Int]()).toMap
      val toReplace = allAffectedFunctions.map(_ -> mutable.Map[Int, String]()).toMap
      if (options.log.traceEnabled){
        val functionNameList = if (allAffectedFunctions.size < 7) allAffectedFunctions.mkString(" (", ",", ")") else ""
        options.log.debug(s"Extracted ${best._2.size} common code subroutines from ${allAffectedFunctions.size} functions$functionNameList, saving ${best._1} bytes")
      }
      for((code, instances) <- best._2) {
        val newName = env.nextLabel("xc")
        val newCode = createLabel(newName) :: tco(instances.head.renumerateLabels(this, temporary = false).value :+ createReturn)
        result += newName -> NormalCompiledFunction(segmentName, newCode, hasFixedAddress = false, alignment = NoAlignment)
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
          val value = segContents(functionName).right.get
          val newCode = value.code.zipWithIndex.flatMap{
            case (line, i) =>
               if (linesToRemove(i)) None
               else if (linesToReplace.contains(i)) Some(createCall(linesToReplace(i)))
               else Some(line)
          }
          NormalCompiledFunction(segmentName, tco(newCode), hasFixedAddress = false, alignment = value.alignment)
        }
      }

    }
    result
  }

  def deduplicateIdenticalFunctions(segmentName: String, segContents: Map[String, Either[String, CodeAndAlignment[T]]]): Seq[(String, CompiledFunction[T])] = {
    var result = ListBuffer[(String, CompiledFunction[T])]()
    val identicalFunctions = segContents.flatMap{
      case (name, code) =>
        code.toOption.map(c => name -> removePositionInfo(actualCode(name, c.code)))
    }.filter{
      case (_, code) => checkIfLabelsAreInternal(code, code)
    }.groupBy{
      case (_, code) => renumerateLabels(code, temporary = true)
    }.values.toSeq.map(_.keySet).filter(set => set.size > 1)
    for(set <- identicalFunctions) {
      val representative = if (set("main")) "main" else set.head
      options.log.debug(s"Functions [${set.mkString(",")}] are identical")
      for (function <- set) {
        if (function != representative && !options.platform.bankLayouts(segmentName).contains(function)) {
          result += function -> RedirectedFunction(segmentName, representative, 0)
        } else {
          segContents(function) match {
            case Right(CodeAndAlignment(code, alignment)) =>
              result += function -> NormalCompiledFunction(segmentName,
                set.toList.map(name => createLabel(name)) ++ actualCode(function, code),
                hasFixedAddress = false,
                alignment = alignment)
            case Left(_) =>
          }
        }
      }
    }
    result
  }

  private def follow(segContents: Map[String, Either[String, CodeAndAlignment[T]]], to: String): Option[String] = {
    var result: String = to
    val visited = mutable.Set[String]()
    do {
      segContents.get(result) match {
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

  def eliminateTailJumps(segmentName: String, segContents: Map[String, Either[String, CodeAndAlignment[T]]]): Seq[(String, CompiledFunction[T])] = {
    var result = ListBuffer[(String, CompiledFunction[T])]()
    val fallThroughList = segContents.flatMap {
      case (name, Right(CodeAndAlignment(code, alignment))) =>
        if (code.isEmpty) None
        else getJump(code.last)
          .filter(segContents.contains)
          .filter(_ != name)
          .filter(n => !options.platform.bankLayouts(segmentName).contains(n))
          .filter(_ != "main")
          .flatMap(to => follow(segContents, to))
          .map(name -> _)
      case _ => None
    }
    val fallthroughPredecessors = removeChains(fallThroughList).groupBy(_._2).mapValues(_.head._1) // TODO: be smarter than head
    fallthroughPredecessors.foreach {
      case (to, from) =>
        options.log.debug(s"Fallthrough from $from to $to")
        val value = segContents(from).right.get
        val init = value.code.init
        result += from -> NormalCompiledFunction(segmentName,
          init ++ segContents(to).right.get.code,
          hasFixedAddress = false,
          alignment = value.alignment
        )
        val initSize = init.map(_.sizeInBytes).sum
        if (initSize <= 2) {
          result += to -> RedirectedFunction(segmentName, from, initSize)
        } else {
          result += to -> NonexistentFunction()
        }
    }
    result
  }

  def eliminateRemainingTrivialTailJumps(segmentName: String, segContents: Map[String, Either[String, CodeAndAlignment[T]]]): Seq[(String, CompiledFunction[T])] = {
    var result = ListBuffer[(String, CompiledFunction[T])]()
    val fallThroughList = segContents.flatMap {
      case (name, Right(CodeAndAlignment(code, alignment))) =>
        if (code.length != 2) None
        else getJump(code.last)
          .filter(segContents.contains)
          .filter(_ != name)
          .filter(n => !options.platform.bankLayouts(segmentName).contains(n))
          .filter(_ != "main")
          .map(name -> _)
      case _ => None
    }
    val fallthroughPredecessors = removeChains(fallThroughList).groupBy(_._2).mapValues(_.keySet)
    fallthroughPredecessors.foreach {
      case (to, froms) =>
        for (from <- froms) {
          options.log.debug(s"Trivial fallthrough from $from to $to")
          result += from -> RedirectedFunction(segmentName, to, 0)
          follow(segContents, to) match {
            case Some(actualTo) =>
              options.log.trace(s"which physically is $actualTo")
              val value = result.find(_._1 == actualTo).fold(segContents(actualTo).right.get){
                case (_, NormalCompiledFunction(_, code, _, alignment)) => CodeAndAlignment(code, alignment)
              }
              result += actualTo -> NormalCompiledFunction(segmentName,
                createLabel(from) :: value.code,
                hasFixedAddress = false,
                alignment = value.alignment
              )
            case _ =>
          }
        }
    }
    result
  }

  def fixDoubleRedirects(compiledFunctions: mutable.Map[String, CompiledFunction[T]]): Unit = {
    var changed = true
    while (changed) {
      changed = false
      val functionNames = compiledFunctions.keys.toSeq
      for (name <- functionNames) {
        compiledFunctions(name) match {
          case RedirectedFunction(_, redirect, offset1) =>
            compiledFunctions.get(redirect) match {
              case Some(r: RedirectedFunction[T]) =>
                compiledFunctions(name) = r.copy(offset = r.offset + offset1)
                changed = true
              case _ =>
            }
          case _ =>
        }
      }
    }
  }

  def tco(code: List[T]): List[T]

  def isBadExtractedCodeHead(head: T): Boolean

  def isBadExtractedCodeLast(last: T): Boolean

  def getJump(line: T): Option[String]

  def createCall(functionName: String): T

  def createReturn(): T

  def createLabel(name: String): T

  def renumerateLabels(code: List[T], temporary: Boolean): Prehashed[List[T]]

  def removePositionInfo(line: T): T

  def removePositionInfo(code: List[T]): List[T] = code.map((t:T) => removePositionInfo(t))

  def checkIfLabelsAreInternal(snippet: List[T], code: List[T]): Boolean

  def bySegment(compiledFunctions: mutable.Map[String, CompiledFunction[T]]): Map[String, Map[String, Either[String, CodeAndAlignment[T]]]] = {
    compiledFunctions.flatMap {
      case (name, NormalCompiledFunction(segment, code, false, alignment)) => Some((segment, name, Right(CodeAndAlignment(code, alignment)))) // TODO
      case (name, RedirectedFunction(segment, target, 0)) => Some((segment, name, Left(target))) // TODO
      case _ => None
    }.groupBy(_._1).mapValues(_.map { case (_, name, code) => name -> code }.toMap).view.force
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
        result += CodeChunk(functionName, cursor, cursor + good.length)(removePositionInfo(good))
        cursor += good.length
      } else {
        //env.log.debug(s"Snippets in $functionName: $result")
        return result.toList
      }
    }
    null
  }

  def powerSet[A](t: Iterable[A])(f: (Set[A], A) => Boolean): Set[Set[A]] = {
    @annotation.tailrec
    def pwr(t: Iterable[A], ps: Set[Set[A]]): Set[Set[A]] =
      if (t.isEmpty) ps
      else {
        env.log.trace(s"Powerset size so far: ${ps.size} Remaining chunks: ${t.size}")
        pwr(t.tail, ps ++ (ps.filter(p => f(p, t.head)) map (_ + t.head)))
      }
    val ps = pwr(t, Set(Set.empty[A]))
    env.log.trace("Powerset size: " +  ps.size)
    ps
  }

  def allChunksDisjoint(t: Iterable[CodeChunk[T]]): Boolean = {
    for (x <- t) {
      for (y <- t) {
        if ((x & y) && x != y) return false
      }
    }
    true
  }
}

case class CodeAndAlignment[T <: AbstractCode](code: List[T], alignment: MemoryAlignment)

case class CodeChunk[T <: AbstractCode](functionName: String, offset: Int, endOffset: Int)(val code: List[T]) {

  private var renumerated: Prehashed[List[T]] = _
  private var lastTemporary: Boolean = false
  private var codeSizeMeasured = -1

  def renumerateLabels(deduplicate: Deduplicate[T], temporary: Boolean): Prehashed[List[T]] = {
    if ((renumerated eq null) || lastTemporary != temporary) {
      renumerated = deduplicate.renumerateLabels(code, temporary = temporary)
      lastTemporary = temporary
    }
    renumerated
  }

  @inline
  def codeSizeInBytes: Int = {
    if (codeSizeMeasured < 0) {
      codeSizeMeasured = code.map(_.sizeInBytes).sum
    }
    codeSizeMeasured
  }

  @inline
  def &(that: CodeChunk[T]): Boolean =
    this.functionName == that.functionName &&
      this.offset <= that.endOffset &&
      that.offset <= this.endOffset

  override def toString: String = s"$functionName:$offset:${code.map(_.toString.trim).mkString(";")}($codeSizeInBytes bytes)"
}
