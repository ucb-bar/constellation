package constellation.routing

import constellation.topology._
import scala.collection.mutable
import scala.collection.mutable.{HashMap, HashSet, Queue}
import scala.collection.mutable.{Set => MSet, Map => MMap}
import org.chipsalliance.cde.config.Parameters
import scala.util.control.Breaks._


/**
 * CustomGraph: A utility class for layered routing over an arbitrary directed topology
 * 
 * This class helps perform some computations to generate a deadlock-free routing configuration:
 *   - generateSSPs: Computes all-pairs single-source shortest paths using BFS.
 *   - deduplicateSSPs: Removes duplicate paths with the same edge sequences.
 *   - pruneSubpaths: Eliminates paths that are strict subsets of other paths.
 *   - packLayersMinimal: Packs flows into the minimum number of VC layers ensuring each layer is a DAG.
 * 
 * The final output is a list of layers (each a list of flows and their edge paths) which are suitable for safe routing.
 * These layers are then enforced through VC constraints during path traversal.
 */
class CustomGraph(val nNodes: Int, val edges: Seq[(Int, Int)]) {

  def generateSSPs(): List[((Int, Int), List[(Int, Int)])] = {
    val adj = Array.fill(nNodes)(mutable.ListBuffer[Int]())
    edges.foreach { case (u, v) => adj(u) += v }

    val result = mutable.ListBuffer[((Int, Int), List[(Int, Int)])]()

    for (src <- 0 until nNodes; dst <- 0 until nNodes if src != dst) {
      val visited = Array.fill(nNodes)(false)
      val pred = Array.fill(nNodes)(-1)
      val q = mutable.Queue[Int](src)
      visited(src) = true

      var found = false
      while (q.nonEmpty && !found) {
        val u = q.dequeue()
        for (v <- adj(u) if !visited(v)) {
          visited(v) = true
          pred(v) = u
          if (v == dst) found = true else q.enqueue(v)
        }
      }

      if (found) {
        val path = mutable.ListBuffer[Int](dst)
        var cur = dst
        while (pred(cur) != -1) {
          cur = pred(cur)
          path.prepend(cur)
        }

        val edgePath: List[(Int, Int)] = path.sliding(2).toList.map { pair =>
          pair.toSeq match {
            case Seq(a, b) => (a, b)
            case other => throw new RuntimeException(s"Custom Graph Error: $other")
          }
        }

        result.append(((src, dst), edgePath))
      }
    }

    result.toList
  }

  def deduplicateSSPs(ssps: List[((Int, Int), List[(Int, Int)])]): List[((Int, Int), List[(Int, Int)])] = {
    val seen = mutable.HashSet[Seq[(Int, Int)]]()
    val result = mutable.ListBuffer[((Int, Int), List[(Int, Int)])]()

    for ((label, path) <- ssps) {
      val edgeSeq = path.toIndexedSeq  // Ordered sequence
      if (!seen.contains(edgeSeq)) {
        seen += edgeSeq
        result.append((label, path))
      }
    }

    result.toList
  }

  def pruneSubpaths(ssps: List[((Int, Int), List[(Int, Int)])]): List[((Int, Int), List[(Int, Int)])] = {
    val edgeSets = ssps.map { case (_, path) => path.toSet }

    val pruned = ssps.zipWithIndex.filterNot { case ((labelI, pathI), i) =>
      val setI = pathI.toSet
      val isSubpath = edgeSets.zipWithIndex.exists { case (setJ, j) =>
        i != j && setI.subsetOf(setJ) && setI != setJ // strict subset only
      }
      isSubpath
    }.map(_._1)

    pruned
  }

  def packLayersMinimal(ssps: List[((Int, Int), List[(Int, Int)])]): List[List[((Int, Int), List[(Int, Int)])]] = {
    for (k <- 1 to ssps.length) {
      val result = solve(ssps, k)
      if (result.nonEmpty) return result
    }
    List()
  }

  def solve(ssps: List[((Int, Int), List[(Int, Int)])], k: Int): List[List[((Int, Int), List[(Int, Int)])]] = {
    val layers = Array.fill(k)(mutable.ListBuffer[((Int, Int), List[(Int, Int)])]())
    val dags = Array.fill(k)(mutable.Map[(Int, Int), Int]())
    val dagGraphs = Array.fill(k)(mutable.Map[Int, mutable.Set[Int]]())

    def isDAG(g: Map[Int, Set[Int]]): Boolean = {
      // 0 = unvisited, 1 = visiting, 2 = visited
      val visited = mutable.Map[Int, Int]()

      def dfs(u: Int): Boolean = {
        visited.get(u) match {
          case Some(1) => return false // cycle
          case Some(2) => return true
          case _ => // proceed
        }

        visited(u) = 1
        for (v <- g.getOrElse(u, Set())) {
          if (!dfs(v)) return false
        }
        visited(u) = 2
        true
      }

      g.keys.forall(dfs)
    }

    def addEdges(layer: Int, path: List[(Int, Int)]): Boolean = {
      val g = dagGraphs(layer)
      val edgeCount = dags(layer)

      val newlyAdded = mutable.ListBuffer[(Int, Int)]()
      for ((u, v) <- path) {
        if (!g.contains(u)) g(u) = mutable.Set()
        val added = g(u).add(v)
        if (added) newlyAdded += ((u, v))
        edgeCount((u, v)) = edgeCount.getOrElse((u, v), 0) + 1
      }

      // Check DAG
      val snapshot = g.map { case (k, v) => (k, v.toSet) }.toMap
      if (!isDAG(snapshot)) {
        for ((u, v) <- newlyAdded) g(u).remove(v)
        for ((u, v) <- path) {
          edgeCount((u, v)) -= 1
          if (edgeCount((u, v)) == 0) edgeCount.remove((u, v))
        }
        return false
      }

      true
    }

    def removeEdges(layer: Int, path: List[(Int, Int)]): Unit = {
      val g = dagGraphs(layer)
      val edgeCount = dags(layer)

      for ((u, v) <- path) {
        edgeCount((u, v)) -= 1
        if (edgeCount((u, v)) == 0) {
          edgeCount.remove((u, v))
          g(u).remove(v)
          if (g(u).isEmpty) g.remove(u)
        }
      }
    }

    def backtrack(i: Int): Boolean = {
      if (i == ssps.length) return true
      val (label, path) = ssps(i)

      for (j <- 0 until k) {
        if (addEdges(j, path)) {
          layers(j) += ((label, path))
          if (backtrack(i + 1)) return true
          layers(j).remove(layers(j).length - 1)
          removeEdges(j, path)
        }
      }
      false
    }

    if (backtrack(0)) layers.map(_.toList).toList else List()
  }

}

/**
 * DatelineAnalyzer identifies cycles in the channel dependency graph (CDG)
 * for a given physical topology and selects a minimal set of "dateline" edges
 * to break these cycles, enabling deadlock-free routing.
 *
 * This is done by:
 * 1. Computing all shortest paths between node pairs.
 * 2. Building a CDG from edges used in shortest paths.
 * 3. Detecting all simple cycles in the CDG.
 * 4. Selecting one edge per cycle to act as a dateline (breaking the cycle).
 * 5. Estimating the number of required virtual channels (VCs), based on the
 *    maximum number of dateline crossings on any shortest path.
 *
 * It returns:
 *  - datelineEdges: Set of selected (u, v) edges to act as datelines.
 *  - vcCount: Minimum number of VCs needed to support these crossings.
 *  - nextHop: Map from (src, dst) to possible next-hop nodes.
 */

object DatelineAnalyzer {

  case class DatelineResult(datelineEdges: Set[(Int, Int)], vcCount: Int, nextHop: Map[(Int, Int), Set[Int]])

  def analyze(topo: PhysicalTopology): DatelineResult = {
    val nodes = 0 until topo.nNodes

    def bfsPaths(src: Int, dst: Int): Seq[Seq[Int]] = {
      val queue = mutable.Queue[Seq[Int]](Seq(src))
      val paths = mutable.Buffer[Seq[Int]]()
      var minLen = Int.MaxValue

      while (queue.nonEmpty) {
        val path = queue.dequeue()
        val last = path.last

        if (path.length > minLen) {
          // skip
        } else if (last == dst) {
          if (path.length < minLen) {
            paths.clear()
            minLen = path.length
          }
          paths += path
        } else {
          for (nxt <- nodes if topo.topo(last, nxt) && !path.contains(nxt)) {
            queue.enqueue(path :+ nxt)
          }
        }
      }
      paths.toSeq
    }

    val shortestPaths = MMap[(Int, Int), Seq[Seq[(Int, Int)]]]()
    val nextHopMap = MMap[(Int, Int), MSet[Int]]().withDefaultValue(MSet())

    for (src <- nodes; dst <- nodes if src != dst) {
      val paths = bfsPaths(src, dst)
      val edgePaths = paths.map(_.sliding(2).map(p => (p(0), p(1))).toSeq)
      shortestPaths((src, dst)) = edgePaths
      if (paths.nonEmpty) {
        nextHopMap((src, dst)) ++= paths.map(_(1)).toSet
      }
    }

    val cdgEdges = mutable.Set[((Int, Int), (Int, Int))]()
    for ((_, paths) <- shortestPaths; path <- paths) {
      for (i <- 0 until path.length - 1) {
        val ch1 = path(i)
        val ch2 = path(i + 1)
        cdgEdges += ((ch1, ch2))
      }
    }

    cdgEdges.foreach { case ((u1, u2), (v1, v2)) => println(s"  ($u1->$u2) -> ($v1->$v2)") }

    def findCycles(edges: Iterable[((Int, Int), (Int, Int))]): Seq[List[(Int, Int)]] = {
      val graph = mutable.Map[(Int, Int), List[(Int, Int)]]().withDefaultValue(Nil)
      edges.foreach { case (u, v) => graph(u) ::= v }

      val result = mutable.Buffer[List[(Int, Int)]]()

      def dfs(current: (Int, Int), path: List[(Int, Int)], visited: Set[(Int, Int)]): Unit = {
        for (neighbor <- graph(current)) {
          if (path.contains(neighbor)) {
            val cycleStart = path.indexOf(neighbor)
            if (cycleStart != -1) {
              val cycle = path.drop(cycleStart) :+ neighbor
              val rotated = cycle.dropWhile(_ != cycle.min) ++ cycle.takeWhile(_ != cycle.min)
              if (!result.exists(_.toSet == rotated.toSet)) {
                result += rotated
              }
            }
          } else if (!visited.contains(neighbor)) {
            dfs(neighbor, path :+ neighbor, visited + neighbor)
          }
        }
      }

      for (start <- graph.keys) {
        dfs(start, List(start), Set(start))
      }

      result.toSeq
    }

    val cycles = findCycles(cdgEdges)
    println(s"[DatelineAnalyzer] Detected ${cycles.length} cycles")
    for ((cycle, idx) <- cycles.zipWithIndex) {
      println(s"  Cycle $idx: ${cycle.map(e => s"(${e._1}->${e._2})").mkString(" -> ")}")
    }

    // Greedy Approach for Minimum Datelines
    val cycleEdgeSets = cycles.map { cycle =>
      val extended = cycle :+ cycle.head
      extended.sliding(2).map {
        case Seq((_, _), (v1, v2)) => (v1, v2)
      }.toSet
    }

    val uncovered = mutable.Set() ++ cycleEdgeSets.indices
    val datelineEdges = mutable.Set[(Int, Int)]()

    while (uncovered.nonEmpty) {
      val edgeCoverage = mutable.Map[(Int, Int), mutable.Set[Int]]()

      for (i <- uncovered; e <- cycleEdgeSets(i)) {
        val s = edgeCoverage.getOrElseUpdate(e, mutable.Set())
        s += i
      }

      // Select the edge covering most uncovered cycles
      val bestEdge = edgeCoverage.maxBy(_._2.size)._1
      val hitCycles = edgeCoverage(bestEdge)

      datelineEdges += bestEdge
      uncovered --= hitCycles

      println(s"[DatelineAnalyzer] Selected dateline edge: (${bestEdge._1} -> ${bestEdge._2})")
    }


    val datelinePhysicalEdges = datelineEdges.toSet

    var maxDatelineCrossings = 0
    for ((_, paths) <- shortestPaths; path <- paths) {
      val count = path.count(e => datelinePhysicalEdges.contains(e))
      if (count > maxDatelineCrossings) maxDatelineCrossings = count
    }

    val vcCount = maxDatelineCrossings + 1
    println(s"[DatelineAnalyzer] Selected ${datelineEdges.size} dateline edges")
    println(s"[DatelineAnalyzer] Max dateline crossings: $maxDatelineCrossings, Required VCs: $vcCount")

    DatelineResult(datelinePhysicalEdges, vcCount, nextHopMap.map { case (k, v) => k -> v.toSet }.toMap)
  }
}
