package problems

import tools.Sandbox

/** [[https://aperiodic.net/pip/scala/s-99/#graphs]]
  *
  * [[https://aperiodic.net/pip/scala/s-99/#p80]]
  */
object P80 {

  abstract class GraphBase[N, E] {

    case class Edge(n1: Node, n2: Node, value: E) {
      def toTuple = (n1.value, n2.value, value)
    }

    case class Node(value: N) {
      var adj: List[Edge] = Nil
      def neighbors: List[Node] = adj.map(edgeTarget(_, this).get)
    }

    var nodes: Map[N, Node] = Map.empty
    var edges: List[Edge] = List.empty

    // If the edge E connects N to another node, returns the other node,
    // otherwise returns None.
    def edgeTarget(e: Edge, n: Node): Option[Node]

    override def equals(g: Any): Boolean = g match {
      case g: GraphBase[N, E] => nodes.keySet == g.nodes.keySet && edges.toSet == g.edges.toSet
      case _                  => false
    }

    def addNode(value: N) = {
      val n = Node(value)
      nodes = Map(value -> n) ++ nodes
    }
  }

  final class Graph[N, E] extends GraphBase[N, E] {
    override def equals(o: Any): Boolean = o match {
      case g: Graph[N, E] => super.equals(g)
      case _              => false
    }

    def edgeTarget(e: Edge, n: Node): Option[Node] =
      Option.when(e.n1 == n)(e.n2) orElse
        Option.when(e.n2 == n)(e.n1)

    def addEdge(n1: N, n2: N, value: E) = {
      val n1n = nodes(n1) // unsafe !
      val n2n = nodes(n2) // unsafe !
      val e = Edge(n1n, n2n, value)
      edges = e :: edges
      n1n.adj = e :: n1n.adj
      n2n.adj = e :: n2n.adj
    }
  }

  final class Digraph[N, E] extends GraphBase[N, E] {
    override def equals(o: Any) = o match {
      case g: Digraph[N, E] => super.equals(g)
      case _                => false
    }

    def edgeTarget(e: Edge, n: Node): Option[Node] = Option.when(e.n1 == n)(e.n2)

    def addArc(src: N, dst: N, value: E) = {
      val e = Edge(nodes(src), nodes(dst), value)
      edges = e :: edges
      nodes(src).adj = e :: nodes(src).adj
    }

  }

  abstract class GraphObjBase {
    type GraphClass[N, E]

    def termLabel[N, E](nodes: List[N], edges: List[(N,N,E)]): GraphClass[N, E]
    def adjacentLabel[N, E](nodes: List[(N, List[(N,E)])]): GraphClass[N, E]

    def addLabel[T](edges: List[(T, T)]) = edges.map(v => (v._1, v._2, ()))
    def term[T](nodes: List[T], edges: List[(T,T)]) = termLabel(nodes, addLabel(edges))

    def addAdjacentLabel[T](nodes: List[(T, List[T])]) = nodes.map(a => (a._1, a._2.map((_, ()))))
    def adjacent[T](nodes: List[(T, List[T])]) = adjacentLabel(addAdjacentLabel(nodes))
  }

  object Graph extends GraphObjBase {
    type GraphClass[T, U] = Graph[T, U]

    def termLabel[T, U](nodes: List[T], edges: List[(T,T,U)]) = {
      val g = new Graph[T, U]
      nodes.map(g.addNode)
      edges.map(v => g.addEdge(v._1, v._2, v._3))
      g
    }

    def adjacentLabel[T, U](nodes: List[(T, List[(T,U)])]) = {
      val g = new Graph[T, U]
      for ((v, a) <- nodes) g.addNode(v)
      for ((n1, a) <- nodes; (n2, l) <- a) {
        if (!g.nodes(n1).neighbors.contains(g.nodes(n2)))
          g.addEdge(n1, n2, l)
      }
      g
    }

  }

  object Digraph extends GraphObjBase {
    type GraphClass[T, U] = Digraph[T, U]

    def termLabel[T, U](nodes: List[T], edges: List[(T,T,U)]) = {
      val g = new Digraph[T, U]
      nodes.map(g.addNode)
      edges.map(v => g.addArc(v._1, v._2, v._3))
      g
    }

    def adjacentLabel[T, U](nodes: List[(T, List[(T,U)])]) = {
      val g = new Digraph[T, U]
      for ((n, a) <- nodes) g.addNode(n)
      for ((s, a) <- nodes; (d, l) <- a) g.addArc(s, d, l)
      g
    }

  }

}

class P80 extends Sandbox {

  import P80._

  test("1") {}

}
