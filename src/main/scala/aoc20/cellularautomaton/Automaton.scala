package aoc20.cellularautomaton

object Automaton {

  def runCycle[A, C[_]](
    container: C[A],
    enabledRule: Int => Boolean,
    disabledRule: Int => Boolean,
  )(implicit C: Container[C], CD: Coord[A]) = {
    val defined = C.defined(container)
    val changeCandidates = defined ++ defined.flatMap(CD.neighbours(_))
    changeCandidates
      .foldLeft(C.empty[A]) { (s, c) =>
        val enabled = C.enabled(container, c)
        val enabledNeighbours = CD.neighbours(c).count(C.enabled(container, _))
        if (
          enabled && enabledRule(
            enabledNeighbours,
          ) || !enabled && disabledRule(enabledNeighbours)
        ) C.addEnabled(s, c)
        else s
      }
  }

  def runN[A: Coord, C[_]: Container](
    initial: C[A],
    enabledRule: Int => Boolean,
    disabledRule: Int => Boolean,
    n: Int,
  ) =
    (1 to n).foldLeft(initial)((c, _) => runCycle(c, enabledRule, disabledRule))
}

trait Container[C[_]] {
  def concat[A](a: C[A], b: C[A]): C[A]
  def defined[A](a: C[A]): Set[A]
  def empty[A]: C[A]
  def enabled[A](c: C[A], a: A): Boolean
  def addEnabled[A](c: C[A], a: A): C[A]
}

object Container {
  implicit val setContainer: Container[Set] = new Container[Set] {
    override def concat[A](a: Set[A], b: Set[A]): Set[A] = a ++ b
    override def defined[A](a: Set[A]): Set[A] = a
    override def empty[A]: Set[A] = Set.empty
    override def enabled[A](c: Set[A], a: A): Boolean = c.contains(a)
    override def addEnabled[A](c: Set[A], a: A): Set[A] = c + a
  }

  implicit def mapContainer[B](implicit
    B: BooleanIsomorphism[B],
  ): Container[Map[?, B]] = new Container[Map[?, B]] {
    override def concat[A](a: Map[A, B], b: Map[A, B]): Map[A, B] = a ++ b
    override def defined[A](a: Map[A, B]): Set[A] =
      a.filter { case (_, b) => B.value(b) }.keys.toSet
    override def empty[A]: Map[A, B] = Map.empty
    override def enabled[A](c: Map[A, B], a: A): Boolean =
      c.get(a).fold(false)(B.value(_))
    override def addEnabled[A](c: Map[A, B], a: A): Map[A, B] =
      c + (a -> B.truthy)
  }
}

trait Coord[A] {
  def neighbours(a: A): Seq[A]
}

trait BooleanIsomorphism[A] {
  def truthy: A
  def value(a: A): Boolean
  def fromBool(b: Boolean): A
}

object BooleanIsomorphism {
  implicit val booleanIsBoolean = new BooleanIsomorphism[Boolean] {
    override def truthy: Boolean = true
    override def value(a: Boolean): Boolean = a
    override def fromBool(b: Boolean): Boolean = b
  }
}
