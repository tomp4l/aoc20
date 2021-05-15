package aoc20.cellularautomaton

object Automaton:

  def runCycle[A, C[_]](
    container: C[A],
    enabledRule: Int => Boolean,
    disabledRule: Int => Boolean,
  )(using C: Container[C], CD: Coord[A]) =
    val defined = C.defined(container)
    val changeCandidates = defined ++ defined.flatMap(CD.neighbours(_))
    changeCandidates
      .foldLeft(C.empty[A]) { (s, c) =>
        val enabled = C.enabled(container, c)
        val enabledNeighbours = CD.neighbours(c).count(C.enabled(container, _))
        if enabled && enabledRule(
          enabledNeighbours,
        ) || !enabled && disabledRule(enabledNeighbours) then C.addEnabled(s, c)
        else s
      }

  def runN[A: Coord, C[_]: Container](
    initial: C[A],
    enabledRule: Int => Boolean,
    disabledRule: Int => Boolean,
    n: Int,
  ) =
    (1 to n).foldLeft(initial)((c, _) => runCycle(c, enabledRule, disabledRule))

trait Container[C[_]]:
  def concat[A](a: C[A], b: C[A]): C[A]
  def defined[A](a: C[A]): Set[A]
  def empty[A]: C[A]
  def enabled[A](c: C[A], a: A): Boolean
  def addEnabled[A](c: C[A], a: A): C[A]

object Container:
  given setContainer: Container[Set] with
    def concat[A](a: Set[A], b: Set[A]): Set[A] = a ++ b
    def defined[A](a: Set[A]): Set[A] = a
    def empty[A]: Set[A] = Set.empty
    def enabled[A](c: Set[A], a: A): Boolean = c.contains(a)
    def addEnabled[A](c: Set[A], a: A): Set[A] = c + a

  given mapContainer[B](using
    B: BooleanIsomorphism[B],
  ): Container[[A] =>> Map[A, B]] with
    def concat[A](a: Map[A, B], b: Map[A, B]): Map[A, B] = a ++ b
    def defined[A](a: Map[A, B]): Set[A] =
      a.filter { case (_, b) => B.value(b) }.keys.toSet
    def empty[A]: Map[A, B] = Map.empty
    def enabled[A](c: Map[A, B], a: A): Boolean =
      c.get(a).fold(false)(B.value(_))
    def addEnabled[A](c: Map[A, B], a: A): Map[A, B] =
      c + (a -> B.truthy)

trait Coord[A]:
  def neighbours(a: A): Seq[A]

trait BooleanIsomorphism[A]:
  def truthy: A
  def value(a: A): Boolean
  def fromBool(b: Boolean): A

object BooleanIsomorphism:
  given BooleanIsomorphism[Boolean] with
    def truthy: Boolean = true
    def value(a: Boolean): Boolean = a
    def fromBool(b: Boolean): Boolean = b
