package minikanren

object kanren {

  // 1 

  type Nond[A] = List[A] // nondeterministic functions via multisets  

  def fail[A]: A => Nond[A] = { x => List[A]() }
  def succeed[A]: A => Nond[A] = { x => List[A](x) }

  def disj[A, B](f1: A => Nond[B], f2: A => Nond[B]): A => Nond[B] =
    { x: A => f1(x) ++ f2(x) }

  def conj[A, B, C](f1: A => Nond[B], f2: B => Nond[C]): A => Nond[C] =
    { x: A => f1(x).flatMap(f2) }

  // 2

  sealed class Term
  case class Var(name: String) extends Term
  case class IntVal(i: Int) extends Term
  case class ListVal(l: List[Term]) extends Term

  def wrapI(i: Int*) = i.map(IntVal(_)).toList
  def wrapL(i: Int*) = ListVal(wrapI(i: _*))

  type Subst = Map[String, Term]
  def emptySubst: Subst = Map[String, Term]().empty

  def extend(v: Var, value: Term, s: Subst): Subst = s + (v.name -> value)
  def lookup(v: Term, s: Subst): Term =
    v match {
      case IntVal(x) => IntVal(x)
      case ListVal(l) => ListVal(l)
      case Var(y) =>
        s.get(y) match {
          case Some(x: Term) => lookup(x, s)
          case None => Var(y)
        }
    }

  def unify(t1: Term, t2: Term): Subst => Option[Subst] = { s: Subst =>
    val (tt1, tt2) = (lookup(t1, s), lookup(t2, s))
    //   println("LOOKUP " + tt1 + " " + tt2)
    (tt1, tt2) match {
      case (a, b) if (a == b) => Some(s)
      case (Var(x), y) => Some(extend(Var(x), y, s))
      case (x, Var(y)) => Some(extend(Var(y), x, s))
      case (ListVal(l1), ListVal(l2)) => (l1, l2) match {
        case (l1v :: Nil, _) => unify(l1v, ListVal(l2))(s)
        case (_, l2v :: Nil) => unify(ListVal(l1), l2v)(s)
        case (l1h :: l1t, l2h :: l2t) => unify(l1h, l2h)(s).flatMap {
          unify(ListVal(l1t), ListVal(l2t))
        }
      }
      //case (Right(xx), Right(yy)) => unify(xx, yy, s)
      case _ => None
    }

  }

  // 3

  def equate(t1: Term, t2: Term) = { s: Subst =>
    unify(t1, t2)(s) match {
      case Some(s) => succeed(s)
      case None => fail(s)
    }
  }

  def run(g: Subst => Nond[Subst]) = g(emptySubst)

  def choice(v: Term, lst: List[Term]): Subst => Nond[Subst] = lst match {
    case Nil => fail[Subst]
    case hd :: tl =>
      disj(equate(v, hd), choice(v, tl))
  }

  def commonElement(l1: List[Term], l2: List[Term]) = {
    val vx = Var("x")
    conj(
      choice(vx, l1),
      choice(vx, l2))
  }

  def conso(a: Term, b: Term, l: Term) = b match {
    case ListVal(lb) => equate(ListVal(a +: lb), l)
    case _ => equate(ListVal(List(a, b)), l)
  }

  def appendo(l1: Term, l2: Term, l3: Term): Subst => Nond[Subst] = {
    disj(
      conj(equate(l1, ListVal(Nil)), equate(l2, l3)),
      {
        val h = Var("h")
        val t = Var("t")
        val l3p = Var("l3p")
        conj(
          conso(h, t, l1),
          { s: Subst =>
            conj(
              conso(h, l3p, l3),
              { s: Subst =>
                appendo(t, l2, l3p)(s)
              })(s)
          })
      })
  }

  // TESTS

  def cout(name: String)(subs: List[Subst]) = {
    println(name)
    println(subs)
  }

  def test1 {
    println("test1")
    println(disj(
      disj(fail[Int], succeed[Int]),
      conj(
        disj({ x: Int => succeed(x + 1) }, { x: Int => succeed(x + 10) }),
        disj(succeed[Int], succeed[Int])))(100))
  }

  def test2 {
    val vx = Var("x")
    val vy = Var("y")

    val xy = unify(vx, vy)(emptySubst).get
    cout("test-u1")(xy :: Nil)
    println("test-u2")
    val xy1 = unify(vx, IntVal(1))(xy).get
    println(xy1)
    println("test-u3")
    println(lookup(vx, xy1))
  }

  def test3 {
    val vx = Var("x")

    val ott = wrapI(1, 2, 3)

    cout("test-c1")(run(choice(IntVal(2), ott)))
    cout("test-c2")(run(choice(IntVal(10), ott)))
    cout("test-c3")(run(choice(vx, ott)))

  }

  def test4 {
    cout("common-el-1")(
      run(commonElement(wrapI(1, 2, 3), wrapI(3, 4, 5))))
    cout("common-el-2")(
      run(commonElement(wrapI(1, 2, 3), wrapI(3, 4, 1, 7))))
    cout("common-el-3")(
      run(commonElement(wrapI(11, 2, 3), wrapI(13, 4, 1, 7))))
  }

  def testCo {
    val vx = Var("x")
    val vy = Var("y")

    cout("conso-1")(run(conso(IntVal(1), wrapL(2, 3), vx)))
    cout("conso-2")(run(conso(vx, vy, wrapL(1, 2, 3))))

  }

  def testAp {
    val vq = Var("q")
    cout("t1")(run(appendo(wrapL(1), wrapL(2), vq)))

  }

  def main(args: Array[String]) {
    testAp

  }
}