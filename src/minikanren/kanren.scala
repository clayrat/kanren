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

  sealed abstract class Term
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

  def lookup2(v: Term, s: Subst): Term = lookup(v, s) match {
    case Var(x) => Var(x)
    case IntVal(x) => IntVal(x)
    case ListVal(x) if (!x.isEmpty) =>
      if (x.tail.isEmpty) lookup2(x.head, s) else ListVal(List(lookup2(x.head, s), lookup2(ListVal(x.tail), s)))
    case ListVal(List()) => ListVal(List())
  }

  def unify(t1: Term, t2: Term): Subst => Nond[Subst] = { s: Subst =>
    (lookup(t1, s), lookup(t2, s)) match {
      case (a, b) if (a == b) => succeed(s)
      case (Var(x), y) => succeed(extend(Var(x), y, s))
      case (x, Var(y)) => succeed(extend(Var(y), x, s))
      case (ListVal(l1), ListVal(l2)) if (!l1.isEmpty && !l2.isEmpty) =>
        (l1, l2) match {
          case (l1h :: l1t, l2h :: l2t) if (l1t.size == 1) => unify(l1h, l2h)(s).flatMap(unify(l1t.head, ListVal(l2t)))
          case (l1h :: l1t, l2h :: l2t) if (l2t.size == 1) => unify(l1h, l2h)(s).flatMap(unify(ListVal(l1t), l2t.head))
          case (l1h :: l1t, l2h :: l2t) => unify(l1h, l2h)(s).flatMap(unify(ListVal(l1t), ListVal(l2t)))
        }
      case _ => fail(s)
    }

  }

  // 3

  def run(g: Subst => Nond[Subst]) = g(emptySubst)

  def run2(g: Subst => Nond[Subst]) = g(emptySubst).map { x => lookup2(Var("q"), x) }

  def choice(v: Term, lst: List[Term]): Subst => Nond[Subst] = lst match {
    case Nil => fail[Subst]
    case hd :: tl =>
      disj(unify(v, hd), choice(v, tl))
  }

  def commonElement(l1: List[Term], l2: List[Term]) = {
    val vx = Var("x")
    conj(
      choice(vx, l1),
      choice(vx, l2))
  }

  def conso(a: Term, b: Term, l: Term) = b match {
    case ListVal(lb) => unify(ListVal(a +: lb), l)
    case _ => unify(ListVal(List(a, b)), l)
  }

  def appendo(l1: Term, l2: Term, l3: Term): Subst => Nond[Subst] = {
    def appendoHelper(l1: Term, l2: Term, l3: Term)(depth: Int): Subst => Nond[Subst] = {
      disj(
        conj(unify(l1, ListVal(Nil)), unify(l2, l3)),
        {
          val h = Var("h" + depth)
          val t = Var("t" + depth)
          val l3p = Var("l3p" + depth)
          conj(
            conso(h, t, l1),
            { s1: Subst =>
              conj(
                conso(h, l3p, l3),
                { s2: Subst =>
                  appendoHelper(t, l2, l3p)(depth + 1)(s2)
                })(s1)
            })
        })
    }
    appendoHelper(l1, l2, l3)(0)
  }

  // TESTS

  def cout[A](name: String)(as: List[A]) = {
    println(name)
    println(as)
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

    val xy = unify(vx, vy)(emptySubst)
    cout("test-u1")(xy)
    println("test-u2")
    val xy1 = unify(vx, IntVal(1))(xy.head)
    println(xy1)
    println("test-u3")
    println(lookup(vx, xy1.head))
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
    val vx = Var("x")
    val vy = Var("y")
    val vq = Var("q")

    cout("t0")(run(appendo(wrapL(1), wrapL(2), vq)))
    cout("t1")(run2(appendo(wrapL(1), wrapL(2), vq)))
    cout("t2")(run2(appendo(wrapL(1), wrapL(2), wrapL(1))))
    cout("t3")(run2(appendo(wrapL(1, 2, 3), vq, wrapL(1, 2, 3, 4, 5))))
    cout("t4")(run2(appendo(vq, wrapL(4, 5), wrapL(1, 2, 3, 4, 5))))
    cout("t5")(run2(appendo(vq, vx, wrapL(1, 2, 3, 4, 5))))
    cout("t6")(run2(appendo(vx, vq, wrapL(1, 2, 3, 4, 5))))
    cout("t7")(run2(
      conj(
        appendo(vx, vy, wrapL(1, 2, 3, 4, 5)),
        unify(vq, ListVal(List(vx, vy))))))
  }

  def main(args: Array[String]) {
    testAp

  }
}