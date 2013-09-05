package minikanren

object kanren {

  // 1 

  type Nond[A] = List[A] // nondeterministic functions via multisets  

  def fail[A]: A => Nond[A] = { x => List[A]() }
  def succeed[A]: A => Nond[A] = { x => List[A](x) }

  def disj[A, B](f1: A => Nond[B], f2: A => Nond[B]): A => Nond[B] =
    { x: A => f1(x) ++ f2(x) }

  def conj1[A, B, C](f1: A => Nond[B], f2: B => C): A => Nond[C] =
    { x: A => f1(x).map(f2) }

  def conj2[A, B, C](f1: A => Nond[B], f2: B => Nond[C]): A => Nond[C] =
    { x: A => f1(x).flatMap(f2) }

  // 2

  sealed class Term
  case class Var(name: String) extends Term
  case class IntVal(i: Int) extends Term

  type Subst = Map[String, Term]
  def emptySubst: Subst = Map[String, Term]().empty
  def extend(v: Var, value: Term, s: Subst): Subst = s + (v.name -> value)
  def lookup(v: Term, s: Subst): Term =
    v match {
      case IntVal(x) => IntVal(x)
      case Var(y) =>
        s.get(y) match {
          case Some(x: Term) => lookup(x, s)
          case None => Var(y)
        }
    }

  def unify(t1: Term, t2: Term, s: Subst): Option[Subst] = {
    val (tt1, tt2) = (lookup(t1, s), lookup(t2, s))
    //println("LOOKUP ", tt1, tt2)
    (tt1, tt2) match {
      case (a, b) if (a == b) => Some(s)
      case (Var(x), y) => Some(extend(Var(x), y, s))
      case (x, Var(y)) => Some(extend(Var(y), x, s))
      //case (Right(xx), Right(yy)) => unify(xx, yy, s)
      case _ => None
    }

  }

  // 3

  def equate(t1: Term, t2: Term) = { s: Subst =>
    unify(t1, t2, s) match {
      case Some(s) => succeed(s)
      case None => fail(s)
    }
  }

  def run(g: Subst => Nond[Subst]) = g(emptySubst)

  def choice(v: Term, lst: List[Term]): Subst => Nond[Subst] = lst match {
    case List() => fail[Subst]
    case hd :: tl => disj(equate(v, hd), choice(v, tl))
  }

  // TESTS

  def test1 {
    println("test1")
    println(disj(
      disj(fail[Int], succeed[Int]),
      conj2(
        disj({ x: Int => succeed(x + 1) }, { x: Int => succeed(x + 10) }),
        disj(succeed[Int], succeed[Int])))(100))
  }

  def test2 {
    val vx = Var("x")
    val vy = Var("y")
    val vz = Var("z")
    val vq = Var("q")

    println("test-u1")
    val xy = unify(vx, vy, emptySubst).get
    println(xy)
    println("test-u2")
    val xy1 = unify(vx, IntVal(1), xy).get
    println(xy1)
    println("test-u3")
    println(lookup(vx, xy1))
  }

  def test3 {
    val vx = Var("x")

    println("test-c1")
    println(run(choice(IntVal(2), List(IntVal(1), IntVal(2), IntVal(3)))))
    println("test-c2")
    println(run(choice(IntVal(10), List(IntVal(1), IntVal(2), IntVal(3)))))
    println("test-c3")
    println(run(choice(vx, List(IntVal(1), IntVal(2), IntVal(3)))))

  }

  def main(args: Array[String]) {
    test3

  }
}