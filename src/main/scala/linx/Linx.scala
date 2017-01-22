package linx

import scala.language.implicitConversions

object Root extends StaticLinx(Vector.empty)

sealed trait Part
case class Literal(name: String) extends Part
case class Var(name: String) extends Part

object Linx {
  @inline implicit final class VarOps[A, X](private val l: Linx[A, Option[X]])
      extends AnyVal {
    @inline def apply(a: A): String =
      l.links(a).head
  }

  @inline implicit final class NoVarOps[X](private val l: Linx[Unit, X])
      extends AnyVal {
    @inline def apply(): String =
      l.links(()).head
  }
}

sealed trait Linx[A, X] {

  def split(s: String): List[String] =
    s.split("/").filterNot(_.isEmpty).toList

  def links(a: A): Stream[String] =
    elements(a).map(_.mkString("/", "/", ""))

  def /[B](s: Symbol)(implicit param: LinxParam[A, B]): Linx[B, Option[B]] =
    new VariableLinx(this, param, Vector.empty, s)

  def |(or: Linx[A, X])(implicit matcher: UnapplyMatch[X]): Linx[A, X] =
    new UnionLinx(this, or, matcher)

  def /(name: String): Linx[A, X]

  def elements(a: A): Stream[Vector[String]]

  def extract(seq: List[String]): Stream[(A, List[String])]

  def unapply(s: String): X

  def parts: Stream[Vector[Part]]

  def templates(render: String => String): Stream[String] =
    parts.map(_.map {
      case Literal(l) => l
      case Var(v) => render(v)
    }.mkString("/", "/", ""))

  def template(render: String => String): String =
    templates(render).head

  // rfc6570 uri template
  override def toString: String =
    template("{" + _ + "}")
}

class StaticLinx private[linx] (val static: Vector[String])
    extends Linx[Unit, Boolean] {
  def unapply(s: String): Boolean =
    extract(split(s)).exists(_._2.isEmpty)

  def /(name: String): Linx[Unit, Boolean] =
    new StaticLinx(static ++ split(name))

  def elements(a: Unit) =
    Stream(static)

  def extract(seq: List[String]): Stream[(Unit, List[String])] =
    if (seq.startsWith(static)) Stream(((), seq.drop(static.size)))
    else Stream.empty

  def parts: Stream[Vector[Literal]] =
    Stream(static.map(Literal))
}

private class VariableLinx[P, A] private[linx] (parent: Linx[P, _],
                                                param: LinxParam[P, A],
                                                static: Vector[String],
                                                symbol: Symbol)
    extends Linx[A, Option[A]] {

  def /(name: String): Linx[A, Option[A]] =
    new VariableLinx(parent, param, static ++ split(name), symbol)

  def elements(a: A): Stream[Vector[String]] = {
    val (p, part) = param.previous(a)
    parent.elements(p).map(_ ++ (part +: static))
  }

  def extract(seq: List[String]): Stream[(A, List[String])] =
    for {
      (p, head :: tail) <- parent.extract(seq) if tail.startsWith(static)
    } yield (param.next(p, head), tail.drop(static.size))

  def unapply(s: String): Option[A] =
    (for { (a, Nil) <- extract(split(s)) } yield a).headOption

  def parts: Stream[Vector[Part]] =
    parent.parts.map(_ ++ (Var(symbol.name) +: static.map(Literal)))
}

private class UnionLinx[A, X] private[linx] (first: Linx[A, X],
                                             next: Linx[A, X],
                                             matcher: UnapplyMatch[X])
    extends Linx[A, X] {

  def /(name: String): Linx[A, X] =
    new UnionLinx(first / name, next / name, matcher)

  def elements(a: A): Stream[Vector[String]] =
    first.elements(a) #::: next.elements(a)

  def extract(seq: List[String]): Stream[(A, List[String])] =
    first.extract(seq) #::: next.extract(seq)

  def unapply(s: String): X = {
    val firstX = first.unapply(s)
    if (matcher.is(firstX)) firstX else next.unapply(s)
  }

  def parts: Stream[Vector[Part]] =
    first.parts #::: next.parts
}

sealed case class UnapplyMatch[X](is: X => Boolean)

object UnapplyMatch {
  implicit val boolean: UnapplyMatch[Boolean] =
    UnapplyMatch[Boolean](identity)

  implicit def option[A]: UnapplyMatch[Option[A]] =
    UnapplyMatch[Option[A]](_.isDefined)
}

sealed case class LinxParam[A, B](previous: B => (A, String),
                                  next: (A, String) => B)

object LinxParam {
  // format: off
  implicit val param1: LinxParam[Unit, String] = LinxParam[Unit, String](((), _: String), (_, n) => n)
  implicit val param2: LinxParam[String, (String, String)] = LinxParam[String, (String, String)](identity, (a, b) => (a, b))
  implicit val param3: LinxParam[(String, String), (String, String, String)] = LinxParam[(String, String), (String, String, String)]({ case (a0, a1, a2) => ((a0, a1), a2) }, { case ((a0, a1), a2) => (a0, a1, a2) })
  implicit val param4: LinxParam[(String, String, String), (String, String, String, String)] = LinxParam[(String, String, String), (String, String, String, String)]({ case (a0, a1, a2, a3) => ((a0, a1, a2), a3) }, { case ((a0, a1, a2), a3) => (a0, a1, a2, a3) })
  implicit val param5: LinxParam[(String, String, String, String), (String, String, String, String, String)] = LinxParam[(String, String, String, String), (String, String, String, String, String)]({ case (a0, a1, a2, a3, a4) => ((a0, a1, a2, a3), a4) }, { case ((a0, a1, a2, a3), a4) => (a0, a1, a2, a3, a4) })
  implicit val param6: LinxParam[(String, String, String, String, String), (String, String, String, String, String, String)] = LinxParam[(String, String, String, String, String), (String, String, String, String, String, String)]({ case (a0, a1, a2, a3, a4, a5) => ((a0, a1, a2, a3, a4), a5) }, { case ((a0, a1, a2, a3, a4), a5) => (a0, a1, a2, a3, a4, a5) })
  implicit val param7: LinxParam[(String, String, String, String, String, String), (String, String, String, String, String, String, String)] = LinxParam[(String, String, String, String, String, String), (String, String, String, String, String, String, String)]({ case (a0, a1, a2, a3, a4, a5, a6) => ((a0, a1, a2, a3, a4, a5), a6) }, { case ((a0, a1, a2, a3, a4, a5), a6) => (a0, a1, a2, a3, a4, a5, a6) })
  implicit val param8: LinxParam[(String, String, String, String, String, String, String), (String, String, String, String, String, String, String, String)] = LinxParam[(String, String, String, String, String, String, String), (String, String, String, String, String, String, String, String)]({ case (a0, a1, a2, a3, a4, a5, a6, a7) => ((a0, a1, a2, a3, a4, a5, a6), a7) }, { case ((a0, a1, a2, a3, a4, a5, a6), a7) => (a0, a1, a2, a3, a4, a5, a6, a7) })
  implicit val param9: LinxParam[(String, String, String, String, String, String, String, String), (String, String, String, String, String, String, String, String, String)] = LinxParam[(String, String, String, String, String, String, String, String), (String, String, String, String, String, String, String, String, String)]({ case (a0, a1, a2, a3, a4, a5, a6, a7, a8) => ((a0, a1, a2, a3, a4, a5, a6, a7), a8) }, { case ((a0, a1, a2, a3, a4, a5, a6, a7), a8) => (a0, a1, a2, a3, a4, a5, a6, a7, a8) })
  implicit val param10: LinxParam[(String, String, String, String, String, String, String, String, String), (String, String, String, String, String, String, String, String, String, String)] = LinxParam[(String, String, String, String, String, String, String, String, String), (String, String, String, String, String, String, String, String, String, String)]({ case (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9) => ((a0, a1, a2, a3, a4, a5, a6, a7, a8), a9) }, { case ((a0, a1, a2, a3, a4, a5, a6, a7, a8), a9) => (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9) })
  implicit val param11: LinxParam[(String, String, String, String, String, String, String, String, String, String), (String, String, String, String, String, String, String, String, String, String, String)] = LinxParam[(String, String, String, String, String, String, String, String, String, String), (String, String, String, String, String, String, String, String, String, String, String)]({ case (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) => ((a0, a1, a2, a3, a4, a5, a6, a7, a8, a9), a10) }, { case ((a0, a1, a2, a3, a4, a5, a6, a7, a8, a9), a10) => (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) })
  implicit val param12: LinxParam[(String, String, String, String, String, String, String, String, String, String, String), (String, String, String, String, String, String, String, String, String, String, String, String)] = LinxParam[(String, String, String, String, String, String, String, String, String, String, String), (String, String, String, String, String, String, String, String, String, String, String, String)]({ case (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11) => ((a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10), a11) }, { case ((a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10), a11) => (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11) })
  implicit val param13: LinxParam[(String, String, String, String, String, String, String, String, String, String, String, String), (String, String, String, String, String, String, String, String, String, String, String, String, String)] = LinxParam[(String, String, String, String, String, String, String, String, String, String, String, String), (String, String, String, String, String, String, String, String, String, String, String, String, String)]({ case (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12) => ((a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11), a12) }, { case ((a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11), a12) => (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12) })
  implicit val param14: LinxParam[(String, String, String, String, String, String, String, String, String, String, String, String, String), (String, String, String, String, String, String, String, String, String, String, String, String, String, String)] = LinxParam[(String, String, String, String, String, String, String, String, String, String, String, String, String), (String, String, String, String, String, String, String, String, String, String, String, String, String, String)]({ case (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13) => ((a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12), a13) }, { case ((a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12), a13) => (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13) })
  implicit val param15: LinxParam[(String, String, String, String, String, String, String, String, String, String, String, String, String, String), (String, String, String, String, String, String, String, String, String, String, String, String, String, String, String)] = LinxParam[(String, String, String, String, String, String, String, String, String, String, String, String, String, String), (String, String, String, String, String, String, String, String, String, String, String, String, String, String, String)]({ case (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14) => ((a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13), a14) }, { case ((a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13), a14) => (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14) })
  implicit val param16: LinxParam[(String, String, String, String, String, String, String, String, String, String, String, String, String, String, String), (String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String)] = LinxParam[(String, String, String, String, String, String, String, String, String, String, String, String, String, String, String), (String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String)]({ case (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15) => ((a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14), a15) }, { case ((a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14), a15) => (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15) })
  implicit val param17: LinxParam[(String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String), (String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String)] = LinxParam[(String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String), (String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String)]({ case (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16) => ((a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15), a16) }, { case ((a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15), a16) => (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16) })
  implicit val param18: LinxParam[(String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String), (String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String)] = LinxParam[(String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String), (String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String)]({ case (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17) => ((a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16), a17) }, { case ((a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16), a17) => (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17) })
  implicit val param19: LinxParam[(String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String), (String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String)] = LinxParam[(String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String), (String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String)]({ case (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18) => ((a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17), a18) }, { case ((a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17), a18) => (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18) })
  implicit val param20: LinxParam[(String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String), (String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String)] = LinxParam[(String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String), (String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String)]({ case (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19) => ((a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18), a19) }, { case ((a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18), a19) => (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19) })
  implicit val param21: LinxParam[(String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String), (String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String)] = LinxParam[(String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String), (String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String)]({ case (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20) => ((a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19), a20) }, { case ((a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19), a20) => (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20) })
  implicit val param22: LinxParam[(String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String), (String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String)] = LinxParam[(String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String), (String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String)]({ case (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21) => ((a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20), a21) }, { case ((a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20), a21) => (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21) })
  // format: off
}
