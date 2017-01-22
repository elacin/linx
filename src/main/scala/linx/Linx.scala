package linx

import scala.language.implicitConversions

sealed trait Linx[A, X] {
  final def links(a: A): Stream[String] =
    elements(a) map (_.mkString("/", "/", ""))

  final def /[B](s: Symbol)(implicit p: LinxParam[A, B]): Linx[B, Option[B]] =
    new Linx.VariableLinx(this, p, Vector.empty, s)

  final def |(or: Linx[A, X])(implicit m: UnapplyMatch[X]): Linx[A, X] =
    new Linx.UnionLinx(this, or, m)

  final def templates(render: String => String): Stream[String] =
    parts.map(_.map {
      case Literal(l) => l
      case Var(v) => render(v)
    }.mkString("/", "/", ""))

  final def template(render: String => String): String =
    templates(render).head

  // rfc6570 uri template
  final override def toString: String =
    template("{" + _ + "}")

  def /(name: String): Linx[A, X]

  def elements(a: A): Stream[Vector[String]]

  def extract(seq: List[String]): Stream[(A, List[String])]

  def unapply(s: String): X

  private[linx] val parts: Stream[Vector[Part]]
}

object Linx {
  private def split(s: String): List[String] =
    (s split "/" filterNot (_.isEmpty)).toList

  @inline implicit final class VarOps[A, X](val l: Linx[A, Option[X]])
      extends AnyVal {
    @inline def apply(a: A): String =
      l.links(a).head
  }

  @inline implicit final class NoVarOps[X](val l: Linx[Unit, X])
      extends AnyVal {
    @inline def apply(): String =
      l.links(()).head
  }

  private[linx] final class StaticLinx(static: Vector[String])
      extends Linx[Unit, Boolean] {

    override def unapply(s: String): Boolean =
      extract(split(s)).exists(_._2.isEmpty)

    override def /(name: String): Linx[Unit, Boolean] =
      new StaticLinx(static ++ split(name))

    override def elements(a: Unit) =
      Stream(static)

    override def extract(seq: List[String]): Stream[(Unit, List[String])] =
      if (seq.startsWith(static)) Stream(((), seq.drop(static.size)))
      else Stream.empty

    override private[linx] val parts: Stream[Vector[Literal]] =
      Stream(static.map(Literal))
  }

  private final class VariableLinx[P, A](parent: Linx[P, _],
                                         param: LinxParam[P, A],
                                         static: Vector[String],
                                         symbol: Symbol)
      extends Linx[A, Option[A]] {

    override def /(name: String): Linx[A, Option[A]] =
      new VariableLinx(parent, param, static ++ split(name), symbol)

    override def elements(a: A): Stream[Vector[String]] = {
      val (p, part) = param.previous(a)
      parent.elements(p).map(_ ++ (part +: static))
    }

    override def extract(seq: List[String]): Stream[(A, List[String])] =
      for {
        (p, head :: tail) <- parent.extract(seq) if tail.startsWith(static)
      } yield (param.next(p, head), tail.drop(static.size))

    override def unapply(s: String): Option[A] =
      (for { (a, Nil) <- extract(split(s)) } yield a).headOption

    override private[linx] val parts: Stream[Vector[Part]] =
      parent.parts.map(_ ++ (Var(symbol.name) +: static.map(Literal)))
  }

  private final class UnionLinx[A, X](first: Linx[A, X],
                                      next: Linx[A, X],
                                      matcher: UnapplyMatch[X])
      extends Linx[A, X] {

    override def /(name: String): Linx[A, X] =
      new UnionLinx(first / name, next / name, matcher)

    override def elements(a: A): Stream[Vector[String]] =
      first.elements(a) #::: next.elements(a)

    override def extract(seq: List[String]): Stream[(A, List[String])] =
      first.extract(seq) #::: next.extract(seq)

    override def unapply(s: String): X = {
      val firstX = first.unapply(s)
      if (matcher.is(firstX)) firstX else next.unapply(s)
    }

    override private[linx] val parts: Stream[Vector[Part]] =
      first.parts #::: next.parts
  }
}
