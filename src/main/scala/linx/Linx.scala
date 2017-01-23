package linx

private [linx] sealed trait BaseLinx[A] {
  private[linx] val parts: Stream[Vector[Part]]

  def elements(a: A): Stream[Vector[String]]

  def extract(seq: List[String]): Stream[(A, List[String])]

  final def /[B](s: Symbol)(implicit p: LinxParam[A, B]): Linx[B] =
    Linx.VariableLinx(this, p, Vector.empty, s)

  final def links(a: A): Stream[String] =
    elements(a) map (_.mkString("/", "/", ""))

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
}

sealed trait StaticLinx extends BaseLinx[Unit] {
  def /(name: String): StaticLinx
  def unapply(s: String): Boolean

  final def apply(): String =
    links(()).head

  final def |(or: StaticLinx): StaticLinx =
    Linx.UnionStaticLinx(this, or)
}

sealed trait Linx[A] extends BaseLinx[A] {
  def /(name: String): Linx[A]
  def unapply(s: String): Option[A]

  final def apply(a: A): String =
    links(a).head

  final def |(or: Linx[A]): Linx[A] =
    Linx.UnionVariableLinx(this, or)
}

private[linx] object Linx {
  final case class StaticLinxImpl(static: Vector[String]) extends StaticLinx {

    override def /(name: String): StaticLinx =
      StaticLinxImpl(static ++ split(name))

    override def elements(a: Unit) =
      Stream(static)

    override def extract(seq: List[String]): Stream[(Unit, List[String])] =
      if (seq startsWith static) Stream(((), seq drop static.size))
      else Stream.empty

    override def unapply(s: String): Boolean =
      extract(split(s)) exists (_._2.isEmpty)

    override private[linx] val parts: Stream[Vector[Literal]] =
      Stream(static map Literal)
  }

  final case class VariableLinx[P, A](parent: BaseLinx[P],
                                      param: LinxParam[P, A],
                                      static: Vector[String],
                                      symbol: Symbol)
      extends Linx[A] {

    override def /(name: String): Linx[A] =
      VariableLinx(parent, param, static ++ split(name), symbol)

    override def elements(a: A): Stream[Vector[String]] = {
      val (p, part) = param previous a
      parent.elements(p).map(_ ++ (part +: static))
    }

    override def extract(seq: List[String]): Stream[(A, List[String])] =
      for {
        (p, head :: tail) <- parent extract seq if tail startsWith static
      } yield (param.next(p, head), tail drop static.size)

    override def unapply(s: String): Option[A] =
      (for { (a, Nil) <- extract(split(s)) } yield a).headOption

    override private[linx] val parts: Stream[Vector[Part]] =
      parent.parts.map(_ ++ (Var(symbol.name) +: (static map Literal)))
  }

  trait UnionLinx[A] extends BaseLinx[A] {
    def first: BaseLinx[A]
    def next: BaseLinx[A]

    override final def elements(a: A): Stream[Vector[String]] =
      (first elements a) #::: (next elements a)

    override final def extract(seq: List[String]): Stream[(A, List[String])] =
      (first extract seq) #::: (next extract seq)

    override private[linx] final val parts: Stream[Vector[Part]] =
      first.parts #::: next.parts
  }

  final case class UnionVariableLinx[A](first: Linx[A], next: Linx[A])
      extends Linx[A]
      with UnionLinx[A] {

    override def /(name: String): Linx[A] =
      UnionVariableLinx(first / name, next / name)

    override def unapply(s: String): Option[A] =
      first unapply s orElse (next unapply s)
  }

  final case class UnionStaticLinx(first: StaticLinx, next: StaticLinx)
      extends StaticLinx
      with UnionLinx[Unit] {

    override def /(name: String): StaticLinx =
      UnionStaticLinx(first / name, next / name)

    override def unapply(s: String): Boolean =
      (first unapply s) || (next unapply s)
  }
}
