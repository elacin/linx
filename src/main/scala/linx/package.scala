package object linx {
  private[linx] def split(s: String): List[String] =
    (s split "/" filterNot (_.isEmpty)).toList

  val Root: StaticLinx =
    Linx.StaticLinxImpl(Vector.empty)
}
