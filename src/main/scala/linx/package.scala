package object linx {
  val Root: Linx[Unit, Boolean] =
    new Linx.StaticLinx(Vector.empty)
}
