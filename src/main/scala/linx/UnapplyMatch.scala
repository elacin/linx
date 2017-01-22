package linx

/**
  * This exploits a somewhat little known feature of extractors in
  *  scala, where they can either return `Option[X]` or `Boolean`.
  * In the case of a boolean nothing is extracted, and the branch is
  *  taken on `true`.
  *
  * `UnapplyMatch` exists to abstract over these two options so users
  *  won't have to ignore unit in pattern matches for static urls.
  */
@inline final class UnapplyMatch[X](val is: X => Boolean) extends AnyVal

object UnapplyMatch {
  implicit val boolean: UnapplyMatch[Boolean] =
    new UnapplyMatch[Boolean](identity)

  @inline implicit def option[A]: UnapplyMatch[Option[A]] =
    new UnapplyMatch[Option[A]](_.isDefined)
}
