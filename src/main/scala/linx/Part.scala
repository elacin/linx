package linx

/**
  * Representation of the individual fragments of the links
  */
private[linx] sealed trait Part
private[linx] final case class Literal(name: String) extends Part
private[linx] final case class Var(name: String) extends Part
