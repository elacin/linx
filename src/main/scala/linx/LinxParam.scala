package linx

/* abstract over arity of tuples */
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