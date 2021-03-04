package scala2c.tools.fp.syntax

trait OptionSyntax {
  extension[A] (ma: Option[A]) {
    def <|>[B](mb: Option[B]): Option[A | B] = {
      ma match {
        case Some(v) => Some(v)
        case None => mb
      }
    }
  }
}
