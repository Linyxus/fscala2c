package fs2c.tools.fp.syntax

trait OptionSyntax {
  extension[A] (ma: Option[A]) {
    def <|>[B](mb: Option[B]): Option[A | B] = {
      ma match {
        case Some(v) => Some(v)
        case None => mb
      }
    }
    
    def !![B](b: B): A | B = {
      ma match {
        case Some(v) => v
        case None => b
      }
    }
  }
}
