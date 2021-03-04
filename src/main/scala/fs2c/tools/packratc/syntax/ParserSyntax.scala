package fs2c.tools.packratc.syntax

import fs2c.tools.packratc.Parser
import Parser._

trait ParserSyntax {
  extension[T, X] (p: Parser[T, X]) {
    /** `p ~ q === p seq q`
     */
    def ~[Y](q: => Parser[T, Y]): Parser[T, (X, Y)] = p seq q

    /** `p | q === p or q`
     */
    def |[Y](q: => Parser[T, Y]): Parser[T, X | Y] = p or q

    /** `p <| f === p map f`
     */
    def <|[Y](func: X => Y): Parser[T, Y] = p map func

    /** `p <* y === p map (_ => y)`
     */
    def <*[Y](y: Y): Parser[T, Y] = p map (_ => y)

    /** `p >>= f === p flatMap f`
     */
    def >>=[Y](mfunc: X => Parser[T, Y]): Parser[T, Y] = p flatMap mfunc

    /** `p >> q === (p ~ q) <| (_._2)`
     */
    def >>[Y](q: => Parser[T, Y]): Parser[T, Y] = (p ~ q) <| (_._2)

    /** `p ?? what === p is what`
     */
    def ??(what: String): Parser[T, X] = p is what
  }
}
