package scala2c.tools.packratc

/** Interface for a token stream, as an input to the packrat parser.
 *
 *  [[TokenStream.startBlock]] and [[TokenStream.endBlock]] should be called at the beginning and the end of a block.
 *  For example:
 *  ``` scala
 *    {
 *      val x = 1
 *  //  ^ Called here. Push the current indent level of the block.
 *      val y =
 *  //  ^ Will produce a NewLine token.
 *        1
 *  //    ^ Will NOT produce a NewLine token. Assumed as the same line logically.
 *    }
 *  //^ End of block. Call end block here to pop the indent level.
 *  ```
 *  
 * @tparam T  The type of produced token.
 */
trait TokenStream[T] {
  /** The lazy list for all tokens.
   */
  val allTokens: LazyList[T]

  /** Push current position into the block indent stack.
   *
   */
  def startBlock: Unit

  /** Pop the indent stack.
   *
   *  Will throw an error when it is called at the root indent level (indention = 0 at the begining of each source).
   */
  def endBlock: Unit
}
