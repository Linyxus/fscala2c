package scala2c.tools.packratc

/** Interface for a token stream, as an input to the packrat parser.
 *
 * @tparam T  The type of produced token.
 */
trait TokenStream[T] {
  /** The lazy list for all tokens.
   */
  val allTokens: List[T]

  /** Push current position into the block indent stack.
   *
   *  The function is called at the beginning of a block. For example:
   *  
   *  {{{
   *    {
   *      val x = 1
   * //   ^ Called here. Push the current indent level of the block.
   *      val y =
   * //   ^ Will produce a NewLine token.
   *        1
   * //     ^ Will NOT produce a NewLine token. Assumed as the same line logically.
   *    }
   * // ^ End of block. Call end block here to pop the indent level.
   *  }}}
   */
  def startBlock: Unit

  /** Pop the indent stack.
   *
   *  The usage has been illustrated in the example of [[TokenStream.startBlock]].
   *
   *  Will throw an error when it is called at the root indent level (indention = 0 at the begining of each source).
   */
  def endBlock: Unit
}
