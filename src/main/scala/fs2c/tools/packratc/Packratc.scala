package fs2c.tools.packratc

import fs2c.tools.packratc.syntax.ParserSyntax

/** Packratc is a toolkit of packrat parser combinators for Parsing Expression Grammar (PEG).
 *  PEGs can describe a wide set of formal language, yet less powerful than CFGs.
 *  It is known that packrat parsers can parse PEGs in linear time, and it takes up extra
 *  memory space (for memoization) of O(N x S) where N is the number of symbols in the grammar,
 *  and S is the source length. Since the grammar is fixed, therefore the space complexity is also
 *  linear to the source size.
 */
object Packratc extends ParserSyntax with ParserFunctions
