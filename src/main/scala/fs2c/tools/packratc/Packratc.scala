package fs2c.tools.packratc

import fs2c.tools.packratc.syntax.ParserSyntax

/** Packratc is a toolkit of packrat parser combinators for Parsing Expression Grammar (PEG).
 *  PEGs can describe a wide set of formal language, yet less powerful than CFGs.
 *  It is known that packrat parsers can parse PEGs in linear time.
 */
object Packratc extends ParserSyntax with ParserFunctions
