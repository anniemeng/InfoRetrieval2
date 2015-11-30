package Parsing

abstract class ParsedXMLStream (val unparsed: DocStream) {
  def stream : Stream[XMLDocument]
  def length : Int

}