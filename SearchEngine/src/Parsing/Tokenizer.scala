package Parsing

import main._

object Tokenizer {
  def tokenize (text: String) : List[String] =
    text.toLowerCase.split("[ .,;:?!\t\n\r\f-]+").toList.filterNot(main.stopWords)
}