package search.src
/**
  * Provides a contract for Porter Stemmer (and other Stemmers).
  * Serves as an API.
  */
trait Stemmer {
  /**
    * Stems an array of words
    *
    * @param input - the array of words to be stemmed
    * @return - an array containing the stemmed words
    */
  def stemArray(input: Array[String]): Array[String]

  /**
    * Stems a string of words separated by spaces
    *
    * @param input - the string of words to be stemmed
    * @return - a string containing the stemmed words separated by spaces
    */
  def stem(input: String): String
}
