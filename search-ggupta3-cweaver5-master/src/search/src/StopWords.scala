package search.src

import java.io.BufferedReader
import java.io.FileReader
import java.io.IOException
import scala.collection.mutable.HashSet

/**
  * The StopWords object provides two methods to check if a given
  * word is a stop word and should be ignored in the search engine
  */
object StopWords {

  val words = {
    val wordsArray = Array(
      "a", "about", "above", "after", "against", "all", "am", "an",
      "and", "any", "are", "aren't", "as", "at", "be", "because", "been",
      "before", "be", "below", "between", "both", "but", "by", "can't",
      "cannot", "could", "couldn't", "did", "didn't", "does", "doesn't",
      "do", "don't", "down", "each", "few", "for", "from", "further",
      "had", "hadn't", "hasn't", "have", "haven't", "he", "he'd", "he'll",
      "her", "here", "herself", "him", "himself", "hi", "how", "i", "i'd",
      "i'll", "i'm", "i've", "if", "in", "into", "is", "isn't", "it", "it's",
      "its", "itself", "let's", "me", "more", "most", "mustn't", "my",
      "myself", "no", "nor", "not", "of", "off", "on", "or", "other",
      "ought", "our", "ours", "ourselves", "out", "over", "own", "same",
      "shan't", "she", "she'd", "she'll", "she's", "should", "shouldn't",
      "so", "some", "such", "than", "that", "that's", "the", "their", "them",
      "themselves", "then", "there", "there's", "these", "their", "they'd",
      "they'll", "they're", "they've", "this", "those", "through", "to", "too",
      "under", "until", "up", "was", "wasn't", "we", "we'd", "we'll", "we're",
      "we've", "were", "weren't", "what", "what's", "when", "when's", "where",
      "where's", "which", "while", "who", "who's", "whom", "why", "why's", "with",
      "won't", "would", "wouldn't", "you", "you'd", "you'll", "you're", "you've",
      "your", "yourself", "yourselves", "follow")
    val wordsSet = new HashSet[String]
    for (word <- wordsArray) {
      wordsSet += word
    }
    wordsSet
  }

  /**
    * Determines if the given word is a stop word, i.e., was contained
    * in the stop words file.
    *
    * @param word A word, as it appears in the corpus/query
    * @return True if the given word is literally a stop word.
    *  False otherwise
    */
  def isStopWord(word: String) = words.contains(word.toLowerCase)

}
