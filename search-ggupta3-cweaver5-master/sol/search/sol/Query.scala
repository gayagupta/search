package search.sol

import java.io._
import search.src.{FileIO, PorterStemmer, StopWords}
import scala.collection.mutable.HashMap

/**
 * Represents a query REPL built off of a specified index
 *
 * @param titleIndex    - the filename of the title index
 * @param documentIndex - the filename of the document index
 * @param wordIndex     - the filename of the word index
 * @param usePageRank   - true if page rank is to be incorporated into scoring
 */
class Query(titleIndex: String, documentIndex: String, wordIndex: String,
            usePageRank: Boolean) {

  // Maps the document ids to the title for each document
  private val idsToTitle = new HashMap[Int, String]

  // Maps the document ids to the euclidean normalization for each document
  private val idsToMaxFreqs = new HashMap[Int, Double]

  // Maps the document ids to the page rank for each document
  private val idsToPageRank = new HashMap[Int, Double]

  // Maps each word to a map of document IDs and frequencies of documents that
  // contain that word
  private val wordsToDocumentFrequencies =
  new HashMap[String, HashMap[Int, Double]]


  /**
   * Handles a single query and prints out results
   *
   * @param userQuery - the query text
   */
  private def query(userQuery: String) {

    val stopWords = StopWords
    val stemmer = PorterStemmer
    //listOfWords is an array of each of the words inputted in the query
    var listOfWords = userQuery.split(" ")
    listOfWords = listOfWords.map(f => f.toLowerCase)
    listOfWords = listOfWords.filter(f =>
      (!stopWords.isStopWord(f) & wordsToDocumentFrequencies.contains(f)))
    listOfWords = stemmer.stemArray(listOfWords)

    //We filter to get rid of stopWords and also to only calculate relevancies
    //for words in our corpus
    //if none of the query's words are in corpus, we return an error message
    if (listOfWords.isEmpty) {
      println("Sorry, your query yielded no results.")
      //otherwise, we calculate the scores and print them from 1-10, or as many
      // as we can if we score < 10 pages
    } else {
      val scores = calculateScore(listOfWords)
      var keys: Array[Int] = scores.keys.toArray
      keys = keys.sortWith((l, r) => scores(l) > scores(r))
      printResults(keys)
    }
  }

  /**
   * Calculates the relevancy score for a query (represented by an array of
   * strings) and returns the scores for each of the pages they are found on in
   * a Hashmap of Ints mapping to doubles
   *
   * @param listOfWords - the words in a query, represented by an array of
   *                    strings
   * @return - a hashmap mapping page IDs of every page each query word is
   *         found in to relevancy scores
   */
  private def calculateScore(listOfWords: Array[String]):
  HashMap[Int, Double] = {

    //Hashmap to stores scores of documents
    val scores = new HashMap[Int, Double]

    //first, we go through each word in our query
    for (word <- listOfWords) {

      //We find all the pages that contain a given word
      for ((id, freq) <- wordsToDocumentFrequencies(word)) {

        //We check if we've already calculated the score for a certain page by
        // checking if it's already in scores
        if (!scores.contains(id)) {

          //for each page, we set the summedTotal to 0
          var summedTotal = 0.0

          //next, we go through each word (eachWord) in the query, and
          //calculate the word's relevancy to our page (the id) and add it to
          //the summedTotal

          for (eachWord <- listOfWords) {
            //we calculate relevancy using the word's frequency and the given id
            if (wordsToDocumentFrequencies(eachWord).contains(id)) {
              summedTotal +=
                relevancy(eachWord, wordsToDocumentFrequencies(eachWord)(id), id)
            }
          }

          //if the query's initial arguments include page rank, we multiply the
          //page's pageRank value to the summedTotal before returning it
          if (usePageRank) {
            scores(id) = idsToPageRank(id) * summedTotal
          } else {
            //if it doesn't use page rank, we add the id to the score hashmap
            scores(id) = summedTotal
          }
        }
      }
    }
    scores
  }

  /**
   * @param eachWord the given word in a corpus whose term frequency and Inverse
   *                 Doc Freq we're calculating
   * @param frequency the frequency of eachWord in a given ID
   * @param id the id of a page that contains eachWord
   * @return
   */
  private def relevancy(eachWord: String,
                        frequency: Double, id: Int): Double = {

    //idsToTitle.size represents the total number of pages in a corpus
    //wordsToDocumentFrequencies(eachWord).size represents the total number of
    //pages containing eachWord
    //idsToMaxFreqs is the number of occurances of the most frequently occurring
    //term in page id
    (math.log(idsToTitle.size.toDouble /
      wordsToDocumentFrequencies(eachWord).size.toDouble) *
      (frequency / idsToMaxFreqs(id)))
  }


  /**
   * Format and print up to 10 results from the results list
   *
   * @param results - an array of all results to be printed
   */
  private def printResults(results: Array[Int]) {
    for (i <- 0 until math.min(10, results.size)) {
      println("\t" + (i + 1) + " " + idsToTitle(results(i)))
    }
  }

  /**
   * Reads in the text files.
   */
  def readFiles(): Unit = {
    FileIO.readTitles(titleIndex, idsToTitle)
    FileIO.readDocuments(documentIndex, idsToMaxFreqs, idsToPageRank)
    FileIO.readWords(wordIndex, wordsToDocumentFrequencies)
  }

  /**
   * Starts the read and print loop for queries
   */
  def run() {
    val inputReader = new BufferedReader(new InputStreamReader(System.in))

    // Print the first query prompt and read the first line of input
    print("search> ")
    var userQuery = inputReader.readLine()

    // Loop until there are no more input lines (EOF is reached)
    while (userQuery != null) {
      // If ":quit" is reached, exit the loop
      if (userQuery == ":quit") {
        inputReader.close()
        return
      }

      // Handle the query for the single line of input
      query(userQuery)

      // Print next query prompt and read next line of input
      print("search> ")
      userQuery = inputReader.readLine()
    }
    inputReader.close()
  }
}

object Query {
  def main(args: Array[String]) {
    try {
      // Run queries with page rank
      var pageRank = false
      var titleIndex = 0
      var docIndex = 1
      var wordIndex = 2
      if (args.size == 4 && args(0) == "--pagerank") {
        pageRank = true;
        titleIndex = 1
        docIndex = 2
        wordIndex = 3
      } else if (args.size != 3) {
        println("Incorrect arguments. Please use [--pagerank] <titleIndex> "
          + "<documentIndex> <wordIndex>")
        System.exit(1)
      }
      val query: Query =
        new Query(args(titleIndex), args(docIndex), args(wordIndex), pageRank)
      query.readFiles()
      query.run()
    } catch {
      case _: FileNotFoundException =>
        println("One (or more) of the files were not found")
      case _: IOException => println("Error: IO Exception")
    }
  }
}