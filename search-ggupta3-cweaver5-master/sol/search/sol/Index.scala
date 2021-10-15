package search.sol

import search.src.{FileIO, PorterStemmer, StopWords}

import java.io.{FileNotFoundException, IOException}
import scala.collection.mutable
import scala.util.matching.Regex
import scala.xml.{Node, NodeSeq}

/**
 * Provides an XML indexer, produces files for a querier
 *
 * @param inputFile - the filename of the XML wiki to be indexed
 */
class Index(val inputFile: String) {

  /**
   * Global data to fill for index
   */
  private var titles: mutable.HashMap[Int, String] = null
  private var maxWordCount: mutable.HashMap[Int, Double] = null
  private var wordFreq: mutable.HashMap[String, mutable.HashMap[Int, Double]]
  = null
  private var links: mutable.HashMap[Int, mutable.Set[String]] = null
  private var pagerankMap:mutable.HashMap[Int, Double] = null

  /**
   * Parse and create pagerank data and fill in corresponding files
   */
  parse()
  pageRank()
  FileIO.printWordsFile("words.txt",wordFreq)
  FileIO.printTitleFile("titles.txt", titles)
  FileIO.printDocumentFile("docs.txt", maxWordCount, pagerankMap)

  private def importXMLNode(): Array[xml.Node] = {

    val otherNode: Node = xml.XML.loadFile(inputFile)
    (otherNode \ "page").toArray[xml.Node]
  }
  /**
   * Method to parse an xml file
   */
  private def parse() = {

    val pageSeq = importXMLNode()
    val regex = new Regex("""\[\[[^\[]+?\]\]|[^\W_]+'[^\W_]+|[^\W_]+""")
    val stopWords = StopWords
    val stemmer = PorterStemmer
    var identification: Int = 0
    var title: String = ""

    //Hashmaps of how we plan to store info
    if (pageSeq.size != 0) {
      titles =
        new mutable.HashMap[Int, String]
      maxWordCount =
        new mutable.HashMap[Int, Double]
      wordFreq = new mutable.HashMap[String, mutable.HashMap[Int, Double]]
      ((pageSeq.size / 0.75).toInt, 1.0)
      links = new mutable.HashMap[Int, mutable.Set[String]]
      ((pageSeq.size / 0.75).toInt, 1.0)
    }

    //Goes through all the pages, adds titles and links to their corresponding
    //hashmaps, and runs pageIterator to process each page.
    //Sets the pageSeq = null at the end to save space!
    for (i <- pageSeq.indices) {
      identification =
        (pageSeq(i) \ "id").text.replace("\n", "").toInt
      title = (pageSeq(i) \ "title").text.replace("\n", "")
      titles(identification) = title
      links(identification) = mutable.Set[String]()
      pageIterator(generateWordList(pageSeq(i)), identification)
      pageSeq(i) = null
    }

    /**
     * Method to generate a list of words from a page of the xml file
     * @param page to extract words from
     * @return list of Strings of regex'ed words
     */
    def generateWordList(page : Node): List[String] = {
      val matchesIterator =
        regex.findAllMatchIn((page \ "text").text + " " + title)
      matchesIterator.toList.map { aMatch => aMatch.matched }
    }

    /**
     * Method to iterate through list of words to insert words into wordFreq,
     * links (when appropriate)
     * @param matchesList list of String words to iterate through
     * @param id identification number of page
     */
    def pageIterator(matchesList: List[String], id: Int): Unit = {

      var newWord: String = null
      var linkToAdd: String = null
      var firstHalf: Iterator[Regex.Match] = null
      var firstList: List[String] = null
      var secondHalf: Iterator[Regex.Match] = null
      var secondList: List[String] = null

      for (word <- matchesList) {

        //Check if the word is a category
        if (checkLink(word)) {

          //Remove Square Brackets
          newWord = word.substring(2, word.length - 2)


          //Link with ':' => All words are processed, link is first half
          if (newWord.contains(':')) {
            firstHalf = regex.findAllMatchIn(newWord.
              substring(0, newWord.indexOf(':')))

            firstList = firstHalf.toList.map { aMatch => aMatch.matched }
            secondHalf = regex.findAllMatchIn(newWord.
              substring(newWord.indexOf(':') + 1))

            secondList = secondHalf.toList.map { aMatch => aMatch.matched }

            //Process all words
            pageIterator(firstList, id)
            pageIterator(secondList, id)

            //Add link
            linkToAdd = newWord

          } else if (newWord.contains('|')) {
            //Link contains '|', before pipe is link, after pipe is text

            secondHalf = regex.findAllMatchIn(newWord.
              substring(newWord.indexOf('|') + 1))

            secondList = secondHalf.toList.map { aMatch => aMatch.matched }
            pageIterator(secondList, id)

            //Link is text before pipe
            linkToAdd = newWord.substring(0, newWord.indexOf('|'))
            newWord = linkToAdd

          } else {
            secondHalf = regex.findAllMatchIn(newWord)
            secondList = secondHalf.toList.map { aMatch => aMatch.matched }
            pageIterator(secondList, id)

            linkToAdd = newWord
          }

          links.get(id) match {
            case Some(value) => {

              //Links to itself should not be added
              if (!newWord.equals(titles(id))) {
                value += linkToAdd
              }
            }
            case None => links(id) = mutable.Set[String](linkToAdd)


          }

        } else {
          insertWord(word.toLowerCase())
        }

        /**
         * Method to insert a word into wordFreq and change maxWordCount
         * when needed
         *
         * Note: We include it as a local method within parse() because we want
         * to only generate a stemmer and a stopWords object once, rather than
         * call it every time we want to insert a word into our hashmap. Since
         * we cannot pass in objects in our methods, we simply call the
         * stemmer/stopWords at the beginning of page Iterator and use it in
         * insertWord
         *
         * @param word String of word to insert
         */
        def insertWord(word: String): Unit = {

          var stemmedWord: String = null

          maxWordCount.get(id) match {
            case Some(value) =>
            case None => maxWordCount(id) = 0
          }

          //Process word if it is not a stop word
          if (!stopWords.isStopWord(word)) {
            stemmedWord = stemmer.stem(word)

            //Check if wordFreq has a key for the word and increment frequency
            wordFreq.get(stemmedWord) match {
              case Some(innerHash) =>{
                innerHash.get(id) match {
                  case Some(freqVal) => innerHash(id) = freqVal + 1.0
                  case None => innerHash(id) = 1.0
                }
              }
              case None => wordFreq(stemmedWord) =
                mutable.HashMap[Int, Double](id -> 1.0)
            }

            //Update maxWordCount if necessary
            maxWordCount(id) = Math.max(wordFreq(stemmedWord)
            (id), maxWordCount(id))

          }
        }
      }

    }
  }

  /**
   * Method to check if a String is a link
   * @param word String to check
   * @return boolean whether it is a link
   */
  private def checkLink(word: String): Boolean = {
    try {
      (word.length >= 4) & (word.charAt(0) == '[') & (word.charAt(0) == '[') &
        (word.charAt(word.length - 1) == ']' &
          (word.charAt(word.length - 2) == ']'))
    } catch {
      case e: IndexOutOfBoundsException => false
    }
  }

  /**
   * Method to remove links that do not link to corpus
   */
  private def removeOutsideLinks() = {

    val outsideLinks = mutable.HashSet[String]()
    val titleValues: mutable.HashSet[String] = mutable.HashSet[String]()

    //Create set of titles
    for ((key, value) <- titles) {
      titleValues.add(value)
    }

    //Remove links if they link to outside of Corpus
    for ((key, value)  <- links) {
      value.foreach(link => {
        if(!titleValues.contains(link)
        )  {value.remove(link)}

      })
    }
  }

  /**
   * Method to create a hashmap of weights between two pages
   * @return hashmap of hashmaps full of weights
   */
  private def createWeight():
  mutable.HashMap[Int, mutable.HashMap[Int, Double]] = {

    val weightJK: mutable.HashMap[Int, mutable.HashMap[Int, Double]] =
      new mutable.HashMap[Int, mutable.HashMap[Int, Double]]
    var weight: Double = 0.0
    val epsilon = 0.15
    val corpusLength = titles.size.asInstanceOf[Double]

    removeOutsideLinks()

    for ((idOuter, linksOuter) <- links) {
      for ((idInner, linksInner) <- links) {

        //Create new inner hashmap if it doesn't already exist
        weightJK.get(idOuter) match {
          case None => weightJK(idOuter) = new mutable.HashMap[Int, Double]
          case _ =>
        }

        //Calculate weight between pages k and j
        if(linksInner.isEmpty & (idOuter != idInner)) {
          //Link to nowhere is a link to everywhere except itself

          weight = (1-epsilon) / (corpusLength - 1.0)
          weight = weight + epsilon / corpusLength
          weightJK(idOuter)(idInner) = weight

        } else if (linksInner.contains(titles(idOuter))) {

          //When k links to j

          weight = (1-epsilon) / (linksInner.size.asInstanceOf[Double])
          weight = weight + epsilon / corpusLength
          weightJK(idOuter)(idInner) = weight

        }
      }
    }
    weightJK
  }

  /**
   * Method to calculate the pagerank and insert values into pagerankMap
   */
  private def pageRank(): Unit = {
    val n: Double = titles.size
    val weights = createWeight()
    var r: mutable.HashMap[Int, Double] = null
    var r_prime = createIDtoRank(1.0 / n)
    var summedRanks: Option[Double] = None
    var distance = 1.0

    while (distance > 0.0001) {
      computePageRank()
    }
    pagerankMap = r


    /**
     * Method to update single iteration of ranks for pagerank()
     */
    def computePageRank() = {
      r = r_prime
      r_prime = new mutable.HashMap[Int, Double]()

      //Iterate through r and update rank
      for ((j, outerValue) <- r) {
        r_prime(j) = 0.0

        for ((k, innerValue) <- r) {
          weights(j).get(k) match {
            case Some(value) => r_prime(j) = r_prime(j) + (value* r(k))
            case None =>r_prime(j) = r_prime(j) + ((0.15 / n) * r(k))
          }
        }
        summedRanks match {
          case Some(value) =>
            summedRanks = Some(value + math.pow(r(j) - r_prime(j), 2))
          case None => summedRanks = Some(math.pow(r(j) - r_prime(j), 2))
        }
      }
      distance = math.sqrt(summedRanks.get)
      summedRanks = Some(0.0)
    }
  }

  /**
   * Method to create a Hashmap of Int to Double
   * @param initialValue value to set all values of Hashmap to
   * @return filled Hashmap
   */
  private def createIDtoRank(initialValue: Double):
  mutable.HashMap[Int, Double] = {
    val hashmapOfZeros: mutable.HashMap[Int, Double] =
      new mutable.HashMap[Int, Double]

    for ((idOuter, linksOuter) <- links) {
      hashmapOfZeros(idOuter)= initialValue
    }
    hashmapOfZeros
  }

  def getWordFreq(): mutable.HashMap[String, mutable.HashMap[Int, Double]] = {
    this.wordFreq.clone()
  }
  def getTitles(): mutable.HashMap[Int, String] = {
    this.titles.clone()
  }
  def getLinks(): mutable.HashMap[Int, mutable.Set[String]] = {
    this.links.clone()
  }
  def getMaxWordCount(): mutable.HashMap[Int, Double] = {
    this.maxWordCount.clone()
  }
  def getPageRankMap(): mutable.HashMap[Int, Double] = {
    this.pagerankMap.clone()
  }

}



object Index {
  def main(args: Array[String]) {
    try {
      // Run queries with page rank
      if (args.size == 4) {

        val index = new Index(args(0))
        FileIO.printTitleFile(args(1), index.titles)
        FileIO.printDocumentFile(args(2), index.maxWordCount, index.pagerankMap)
        FileIO.printWordsFile(args(3), index.wordFreq)

      } else {
        println("Incorrect arguments. Please use [filepath to wiki] <titles text file> "
          + "<docs text file> <words text file>")
        System.exit(1)
      }
    } catch {
      case _: FileNotFoundException =>
        println("One (or more) of the files were not found")
      case _: IOException => println("Error: IO Exception")
    }
  }
}