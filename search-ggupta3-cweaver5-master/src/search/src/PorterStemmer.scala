/**
  *
  * Porter stemmer in Scala. The original paper is in
  *
  *    Porter, 1980, An algorithm for suffix stripping, Program, Vol. 14,
  *    no. 3, pp 130-137,
  *
  * See also http://www.tartarus.org/~martin/PorterStemmer
  *
  * A few methods were borrowed from the existing Java port from the above page.
  *
  * Modified by iberkowi Jan. 2013
  * Modified by gpittali Jan. 2016
  */
package search.src

import scala.io.Source

/**
  * The object Porter Stemmer provides a singleton that contains methods for
  * stemming a single word, a string or an array of words.
  * An instance of a Stemmer must be provided to these methods.
  * Note that the words to be stemmed must be in lower case.
  */
object PorterStemmer extends Stemmer {

  /**
    * Stems one word
    *
    * @param input - the word to be stemmed
    * @param stemmer - the Stemmer to be used
    * @return - the stemmed word
    */
  def stemOneWord(input: String, stemmer: PorterStemmer): String = {
    stemmer.add(input.trim())

    if (stemmer.word.length > 2) {
      stemmer.step1()
      stemmer.step2()
      stemmer.step3()
      stemmer.step4()
      stemmer.step5a()
      stemmer.step5b()
    }

    stemmer.word
  }

  override def stem(input: String): String = {
    var stemmer = new PorterStemmer()
    val wordList: Array[String] = input.split(" ")
    var toReturn = ""

    for (word: String <- wordList) {

      var l = word.trim()

      stemmer.add(l)

      if (stemmer.word.length > 2) {
        stemmer.step1()
        stemmer.step2()
        stemmer.step3()
        stemmer.step4()
        stemmer.step5a()
      }

      toReturn += stemmer.word + " "
    }

    toReturn.trim()
  }

  override def stemArray(input: Array[String]): Array[String] = {
    val s = new PorterStemmer()
    input.map(x => stemOneWord(x, s))
  }

}

class PorterStemmer {

  // word to be stemmed
  var word: String = ""

  /**
    * Determines if the ith character of the word to be stemmed is a consonant
    *
    * @param i - the index of the character checked in the word to be stemmed
    * @return - true if the ith character of the word to be stemmed is a consonant,
    *           false if otherwise
    */
  def cons(i: Int): Boolean = {
    var ch: Char = word(i)

    // magic!
    var vowels: String = "aeiou"

    // word(i) is a vowel
    if (vowels.contains(ch))
      return false

    // word(i) is a 'y', need to check the character before it
    if (ch == 'y') {
      if (i == 0) {
        // 'y' is the first character in word
        return true
      } else {
        // loop it!
        return !cons(i - 1)
      }
    }

    // word(i) is a consonant
    true
  }

  /**
    * Adds a character to the word to be stemmed
    *
    * @param ch - the character to be added to the word
    */
  def add(ch: Char) = {
    word += ch
  }

  /**
    * Sets the word to be stemmed to a given word
    *
    * @param newWord - the new word to be stemmed
    */
  def add(newWord: String) = {
    word = newWord
  }

  /**
    * Calculates the quantity m(s) of a given string s.
    * m(s) measures the number of consonant sequences in s. if c is
    * a consonant sequence and v a vowel sequence, and <..> indicates arbitrary
    * presence,
    *    <c><v>       gives 0
    *    <c>vc<v>     gives 1
    *    <c>vcvc<v>   gives 2
    *    <c>vcvcvc<v> gives 3
    *    ....
    *
    * @param s - the string whose number of consonant sequences has to be calculated
    * @return - m(s), the number of consonant sequences in s
    *
    * I think this can be recoded far more neatly.
    */
  def calcM(s: String): Int = {
    // the number of consonant sequences
    var count: Int = 0
    // indicates whether we currently are in a consonant sequence
    var currentConst: Boolean = false

    var l: Int = s.length
    for (c <- 0 to l - 1) {
      if (cons(c)) {
        if (!currentConst && c != 0) {
          count += 1
        }
        currentConst = true
      } else {
        currentConst = false
      }
    }

    count
  }

  /**
    * Determines whether, if we were to remove the suffix 's' from our word,
    * the word to be stemmed still has one or more vowels
    *
    * @param s - the suffix to remove from the word to be stemmed
    * @return - true if after removing 's' our word contains vowels, false otherwise
    */
  def vowelInStem(s: String): Boolean = {
    for (i <- 0 to word.length - 1 - s.length) {
      if (!cons(i)) {
        return true
      }
    }

    false
  }

  /**
    * Determines whether the last and penultimate characters of the word
    * are both the same consonant
    *
    * @return - true if the last and penultimate characters of the word
    *           are the same consonant, false otherwise
    */
  def doublec(): Boolean = {
    var i: Int = word.length - 1

    if (i < 1)
      return false

    if (word(i) != word(i - 1))
      return false

    cons(i)
  }

  /**
    * Determines if the last three characters of a given string constitute a cvc.
    * cvc(i) is true <=> i-2,i-1,i has the form consonant - vowel - consonant
    * and also if the second c is not w,x or y. this is used when trying to
    * restore an e at the end of a short word. e.g.
    *
    *    cav(e), lov(e), hop(e), crim(e), but
    *    snow, box, tray.
    *
    * @param s - the string to be checked
    * @return - true if the last three characters of 's' are a cvc sequence,
    *           false otherwise
    */

  def cvc(s: String): Boolean = {
    var i = word.length - 1 - s.length
    if (i < 2 || !cons(i) || cons(i - 1) || !cons(i - 2))
      return false;

    var ch: Char = word(i)

    var vals: String = "wxy"

    if (vals.contains(ch))
      return false

    true
  }

  /**
    * Replaces a suffix of the word to be stemmed with a given replacement iff:
    * - The suffix to be replaced matches a given string
    * - The m(word) (the number of consonant sequences) satisfies the given checker function
    * If the suffix matches the given string but the m number doesn't satisfy the checker,
    * then the suffix is simply removed from the word.
    *
    * @param orig - the string that might match the suffix of our word
    * @param replace - the given replacement
    * @param checker - the function that checks the number of consonant sequences
    * @return - true if the suffix of our word has been replaced, false otherwise
    */
  def replacer(orig: String, replace: String, checker: Int => Boolean): Boolean = {

    var l: Int = word.length
    var origLength: Int = orig.length
    var res: Boolean = false

    // check if suffix matches
    if (word.endsWith(orig)) {
      var n: String = word.substring(0, l - origLength)

      var m: Int = calcM(n)
      // check if m satisfies the checker function
      if (checker(m)) {
        word = n + replace
      }
      // change has been done (either removal or replacement)
      res = true
    }

    res
  }

  /**
    * Processes a list of tuples.
    * Each tuples is composed by a string that might match the suffix of our word,
    * and a string that can be used as a replacement (see the replacer() method)
    *
    * @param l - the list of tuples to be processed
    * @param checker - the function that is to be passed to the replacer() method
    * @return - true if the word has been changed, false otherwise
    */
  def processSubList(l: List[(String, String)], checker: Int => Boolean): Boolean = {
    var done: Boolean = false

    for (v <- l) {
      done = replacer(v._1, v._2, checker)
      if (done) // word has been changed
        return true
    }

    done
  }

  /**
    * step1() deals with plurals and -ed or -ing. e.g.
    *
    *     caresses  ->  caress
    *     ponies    ->  poni
    *     ties      ->  ti
    *     caress    ->  caress
    *     cats      ->  cat
    *     feed      ->  feed
    *     agreed    ->  agree
    *     disabled  ->  disable
    *
    *     matting   ->  mat
    *     mating    ->  mate
    *     meeting   ->  meet
    *     milling   ->  mill
    *     messing   ->  mess
    *     meetings  ->  meet
    */
  def step1() {
    var l: Int = word.length
    var m: Int = calcM(word)

    // step 1a
    var vals = List(("sses", "ss"), ("ies", "i"), ("ss", "ss"), ("s", ""))
    processSubList(vals, _ >= 0)

    // step 1b
    if (!(replacer("eed", "ee", _ > 0))) {

      if ((vowelInStem("ed") && replacer("ed", "", _ >= 0)) ||
        (vowelInStem("ing") && replacer("ing", "", _ >= 0))) {

        vals = List(("at", "ate"), ("bl", "ble"), ("iz", "ize"))

        if (!processSubList(vals, _ >= 0)) {
          // if this isn't done, then it gets more confusing.

          m = calcM(word)
          var last: Char = word(word.length - 1)
          if (doublec() && !"lsz".contains(last)) {
            word = word.substring(0, word.length - 1)
          } else if (m == 1 && cvc("")) {
            word = word + "e"
          }
        }
      }
    }

    // step 1c
    (vowelInStem("y") && replacer("y", "i", _ >= 0))
  }

  /**
    * step2() maps double suffices to single ones.
    * For example, -ization ( = -ize plus -ation) maps to -ize etc.
    * Note that the string before the suffix must give m() > 0.
    */
  def step2() = {

    var vals = List(("ational", "ate"), ("tional", "tion"), ("enci", "ence"),
      ("anci", "ance"), ("izer", "ize"), ("bli", "ble"), ("alli", "al"),
      ("entli", "ent"), ("eli", "e"), ("ousli", "ous"), ("ization", "ize"),
      ("ation", "ate"), ("ator", "ate"), ("alism", "al"), ("iveness", "ive"),
      ("fulness", "ful"), ("ousness", "ous"), ("aliti", "al"), ("iviti", "ive"),
      ("biliti", "ble"), ("logi", "log"))

    processSubList(vals, _ > 0)

  }

  /**
    * step3() deals with -ic-, -full, -ness etc.
    * It uses the same strategy as step2().
    */
  def step3() = {
    var vals = List(("icate", "ic"), ("ative", ""), ("alize", "al"),
      ("iciti", "ic"), ("ical", "ic"), ("ful", ""), ("ness", ""))

    processSubList(vals, _ > 0)
  }

  /**
    * step4() takes off -ant, -ence etc., in context <c>vcvc<v>.
    */
  def step4() = {
    // first part.
    var vals = List(("al", ""), ("ance", ""), ("ence", ""), ("er", ""),
      ("ic", ""), ("able", ""), ("ible", ""), ("ant", ""), ("ement", ""),
      ("ment", ""), ("ent", ""))

    var res = processSubList(vals, _ > 1)

    // special part.
    if (!res) {
      if (word.length > 4) {
        if (word(word.length - 4) == 's' || word(word.length - 4) == 't') {
          res = replacer("ion", "", _ > 1)

        }
      }
    }

    // third part.
    if (!res) {
      var vals = List(("ou", ""), ("ism", ""), ("ate", ""), ("iti", ""),
        ("ous", ""), ("ive", ""), ("ize", ""))
      res = processSubList(vals, _ > 1)

    }
  }

  /**
    * step5a() removes a final -e if m() > 1.
    */
  def step5a() = {
    replacer("e", "", _ > 1)

    if (!cvc("e")) {
      replacer("e", "", _ == 1)
    }
  }

  /**
    * step5a() removes a final -l if the word ends with -ll and m() > 1.
    */
  def step5b() = {
    var m = calcM(word)
    if (m > 1 && doublec() && word.endsWith("l")) {
      word = word.substring(0, word.length - 1)
    }
  }

}
