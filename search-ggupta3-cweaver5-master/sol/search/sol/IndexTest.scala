package search.sol

import tester.Tester

object IndexTest {

  var smallWiki: Index = null
  var testWiki: Index = null
  initIndexer()

  def initIndexer(): Unit = {
    smallWiki = new Index("src/search/src/SmallWiki.xml")
    testWiki = new Index("src/search/src/IndexTest.xml")
  }

  def testTitles(t : Tester) {

    //Normal Cases
    t.checkExpect(smallWiki.getTitles().size, 107)
    t.checkExpect(smallWiki.getTitles().get(0).get,
      "Macro-historical")
    t.checkExpect(smallWiki.getTitles().get(106).get, "War")
    t.checkExpect(smallWiki.getTitles().get(50).get,
      "Carthage Paleo-Christian Museum")
  }

  def testLinks(t : Tester): Unit = {

    t.checkExpect(testWiki.getLinks().size, 5)
    t.checkExpect(testWiki.getLinks()(1).size, 2)

    //Normal Cases
    t.checkExpect(testWiki.getLinks()(1).contains("Lizards"))
    t.checkExpect(testWiki.getLinks()(1).contains("Dogs"))
    t.checkExpect(testWiki.getLinks()(3).contains("Snakes"))

    //Page with no links
    t.checkExpect(testWiki.getLinks()(2).size, 0)

    //Does not contain links to outside the wiki
    t.checkExpect(!testWiki.getLinks()(1).contains("Dog:Animals"))
    t.checkExpect(!testWiki.getLinks()(3).contains("Category:Obesity"))

    //Does not contain links to itself
    t.checkExpect(!testWiki.getLinks()(3).contains("Lizards"))
  }

  def testMaxWordCount(t : Tester): Unit = {
    t.checkExpect(testWiki.getMaxWordCount().size, 5)
    t.checkExpect(testWiki.getMaxWordCount()(1), 4)
    t.checkExpect(testWiki.getMaxWordCount()(2), 1)
    t.checkExpect(testWiki.getMaxWordCount()(3),2)

    //Empty page
    t.checkExpect(testWiki.getMaxWordCount()(4), 1)

    //Words count as same if stemmed word is identical
    t.checkExpect(testWiki.getMaxWordCount()(5), 4)
  }

  def testWordFreq(t : Tester): Unit = {

    //Normal Cases
    t.checkExpect(testWiki.getWordFreq().get("cat").get.size, 3)
    t.checkExpect(testWiki.getWordFreq().get("cat").get.contains(1))
    t.checkExpect(testWiki.getWordFreq().get("cat").get.contains(5))
    t.checkExpect(testWiki.getWordFreq().get("cat").get(1),4)
    t.checkExpect(testWiki.getWordFreq().get("cat").get(5), 1)
    t.checkExpect(testWiki.getWordFreq().get("dog").get(1), 3)
    t.checkExpect(testWiki.getWordFreq().get("dog").get(2), 1)
  }

  def testPageRank(t : Tester): Unit ={

    //Compare pagerank values to example #1 from project handout
    val pagerankTestWiki1 =
      new Index("src/search/src/PageRankTestWiki1.xml")

    t.checkExpect(0.44 > pagerankTestWiki1.getPageRankMap().get(1).get
      & pagerankTestWiki1.getPageRankMap().get(1).get > .43)
    t.checkExpect(0.24 > pagerankTestWiki1.getPageRankMap().get(2).get
      & pagerankTestWiki1.getPageRankMap().get(2).get > .23)
    t.checkExpect(0.34 > pagerankTestWiki1.getPageRankMap().get(3).get
      & pagerankTestWiki1.getPageRankMap().get(3).get > .33)


    //Compare pagerank values to example #2 from project handout
    val pagerankTestWiki2 =
      new Index("src/search/src/PageRankTestWiki2.xml")

    t.checkExpect(0.21 > pagerankTestWiki2.getPageRankMap().get(1).get
      & pagerankTestWiki2.getPageRankMap().get(1).get > .20)
    t.checkExpect(0.04 > pagerankTestWiki2.getPageRankMap().get(2).get
      & pagerankTestWiki2.getPageRankMap().get(2).get > .03)
    t.checkExpect(0.38 > pagerankTestWiki2.getPageRankMap().get(3).get
      & pagerankTestWiki2.getPageRankMap().get(3).get > .37)
    t.checkExpect(0.39 > pagerankTestWiki2.getPageRankMap().get(4).get
      & pagerankTestWiki2.getPageRankMap().get(4).get > .38)


    t.checkExpect(Math.abs(smallWiki.getPageRankMap().values.sum - 1) < 0.0001)
    t.checkExpect(Math.abs(testWiki.getPageRankMap().values.sum - 1) < 0.0001)
    t.checkExpect(
      Math.abs(pagerankTestWiki1.getPageRankMap().values.sum - 1) < 0.0001)
    t.checkExpect(
      Math.abs(pagerankTestWiki2.getPageRankMap().values.sum - 1) < 0.0001)

  }




}
object Main extends App {
  Tester.run(IndexTest)
}