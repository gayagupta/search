package search.src

import scala.xml.Node

object NodeSample {

  val otherNode: Node = xml.XML.loadFile("/Users/gayagupta/Documents/GitHub/search-ggupta3-cweaver5/src/search/src/SmallWiki.xml")

  val mainNode =
    <xml>
      <page>
        <title>hello</title>
        <id>0</id>
        <body>
          This is the body text!
        </body>
      </page>
    </xml>

  def main(args: Array[String]) {
    // These are both the string "This is the body text!"
    val page = (mainNode \ "page").text
    //println(page)
    //println("  dfsfsdf")
    val identification = (mainNode \ "page" \ "id").text.toInt
    println(identification)
    val page0Body = ((mainNode \ "page") \ "body").text
    val page0AltBody = (mainNode \\ "body").text
    //println(page0Body)
   //println("ddfsdsdf")
    // println(page0AltBody)
  }

}
