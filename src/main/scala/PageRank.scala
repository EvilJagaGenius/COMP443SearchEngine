import scala.util.Random
import scala.collection.parallel.CollectionConverters._

object PageRank {

    def equal(pages: Map[String, WebPage]): Map[String, Double] =
      (for(s <- pages.keys) yield (s -> 1.0)).toMap



    def indegree(pages: Map[String, WebPage]): Map[String, Double] = {
      // TODO: complete implementation

      // Hold all of the links in an iterable
      val allLinks = for((id, page) <- pages; link <- page.links) yield link


      def mapbuilder(
        links: Iterable[String],
        result: Map[String, Double]
      ): Map[String, Double] = links match {

        // If there are no more links, return the result
        case Nil => result


        // Otherwise, make the recursive call on a new result map with
        // the head link's entry incremented by one
        case head :: tail => {
          val weight: Double = result.getOrElse(head, 0)
          mapbuilder(tail, (result + (head -> (weight+1))))
        }
      }

      mapbuilder(allLinks, Map[String, Double]())

    }



    def pagerank(pages: Map[String, WebPage]): Map[String, Double] = {
        // TODO: complete implementation
        (for(s <- pages.keys) yield (s -> 1.0)).toMap
    }
}
