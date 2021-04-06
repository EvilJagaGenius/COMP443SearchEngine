import scala.util.Random
import scala.collection.parallel.CollectionConverters._

object PageRank {

    def equal(pages: Map[String, WebPage]): Map[String, Double] =
      (for(s <- pages.keys) yield (s -> 1.0)).toMap

    def indegree(pages: Map[String, WebPage]): Map[String, Double] = {
        // TODO: complete implementation
    }

    def pagerank(pages: Map[String, WebPage]): Map[String, Double] = {
        // TODO: complete implementation
    }
}
