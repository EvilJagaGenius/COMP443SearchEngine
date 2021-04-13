import scala.math.log
import scala.collection.parallel.CollectionConverters._

object PageSearch {
    def count(pages: List[RankedWebPage], query: List[String]): List[Double] = {
        for (page <- pages)
            yield {
                for (word <- page.text.split(' '))
                    yield (if (query.contains(formatWord(word))) 1 else 0)
            }.foldLeft(0) {_ + _}
    }

    def tf(pages: List[RankedWebPage], query: List[String]): List[Double] = {
        val countResults = count(pages, query)  // List of doubles
        (for (i <- 1 to countResults.length) yield countResults(i) / pages(i).text.length).toList  // Number of matches / number of characters
    }

    def tfidf(pages: List[RankedWebPage], query: List[String]): List[Double] = {
        // TODO: complete implementation
        return List(2.0)
    }
    
    def formatWord(word:String): String = {
    // Converts the string to lowercase and removes punctuation characters
        word.toLowerCase()
        .replace(',', ' ')
        .replace('.', ' ')
        .replace('!', ' ')
        .replace('?', ' ')
        .replace(':', ' ')
        .replace(';', ' ')
        .replace('(', ' ')
        .replace(')', ' ')
        .trim()
    }
}