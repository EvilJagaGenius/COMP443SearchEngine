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
        (for (i <- 0 until countResults.length) yield countResults(i) / pages(i).text.length).toList  // Number of matches / number of characters
    }

    def tfidf(pages: List[RankedWebPage], query: List[String]): List[Double] = {
        val tfResults = tf(pages, query)
        val idfMultiplier = {
            for (word <- query)
                yield idf(pages, word)
        }.foldLeft(1.0) {_ * _}
        return tfResults.map(_ * idfMultiplier)
    }
    
    def idf(pages: List[RankedWebPage], word: String): Double = {
        val docsContainingWord = {
            for (page <- pages)
                yield (if (page.text.split(" ").contains(word)) 1.0 else 0.0)
        }.foldLeft(0.0) {_ + _}
        return math.log(pages.length / (docsContainingWord + 1))
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