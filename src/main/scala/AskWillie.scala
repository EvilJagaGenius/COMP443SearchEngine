import scala.io.Source
import scala.io.StdIn.readLine

object AskWillie {
    def main(args: Array[String]) = {
        println("=============================================================")
        println("   _____          __      __      __.__.__  .__  .__ ")
        println("  /  _  \\   _____|  | __ /  \\    /  \\__|  | |  | |__| ____  ")
        println(" /  /_\\  \\ /  ___/  |/ / \\   \\/\\/   /  |  | |  | |  |/ __ \\")
        println("/    |    \\___ \\|     <   \\        /|  |  |_|  |_|  \\  ___/ ")
        println("\\____|__  /____  >__|_ \\   \\__/\\  / |__|____/____/__|\\___  >")
        println("        \\/     \\/     \\/        \\/                       \\/")
        println("=============================================================")

        // Load WebPage.id -> WebPage map to better handle graph
        val pages: Map[String, WebPage] = mapWebPages(loadWebPages)

        //Ranking Pages with All the Ranking Algorithms
        val pagesEqualRank = PageRank.equal(pages)
        val pagesIndegreeRank = PageRank.indegree(pages)
        val pagesPageRank = PageRank.pagerank(pages);

        //Min-Max Normalization Of Ranking
        val minIndegreeRank = (page <- pagesIndegreeRank.values).foldLeft(Double.MaxValue)(Math.min(_, _))
        val maxIndegreeRank = (page <- pagesIndegreeRank.values).foldLeft(Double.MinValue)(Math.max(_, _))
        val minPageRank = (page <- pagesPageRank.values).foldLeft(Double.MaxValue)(Math.min(_,_))
        val maxPageRank = (page <- pagesPageRank.values).foldLeft(Double.MinValue)(Math.max(_,_))
        for(page <- pagesIndegreeRank.keys){
            pagesIndegreeRank(page) = (pagesIndegreeRank(page) - minIndegreeRank)/(maxIndegreeRank-minIndegreeRank)
        }
        for(page <- pagesPageRank.keys){
            pagesPageRank(page) = (pagesPageRank(page) - minPageRank)/(maxPageRank-minPageRank)
        }

        //Create rankedWebpageMap
        val equalRankedPageMap = makeRankedWebPageMap(pagesEqualRank, pages)
        val indegreeRankedPageMap = makeRankedWebPageMap(pagesIndegreeRank, pages)
        val pagerankRankedPageMap = makeRankedWebPageMap(pagesPageRank, pages)

        //While query isn't :quit, do search loop
        while(true){
            //Accept user query
                query = "data"
                //TODO: find immutable variable way to accept queries

            //Compute how well each page matches using the methods of the PageSearch object

            //Min-Max Normalization of Match Ratings

            //Compute the overall match as the arithmetic mean of the pages rank and text-match
            //Compute the overall match as the geometric mean of the pages rank and text-match
            //Compute the overall match as the harmonic mean of the pages rank and text-match

            //Sort pages based on their overall match using scala.math.Ordering to support multiple options for computing the mean

            //Create searchedWebPage List

            //Display the name and url of the top 10 results
        }
    }

    // Load a List of WebPage objects from the packaged prolandwiki.csv file
    def loadWebPages: List[WebPage] = {
        // create an input stream to the proglangwiki.csv
        val fh = Source.fromInputStream(
            getClass.getClassLoader.getResourceAsStream("proglangwiki.csv"))
        // load all pages from the file line by line
        val pages = (for (line <- fh.getLines) yield {
            val id::name::url::text::links = line.split(",").toList
            new WebPage(id, name, url, text, links)
        }).toList
        fh.close
        pages
    }

    // Convert a List[WebPage] to a Map[String, WebPage]
    def mapWebPages(pages: List[WebPage]): Map[String, WebPage] = {
        (for (page <- pages) yield (page.id, page)).toMap
    }
}