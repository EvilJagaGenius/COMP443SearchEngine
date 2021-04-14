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
        val pagesEqualRank: Map[String, Double] = PageRank.equal(pages)
        val pagesIndegreeRank: Map[String, Double] = PageRank.indegree(pages)
        val pagesPageRank: Map[String, Double] = PageRank.pagerank(pages);

        //Min-Max Normalization Of Ranking
        val indegreePagesMinmaxMap = minMax(pagesIndegreeRank)
        val pagerankPagesMinmaxMap = minMax(pagesPageRank)

        //Create rankedWebpageMap
        val rankedEqualPagesMap = createRankedPageMap(pagesEqualRank, pages)
        val rankedIndegreePagesMap = createRankedPageMap(pagesIndegreeRank, pages)
        val rankedPagerankPagesMap = createRankedPageMap(pagesPageRank, pages)

        //Initalize variables
        var query = ""

        //Take first query
        println("Please enter a search query (:quit to leave program):")
        query = readLine()

        //While query isn't :quit, do search loop
        while(query != ":quit"){
            //Compute how well each page matches using the methods of the PageSearch object

            //Min-Max Normalization of Match Ratings

            //Compute the overall match as the arithmetic mean of the pages rank and text-match

            //Compute the overall match as the geometric mean of the pages rank and text-match

            //Compute the overall match as the harmonic mean of the pages rank and text-match


            //Sort pages based on their overall match using scala.math.Ordering to support multiple options for computing the mean


            //Create searchedWebPage List


            //Display the name and url of the top 10 results


            //Accept user query
            println("Please enter a search query (:quit to leave program):")
            query = readLine()
        }




    }

    // Load a List of WebPage objects from the packaged prolangwiki.csv file
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

    def minMax(pages: Map[String, Double]){
        val min = pages.values.min
        val max = pages.values.max
        (for((page, weight) <- pages) yield (page, (weight - min)/(max - min))).toMap
    }

    def createRankedPageMap(rankMap: Map[String, Double], pages: Map[String, WebPage]): Map[String, RankedWebPage] ={
        (for((page, weight) <- rankMap) yield (page, new RankedWebPage(page, pages(page).name, pages(page).url, pages(page).text, pages(page).links, weight))).toMap
    }
}