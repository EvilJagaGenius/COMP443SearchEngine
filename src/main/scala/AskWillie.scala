import scala.io.Source
import scala.io.StdIn.readLine
import scala.math.Ordering
import scala.util.Sorting

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

        //Ranking Pages with The Ranking Algorithms We Choose
        //***CHANGE THIS LINE FOR EXPERIMENTS (RANKING ALGORITHM CHOICE)***
        val pagesRank: Map[String, Double] = PageRank.equal(pages)

        //Min-Max Normalization Of Ranking
        val pagesRankMinMax = minMax(pagesRank)

        //Create rankedWebpageMap
        val rankedPagesMap = createRankedPageMap(pagesRankMinMax, pages)

        //Take first query
        println("Please enter a search query (:quit to leave program):")
        var query = readLine()

        //While query isn't :quit, do search loop
        while(query != ":quit"){
            //Make list of words in query for searching match
            var queryWordList = query.split(' ').toList

            //Compute how well each page matches using the methods of the PageSearch object
            //***CHANGE THIS LINE FOR EXPERIMENTS (SEARCH ALGORITHM CHOICE)***
            var pagesSearch = PageSearch.count((rankedPagesMap.values).toList, queryWordList)

            //Min-Max Normalization of Match Ratings
            var pagesMatchMap = ((pages.keys).zip(pagesSearch)).toMap
            var pagesMinmaxMatchMap = minMax(pagesMatchMap)

            //Compute the overall match as the arithmetic, geometric, or harmonic mean of the pages rank and text-match
            //***CHANGE THIS LINE FOR EXPERIMENTS (MEAN CHOICE)***
            var pagesSearchMap = makeArithmeticMeanMatch(pagesRankMinMax, pagesMinmaxMatchMap)

            //Sort results
            var pagesSearchMapSorted = sortMap(pagesSearchMap)

            //Display the name and url of the top 10 results
            var sortedList = (pagesSearchMapSorted.keys).toList
            for(i <- 0 until 10) println(pages(sortedList(i)).name + ": " + pages(sortedList(i)).url)

            println("=============================================================")

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

    def minMax(rankpages: Map[String, Double]): Map[String, Double] = {
        val min = (rankpages.values).foldLeft(Double.MaxValue)(Math.min(_, _))
        val max = (rankpages.values).foldLeft(Double.MinValue)(Math.max(_, _))
        if(min == max) rankpages else (for((page, weight) <- rankpages) yield (page, (weight - min)/(max - min))).toMap
    }

    def createRankedPageMap(rankMap: Map[String, Double], pages: Map[String, WebPage]): Map[String, RankedWebPage] ={
        (for((page, weight) <- rankMap) yield (page, new RankedWebPage(page, pages(page).name, pages(page).url, pages(page).text, pages(page).links, weight))).toMap
    }

    def createSearchedPageMap(rankMap: Map[String, Double], pages: Map[String, WebPage]): Map[String, SearchedWebPage] ={
        (for((page, textmatch) <- rankMap) yield (page, new SearchedWebPage(page, pages(page).name, pages(page).url, pages(page).text, pages(page).links, textmatch))).toMap
    }

    def makeArithmeticMeanMatch(rankedPages: Map[String, Double], textmatchPages: Map[String, Double]): Map[String, Double] = {
        (for((page, rank) <- rankedPages) yield (page, (rank + textmatchPages(page))/2.0)).toMap
    }

    def makeGeometricMeanMatch(rankedPages: Map[String, Double], textmatchPages: Map[String, Double]): Map[String, Double] = {
        (for((page, rank) <- rankedPages) yield (page, math.sqrt(rank * textmatchPages(page)))).toMap
    }

    def makeHarmonicMeanMatch(rankedPages: Map[String, Double], textmatchPages: Map[String, Double]): Map[String, Double] = {
        (for((page, rank) <- rankedPages) yield (page, (2 * rank * textmatchPages(page))/(rank + textmatchPages(page)))).toMap
    }

    def sortMap(pages: Map[String, Double]): Map[String, Double] = {
        val pagesSortingArray = (for((page, weight) <- pages) yield (page, weight)).toArray
        Sorting.quickSort(pagesSortingArray)(Ordering.by[(String, Double), Double](_._2))
        pagesSortingArray.toMap
    }
}