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
        val pagesRank: Map[String, Double] = PageRank.pagerank(pages)

        //Min-Max Normalization Of Ranking
        val pagesRankMinMax = minMax(pagesRank)

        //Create rankedWebpageMap
        val rankedPagesMap = createRankedPageMap(pagesRankMinMax, pages)

        //Take first query
        println("Please enter a search query (:quit to leave program):")
        var query = readLine().toLowerCase

        //While query isn't :quit, do search loop
        while (query != ":quit") {
            //Make list of words in query for searching match
            var queryWordList = query.split(' ').toList

            //Compute how well each page matches using the methods of the PageSearch object
            //***CHANGE THIS LINE FOR EXPERIMENTS (SEARCH ALGORITHM CHOICE)***
            var pagesSearch = PageSearch.count((rankedPagesMap.values).toList, queryWordList)

            //Min-Max Normalization of Match Ratings
            var pagesMatchMap = ((pages.keys).zip(pagesSearch)).toMap
            var pagesMinmaxMatchMap = minMax(pagesMatchMap)

            //Create searchedWebPageMap
            var pagesSearchMap = createSearchedPageMap(pagesMinmaxMatchMap, rankedPagesMap)
            var pagesSearchMapSortingArray = (pagesSearchMap.values).toArray

            //Compute the overall match as the arithmetic, geometric, or harmonic mean of the pages rank and text-match
            //Sort results
            //***CHANGE THIS LINE FOR EXPERIMENTS (MEAN CHOICE)***
            Sorting.quickSort(pagesSearchMapSortingArray)(ArithmeticOrdering)


            //Display the name and url of the top 10 results
            var pagesList = pagesSearchMapSortingArray.toList
            for (i <- 0 until 10) println(pagesList(pagesList.length-(i+1)).name + ": " + pagesList(pagesList.length-(i+1)).url)

            println("=============================================================")

            //Accept user query
            println("Please enter a search query (:quit to leave program):")
            query = readLine().toLowerCase
        }
    }

    // Load a List of WebPage objects from the packaged prolangwiki.csv file
    def loadWebPages: List[WebPage] = {
        // create an input stream to the proglangwiki.csv
        val fh = Source.fromInputStream(
            getClass.getClassLoader.getResourceAsStream("proglangwiki.csv"))
        // load all pages from the file line by line
        val pages = (for (line <- fh.getLines) yield {
            val id :: name :: url :: text :: links = line.split(",").toList
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
        if (min == max) rankpages else (for ((page, weight) <- rankpages) yield (page, (weight - min) / (max - min))).toMap
    }

    def createRankedPageMap(rankMap: Map[String, Double], pages: Map[String, WebPage]): Map[String, RankedWebPage] = {
        (for ((page, weight) <- rankMap) yield (page, new RankedWebPage(page, pages(page).name, pages(page).url, pages(page).text, pages(page).links, weight))).toMap
    }

    def createSearchedPageMap(rankMap: Map[String, Double], pages: Map[String, RankedWebPage]): Map[String, SearchedWebPage] = {
        (for ((page, textmatch) <- rankMap) yield (page, new SearchedWebPage(page, pages(page).name, pages(page).url, pages(page).text, pages(page).links, pages(page).weight, textmatch))).toMap
    }
}

object ArithmeticOrdering extends Ordering[SearchedWebPage] {
    def compare(a:SearchedWebPage, b:SearchedWebPage) = (a.weight + a.textmatch)/2.0 compare (b.weight + b.textmatch)/2.0
}

object GeometricOrdering extends Ordering[SearchedWebPage] {
    def compare(a:SearchedWebPage, b:SearchedWebPage) = math.sqrt(a.weight * a.textmatch) compare math.sqrt(a.weight * a.textmatch)
}

object HarmonicOrdering extends Ordering[SearchedWebPage] {
    def compare(a:SearchedWebPage, b:SearchedWebPage) = (2.0 * a.weight * a.textmatch)/(a.weight + a.textmatch) compare (2.0 * b.weight * b.textmatch)/(b.weight + b.textmatch)
}