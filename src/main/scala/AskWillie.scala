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
            //Make list of words in query for searching match
            var queryWordList = query.split(' ').toList

            //Compute how well each page matches using the methods of the PageSearch object
            var pagesCountSearch = PageSearch.count((rankedEqualPagesMap.values).toList, queryWordList)
            var pagesTfSearch = PageSearch.tf((rankedEqualPagesMap.values).toList, queryWordList)
            var pagesTfidfSearch = PageSearch.tfidf((rankedEqualPagesMap.values).toList, queryWordList)

            //Min-Max Normalization of Match Ratings
            var pagesCountMatchMap = ((pages.keys).zip(pagesCountSearch)).toMap
            var pagesTfMatchMap = ((pages.keys).zip(pagesTfSearch)).toMap
            var pagesTfidfMatchMap = ((pages.keys).zip(pagesTfidfSearch)).toMap

            var pagesCountMinmaxMatchMap = minMax(pagesCountMatchMap)
            var pagesTfMinmaxMatchMap = minMax(pagesTfMatchMap)
            var pagesTfidfMinmaxMatchMap = minMax(pagesTfidfMatchMap)


            //Compute the overall match as the arithmetic mean of the pages rank and text-match
            var pagesCountEqualArithmeticMap = makeArithmeticMeanMatch(pagesEqualRank, pagesCountMinmaxMatchMap)
            var pagesTfEqualArithmeticMap = makeArithmeticMeanMatch(pagesEqualRank, pagesTfMinmaxMatchMap)
            var pagesTfidfEqualArithmeticMap = makeArithmeticMeanMatch(pagesEqualRank, pagesTfidfMinmaxMatchMap)
            var pagesCountIndegreeArithmeticMap = makeArithmeticMeanMatch(pagesIndegreeRank, pagesCountMinmaxMatchMap)
            var pagesTfIndegreeArithmeticMap = makeArithmeticMeanMatch(pagesIndegreeRank, pagesTfMinmaxMatchMap)
            var pagesTfidfIndegreeArithmeticMap = makeArithmeticMeanMatch(pagesIndegreeRank, pagesTfidfMinmaxMatchMap)
            var pagesCountPagerankArithmeticMap = makeArithmeticMeanMatch(pagesPageRank, pagesCountMinmaxMatchMap)
            var pagesTfPagerankArithmeticMap = makeArithmeticMeanMatch(pagesPageRank, pagesTfMinmaxMatchMap)
            var pagesTfidfPagerankArithmeticMap = makeArithmeticMeanMatch(pagesPageRank, pagesTfidfMinmaxMatchMap)

            //Compute the overall match as the geometric mean of the pages rank and text-match
            var pagesCountEqualGeometricMap = makeGeometricMeanMatch(pagesEqualRank, pagesCountMinmaxMatchMap)
            var pagesTfEqualGeometricMap = makeGeometricMeanMatch(pagesEqualRank, pagesTfMinmaxMatchMap)
            var pagesTfidfEqualGeometricMap = makeGeometricMeanMatch(pagesEqualRank, pagesTfidfMinmaxMatchMap)
            var pagesCountIndegreeGeometricMap = makeGeometricMeanMatch(pagesIndegreeRank, pagesCountMinmaxMatchMap)
            var pagesTfIndegreeGeometricMap = makeGeometricMeanMatch(pagesIndegreeRank, pagesTfMinmaxMatchMap)
            var pagesTfidfIndegreeGeometricMap = makeGeometricMeanMatch(pagesIndegreeRank, pagesTfidfMinmaxMatchMap)
            var pagesCountPagerankGeometricMap = makeGeometricMeanMatch(pagesPageRank, pagesCountMinmaxMatchMap)
            var pagesTfPagerankGeometricMap = makeGeometricMeanMatch(pagesPageRank, pagesTfMinmaxMatchMap)
            var pagesTfidfPagerankGeometricMap = makeGeometricMeanMatch(pagesPageRank, pagesTfidfMinmaxMatchMap)

            //Compute the overall match as the harmonic mean of the pages rank and text-match
            var pagesCountEqualHarmonicMap = makeHarmonicMeanMatch(pagesEqualRank, pagesCountMinmaxMatchMap)
            var pagesTfEqualHarmonicMap = makeHarmonicMeanMatch(pagesEqualRank, pagesTfMinmaxMatchMap)
            var pagesTfidfEqualHarmonicMap = makeHarmonicMeanMatch(pagesEqualRank, pagesTfidfMinmaxMatchMap)
            var pagesCountIndegreeHarmonicMap = makeHarmonicMeanMatch(pagesIndegreeRank, pagesCountMinmaxMatchMap)
            var pagesTfIndegreeHarmonicMap = makeHarmonicMeanMatch(pagesIndegreeRank, pagesTfMinmaxMatchMap)
            var pagesTfidfIndegreeHarmonicMap = makeHarmonicMeanMatch(pagesIndegreeRank, pagesTfidfMinmaxMatchMap)
            var pagesCountPagerankHarmonicMap = makeHarmonicMeanMatch(pagesPageRank, pagesCountMinmaxMatchMap)
            var pagesTfPagerankHarmonicMap = makeHarmonicMeanMatch(pagesPageRank, pagesTfMinmaxMatchMap)
            var pagesTfidfPagerankHarmonicMap = makeHarmonicMeanMatch(pagesPageRank, pagesTfidfMinmaxMatchMap)

            //Sort pages based on their overall match using scala.math.Ordering to support multiple options for computing the mean

            //Display the name and url of the top 10 results for each category
            print("Begin listing lists: \n\n")
            //Equal and Count
            println("Equal Ranking and Count Search:")
            var equalCountArithmeticList = (pagesCountEqualArithmeticMap.keys).toList
            for(i <- 0 until 10) println(pages(equalCountArithmeticList(i)).name + ": " + pages(equalCountArithmeticList(i)).url)
            //Equal and TF
            //Equal and TFIDF
            //Indegree and Count
            //Indegree and TF
            //Indegree and TFIDF
            //Pagerank and Count
            //PageRank and TF
            //PageRank and TFIDF

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
}