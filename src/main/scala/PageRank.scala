import scala.util.Random
import scala.collection.parallel.CollectionConverters._

object PageRank {

    def equal(pages: Map[String, WebPage]): Map[String, Double] =
      (for(s <- pages.keys) yield (s -> 1.0)).toMap



    def indegree(pages: Map[String, WebPage]): Map[String, Double] = {

      // Create a map with the count of each link destination as weights
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
          mapbuilder(tail, (result + (head -> (weight+1.0))))

        }
      }

      // Create an Iterable of all the links in our WebPages
      val allLinks: Iterable[String] =
        for((id, page) <- pages; link <- page.links) yield link

      // Create a map with all WebPages starting with a weight of zero
      val startingMap = (for(id <- pages.keys) yield (id -> 0.0)).toMap
      
      // Call mapbuilder on the links
      mapbuilder(allLinks, startingMap)

    }



    def pagerank(pages: Map[String, WebPage]): Map[String, Double] = {
        
        // Create a map from WebPage IDs to the WebPage weight by
        // randomly stepping through links
        def stepper(
          walkers: Iterable[WebPage],
          steps: Int,
          result: Map[String, Double]
        ): Map[String, Double] = steps match {
          
          // If there are no more steps, return the result
          case 0 => result

          // Otherwise, make the recursive call on a newly randomized
          // Iterable of walkers
          case _ => {

            // "Step" all of the walkers to a new WebPage
            val newWalkers = for(walker <- walkers) yield {
              // Apply 85% damping factor
              if(Random.nextInt(100) > 14 && walker.links.length > 0 ) {
                // Follow a link
                val newID = walker.links(Random.nextInt(walker.links.length))
                pages.getOrElse(newID, walker)
              } else {
                // Pick a random page
                pages.values.toList(Random.nextInt(pages.values.size))
              }
            }

            // Create a new result map with the new weights
            val newResult = result ++ (for(page <- newWalkers) yield {
              val weight: Double = result.getOrElse(page.id, 0)
              (page.id -> (weight+1.0))
            }).toMap[String, Double]
            
            // Make the recursive call
            stepper(newWalkers, steps-1, newResult)
          }
        }


        // Create a shuffled Iterable of all the WebPages
        val randomStart: Iterable[WebPage] = Random.shuffle(pages.values)

        // Create a map with all the weights starting at 0
        val startingMap = (for(id <- pages.keys) yield (id -> 0.0)).toMap

        // Call stepper for 10000 steps
        val results = stepper(randomStart, 10000, startingMap)

        // Normalize the weights in results
        (for((id, steps) <- results) yield (id -> (steps+1)/(10000+pages.size+1.0))).toMap

        //(for(s <- pages.keys) yield (s -> 1.0)).toMap
    }
}
