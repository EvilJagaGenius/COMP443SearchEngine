class WebPage(val id: String,
              val name: String,
              val url: String,
              val text: String,
              val links: List[String]) {

}

class RankedWebPage(override val id: String,
                    override val name: String,
                    override val url: String,
                    override val text: String,
                    override val links: List[String],
                    val weight: Double) extends WebPage (id, name, url, text, links){

}

class SearchedWebPage(override val id: String,
                      override val name: String,
                      override val url: String,
                      override val text: String,
                      override val links: List[String],
                      val textmatch: Double) extends WebPage (id, name, url, text, links){

}




