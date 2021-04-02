class WebPage(val id: String, val name: String, val url: String,
              val text: String, val links: List[String]) {
  val id: String
  val name: String
  val url: String
  val text: String
  val links: List[String] // a list of ids this page links to
}

class RankedWebPage extends WebPage {
  val weight: Double
}

class SearchedWebPage extends RankedWebPage {
  val textmatch: Double
}




