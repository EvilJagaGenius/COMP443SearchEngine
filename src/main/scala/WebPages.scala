class WebPage(val id: String, val name: String, val url: String,
              val text: String, val links: List[String]) {

}

class RankedWebPage (val weight: Double) {

}

class SearchedWebPage (val textmatch: Double) {

}




