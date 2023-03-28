import java.net.MalformedURLException

class URLParser(val link: String)

object URLParser { //Companion Object

  def apply(str: String): URLParser = new URLParser("https://www.mywebsite.com/home")

  def unapply(url: URLParser): Option[(String, String, String)] = {
    try {
      val urlCopy = url.link
      if (urlCopy.contains("://")) {

        //Extracting Protocol from URL
        val protocolStartingIndex = urlCopy.indexOf("://")
        val finalProtocol = urlCopy.substring(0, protocolStartingIndex)

        //Extracting Domain from URL
        val domainStartingIndex = urlCopy.substring(protocolStartingIndex + 3, urlCopy.length)
        val index = domainStartingIndex.indexOf("/")
        val finalDomain = domainStartingIndex.substring(0, index)

        //Extracting Path from URL
        val pathStartingIndex: String = urlCopy.substring(index + protocolStartingIndex + 3, urlCopy.length)
        val finalPath = pathStartingIndex.substring(1, pathStartingIndex.length)

        Some(finalProtocol, finalDomain, finalPath)
      }
      else None // If "://" not present in the URL then It is not a valid URL
    }
    catch {
      case e: MalformedURLException =>
        println(s"Invalid URL: ${url.link}" + e.getMessage)
        None
    }
  }
}

object Extractor extends App {

  private val url = new URLParser("https://www.myWebsite.com/home")
  private val result = url match {
    case URLParser(protocol, domain, path) => s"Protocol : $protocol \nDomain : $domain \nPath : $path"
    case _ => "Invalid URL"
  }
  println(result)
}
