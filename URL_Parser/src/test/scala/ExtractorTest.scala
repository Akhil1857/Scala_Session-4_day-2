import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ExtractorTest extends AnyFlatSpec with Matchers {
  "URLParser" should "parse a valid URL String : Google" in {
    val url = new URLParser("https://www.google.com/head")
    url match {
      case URLParser(protocol, domain, path) =>
        protocol should be("https")
        domain should be("www.google.com")
        path should be("head")
      case _ => fail("Failed to parse URL")
    }
  }

  it should "parse a valid URL String : Facebook" in {
    val url = new URLParser("https://www.facebook.com/head/document/information")
    url match {
      case URLParser(protocol, domain, path) =>
        protocol should be("https")
        domain should be("www.facebook.com")
        path should be("head/document/information")
      case _ => fail("Failed to parse URL")
    }
  }

  it should "return Invalid URL" in {
    val url = new URLParser("https:/www.facebook.com/head/document/information")
    url match {
      case _ => ("Invalid URL")
    }
  }


}