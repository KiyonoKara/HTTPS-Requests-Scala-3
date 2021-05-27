import HTTPSRequestsScala3.Request

object RequestApp {
  def main(args: Array[String]): Unit = {
    val requester: Request = new Request()
    val output: String = requester.request("https://www.scala-lang.org/", "GET", Map("Accept-Encoding" -> "gzip"))
    //println(output)

    val output1: String = requester.request("https://reqbin.com/echo/post/json", "POST", Map("Accept" -> "application/json"), "{\"Id\":78912,\"Customer\":\"Jason Sweet\",\"Quantity\":1,\"Price\":18}")
    println(output1)
  }
}
