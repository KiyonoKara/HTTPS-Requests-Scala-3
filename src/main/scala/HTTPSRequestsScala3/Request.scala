package HTTPSRequestsScala3

/**
 * Created by KaNguy - 05/23/2021
 * File HTTPSRequestsScala3.Request.scala
 */

// Networking and web
import java.net.{ConnectException, HttpURLConnection, URI, URL}

// IO & NIO
import java.io.{InputStream, OutputStream}
import java.nio.charset.StandardCharsets

// SSL
import javax.net.ssl.SSLException

// Java HTTP
import java.net.http.{HttpClient, HttpHeaders, HttpRequest, HttpResponse}

// Scala IO Source
import scala.io.Source.fromInputStream

// Local classes
import HTTPSRequestsScala3.Utility

// Other
import java.lang.reflect.Field

/** Main class for making HTTP/HTTPS requests
 *
 * @param url - String; Provide an URL with its path (if you are requesting with the path)
 * @param method - String; Request method, refer to the Constants file for supported methods
 * @param headers - Iterable[(String, String)]; Headers in the form of a Map collection is primarily valid
 */
class Request(var url: String = null, var method: String = Utility.Constants.GET, headers: Iterable[(String, String)] = Nil) {
  private lazy val methodField: Field = {
    val method = classOf[HttpURLConnection].getDeclaredField("method")
    method.setAccessible(true)
    method
  }


  /** Class method that ultimately does the requesting
   *
   * @param url - String; Provide an URL
   * @param method - String; Request method, defaults to the class' default method
   * @param headers - Iterable[(String, String)]; Headers for requesting
   * @param data - String; Preferably JSON data that is in the form of a string
   * @param parameters - Iterable[(String, String)]; URL parameters that can be used for querying
   * @return {String}
   */
  def request(url: String = this.url, method: String = this.method, headers: Iterable[(String, String)] = this.headers, data: String = null, parameters: Iterable[(String, String)] = Nil): String = {
    // Parse the URL along with the parameters
    val requestURL: String = Utility.createURL(url, parameters)
    val parsedURL: URL = new URL(requestURL)

    // Create the connection from the provided URL
    var connection: HttpURLConnection = null
    try {
      connection = parsedURL.openConnection.asInstanceOf[HttpURLConnection] match {
        case _: HttpURLConnection => parsedURL.openConnection.asInstanceOf[HttpURLConnection];
      }
    } catch {
      case connectException: ConnectException =>
        connectException.printStackTrace()
        throw connectException
      case sslException: SSLException =>
        sslException.printStackTrace()
        throw sslException
    }

    // Set the request method
    if (Utility.Constants.HTTPMethods.contains(method.toUpperCase)) {
      connection.setRequestMethod(method.toUpperCase)
    } else {
      /** For PATCH requests, the method will default to POST.
       * PATCH requests can still be done with X-HTTP-Method-Override header that changes the request method.
       *
       * Example for adding the PATCH override:
       * {{{
       *   val PATCH: String = new Request().request("http://localhost:8080/echo",
       *                                         "PATCH", Map("Accept" -> "*",
       *                                         "User-Agent" -> "*",
       *                                         "X-HTTP-Method-Override" -> "PATCH"),
       *                                         data = "{\"message\": \"PATCH message\"}")
       * }}}
       *
       */
      connection match {
        case httpURLConnection: HttpURLConnection =>
          httpURLConnection.getClass.getDeclaredFields.find(_.getName == "delegate") foreach { i =>
            i.setAccessible(true)
            this.methodField.set(i.get(httpURLConnection), method.toUpperCase)
          }
        case other =>
          this.methodField.set(other, method.toUpperCase)
      }
    }

    // Timeouts
    connection.setConnectTimeout(Utility.Constants.DEFAULT_TIMEOUT)
    connection.setReadTimeout(Utility.Constants.DEFAULT_TIMEOUT)

    // Sets headers
    if (headers.nonEmpty) {
      Utility.setHeaders(connection, headers)
    }

    if (method.toUpperCase.equals(Utility.Constants.GET)) {
      return Utility.read(connection, connection.getInputStream)
    }

    if (method.toUpperCase.equals(Utility.Constants.POST) || method.toUpperCase.equals(Utility.Constants.DELETE) || method.toUpperCase.equals(Utility.Constants.PUT) || method.toUpperCase.equals(Utility.Constants.PATCH)) {
      return this.writeToRequest(connection, method, data)
    }

    // Input stream for data with a GET request if all of the requests fail
    val inputStream = connection.getInputStream
    val content = fromInputStream(inputStream).mkString
    if (inputStream != null) inputStream.close()
    // Return the content or data, read-only
    content
  }

  def head(url: String = this.url): String = {
    val client: HttpClient = HttpClient.newHttpClient()
    val headRequest: HttpRequest  = HttpRequest.newBuilder(URI.create(url))
      .method(Utility.Constants.HEAD, HttpRequest.BodyPublishers.noBody())
      .build()
    val response: HttpResponse[Void] = client.send(headRequest, HttpResponse.BodyHandlers.discarding())
    val headers: HttpHeaders = response.headers()

    var strHeaders: String = new String()
    headers.map.forEach((key, values) => {
      strHeaders += "%s: %s%n".format(key, values)
    })

    strHeaders
  }

  /** Can turn collections into JSON data as a string
   *
   * @param collections - Any; Accepts collections and primitive types
   * @return {String}
   */
  def collectionToJSON(collections: Any): String = {
    Utility.CollectionsToJSON(collections).toString
  }

  /** Method to turn maps that have only two strings per index into JSON data as a string
   *
   * @param map - Map[String, String]; String map with only two strings, this is for regular JSON objects that have no nesting or lists
   * @return {String}
   */
  def mapToJSON(map: Map[String, String] = Map.empty): String = {
    Utility.singleMapToJSON(map).toString
  }

  /**
   *
   * @param connection - HttpURLConnection; The connection established will be used so it can be written to.
   * @param method - String; Method is always required, cannot default to a common request method
   * @param data - String; Preferably JSON data in the form of a string.
   * @return output - String; Generally returns the output of the Output Reader
   */
  def writeToRequest(connection: HttpURLConnection, method: String, data: String): String = {
    val theMethod: String = method.toUpperCase
    if (theMethod.equals(Utility.Constants.POST) || theMethod.equals(Utility.Constants.PUT) || theMethod.equals(Utility.Constants.PATCH)) connection.setDoOutput(true)

    // Processing the data
    val byte: Array[Byte] = data.getBytes(StandardCharsets.UTF_8)
    val length: Int = byte.length
    connection.setFixedLengthStreamingMode(length)

    try {
      // Write to the request
      val outputStream: OutputStream = connection.getOutputStream
      outputStream.write(byte, 0, byte.length)
      if (theMethod.equals(Utility.Constants.POST)) {
        outputStream.flush()
        outputStream.close()
      }
      // Get output of request
      val inputStream: InputStream = connection.getInputStream
      if (connection.getContentEncoding != null && connection.getContentEncoding.toLowerCase.equals("gzip")) {
        val content: String = Utility.read(connection, inputStream)
        content
      } else {
        val content: String = fromInputStream(inputStream).mkString
        inputStream.close()
        content
      }
    } catch {
      case e => e.toString
    }
  }
}