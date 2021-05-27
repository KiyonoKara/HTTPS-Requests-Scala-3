package HTTPSRequestsScala3

/**
 * Created by KaNguy - 05/23/2021
 * File HTTPSRequestsScala3.Utility.scala
 */

// Networking & web
import java.net.{URI, URL, URLEncoder, HttpURLConnection}

// Java Input/Output
import java.io.{InputStream, InputStreamReader, Reader}

// Java utilities
import java.util.zip.GZIPInputStream

// Collections
import scala.collection.mutable.ListBuffer

object Utility {
  /**
   * HTTP-related constants
   * {{{
   *   import HTTPSRequestsScala.util.Constants
   *   val GET: String = Constants.GET
   * }}}
   */
  object Constants {
    /**
     * Main HTTP/HTTPS methods
     */
    val GET: String = "GET"
    val POST: String = "POST"
    val DELETE: String = "DELETE"
    val PUT: String = "PUT"
    val HEAD: String = "HEAD"
    val OPTIONS: String = "OPTIONS"
    val PATCH: String = "PATCH"

    /**
     * Environment HTTP/HTTPS methods
     */
    val CONNECT: String = "CONNECT"
    val TRACE: String = "TRACE"

    /**
     * Officially supported methods
     */
    val HTTPMethods: Set[String] = Set(GET, POST, DELETE, PUT, HEAD, OPTIONS, TRACE)

    /**
     * Timeouts
     */
    val DEFAULT_TIMEOUT: Int = 5000
  }

  def encodeURLParameters(str: Iterable[(String, String)]): String = {
    str.map({
      case (k, v) =>
        s"""${URLEncoder.encode(k, "UTF-8")}=${URLEncoder.encode(v, "UTF-8")}"""
    }).mkString("&")
  }

  def createURL(url: String, urlParameters: Iterable[(String, String)] = Nil): String = {
    val newURL: URL = new URL(new URI(url).toASCIIString)
    if (urlParameters == Nil) return s"""$newURL"""
    val separator: String = if (newURL.getQuery != null) "&" else "?"
    val encodedURLParameters: String = Utility.encodeURLParameters(urlParameters)
    s"""$newURL$separator$encodedURLParameters"""
  }

  def getKeyByValue(map: Map[String, String] = Map.empty[String, String], value: String): String = {
    map.find(_._2.contains(value)).map(_._1.toString).getOrElse("")
  }

  def lowerCaseSingleKV(map: Map[String, String] = Map.empty[String, String], key: String): Map[String, String] = {
    val fin: Map[String, String] = Map.empty[String, String]
    map.foreach(item => {
      if (item._1 == key) {
        return fin updated(item._1.toLowerCase, item._2.toLowerCase)
      }
    })
    fin
  }

  def singleMapToJSON(map: Map[String, String]): String = {
    var json: String = ""

    var iterator: Int = 0
    map.foreach(i => {
      if (iterator == map.size - 1) {
        json += f"${'"'}${i._1}${'"'}" + ": " + f"${'"'}${i._2}${'"'}"
      } else {
        json += f"${'"'}${i._1}${'"'}" + ": " + f"${'"'}${i._2}${'"'}${','}" + " "
      }
      iterator += 1
    })
    json = "{" + json + "}"
    json
  }

  def CollectionsToJSON(collections: Any): String = {
    var JSON = new ListBuffer[String]()
    collections match {
      case map: Map[_, _] =>
        for ((k, v) <- map) {
          val key = k.asInstanceOf[String].replaceAll("\"", "\\\\\"")
          v match {
            case map: Map[_, _] => JSON += s""""$key": ${CollectionsToJSON(map)}""";
            case list: List[_] => JSON += s""""$key": ${CollectionsToJSON(list)}""";
            case int: Int => JSON += s""""$key": $int""";
            case boolean: Boolean => JSON += s""""$key": $boolean""";
            case string: String => JSON += s""""$key": "${string.replaceAll("\"", "\\\\\"")}""""
            case _ => ();
          }
        };

      case theList: List[_] =>
        var list = new ListBuffer[String]()
        for (listing <- theList) {
          listing match {
            case map: Map[_, _] => list += CollectionsToJSON(map);
            case caseList: List[_] => list += CollectionsToJSON(caseList);
            case int: Int => list += int.toString;
            case boolean: Boolean => list += boolean.toString;
            case string: String => list += s""""${string.replaceAll("\"", "\\\\\"")}"""";
            case _ => ();
          }
        }

        return "[" + list.mkString(",") + "]"

      case _ => ();
    }

    val JSONString: String = "{" + JSON.mkString(",") + "}"
    JSONString
  }

  def read(connection: HttpURLConnection, inputStream: InputStream = null): String = {
    var connectionInputStream: InputStream = null
    if (inputStream != null) connectionInputStream = inputStream else connectionInputStream = connection.getInputStream

    var reader: Reader = null

    if (connection.getContentEncoding != null && connection.getContentEncoding.equals("gzip")) {
      reader = new InputStreamReader(new GZIPInputStream(connectionInputStream))
    } else {
      reader = new InputStreamReader(connection.getInputStream)
    }

    // Empty char value
    var ch: Int = 0

    // String Builder to add to the final string
    val stringBuilder: StringBuilder = new StringBuilder()

    // Appending the data to a String Builder
    while (true) {
      ch = reader.read()
      if (ch == -1) {
        return stringBuilder.toString()
      }

      stringBuilder.append(ch.asInstanceOf[Char]).toString
    }
    stringBuilder.toString
  }

  def setHeaders(connection: HttpURLConnection, headers: Iterable[(String, String)] = Nil): Unit = {
    if (headers.nonEmpty) {
      headers foreach {
        case (key, value) =>
          try {
            connection.setRequestProperty(key, value)
          } catch {
            case _ => ()
          }
      }
    }
  }

  def addHeaders(connection: HttpURLConnection, headers: Iterable[(String, String)] = Nil): Unit = {
    if (headers.isEmpty) {
      return
    }
    headers foreach {
      case (key, value) =>
        try {
          connection.addRequestProperty(key, value)
        } catch {
          case _ => ()
        }
    }
  }
}
