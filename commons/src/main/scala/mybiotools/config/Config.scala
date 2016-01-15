/* 
* The MIT License
*
* Copyright (c) 2015 ECOLE POLYTECHNIQUE FEDERALE DE LAUSANNE, Switzerland, 
* Group Fellay
*
* Permission is hereby granted, free of charge, to any person obtaining
* a copy of this software and associated documentation files (the "Software"),
* to deal in the Software without restriction, including without limitation 
* the rights to use, copy, modify, merge, publish, distribute, sublicense, 
* and/or sell copies of the Software, and to permit persons to whom the Software
* is furnished to do so, subject to the following conditions:
*
* The above copyright notice and this permission notice shall be included in all
* copies or substantial portions of the Software.
*
* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR 
* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, 
* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE 
* AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER 
* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, 
* OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE 
* SOFTWARE.
*/

package mybiotools.config

import com.typesafe.config.ConfigFactory

/** Convenience singleton wrapper object for [[com.typesafe.config]] */
object Config {

  val configInstance = ConfigFactory.load()

  /**
   * Pretty formatting of a config subtree
   * @param path Restricts output to this path. `None` means no restriction.
   * @return a pretty formatted string
   */
  def prettyPrint(path: Option[String] = None): String = prettyPrintConfig(configInstance, path)

  /**
   * Pretty formatting of a config subtree
   * @param path Restricts output to this path.
   * @return a pretty formatted string
   */
  def prettyPrint(path: String): String = prettyPrintConfig(configInstance, Some(path))

  /**
   * Pretty formatting of the whole config tree.
   * @return a pretty formatted string
   */
  def prettyPrint: String = prettyPrint(None)

  /**
   * Pretty formatting of a config subtree
   * @param path Restricts output to this path. `None` means no restriction.
   * @return a pretty formatted string
   */
  def prettyPrintConfig(configInstance: com.typesafe.config.Config, path: Option[String] = None): String = {
    import scala.collection.JavaConversions._
    val restricted = path match {
      case Some(x) => configInstance.withOnlyPath(x)
      case None => configInstance
    }
    restricted.entrySet.toList.sortBy(_.getKey).map(x => x.getKey + " = " + x.getValue.render).mkString("---- Configuration of subpath: " + path.getOrElse("full") + " ----\n", "\n", "\n-----------------------------------")
  }

  val globalThreads = mybiotools.catchToLeft(configInstance.getInt("global.threads")).right.toOption.getOrElse(1)

  def prettyPrintVersion(name: String, version: String) = {
    val middle = "|    " + name + "    -    version: " + version + "    |"
    val frame1 = "." + List.fill[Char](middle.size - 2)('-').mkString + "."
    val frame2 = "*" + List.fill[Char](middle.size - 2)('-').mkString + "*"
    List(frame1, middle, frame2).mkString("\n", "\n", "\n")
  }

}