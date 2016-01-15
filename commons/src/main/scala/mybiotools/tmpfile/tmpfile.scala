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

package mybiotools

import java.io.File

object TempFile {

  private def createTempDir(baseName: String): File = {
    val baseDir = new File(System.getProperty("java.io.tmpdir"));

    val max = 5

    var counter = 0
    var b = false
    var t: Option[File] = None
    while (!b && counter < max) {
      val tempDir = new File(baseDir, baseName + counter);
      if (tempDir.mkdir()) {
        b = true
        t = Some(tempDir)
      }
      counter += 1
    }

    if (t.isDefined) {
      val x = t.get
      x.deleteOnExit
      x
    } else {
      throw new IllegalStateException("Failed to create directory within "
        + max + " attempts (tried "
        + baseDir.getAbsolutePath + "/" + baseName + "0 to " + baseName + (max - 1) + ')');

    }

  }

  val id: String = {
    val f = new java.text.SimpleDateFormat("yyyy_MM_dd_HH_mm_ss");
    f.format(new java.util.Date)
  }

  val prefix = "bio" + id

  lazy val folder =
    synchronized {
      createTempDir(prefix)
    }

  def createTempFile(suffix: String): File = File.createTempFile(prefix, suffix, folder)

  def createFileInTempFolderIfPossibleWithName(fileName: String): File = {
    val f = new File(folder, fileName)
    val success = f.createNewFile
    if (success) f
    else createTempFile(suffix = fileName)
  }

  val writtenExecutables = collection.mutable.Map[String, File]()

  def getExecutableFromJar(name: String): File = writtenExecutables.get(name).getOrElse {
    synchronized {
      val f = writeFreshExecutable(name)

      writtenExecutables.update(name, f)

      f
    }
  }

  private def writeFreshExecutable(name: String): File = {
    val tmpFile = createTempFile(".executable")
    tmpFile.deleteOnExit()
    tmpFile.setExecutable(true)

    val d = readStreamAndClose(getClass().getResource(name).openStream()).toArray
    mybiotools.writeBinaryToFile(tmpFile.getAbsolutePath, d)
    tmpFile

  }

}