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

package mybiotools.tasks

import mybiotools.{ readBinaryFile, writeBinaryToFile }
import java.io.File
import java.io.FileInputStream
import java.io.{ ObjectOutputStream, ObjectInputStream, FileOutputStream }
import mybiotools.logger.Logger
import scala.collection.immutable.ListMap

abstract class Cache {

  def get(x: TaskDescription): Option[Result]

  def set(x: TaskDescription, r: Result): Unit

  def shutDown: Unit

}

class DisabledCache extends Cache {

  def get(x: TaskDescription) = None

  def remove(x: TaskDescription) = ()

  def set(x: TaskDescription, r: Result) = ()

  def shutDown = {}

}

class FakeCacheForTest extends Cache {
  private[this] var cacheMap: scala.collection.mutable.ListMap[TaskDescription, Result] = scala.collection.mutable.ListMap[TaskDescription, Result]()

  def get(x: TaskDescription) = cacheMap.get(x)

  def remove(x: TaskDescription) = cacheMap.remove(x)

  def set(x: TaskDescription, r: Result) = cacheMap.getOrElseUpdate(x, r)

  def shutDown = {}

}
