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

import org.scalatest._
import com.typesafe.config.ConfigFactory

class LeveldBDCacheTestSuite extends FunSuite with BeforeAndAfterAll {
  val file = new java.io.File(mybiotools.TempFile.createTempFile(".leveldb").getAbsolutePath + ".2")
  println(file.getAbsolutePath)

  val system = akka.actor.ActorSystem("cachetest", ConfigFactory.load("akkaoverrides.conf"))

  test("simple") {
    val cache = new LevelDBCache(file, akka.serialization.SerializationExtension(system))
    val td = TaskDescription(
      classOf[mybiotools.tasks.simpletask.SimpleTask.RunTask].getName,
      mybiotools.tasks.simpletask.SimpleTask.MyResultSet(Some(1), Some(0))
    )
    cache.set(
      td,
      mybiotools.tasks.simpletask.IntResult(1)
    )
    cache.shutDown
    val cache2 = new LevelDBCache(file, akka.serialization.SerializationExtension(system))

    val read = cache2.get(td)
    expectResult(Some(mybiotools.tasks.simpletask.IntResult(1)))(read)

  }

  test("1000 elements") {

    val file2 = new java.io.File(mybiotools.TempFile.createTempFile(".leveldb").getAbsolutePath + ".2")

    println(file2)

    val cache = new LevelDBCache(file2, akka.serialization.SerializationExtension(system))
    for (i <- 1 to 1000) {
      val td = TaskDescription(
        classOf[mybiotools.tasks.simpletask.SimpleTask.RunTask].getName,
        mybiotools.tasks.simpletask.SimpleTask.MyResultSet(Some(i), Some(0))
      )
      cache.set(
        td,
        mybiotools.tasks.simpletask.IntResult(i)
      )
    }
    cache.shutDown

    val cache2 = new LevelDBCache(file2, akka.serialization.SerializationExtension(system))

    for (i <- 1 to 1000) {
      val td = TaskDescription(
        classOf[mybiotools.tasks.simpletask.SimpleTask.RunTask].getName,
        mybiotools.tasks.simpletask.SimpleTask.MyResultSet(Some(i), Some(0))
      )
      val r = cache2.get(td)
      expectResult(Some((mybiotools.tasks.simpletask.IntResult(i))))(r)
    }

  }

}