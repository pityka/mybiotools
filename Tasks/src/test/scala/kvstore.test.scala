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

import java.io._

import org.scalatest.FunSpec
import org.scalatest.Matchers

class SimpleKeyValueStoreSpec extends FunSpec with Matchers {

  describe("empty") {
    val ba = new java.io.ByteArrayInputStream(Array())
    val ds = new DataInputStream(ba)
    it("should be empty") {
      SimpleKeyValueStore.reconstruct(ds).toList should equal(List())
    }
  }

  describe("short nonempty") {
    it("test") {
      val ba = new java.io.ByteArrayOutputStream()
      val ds = new DataOutputStream(ba)
      val skvs = new SimpleKeyValueStore(ds)
      skvs.put(Array(1), Array(2))
      skvs.put(Array(1), Array(3))
      skvs.put(Array(2), Array(4))
      SimpleKeyValueStore.reconstruct(new DataInputStream(new ByteArrayInputStream(ba.toByteArray))).toList.map(x => (x._1.deep, x._2.deep)) should equal(List(
        (Array(1), Array(2)),
        (Array(1), Array(3)),
        (Array(2), Array(4))
      ).map(x => (x._1.deep, x._2.deep)))
    }
  }

  describe("long nonempty") {
    it("test") {
      val ba = new java.io.ByteArrayOutputStream()
      val ds = new DataOutputStream(ba)
      val skvs = new SimpleKeyValueStore(ds)

      for (i <- 1 to 10000) {
        skvs.put(Array(1), Array(2))
      }

      SimpleKeyValueStore.reconstruct(new DataInputStream(new ByteArrayInputStream(ba.toByteArray))).toList.size should equal(10000)

    }
  }

}

class LevelDBWrapperKeyValueStoreSpec extends FunSpec with Matchers {

  describe("leveldb wrapper") {
    it("simple") {
      val tmp = mybiotools.TempFile.createTempFile(".leveldb")
      tmp.delete
      val lw = new LevelDBWrapper(tmp)
      lw.put(Array(0, 1, 3), Array(0, 1, 3))
      lw.get(Array(0, 1, 3)).get.deep should equal(Array(0, 1, 3).deep)
      lw.close
    }
    it("simple 2") {
      val tmp = mybiotools.TempFile.createTempFile(".leveldb")
      tmp.delete
      val lw = new LevelDBWrapper(tmp)
      lw.put(Array(0, 1, 3), Array(0, 1, 3))
      lw.get(Array(0, 1, 3)).get.deep should equal(Array(0, 1, 3).deep)
      lw.put(Array(0, 1, 3), Array(0, 1, 3))
      lw.get(Array(0, 1, 3)).get.deep should equal(Array(0, 1, 3).deep)
      lw.put(Array(0, 1, 4), Array(0, 1, 3))
      lw.put(Array(0, 1, 5), Array(0, 1, 3))
      lw.put(Array(0, 1, 6), Array(0, 1, 6))
      lw.put(Array(0, 1, 7), Array(0, 1, 6))
      lw.put(Array(0, 1, 7), Array(0, 1, 8))
      lw.get(Array(0, 1, 4)).get.deep should equal(Array(0, 1, 3).deep)
      lw.get(Array(0, 1, 5)).get.deep should equal(Array(0, 1, 3).deep)
      lw.get(Array(0, 1, 6)).get.deep should equal(Array(0, 1, 6).deep)
      lw.get(Array(0, 1, 7)).get.deep should equal(Array(0, 1, 8).deep)
      lw.close
    }
    it("overwrite") {
      val tmp = mybiotools.TempFile.createTempFile(".leveldb")
      tmp.delete
      val lw = new LevelDBWrapper(tmp)
      lw.put(Array(0, 1, 3), Array(0, 1, 3))
      lw.get(Array(0, 1, 3)).get.deep should equal(Array(0, 1, 3).deep)
      lw.put(Array(0, 1, 3), Array(0, 1, 2))
      lw.get(Array(0, 1, 3)).get.deep should equal(Array(0, 1, 2).deep)
      lw.close
    }
    it("big") {
      val tmp = mybiotools.TempFile.createTempFile(".leveldb")
      try {
        tmp.delete
        val tenmeg = Array.fill[Byte](1E7.toInt)(0)
        val lw = new LevelDBWrapper(tmp)
        0 until 100 foreach { i =>
          lw.put(java.nio.ByteBuffer.allocate(4).putInt(i).array, tenmeg)
        }
        lw.close
        val lw2 = new LevelDBWrapper(tmp)
        0 until 100 foreach { i =>
          assert(lw2.get(java.nio.ByteBuffer.allocate(4).putInt(i).array).get.deep == (tenmeg.deep))
        }
        100 until 200 foreach { i =>
          lw2.put(java.nio.ByteBuffer.allocate(4).putInt(i).array, tenmeg)
        }
        lw2.close
        val lw3 = new LevelDBWrapper(tmp)
        0 until 200 foreach { i =>
          assert(lw3.get(java.nio.ByteBuffer.allocate(4).putInt(i).array).get.deep == (tenmeg.deep))
        }
        lw3.close
      } finally {
        tmp.listFiles.map(_.delete)
      }

    }
  }

}