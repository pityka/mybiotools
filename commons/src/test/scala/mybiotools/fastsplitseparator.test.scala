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
import org.scalatest.FunSuite
import scala.io.Source

import mybiotools._

class FastSplitSeparatorTestSuite extends FunSuite {

  test("fastSplitSeparator") {
    assertResult(IndexedSeq("1", "2", "3")) { fastSplitSeparator("1,2,3", ',') }
    assertResult(IndexedSeq("1", "2", "3")) { fastSplitSeparator("1,2,,,,3", ',') }
    assertResult(IndexedSeq("1", "2", "3")) { fastSplitSeparator(",,,1,,2,,,3,,,", ',') }
    assertResult(IndexedSeq("1", "2", "3")) { fastSplitSeparator("\n\n\n1\n\n2\n\n\n3\n\n\n", '\n') }
    assertResult(IndexedSeq("1", "2", "3")) { fastSplitSetSeparator("\n\n\n1\n\n2\n\n\n3\n\n\n", Set('\n', '\t')) }
    assertResult(IndexedSeq("1", "2", "3")) { fastSplitSeparator(",1,2,3", ',') }
    assertResult(IndexedSeq("1", "2", "3")) { fastSplitSeparator(",,,,,,1,,,,,2,,,,,3,,,,,,", ',') }
    assertResult(IndexedSeq("1", "2", "3")) { fastSplitSeparator("1,,,,,,,,,,,2,,,,,,,,,,,3", ',') }
    assertResult(IndexedSeq()) { fastSplitSeparator("", ',') }
    assertResult(IndexedSeq()) { fastSplitSeparator(",,,,,", ',') }
    assertResult(IndexedSeq()) { fastSplitSetSeparator(",,,,,", Set(',')) }
    assertResult(IndexedSeq()) { fastSplitSeparator(",", ',') }
    assertResult(IndexedSeq("1")) { fastSplitSeparator(",1", ',') }
    assertResult(IndexedSeq("1")) { fastSplitSeparator("1,", ',') }
    assertResult(IndexedSeq("  1")) { fastSplitSeparator("  1,,,,,,", ',') }
    assertResult(IndexedSeq("1  ")) { fastSplitSeparator("1  ,,,,,", ',') }
    assertResult(IndexedSeq("1")) { fastSplitSeparator(",,1,,,,", ',') }
  }

  test("fastSplit1WideSeparator") {
    assertResult(IndexedSeq("1", "2", "3")) { fastSplit1WideSeparator("1,2,3", ',') }
    assertResult(IndexedSeq("1", "2", "", "", "", "3")) { fastSplit1WideSeparator("1,2,,,,3", ',') }
    assertResult(IndexedSeq("", "", "", "1", "", "2", "", "", "3", "", "", "")) { fastSplit1WideSeparator(",,,1,,2,,,3,,,", ',') }
    assertResult(IndexedSeq("", "", "", "1", "", "2", "", "", "3", "", "", "")) { fastSplit1WideSeparator("\n\n\n1\n\n2\n\n\n3\n\n\n", '\n') }
    assertResult(IndexedSeq("", "1", "2", "3")) { fastSplit1WideSeparator(",1,2,3", ',') }
    assertResult(IndexedSeq("")) { fastSplit1WideSeparator("", ',') }
    assertResult(IndexedSeq("asdf")) { fastSplit1WideSeparator("asdf", ',') }
    assertResult(IndexedSeq("", "", "", "", "", "")) { fastSplit1WideSeparator(",,,,,", ',') }
    assertResult(IndexedSeq("", "")) { fastSplit1WideSeparator(",", ',') }
    assertResult(IndexedSeq("", "1")) { fastSplit1WideSeparator(",1", ',') }
    assertResult(IndexedSeq("1", "")) { fastSplit1WideSeparator("1,", ',') }
    assertResult(IndexedSeq("  1", "", "", "", "", "", "")) { fastSplit1WideSeparator("  1,,,,,,", ',') }
    assertResult(IndexedSeq("1  ", "", "", "", "", "")) { fastSplit1WideSeparator("1  ,,,,,", ',') }
    assertResult(IndexedSeq("", "", "", "", "", "")) { fastSplit1WideSetSeparator(",,,,,", Set(',')) }
    assertResult(IndexedSeq("", "", "", "1", "", "2", "", "", "3", "", "", "")) { fastSplit1WideSetSeparator("\n\n\n1\n\n2\n\n\n3\n\n\n", Set('\n', '\t')) }

  }

  test("store in array shared all") {
    val mut = scala.collection.mutable.ArrayBuffer[String]();
    assertResult(IndexedSeq("1", "2", "3")) { storeIterInArrayAll(fastSplit1WideSeparatorIterator("1,2,3", ','), mut); mut.toVector }
    assertResult(IndexedSeq("4", "5", "6")) { storeIterInArrayAll(fastSplit1WideSeparatorIterator("4,5,6", ','), mut); mut.toVector }

  }

  test("store in array shared interval") {
    {
      val mut = scala.collection.mutable.ArrayBuffer[String]();
      assertResult(IndexedSeq("1")) { storeIterInArrayInterval(fastSplit1WideSeparatorIterator("1,2,3", ','), mut, 0, 1); mut.toVector }
      assertResult(IndexedSeq("4")) { storeIterInArrayInterval(fastSplit1WideSeparatorIterator("4,5,6", ','), mut, 0, 1); mut.toVector }
    }

    {
      val mut = scala.collection.mutable.ArrayBuffer[String]();
      assertResult(IndexedSeq("1", "2")) { storeIterInArrayInterval(fastSplit1WideSeparatorIterator("1,2,3", ','), mut, 0, 2); mut.toVector }
      assertResult(IndexedSeq("4", "5")) { storeIterInArrayInterval(fastSplit1WideSeparatorIterator("4,5,6", ','), mut, 0, 2); mut.toVector }
    }

    {
      val mut = scala.collection.mutable.ArrayBuffer[String]();
      assertResult(IndexedSeq("1", "2", "3")) { storeIterInArrayInterval(fastSplit1WideSeparatorIterator("1,2,3", ','), mut, 0, 5); mut.toVector }
      assertResult(IndexedSeq("4", "5", "6")) { storeIterInArrayInterval(fastSplit1WideSeparatorIterator("4,5,6", ','), mut, 0, 5); mut.toVector }
    }
    {
      val mut = scala.collection.mutable.ArrayBuffer[String]();
      assertResult(IndexedSeq("2", "3")) { storeIterInArrayInterval(fastSplit1WideSeparatorIterator("1,2,3", ','), mut, 1, 5); mut.toVector }
      assertResult(IndexedSeq("5", "6")) { storeIterInArrayInterval(fastSplit1WideSeparatorIterator("4,5,6", ','), mut, 1, 5); mut.toVector }
    }
  }

}