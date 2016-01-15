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

trait Closeable {
  def close
}

case class CloseableWrap(x: { def close(): Unit }) extends Closeable {
  def close = x.close
}

// These case classes were originally defined in package.scala's package object 

/*
/**
 * Verifies fasta file line by line
 *
 * Accepts IUPAC nucleotide and proteinc codes. Capitalized or not. Accepts dash for gaps and asterisk(*) for stops.
 *
 * Fasta file should be fed line by line into next method.
 * The object has mutable state, calling finish resets the object.
 *
 * @example {{{
 *     val ff = FastaFormatLineByLineVerifier()
 *     ff.next(">h1")
 *     ff.next("atgtcatgcatgcatgcatgcatgc")
 *     ff.next(">h2")
 *     ff.next("atgtcatgcatgcatgcatgcatgc")
 *     ff.finish
 * }}}
 */
@deprecated("", "this is slow and bad design")
class FastaFormatLineByLineVerifier {
  private var first = true
  private var lastWasHeader = false

  /**
   * Verifies fasta line
   *
   * Accepts standard IUPAC nucleotide or protein codes.
   *
   * @param line next line of fasta file
   * @throws AssertionError on error (empty fasta entry or unaccepted character)
   */
  def next(line: String) {
    val startsWithHeader = line.startsWith(">")
    assert(!first || startsWithHeader, "First line of fasta file should start with >")
    assert(!lastWasHeader || !startsWithHeader, "Empty fasta entry")
    first = false
    if (startsWithHeader) lastWasHeader = true
    if (!startsWithHeader) {
      assert(line.matches("""[aAtTgGcC-\\*nNuUrRyYkKmMsSwWbBdDhHvVxXeEgGhHiIlLoOpPqQrRtT]*"""))
      lastWasHeader = false
    }
  }

  /**
   * Resets this object
   *
   * Further documentation.
   *
   * @throws AssertionError if last fasta line started with >
   */
  def finish {
    val tmp = lastWasHeader
    first = true
    lastWasHeader = false
    assert(tmp == false)
  }
}*/

/**
 * Represents a 2x2 Contingency table
 * @param a11 First row first column
 * @param a12 First row second column
 * @param a21 Second row first column
 * @param a22 Second row second column
 */
case class ContingencyTable2x2(a11: Int, a12: Int, a21: Int, a22: Int) {
  def sum = a11 + a12 + a21 + a22
  def rowMargin1 = a11 + a12
  def rowMargin2 = a21 + a22
  def columnMargin1 = a11 + a21
  def columnMargin2 = a12 + a22
}