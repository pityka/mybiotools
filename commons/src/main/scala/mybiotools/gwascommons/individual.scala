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

package mybiotools.gwascommons
import mybiotools.stringstore._

trait Individual {
  val FID: String8
  val IID: String8

  def id = FID

  def toLine = FID + " " + IID

  def toFamLine = familyID + " " + individualID + " 0 0 0 -9"

  def familyID = FID.value

  def individualID = IID.value

  override def toString = FID + "_" + IID
}

object Individual {
  private case class IndImpl(FID: String8, IID: String8) extends Individual {
    override val hashCode = 41 * (FID.hashCode + 41 * IID.hashCode) + 41
  }

  def apply(s: String): Individual = IndImpl(StringStore(s), StringStore("1"))

  def apply(fid: String, iid: String): Individual = IndImpl(StringStore(fid), StringStore(iid))

  def apply(fid: String8, iid: String8): Individual = IndImpl(fid, iid)

  def fromFamLine(line: String) = {
    val spl = mybiotools.fastSplitSetSeparator(line, Set(' ', '\t'))
    apply(spl(0), spl(1))
  }

  def unapply(d: Individual): Option[(String, String)] = Some((d.FID.value, d.IID.value))
}

