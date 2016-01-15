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

package object vcfhelpers {

  def minimalRepresentation(ref: String, alt: String, position: Int): MinimalRepresentation =
    // snps and symbolics keep unchanged
    if ((ref.length == 1 && alt.length == 1) || (ref.startsWith("<") || alt.startsWith("<"))) MinimalRepresentation(ref, alt, position)
    else {
      @scala.annotation.tailrec
      def stripPrefix(ref: String, alt: String, position: Int, idx: Int): (String, String, Int) = (ref, alt) match {
        case (r, a) if r.length - 1 <= idx || a.length - 1 <= idx => if (idx == 0) (r, a, position) else (r.substring(idx), a.substring(idx), position)
        case (r, a) if r.charAt(idx) != a.charAt(idx) => if (idx == 0) (r, a, position) else (r.substring(idx), a.substring(idx), position)
        case (r, a) => stripPrefix(r, a, position + 1, idx + 1)
      }

      val (refSuffixDropped, altSuffixDropped, _) = stripPrefix(ref.reverse, alt.reverse, 0, 0)

      val (refPrefixDropped, altPrefixDropped, newposition) = stripPrefix(refSuffixDropped.reverse, altSuffixDropped.reverse, position, 0)

      MinimalRepresentation(refPrefixDropped, altPrefixDropped, newposition)

    }

}