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

package dispensability

import mybiotools.config.Config.configInstance
import mybiotools._
import java.io.File
import htsjdk.variant.variantcontext.writer.VariantContextWriterFactory
import htsjdk.samtools.SAMSequenceDictionary
import htsjdk.samtools.SAMSequenceRecord
import scala.collection.JavaConversions._
import htsjdk.variant.vcf.{ VCFHeader, VCFHeaderLine }
import htsjdk.variant.variantcontext.VariantContextBuilder
import htsjdk.variant.variantcontext.GenotypeBuilder
import htsjdk.variant.variantcontext._
import htsjdk.variant.vcf._
import mybiotools.stringstore._
import mybiotools.gwascommons._
import vcfhelpers._

object JoinAntoniosFile extends App {

  implicit private val codec = scala.io.Codec("ISO-8859-1")

  case class Genotypes(homref: Int, het: Int, homvar: Int) {
    def +(that: Genotypes) = Genotypes(this.homref + that.homref, this.het + that.het, this.homvar + that.homvar)
  }

  def update(iter: Iterator[VariantWithoutAnnotation], queryvariants: collection.mutable.Map[VariantKey, (Option[Genotypes])], file: String): Unit = {

    iter.foreach { variant =>

      synchronized {
        queryvariants.get(variant.variantkey) match {
          case None => {}
          case Some((maybegenotypes)) => {

            val homvar = variant.homVarCount
            val homref = variant.homRefCount
            val het = variant.hetCount
            val genotypes = maybegenotypes match {
              case None => Genotypes(0, 0, 0)
              case Some(g) => g
            }
            if (homvar + homref + het == 0) {
              println("all genotypes 0! " + file + " " + variant)
            }

            queryvariants.update(variant.variantkey, Some(genotypes + Genotypes(homref, het, homvar)))

          }
        }
      }

    }

  }

  val antoniosFile = args(0)
  val vcflist = args(1)
  val output = args(2)

  val antoniosPositions: collection.mutable.Map[VariantKey, (Option[Genotypes])] = openSource(antoniosFile)(s => scala.collection.mutable.Map(s.getLines.drop(1).map { line =>
    val spl = fastSplit1WideSeparator(line, '\t')
    val positionstring = spl(0)
    val alleles = spl(1)
    val ccds = StringStore(spl(2))
    val positionspl = fastSplit1WideSeparator(positionstring, ':')
    val allelesspl = fastSplit1WideSeparator(alleles, '>')
    val chr = positionspl(0)
    val bp1 = positionspl(1)
    val ref1 = allelesspl(0)
    val alt1 = {
      val s = alleles
      val index = s.indexOf(">")
      s.substring(index + 1, s.size)
    }
    val vcfhelpers.MinimalRepresentation(minRef, minAlt, minBp) = vcfhelpers.minimalRepresentation(ref1, alt1, bp1.toInt)
    val gloc = GenomicLocation(minBp, chr)
    val vkey = VariantKey(gloc, StringStore(minRef), StringStore(minAlt))
    vkey -> (None)
  }.toSeq: _*))

  println("Unique keys in Antonio's file after conversion to minimal representation: " + antoniosPositions.size)

  openSource(vcflist)(_.getLines.toList.foreach { f =>

    if (openSource(f)(s => Input.isEVSVCF(s))) {
      openSource(f)(s => update(Input.readGenotypeCountsFromVCF(s)(Input.extractGenotypeCountFromEVS), antoniosPositions, f))

    } else if (openSource(f)(s => Input.isExACVCF(s))) {
      openSource(f)(s => update(Input.readGenotypeCountsFromVCF(s)(Input.extractGenotypeCountFromEXAC), antoniosPositions, f))

    } else if (openSource(f)(s => Input.isGONLVCF(s))) {
      openSource(f)(s => update(Input.readGenotypeCountsFromVCF(s)(Input.extractGenotypeCountFromGONL), antoniosPositions, f))

    } else {
      openSource(f)(s => update(Input.readGenotypeCountsFromVCF(s)(Input.extractGenotypeCountFromGeneralVCF), antoniosPositions, f))
    }

  })

  openFileWriter(new java.io.File(output)) { writer =>
    antoniosPositions.foreach {
      case (VariantKey(gloc, ref, alt), (maybegenotype)) =>
        maybegenotype match {
          case Some(Genotypes(homref, het, homvar)) => {
            writer.write(s"${gloc.chromosome}:${gloc.basePairPosition}\t$ref\t$alt\t$homref\t$het\t$homvar\n")

          }
          case None => {}
        }
    }
  }

}

object JoinAntoniosFileMyParser extends App {

  implicit private val codec = scala.io.Codec("ISO-8859-1")

  case class Genotypes(homref: Int, het: Int, homvar: Int) {
    def +(that: Genotypes) = Genotypes(this.homref + that.homref, this.het + that.het, this.homvar + that.homvar)
  }

  def update(iter: Iterator[VariantWithoutAnnotation], queryvariants: collection.mutable.Map[VariantKey, (Option[Genotypes])], file: String): Unit = {

    iter.foreach { variant =>

      synchronized {
        queryvariants.get(variant.variantkey) match {
          case None => {}
          case Some((maybegenotypes)) => {

            val homvar = variant.homVarCount
            val homref = variant.homRefCount
            val het = variant.hetCount
            val genotypes = maybegenotypes match {
              case None => Genotypes(0, 0, 0)
              case Some(g) => g
            }
            if (homvar + homref + het == 0) {
              println("all genotypes 0! " + file + " " + variant)
            }

            queryvariants.update(variant.variantkey, Some(genotypes + Genotypes(homref, het, homvar)))

          }
        }
      }

    }

  }

  val antoniosFile = args(0)
  val vcflist = args(1)
  val output = args(2)
  val cpus = args(3).toInt

  val antoniosPositions: collection.mutable.Map[VariantKey, (Option[Genotypes])] = openSource(antoniosFile)(s => scala.collection.mutable.Map(s.getLines.drop(1).map { line =>
    val spl = fastSplit1WideSeparator(line, '\t')
    val positionstring = spl(0)
    val alleles = spl(1)
    val ccds = StringStore(spl(2))
    val positionspl = fastSplit1WideSeparator(positionstring, ':')
    val allelesspl = fastSplit1WideSeparator(alleles, '>')
    val chr = positionspl(0)
    val bp1 = positionspl(1)
    val ref1 = allelesspl(0)
    val alt1 = {
      val s = alleles
      val index = s.indexOf(">")
      s.substring(index + 1, s.size)
    }
    val vcfhelpers.MinimalRepresentation(minRef, minAlt, minBp) = vcfhelpers.minimalRepresentation(ref1, alt1, bp1.toInt)
    val gloc = GenomicLocation(minBp, chr)
    val vkey = VariantKey(gloc, StringStore(minRef), StringStore(minAlt))
    vkey -> (None)
  }.toSeq: _*))

  println("Unique keys in Antonio's file after conversion to minimal representation: " + antoniosPositions.size)

  openSource(vcflist)(_.getLines.toList.foreach { f =>

    if (openSource(f)(s => Input.isEVSVCF(s))) {
      openSource(f)(s => update(Input.readGenotypeCountsFromVCFMyParser(s, cpus)(Input.extractGenotypeCountFromEVS), antoniosPositions, f))

    } else if (openSource(f)(s => Input.isExACVCF(s))) {
      openSource(f)(s => update(Input.readGenotypeCountsFromVCFMyParser(s, cpus)(Input.extractGenotypeCountFromEXAC), antoniosPositions, f))

    } else if (openSource(f)(s => Input.isGONLVCF(s))) {
      openSource(f)(s => update(Input.readGenotypeCountsFromVCFMyParser(s, cpus)(Input.extractGenotypeCountFromGONL), antoniosPositions, f))

    } else {
      openSource(f)(s => update(Input.readGenotypeCountsFromVCFMyParser(s, cpus)(Input.extractGenotypeCountFromGeneralVCF), antoniosPositions, f))
    }

  })

  openFileWriter(new java.io.File(output)) { writer =>
    antoniosPositions.foreach {
      case (VariantKey(gloc, ref, alt), (maybegenotype)) =>
        maybegenotype match {
          case Some(Genotypes(homref, het, homvar)) => {
            writer.write(s"${gloc.chromosome}:${gloc.basePairPosition}\t$ref\t$alt\t$homref\t$het\t$homvar\n")
          }
          case None => {}
        }

    }
  }

}