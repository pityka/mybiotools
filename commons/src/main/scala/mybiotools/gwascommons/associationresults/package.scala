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

import scala.io.Source
import java.io.File
import mybiotools._
import gwascommons._
import stringstore._
import java.io.{ DataInputStream, DataOutputStream, ByteArrayOutputStream, ByteArrayInputStream, InputStream, IOException }

package object associationresults {

  trait HasPValue {
    def pValue: Double
  }

  trait HasPhenotypeName {
    def phenotypeName: String8
  }

  trait MightHaveInteractionSNP {
    def interaction: Option[String8]
  }

  trait HasEffectSize {
    def effectSize: Double
    def error: Option[Double]
    def allele: Allele
  }

  trait HasNonMissingCount {
    def nonMiss: Int
  }

  trait HasTest {
    def test: String8
  }

  trait AssociationResult extends HasName with HasGenomicLocation with HasPValue {

    def toLine = name + " " + genomicLocation.chromosome + " " + genomicLocation.basePairPosition + " " + pValue

  }

  type PlinkLinearLine = AssociationResult with HasEffectSize with HasNonMissingCount with HasTest
  type GWASAppLine = AssociationResult with HasEffectSize with HasNonMissingCount with HasTest with HasPhenotypeName with MightHaveInteractionSNP

  case class FullAssociationResult(
      name: String8,
      genomicLocation: GenomicLocation,
      pValue: Double,
      test: String8,
      effectSize: Double,
      statistic: Double,
      error: Option[Double],
      allele: Allele,
      phenotypeName: String8,
      interaction: Option[String8],
      nonMiss: Int,
      exception: Option[String]
  ) extends AssociationResult with HasEffectSize with HasNonMissingCount with HasTest with HasPhenotypeName with MightHaveInteractionSNP {
    import FullAssociationResult._

    override def toLine: String =
      genomicLocation.chromosome + " " + name.value + " " + genomicLocation.basePairPosition + " " + allele.nucleotides + " " + test.value + " " + nonMiss + " " + formatDouble(effectSize, 4) + " " + formatDouble(statistic, 4) + " " + pValue + " " + phenotypeName.value + " " + interaction.map(_.value).getOrElse("") + " " + error.map(x => formatDouble(x, 4)).getOrElse("NaN") + exception.map(x => " " + x.toString.replaceAllLiterally("\n", "").replaceAllLiterally("\r", "")).getOrElse("")

    def toBytes: Array[Byte] = {
      val baos = new ByteArrayOutputStream()
      val dos = new DataOutputStream(baos)

      writeString8(name, dos)
      dos.writeDouble(pValue)
      writeString8(test, dos)
      dos.writeDouble(effectSize)
      dos.writeBoolean(error.isDefined)
      if (error.isDefined) {
        dos.writeDouble(error.get)
      }
      val allelestr8 = StringStore(allele.nucleotides)
      writeString8(allelestr8, dos)
      writeString8(phenotypeName, dos)
      dos.writeBoolean(interaction.isDefined)
      if (interaction.isDefined) {
        writeString8(interaction.get, dos)
      }
      writeInt(nonMiss, dos)
      writeInt(chromosomeNumberFromString(genomicLocation.chromosome), dos)
      writeInt(genomicLocation.basePairPosition, dos)
      dos.writeDouble(statistic)
      dos.writeBoolean(exception.isDefined)
      if (exception.isDefined) {
        val errorstr8 = StringStore(exception.get)
        writeString8(errorstr8, dos)
      }
      baos.toByteArray
    }

  }

  object FullAssociationResult {

    def fillArray(ar: Array[Byte], is: InputStream) = {
      var len = ar.size
      val size = ar.size
      while (len > 0) {
        val c = is.read(ar, size - len, len)
        if (c < 0) throw new RuntimeException("unexpected end of stream")
        len -= c
      }

    }

    def readString8(dis: DataInputStream) = {
      val size = readInt(dis)
      val buffer = Array.ofDim[Byte](size)
      fillArray(buffer, dis)
      StringStore(buffer)
    }

    def readInt(dis: DataInputStream) = {
      val tipe = dis.readByte
      if (tipe == 1) dis.readByte.toInt else if (tipe == 2) dis.readShort.toInt else dis.readInt
    }

    def writeString8(s: String8, dos: DataOutputStream) = {
      val ar = s.chars
      val size = s.chars.size
      writeInt(size, dos)
      dos.write(ar, 0, size)
    }

    def writeInt(i: Int, dos: DataOutputStream) {
      val tipe = if (math.abs(i) <= Byte.MaxValue) 1 else if (math.abs(i) <= Short.MaxValue) 2 else 4
      dos.writeByte(tipe)
      if (tipe == 1) dos.writeByte(i.toByte) else if (tipe == 2) dos.writeShort(i.toShort) else dos.writeInt(i)
    }

    def encodeSize(a: Array[Byte]): Array[Byte] = {
      val tipe = if (a.size <= Byte.MaxValue) 1 else if (a.size <= Short.MaxValue) 2 else 4

      val n = Array.ofDim[Byte](a.size + tipe + 1)
      val size = {
        val buffer = java.nio.ByteBuffer.allocate(tipe + 1)
        buffer.put(tipe.toByte)
        if (tipe == 1) buffer.put(a.size.toByte) else if (tipe == 2) buffer.putShort(a.size.toShort) else buffer.putInt(a.size)

        buffer.array()
      }
      System.arraycopy(size, 0, n, 0, size.length)
      System.arraycopy(a, 0, n, size.length, a.size)
      n
    }

    def fromInputStream(is: java.io.InputStream): Iterator[FullAssociationResult] = new Iterator[FullAssociationResult] {
      def read: Option[FullAssociationResult] = {
        val tipe = is.read
        if (tipe < 0) {
          None
        } else {
          val sizea = Array.ofDim[Byte](tipe.toInt)
          fillArray(sizea, is)

          val size = if (tipe == 1) java.nio.ByteBuffer.wrap(sizea).get.toInt else if (tipe == 2) java.nio.ByteBuffer.wrap(sizea).getShort.toInt else java.nio.ByteBuffer.wrap(sizea).getInt
          val ar = Array.ofDim[Byte](size)
          fillArray(ar, is)
          Some(fromBytes(ar))

        }
      }

      var n: Option[FullAssociationResult] = read

      def hasNext = {
        n.isDefined
      }

      def next = {
        val r = n.get
        n = read
        r
      }

    }

    def fromBytes(bytes: Array[Byte]): FullAssociationResult = {
      val dis = new DataInputStream(new ByteArrayInputStream(bytes))

      val name = readString8(dis)
      val pvalue = dis.readDouble
      val test = readString8(dis)
      val effectSize = dis.readDouble
      val haserror = dis.readBoolean
      val error = if (haserror) {
        Some(dis.readDouble)
      } else None
      val allele = SimpleAllele.makeSingleton(readString8(dis).value)
      val phenoname = readString8(dis)
      val hasinteraction = dis.readBoolean
      val interaction = if (hasinteraction) Some(readString8(dis)) else None
      val nonmiss = readInt(dis)
      val chr = readInt(dis)
      val bp = readInt(dis)
      val statistic = dis.readDouble
      val hasexception = dis.readBoolean
      val exception = if (hasexception) Some(readString8(dis).value) else None
      FullAssociationResult(name, GenomicLocation(bp, chr), pvalue, test, effectSize, statistic, error, allele, phenoname, interaction, nonmiss, exception)

    }

  }

  private[associationresults] case class AssociationResultImpl(
    name: String8,
    genomicLocation: GenomicLocation,
    pValue: Double
  ) extends AssociationResult

  private[associationresults] case class AssociationResultWithTest(
      name: String8,
      genomicLocation: GenomicLocation,
      pValue: Double,
      test: String8
  ) extends AssociationResult with HasTest {

    override def toLine = super.toLine + test
  }

  private[associationresults] case class AssociationResultWithEffectSizeImpl(
      name: String8,
      genomicLocation: GenomicLocation,
      pValue: Double,
      effectSize: Double,
      allele: Allele,
      error: Option[Double]
  ) extends AssociationResult with HasEffectSize {
    override def toLine = super.toLine + " " + effectSize + " " + allele + " " + error.getOrElse("NA")
  }

  private[associationresults] case class AssociationResultWithEffectSizeWithMafImpl(
      name: String8,
      genomicLocation: GenomicLocation,
      pValue: Double,
      effectSize: Double,
      allele: Allele,
      error: Option[Double],
      maf: Double
  ) extends AssociationResult with HasEffectSize with HasMaf {

    override def toLine = super.toLine + " " + effectSize + " " + allele + " " + error.getOrElse("NA") + " " + maf
  }

  private[associationresults] case class AssociationResultWithEffectSizeWithNonMissImplWithTest(
      name: String8,
      genomicLocation: GenomicLocation,
      pValue: Double,
      effectSize: Double,
      allele: Allele,
      nonMiss: Int,
      test: String8,
      error: Option[Double]
  ) extends AssociationResult with HasEffectSize with HasNonMissingCount with HasTest {

    override def toLine = super.toLine + " " + effectSize + " " + allele + " " + error.getOrElse("NA") + " " + nonMiss + " " + test
  }

  private[associationresults] case class AssociationResultWithEffectSizeWithNonMissImplWithTestWithPhenoname(
      name: String8,
      genomicLocation: GenomicLocation,
      pValue: Double,
      effectSize: Double,
      allele: Allele,
      nonMiss: Int,
      test: String8,
      error: Option[Double],
      phenotypeName: String8
  ) extends AssociationResult with HasEffectSize with HasNonMissingCount with HasTest with HasPhenotypeName {

    override def toLine = super.toLine + " " + effectSize + " " + allele + " " + error.getOrElse("NA") + " " + nonMiss + " " + test + " " + phenotypeName
  }

  private[associationresults] case class AssociationResultWithEffectSizeWithNonMissImpl(
      name: String8,
      genomicLocation: GenomicLocation,
      pValue: Double,
      effectSize: Double,
      allele: Allele,
      nonMiss: Int,
      error: Option[Double]
  ) extends AssociationResult with HasEffectSize with HasNonMissingCount {
    override def toLine = super.toLine + " " + effectSize + " " + allele + " " + error.getOrElse("NA") + " " + nonMiss
  }

  def readAssociationResultsFromGWASApp = readAssociationResultsFromFileWithHeader(_: Source, _: Option[File], "P", "SNP", Some("CHR"), Some("BP"), Some("BETA"), Some("A1"), Some("NMISS"), None, Some("TEST"), None, Some("PHENO")).asInstanceOf[Iterator[AssociationResult with HasEffectSize with HasNonMissingCount with HasTest with HasPhenotypeName]]

  def readAssociationResultsFromPlinkLinearAssoc = readAssociationResultsFromFileWithHeader(_: Source, _: Option[File], "P", "SNP", Some("CHR"), Some("BP"), Some("BETA"), Some("A1"), Some("NMISS"), None, Some("TEST"), None, None).asInstanceOf[Iterator[AssociationResult with HasEffectSize with HasNonMissingCount with HasTest]]

  def readAssociationResultsFromPlinkLogisticAssoc = readAssociationResultsFromFileWithHeader(_: Source, _: Option[File], "P", "SNP", Some("CHR"), Some("BP"), Some("OR"), Some("A1"), Some("NMISS"), None, Some("TEST"), None, None).asInstanceOf[Iterator[AssociationResult with HasEffectSize with HasNonMissingCount with HasTest]]

  def readAssociationResultsFromPlinkFileWithEffectWithCount(eh: String, allele: String = "A1", nmiss: String = "NMISS", error: String = "SE") = readAssociationResultsFromFileWithHeader(_: Source, _: Option[File], "P", "SNP", Some("CHR"), Some("BP"), Some(eh), Some(allele), Some(nmiss), Some(error), Some("TEST"), None, None).asInstanceOf[Iterator[AssociationResult with HasEffectSize with HasNonMissingCount with HasTest]]

  def readAssociationResultsFromPlinkLinearDosageAssoc = readAssociationResultsFromFileWithHeader(_: Source, _: Option[File], "P", "SNP", Some("CHR"), Some("BP"), Some("BETA"), Some("A1"), None, Some("SE"), None, Some("FRQ"), None).asInstanceOf[Iterator[AssociationResult with HasEffectSize with HasMaf]]

  def readAssociationResultsFromPlinkLogisticDosageAssoc = readAssociationResultsFromFileWithHeader(_: Source, _: Option[File], "P", "SNP", Some("CHR"), Some("BP"), Some("OR"), Some("A1"), None, Some("SE"), None, Some("FRQ"), None).asInstanceOf[Iterator[AssociationResult with HasEffectSize with HasMaf]]

  def readAssociationResultsFromFileWithHeader(
    assoc: Source,
    genomicMapFile: Option[File],
    pheader: String,
    snpheader: String,
    chrheader: Option[String],
    bpheader: Option[String]
  ): Iterator[AssociationResult] = readAssociationResultsFromFileWithHeader(assoc, genomicMapFile, pheader, snpheader, chrheader, bpheader, None, None, None, None, None, None, None)

  def readAssociationResultsFromFileWithHeader(
    assoc: Source,
    genomicMapFile: Option[File],
    pheader: String,
    snpheader: String,
    chrheader: Option[String],
    bpheader: Option[String],
    effectheader: Option[String],
    alleleheader: Option[String],
    nonMissheader: Option[String],
    effectErrorHeader: Option[String],
    testHeader: Option[String],
    frqHeader: Option[String],
    phenoHeader: Option[String]
  ): Iterator[AssociationResult] =
    {
      val iter = assoc.getLines
      val header = fastSplitSetSeparator(iter.next, Set(' ', '\t'))
      val pcolumn = header.indexOf(pheader)
      val snpcolumn = header.indexOf(snpheader)
      val chrcolumn = chrheader.map(x => header.indexOf(x))
      val bpcolumn = bpheader.map(x => header.indexOf(x))
      val effectcolumn = effectheader.map(x => header.indexOf(x))
      val allelecolumn = alleleheader.map(x => header.indexOf(x))
      val nonMissColumn = nonMissheader.map(x => header.indexOf(x))
      val errorColumn = effectErrorHeader.map(x => header.indexOf(x))
      val frqColumn = frqHeader.map(x => header.indexOf(x))
      val phenoColumn = phenoHeader.map(x => header.indexOf(x))

      val testColumn = testHeader.map(x => header.indexOf(x))

      if (snpcolumn == -1 || pcolumn == -1) {
        throw new RuntimeException("snpcolumn or pcolumn not found in header. check your association files.")
      }
      if ((effectheader.isDefined && effectcolumn.get == -1) ||
        (alleleheader.isDefined && allelecolumn.get == -1) ||
        (effectErrorHeader.isDefined && errorColumn.get == -1)) {
        throw new RuntimeException("effect, SE or allele column not found in header. check your association files.")
      }

      readAssociationResultsFromFileWithoutHeader(
        iter,
        genomicMapFile,
        snpcolumn,
        pcolumn,
        chrcolumn,
        bpcolumn,
        effectcolumn,
        allelecolumn,
        nonMissColumn,
        errorColumn,
        testColumn,
        frqColumn,
        phenoColumn
      )
    }

  def readAssociationResultsFromFileWithoutHeader(
    iter: Iterator[String],
    genomicMapFile: Option[File],
    snpcolumn: Int,
    pcolumn: Int,
    chrcolumn: Option[Int],
    bpcolumn: Option[Int],
    effectcolumn: Option[Int],
    allelecolumn: Option[Int],
    nonMissColumn: Option[Int],
    errorColumn: Option[Int],
    testColumn: Option[Int],
    frqColumn: Option[Int],
    phenoColumn: Option[Int]
  ): Iterator[AssociationResult] = {

    val genomicmap: Option[GenomicMap] = if (bpcolumn.isDefined && chrcolumn.isDefined) None else {
      Some(getGenomicMapFromBimFile(genomicMapFile.get.getAbsolutePath))
    }

    // val maxindex : Int = (List(chrcolumn,bpcolumn,effectcolumn,allelecolumn,nonMissColumn,errorColumn,testColumn,frqColumn,phenoColumn).map(_.toList).flatten ++ List(pcolumn,snpcolumn)).max
    val buffer = scala.collection.mutable.ArrayBuffer[String]()
    val seps = Set(' ', '\t')
    iter.map { line =>
      storeIterInArrayAll(fastSplitSetSeparatorIterator(line, seps), buffer)
      val pvaluestring = buffer(pcolumn)
      val pvalue = scala.util.Try(pvaluestring.toDouble).getOrElse(Double.NaN)
      val snp = StringStore(new java.lang.String(buffer(snpcolumn)))
      val gl = {
        if (genomicmap.isEmpty) util.Try { GenomicLocation(buffer(bpcolumn.get).toInt, new java.lang.String(buffer(chrcolumn.get))) }.toOption.getOrElse {
          val spl = fastSplit1WideSeparatorIterator(snp, ':')
          val chr = spl.next
          val bp = spl.next.toInt
          GenomicLocation(bp, new java.lang.String(chr))
        }
        else genomicmap.get.get(snp).getOrElse {
          util.Try(GenomicLocation(buffer(bpcolumn.get).toInt, new java.lang.String(buffer(chrcolumn.get)))).toOption.getOrElse {
            val spl = fastSplit1WideSeparatorIterator(snp, ':')
            val chr = spl.next
            val bp = spl.next.toInt
            GenomicLocation(bp, new java.lang.String(chr))
          }
        }
      }
      val effectAlleleError = effectcolumn.map { effectcolumn =>
        val effectstr = buffer(effectcolumn)
        val effect = if (effectstr == "NA") Double.NaN else effectstr.toDouble
        val allele = SimpleAllele.makeSingleton(new java.lang.String(buffer(allelecolumn.get)))
        val error = errorColumn.map { col =>
          val errorStr = buffer(col)
          if (errorStr == "NA") Double.NaN else errorStr.toDouble
        }
        (effect, allele, error)
      }
      val nonMiss = nonMissColumn.map(c => scala.util.Try(buffer(c).toInt).toOption.getOrElse(0))

      val testStr = testColumn.map(c => StringStore(buffer(c)))

      val phenoName = phenoColumn.map(c => StringStore(buffer(c)))

      val maf = frqColumn.map(c => scala.util.Try(buffer(c).toDouble).toOption.getOrElse(Double.NaN))

      effectAlleleError.map {
        case (effect, allele1, error1) =>

          nonMiss.map { nonMiss1 =>

            testStr map { test1 =>
              phenoName map { phenoname1 =>

                AssociationResultWithEffectSizeWithNonMissImplWithTestWithPhenoname(
                  name = snp,
                  genomicLocation = gl,
                  pValue = pvalue,
                  effectSize = effect,
                  allele = allele1,
                  error = error1,
                  test = test1,
                  nonMiss = nonMiss1,
                  phenotypeName = phenoname1
                )

              } getOrElse {
                AssociationResultWithEffectSizeWithNonMissImplWithTest(
                  name = snp,
                  genomicLocation = gl,
                  pValue = pvalue,
                  effectSize = effect,
                  allele = allele1,
                  error = error1,
                  test = test1,
                  nonMiss = nonMiss1
                )
              }
            } getOrElse {
              AssociationResultWithEffectSizeWithNonMissImpl(
                name = snp,
                genomicLocation = gl,
                pValue = pvalue,
                effectSize = effect,
                allele = allele1,
                error = error1,
                nonMiss = nonMiss1
              )
            }
          } getOrElse {
            maf map { maf1 =>
              AssociationResultWithEffectSizeWithMafImpl(
                name = snp,
                genomicLocation = gl,
                pValue = pvalue,
                effectSize = effect,
                allele = allele1,
                error = error1,
                maf = maf1
              )

            } getOrElse {
              AssociationResultWithEffectSizeImpl(
                name = snp,
                genomicLocation = gl,
                pValue = pvalue,
                effectSize = effect,
                allele = allele1,
                error = error1
              )
            }

          }
      } getOrElse {
        testStr map { test =>
          AssociationResultWithTest(name = snp, genomicLocation = gl, pValue = pvalue, test = test)
        } getOrElse {
          AssociationResultImpl(name = snp, genomicLocation = gl, pValue = pvalue)
        }

      }

    }.filterNot(_.pValue.isNaN)
  }

}