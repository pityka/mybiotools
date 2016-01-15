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

package mybiotools.gwascommons.genotypedata

import org.scalatest.FunSuite

import mybiotools._
import mybiotools.gwascommons._
import mybiotools.gwascommons.genotypedata._
import GenotypeStates._
import scala.collection.mutable.Map
import mybiotools.stringstore._
import org.scalatest.Matchers

class ReadersTestSuite extends FunSuite with Matchers {

  test("pdosage reader") {
    val str = ("""SNP A1 A2 F1 I1 F2 I2
            |snp1 A T 1.0 1.3
            |snp2 G C 1.2 -8
            |""".stripMargin)
    val tmp = TempFile.createTempFile("")
    openBlockedZippedFileWriter(tmp)(wr => wr.write(str))
    val index = BGZippedGenotypeHelper.makeIndex(tmp).toMap
    val q = new DelimitedSNPMajorQuery(BGZPDose(tmp, -8f, None, index))

    q.query(new HasName { def name = s8"snp1" }).get._2.toList should equal(List(1.0f, 1.3f))
    q.query(new HasName { def name = s8"snp2" }).get._2.toList should equal(List(1.2f, -8f))

  }

  // gcta
  // 	1	1	5.000000e+00	8.799999e-01
  // 2	1	5.000000e+00	-7.200000e-01
  // 2	2	5.000000e+00	8.799999e-01
  // 3	1	5.000000e+00	-1.600000e-01
  // 3	2	5.000000e+00	-1.600000e-01
  // 3	3	5.000000e+00	3.200000e-01
  test("grm tiny") {
    val tpedfile = new java.io.File(getClass.getResource("/").getPath + "genotypedata/grmtest_tiny.tped")
    val tfamfile = new java.io.File(getClass.getResource("/").getPath + "genotypedata/grmtest_tiny.tfam")
    val tpedfiles = TPedFiles(fam = scala.io.Source.fromFile(tfamfile), tped = scala.io
      .Source.fromFile(tpedfile), '-')
    val tpedfiles2 = TPedFiles(fam = scala.io.Source.fromFile(tfamfile), tped = scala.io.Source.fromFile(tpedfile), '-')
    val reader1 = SNPMajorReaders.getSNPMajorIterator(tpedfiles)
    val reader2 = SNPMajorReaders.getSNPMajorIterator(tpedfiles2)
    val r = SNPMajorReaders.concatenate(List((reader1, new { def close() {} }), (reader2, new { def close() {} })).iterator)
    r.get.snpIterator.toList.size should equal(10)
  }

  test("bed") {
    val tpedfile = new java.io.File(getClass.getResource("/").getPath + "genotypedata/grmtest_tiny2.tped")
    val tfamfile = new java.io.File(getClass.getResource("/").getPath + "genotypedata/grmtest_tiny2.tfam")
    val bed = new java.io.File(getClass.getResource("/").getPath + "genotypedata/grmtest_tiny2.bed")
    val fam = new java.io.File(getClass.getResource("/").getPath + "genotypedata/grmtest_tiny2.fam")
    val bim = new java.io.File(getClass.getResource("/").getPath + "genotypedata/grmtest_tiny2.bim")

    val tpedfiles = TPedFiles(fam = scala.io.Source.fromFile(tfamfile), tped = scala.io
      .Source.fromFile(tpedfile), '0')

    val bedresult = openFileInputStream(bed) { is =>
      openSource(fam.getCanonicalPath) { fam =>
        openSource(bim.getCanonicalPath) { bim =>
          val reader1 = SNPMajorReaders.getSNPMajorIterator(is, fam, bim, Full, Set())
          (reader1.individuals, reader1.snpIterator.toList.map(x => x._1 -> x._2.toVector))
        }
      }
    }

    // val reader1 = SNPMajorReaders.getSNPMajorIterator(tpedfiles)

    bedresult.toString should equal("""(Vector(f1_1, f2_1, f3_1, f4_1, f5_1, f6_1),List((PDosageFileRowSummary(rs1,T,A,4.0,6.0,5,0.4,1),Vector(0.0, 0.0, 0.0, NaN, 2.0, 2.0)), (PDosageFileRowSummary(rs2,A,T,5.0,5.0,5,0.5,1),Vector(1.0, 1.0, 1.0, NaN, 2.0, 0.0)), (PDosageFileRowSummary(rs3,A,T,3.0,9.0,6,0.25,0),Vector(0.0, 0.0, 1.0, 2.0, 0.0, 0.0)), (PDosageFileRowSummary(rs4,A,T,5.0,7.0,6,0.41666666,0),Vector(2.0, 0.0, 1.0, 1.0, 1.0, 0.0)), (PDosageFileRowSummary(rs5,A,T,3.0,7.0,5,0.3,1),Vector(2.0, 0.0, 1.0, NaN, 0.0, 0.0))))""")

  }
  test("bed subset by name") {
    val tpedfile = new java.io.File(getClass.getResource("/").getPath + "genotypedata/grmtest_tiny2.tped")
    val tfamfile = new java.io.File(getClass.getResource("/").getPath + "genotypedata/grmtest_tiny2.tfam")
    val bed = new java.io.File(getClass.getResource("/").getPath + "genotypedata/grmtest_tiny2.bed")
    val fam = new java.io.File(getClass.getResource("/").getPath + "genotypedata/grmtest_tiny2.fam")
    val bim = new java.io.File(getClass.getResource("/").getPath + "genotypedata/grmtest_tiny2.bim")

    val tpedfiles = TPedFiles(fam = scala.io.Source.fromFile(tfamfile), tped = scala.io
      .Source.fromFile(tpedfile), '0')

    val bedresult = openFileInputStream(bed) { is =>
      openSource(fam.getCanonicalPath) { fam =>
        openSource(bim.getCanonicalPath) { bim =>
          val reader1 = SNPMajorReaders.getSNPMajorIterator(is, fam, bim, Full, Set("rs1", "rs3", "rs5"))
          (reader1.individuals, reader1.snpIterator.toList.map(x => x._1 -> x._2.toVector))
        }
      }
    }

    // val reader1 = SNPMajorReaders.getSNPMajorIterator(tpedfiles)

    bedresult.toString should equal("""(Vector(f1_1, f2_1, f3_1, f4_1, f5_1, f6_1),List((PDosageFileRowSummary(rs1,T,A,4.0,6.0,5,0.4,1),Vector(0.0, 0.0, 0.0, NaN, 2.0, 2.0)), (PDosageFileRowSummary(rs3,A,T,3.0,9.0,6,0.25,0),Vector(0.0, 0.0, 1.0, 2.0, 0.0, 0.0)), (PDosageFileRowSummary(rs5,A,T,3.0,7.0,5,0.3,1),Vector(2.0, 0.0, 1.0, NaN, 0.0, 0.0))))""")

  }

  test("bed subset by idx") {
    val tpedfile = new java.io.File(getClass.getResource("/").getPath + "genotypedata/grmtest_tiny2.tped")
    val tfamfile = new java.io.File(getClass.getResource("/").getPath + "genotypedata/grmtest_tiny2.tfam")
    val bed = new java.io.File(getClass.getResource("/").getPath + "genotypedata/grmtest_tiny2.bed")
    val fam = new java.io.File(getClass.getResource("/").getPath + "genotypedata/grmtest_tiny2.fam")
    val bim = new java.io.File(getClass.getResource("/").getPath + "genotypedata/grmtest_tiny2.bim")

    val tpedfiles = TPedFiles(fam = scala.io.Source.fromFile(tfamfile), tped = scala.io
      .Source.fromFile(tpedfile), '0')

    val bedresult = openFileInputStream(bed) { is =>
      openSource(fam.getCanonicalPath) { fam =>
        openSource(bim.getCanonicalPath) { bim =>
          val reader1 = SNPMajorReaders.getSNPMajorIterator(is, fam, bim, FileSubSet(1, 3), Set())
          (reader1.individuals, reader1.snpIterator.toList.map(x => x._1 -> x._2.toVector))
        }
      }
    }

    bedresult.toString should equal("""(Vector(f1_1, f2_1, f3_1, f4_1, f5_1, f6_1),List((PDosageFileRowSummary(rs2,A,T,5.0,5.0,5,0.5,1),Vector(1.0, 1.0, 1.0, NaN, 2.0, 0.0)), (PDosageFileRowSummary(rs3,A,T,3.0,9.0,6,0.25,0),Vector(0.0, 0.0, 1.0, 2.0, 0.0, 0.0))))""")

  }

  test("bed medium") {
    val bed = new java.io.File(getClass.getResource("/").getPath + "genotypedata/medium.bed")
    val fam = new java.io.File(getClass.getResource("/").getPath + "genotypedata/medium.fam")
    val bim = new java.io.File(getClass.getResource("/").getPath + "genotypedata/medium.bim")

    val tfam = new java.io.File(getClass.getResource("/").getPath + "genotypedata/medium.tfam")
    val tped = new java.io.File(getClass.getResource("/").getPath + "genotypedata/medium.tped")
    val tpedfiles = TPedFiles(fam = scala.io.Source.fromFile(tfam), tped = scala.io
      .Source.fromFile(tped), '0')

    var i = 0

    openSource(fam.getCanonicalPath) { fam2 =>
      openSource(bim.getCanonicalPath) { bim2 =>

        val bedrandomaccess = new RandomAccessBedReader(new java.io.RandomAccessFile(bed, "r"), bim2, fam2)

        openFileInputStream(bed) { is =>
          openSource(fam.getCanonicalPath) { fam =>
            openSource(bim.getCanonicalPath) { bim =>
              val reader1 = SNPMajorReaders.getSNPMajorIterator(is, fam, bim, Full, Set()).toLocusIteratorWithGenomicMap(Map())
              val bedresult = (reader1.individuals, reader1.loci.map(x => x._1 -> x._2.toVector))

              val tpedresult = {

                val reader1 = SNPMajorReaders.getSNPMajorIterator(tpedfiles, Full, Set()).toLocusIteratorWithGenomicMap(Map())
                (reader1.individuals, reader1.loci.map(x => x._1 -> x._2.toVector))

              }
              // val tpedresultmap = tpedresult._2.groupBy(_._1.name)

              bedresult._1 should equal(tpedresult._1)

              bedresult._2 zip tpedresult._2 foreach {
                case (b, t) =>
                  val r = bedrandomaccess.queryLocus(b._1, Map()).get
                  b should equal(r)

                  if (b != t) {
                    println(b + " " + t)

                  }
                  i += 1
                  b should equal(t)
              }

            }
          }
        }
      }
    }
    i should equal(24806)

  }

  test("bed medium 10k") {
    val bed = new java.io.File(getClass.getResource("/").getPath + "genotypedata/tenK.bed")
    val fam = new java.io.File(getClass.getResource("/").getPath + "genotypedata/tenK.fam")
    val bim = new java.io.File(getClass.getResource("/").getPath + "genotypedata/tenK.bim")

    val tfam = new java.io.File(getClass.getResource("/").getPath + "genotypedata/tenK.tfam")
    val tped = new java.io.File(getClass.getResource("/").getPath + "genotypedata/tenK.tped")
    val tpedfiles = TPedFiles(fam = scala.io.Source.fromFile(tfam), tped = scala.io
      .Source.fromFile(tped), '0')

    var i = 0

    openSource(fam.getCanonicalPath) { fam2 =>
      openSource(bim.getCanonicalPath) { bim2 =>

        val bedrandomaccess = new RandomAccessBedReader(new java.io.RandomAccessFile(bed, "r"), bim2, fam2)

        openFileInputStream(bed) { is =>
          openSource(fam.getCanonicalPath) { fam =>
            openSource(bim.getCanonicalPath) { bim =>
              val reader1 = SNPMajorReaders.getSNPMajorIterator(is, fam, bim, Full, Set()).toLocusIteratorWithGenomicMap(Map())
              val bedresult = (reader1.individuals, reader1.loci.map(x => x._1 -> x._2.toVector))

              val tpedresult = {

                val reader1 = SNPMajorReaders.getSNPMajorIterator(tpedfiles, Full, Set()).toLocusIteratorWithGenomicMap(Map())
                (reader1.individuals, reader1.loci.map(x => x._1 -> x._2.toVector))

              }
              // val tpedresultmap = tpedresult._2.groupBy(_._1.name)

              bedresult._1 should equal(tpedresult._1)

              bedresult._2 zip tpedresult._2 foreach {
                case (b, t) =>
                  val r = bedrandomaccess.queryLocus(b._1, Map()).get
                  b should equal(r)

                  if (b != t) {
                    println(b + " " + t)

                  }
                  i += 1
                  b should equal(t)
              }

            }
          }
        }
      }
    }
    i should equal(10000)

  }

  test("bed medium 10k subset") {
    val bed = new java.io.File(getClass.getResource("/").getPath + "genotypedata/tenK.bed")
    val fam = new java.io.File(getClass.getResource("/").getPath + "genotypedata/tenK.fam")
    val bim = new java.io.File(getClass.getResource("/").getPath + "genotypedata/tenK.bim")

    val tfam = new java.io.File(getClass.getResource("/").getPath + "genotypedata/tenK.tfam")
    val tped = new java.io.File(getClass.getResource("/").getPath + "genotypedata/tenK.tped")
    val tpedfiles = TPedFiles(fam = scala.io.Source.fromFile(tfam), tped = scala.io
      .Source.fromFile(tped), '0')

    var i = 0

    openSource(fam.getCanonicalPath) { fam2 =>
      openSource(bim.getCanonicalPath) { bim2 =>

        val bedrandomaccess = new RandomAccessBedReader(new java.io.RandomAccessFile(bed, "r"), bim2, fam2)

        openFileInputStream(bed) { is =>
          openSource(fam.getCanonicalPath) { fam =>
            openSource(bim.getCanonicalPath) { bim =>
              val reader1 = SNPMajorReaders.getSNPMajorIterator(is, fam, bim, Full, Set("rs2635471")).toLocusIteratorWithGenomicMap(Map())
              val bedresult = (reader1.individuals, reader1.loci.map(x => x._1 -> x._2.toVector))

              val tpedresult = {

                val reader1 = SNPMajorReaders.getSNPMajorIterator(tpedfiles, Full, Set("rs2635471")).toLocusIteratorWithGenomicMap(Map())
                (reader1.individuals, reader1.loci.map(x => x._1 -> x._2.toVector))

              }
              // val tpedresultmap = tpedresult._2.groupBy(_._1.name)

              bedresult._1 should equal(tpedresult._1)

              bedresult._2 zip tpedresult._2 foreach {
                case (b, t) =>
                  val r = bedrandomaccess.queryLocus(b._1, Map()).get
                  b should equal(r)

                  if (b != t) {
                    println(b + " " + t)

                  }
                  i += 1
                  b should equal(t)
              }

            }
          }
        }
      }
    }
    i should equal(1)

  }

  test("bed medium 10k+1") {
    val bed = new java.io.File(getClass.getResource("/").getPath + "genotypedata/tenK+1.bed")
    val fam = new java.io.File(getClass.getResource("/").getPath + "genotypedata/tenK+1.fam")
    val bim = new java.io.File(getClass.getResource("/").getPath + "genotypedata/tenK+1.bim")

    val tfam = new java.io.File(getClass.getResource("/").getPath + "genotypedata/tenK+1.tfam")
    val tped = new java.io.File(getClass.getResource("/").getPath + "genotypedata/tenK+1.tped")
    val tpedfiles = TPedFiles(fam = scala.io.Source.fromFile(tfam), tped = scala.io
      .Source.fromFile(tped), '0')

    var i = 0

    openSource(fam.getCanonicalPath) { fam2 =>
      openSource(bim.getCanonicalPath) { bim2 =>

        val bedrandomaccess = new RandomAccessBedReader(new java.io.RandomAccessFile(bed, "r"), bim2, fam2)

        openFileInputStream(bed) { is =>
          openSource(fam.getCanonicalPath) { fam =>
            openSource(bim.getCanonicalPath) { bim =>
              val reader1 = SNPMajorReaders.getSNPMajorIterator(is, fam, bim, Full, Set()).toLocusIteratorWithGenomicMap(Map())
              val bedresult = (reader1.individuals, reader1.loci.map(x => x._1 -> x._2.toVector))

              val tpedresult = {

                val reader1 = SNPMajorReaders.getSNPMajorIterator(tpedfiles, Full, Set()).toLocusIteratorWithGenomicMap(Map())
                (reader1.individuals, reader1.loci.map(x => x._1 -> x._2.toVector))

              }
              // val tpedresultmap = tpedresult._2.groupBy(_._1.name)

              bedresult._1 should equal(tpedresult._1)

              bedresult._2 zip tpedresult._2 foreach {
                case (b, t) =>
                  val r = bedrandomaccess.queryLocus(b._1, Map()).get
                  b should equal(r)

                  if (b != t) {
                    println(b + " " + t)

                  }
                  i += 1
                  b should equal(t)
              }

            }
          }
        }
      }
    }
    i should equal(10001)

  }

  test("bed medium 10k+1 subset ") {
    val bed = new java.io.File(getClass.getResource("/").getPath + "genotypedata/tenK+1.bed")
    val fam = new java.io.File(getClass.getResource("/").getPath + "genotypedata/tenK+1.fam")
    val bim = new java.io.File(getClass.getResource("/").getPath + "genotypedata/tenK+1.bim")

    val tfam = new java.io.File(getClass.getResource("/").getPath + "genotypedata/tenK+1.tfam")
    val tped = new java.io.File(getClass.getResource("/").getPath + "genotypedata/tenK+1.tped")
    val tpedfiles = TPedFiles(fam = scala.io.Source.fromFile(tfam), tped = scala.io
      .Source.fromFile(tped), '0')

    var i = 0

    openSource(fam.getCanonicalPath) { fam2 =>
      openSource(bim.getCanonicalPath) { bim2 =>

        val bedrandomaccess = new RandomAccessBedReader(new java.io.RandomAccessFile(bed, "r"), bim2, fam2)

        openFileInputStream(bed) { is =>
          openSource(fam.getCanonicalPath) { fam =>
            openSource(bim.getCanonicalPath) { bim =>
              val reader1 = SNPMajorReaders.getSNPMajorIterator(is, fam, bim, Full, Set("rs2635471")).toLocusIteratorWithGenomicMap(Map())
              val bedresult = (reader1.individuals, reader1.loci.map(x => x._1 -> x._2.toVector))

              val tpedresult = {

                val reader1 = SNPMajorReaders.getSNPMajorIterator(tpedfiles, Full, Set("rs2635471")).toLocusIteratorWithGenomicMap(Map())
                (reader1.individuals, reader1.loci.map(x => x._1 -> x._2.toVector))

              }
              // val tpedresultmap = tpedresult._2.groupBy(_._1.name)

              bedresult._1 should equal(tpedresult._1)

              bedresult._2 zip tpedresult._2 foreach {
                case (b, t) =>
                  val r = bedrandomaccess.queryLocus(b._1, Map()).get
                  b should equal(r)

                  if (b != t) {
                    println(b + " " + t)

                  }
                  i += 1
                  b should equal(t)
              }

            }
          }
        }
      }
    }
    i should equal(1)

  }

  test("bed medium 10k+2") {
    val bed = new java.io.File(getClass.getResource("/").getPath + "genotypedata/tenK+2.bed")
    val fam = new java.io.File(getClass.getResource("/").getPath + "genotypedata/tenK+2.fam")
    val bim = new java.io.File(getClass.getResource("/").getPath + "genotypedata/tenK+2.bim")

    val tfam = new java.io.File(getClass.getResource("/").getPath + "genotypedata/tenK+2.tfam")
    val tped = new java.io.File(getClass.getResource("/").getPath + "genotypedata/tenK+2.tped")
    val tpedfiles = TPedFiles(fam = scala.io.Source.fromFile(tfam), tped = scala.io
      .Source.fromFile(tped), '0')

    var i = 0

    openSource(fam.getCanonicalPath) { fam2 =>
      openSource(bim.getCanonicalPath) { bim2 =>

        val bedrandomaccess = new RandomAccessBedReader(new java.io.RandomAccessFile(bed, "r"), bim2, fam2)

        openFileInputStream(bed) { is =>
          openSource(fam.getCanonicalPath) { fam =>
            openSource(bim.getCanonicalPath) { bim =>
              val reader1 = SNPMajorReaders.getSNPMajorIterator(is, fam, bim, Full, Set()).toLocusIteratorWithGenomicMap(Map())
              val bedresult = (reader1.individuals, reader1.loci.map(x => x._1 -> x._2.toVector))

              val tpedresult = {

                val reader1 = SNPMajorReaders.getSNPMajorIterator(tpedfiles, Full, Set()).toLocusIteratorWithGenomicMap(Map())
                (reader1.individuals, reader1.loci.map(x => x._1 -> x._2.toVector))

              }
              // val tpedresultmap = tpedresult._2.groupBy(_._1.name)

              bedresult._1 should equal(tpedresult._1)

              bedresult._2 zip tpedresult._2 foreach {
                case (b, t) =>
                  val r = bedrandomaccess.queryLocus(b._1, Map()).get
                  b should equal(r)

                  if (b != t) {
                    println(b + " " + t)

                  }
                  i += 1
                  b should equal(t)
              }

            }
          }
        }
      }
    }
    i should equal(10002)

  }

  test("bed medium 10k+2 subset") {
    val bed = new java.io.File(getClass.getResource("/").getPath + "genotypedata/tenK+2.bed")
    val fam = new java.io.File(getClass.getResource("/").getPath + "genotypedata/tenK+2.fam")
    val bim = new java.io.File(getClass.getResource("/").getPath + "genotypedata/tenK+2.bim")

    val tfam = new java.io.File(getClass.getResource("/").getPath + "genotypedata/tenK+2.tfam")
    val tped = new java.io.File(getClass.getResource("/").getPath + "genotypedata/tenK+2.tped")
    val tpedfiles = TPedFiles(fam = scala.io.Source.fromFile(tfam), tped = scala.io
      .Source.fromFile(tped), '0')

    var i = 0

    openSource(fam.getCanonicalPath) { fam2 =>
      openSource(bim.getCanonicalPath) { bim2 =>

        val bedrandomaccess = new RandomAccessBedReader(new java.io.RandomAccessFile(bed, "r"), bim2, fam2)

        openFileInputStream(bed) { is =>
          openSource(fam.getCanonicalPath) { fam =>
            openSource(bim.getCanonicalPath) { bim =>
              val reader1 = SNPMajorReaders.getSNPMajorIterator(is, fam, bim, Full, Set("rs2635471")).toLocusIteratorWithGenomicMap(Map())
              val bedresult = (reader1.individuals, reader1.loci.map(x => x._1 -> x._2.toVector))

              val tpedresult = {

                val reader1 = SNPMajorReaders.getSNPMajorIterator(tpedfiles, Full, Set("rs2635471")).toLocusIteratorWithGenomicMap(Map())
                (reader1.individuals, reader1.loci.map(x => x._1 -> x._2.toVector))

              }
              // val tpedresultmap = tpedresult._2.groupBy(_._1.name)

              bedresult._1 should equal(tpedresult._1)

              bedresult._2 zip tpedresult._2 foreach {
                case (b, t) =>
                  val r = bedrandomaccess.queryLocus(b._1, Map()).get
                  b should equal(r)

                  if (b != t) {
                    println(b + " " + t)

                  }
                  i += 1
                  b should equal(t)
              }

            }
          }
        }
      }
    }
    i should equal(1)

  }

  test("bed medium 10k+3") {
    val bed = new java.io.File(getClass.getResource("/").getPath + "genotypedata/tenK+3.bed")
    val fam = new java.io.File(getClass.getResource("/").getPath + "genotypedata/tenK+3.fam")
    val bim = new java.io.File(getClass.getResource("/").getPath + "genotypedata/tenK+3.bim")

    val tfam = new java.io.File(getClass.getResource("/").getPath + "genotypedata/tenK+3.tfam")
    val tped = new java.io.File(getClass.getResource("/").getPath + "genotypedata/tenK+3.tped")
    val tpedfiles = TPedFiles(fam = scala.io.Source.fromFile(tfam), tped = scala.io
      .Source.fromFile(tped), '0')

    var i = 0

    openSource(fam.getCanonicalPath) { fam2 =>
      openSource(bim.getCanonicalPath) { bim2 =>

        val bedrandomaccess = new RandomAccessBedReader(new java.io.RandomAccessFile(bed, "r"), bim2, fam2)

        openFileInputStream(bed) { is =>
          openSource(fam.getCanonicalPath) { fam =>
            openSource(bim.getCanonicalPath) { bim =>
              val reader1 = SNPMajorReaders.getSNPMajorIterator(is, fam, bim, Full, Set()).toLocusIteratorWithGenomicMap(Map())
              val bedresult = (reader1.individuals, reader1.loci.map(x => x._1 -> x._2.toVector))

              val tpedresult = {

                val reader1 = SNPMajorReaders.getSNPMajorIterator(tpedfiles, Full, Set()).toLocusIteratorWithGenomicMap(Map())
                (reader1.individuals, reader1.loci.map(x => x._1 -> x._2.toVector))

              }
              // val tpedresultmap = tpedresult._2.groupBy(_._1.name)

              bedresult._1 should equal(tpedresult._1)

              bedresult._2 zip tpedresult._2 foreach {
                case (b, t) =>
                  val r = bedrandomaccess.queryLocus(b._1, Map()).get
                  b should equal(r)

                  if (b != t) {
                    println(b + " " + t)

                  }
                  i += 1
                  b should equal(t)
              }

            }
          }
        }
      }
    }
    i should equal(10003)

  }

  test("bed medium 10k+3 subset") {
    val bed = new java.io.File(getClass.getResource("/").getPath + "genotypedata/tenK+3.bed")
    val fam = new java.io.File(getClass.getResource("/").getPath + "genotypedata/tenK+3.fam")
    val bim = new java.io.File(getClass.getResource("/").getPath + "genotypedata/tenK+3.bim")

    val tfam = new java.io.File(getClass.getResource("/").getPath + "genotypedata/tenK+3.tfam")
    val tped = new java.io.File(getClass.getResource("/").getPath + "genotypedata/tenK+3.tped")
    val tpedfiles = TPedFiles(fam = scala.io.Source.fromFile(tfam), tped = scala.io
      .Source.fromFile(tped), '0')

    var i = 0

    openSource(fam.getCanonicalPath) { fam2 =>
      openSource(bim.getCanonicalPath) { bim2 =>

        val bedrandomaccess = new RandomAccessBedReader(new java.io.RandomAccessFile(bed, "r"), bim2, fam2)

        openFileInputStream(bed) { is =>
          openSource(fam.getCanonicalPath) { fam =>
            openSource(bim.getCanonicalPath) { bim =>
              val reader1 = SNPMajorReaders.getSNPMajorIterator(is, fam, bim, Full, Set("rs2635471")).toLocusIteratorWithGenomicMap(Map())
              val bedresult = (reader1.individuals, reader1.loci.map(x => x._1 -> x._2.toVector))

              val tpedresult = {

                val reader1 = SNPMajorReaders.getSNPMajorIterator(tpedfiles, Full, Set("rs2635471")).toLocusIteratorWithGenomicMap(Map())
                (reader1.individuals, reader1.loci.map(x => x._1 -> x._2.toVector))

              }
              // val tpedresultmap = tpedresult._2.groupBy(_._1.name)

              bedresult._1 should equal(tpedresult._1)

              bedresult._2 zip tpedresult._2 foreach {
                case (b, t) =>
                  val r = bedrandomaccess.queryLocus(b._1, Map()).get
                  b should equal(r)

                  if (b != t) {
                    println(b + " " + t)

                  }
                  i += 1
                  b should equal(t)
              }

            }
          }
        }
      }
    }
    i should equal(1)

  }

  test("HLA call") {
    val pdosage = """SNP A1 A2 F1 I1
      |HLA_B_0001 C A 1.0
      |HLA_B_0000 C A 0.0
      |HLA_B_0002 C A 1.0
      |HLA_A_0000 C A 0.0
      |HLA_A_0001 C A 0.0
      |HLA_A_0002 C A 0.0
      |HLA_A_0003 C A 2.0
      |HLA_C_0000 C A 1.0
      |HLA_C_0001 C A 0.0
      |HLA_C_0002 C A 0.0
      |HLA_C_0003 C A 0.0""".stripMargin
    val iter = SNPMajorReaders.getSNPMajorIterator(PDoseFiles(io.Source.fromString(pdosage), -9f, None)).toLocusIteratorWithGenomicMap(Map())
    callHLAFromSNPMajorLocusIterator(iter) should equal(Map(Individual("F1", "I1") -> HLAReportCalled(None, None, Some("HLA_B_0001"), Some("HLA_B_0002"), Some("HLA_C_0000"), None)))

  }

  test("bed prune") {
    val tpedfile = new java.io.File(getClass.getResource("/").getPath + "genotypedata/grmtest_tiny2.tped")
    val tfamfile = new java.io.File(getClass.getResource("/").getPath + "genotypedata/grmtest_tiny2.tfam")
    val bed = new java.io.File(getClass.getResource("/").getPath + "genotypedata/grmtest_tiny2.bed")
    val fam = new java.io.File(getClass.getResource("/").getPath + "genotypedata/grmtest_tiny2.fam")
    val bim = new java.io.File(getClass.getResource("/").getPath + "genotypedata/grmtest_tiny2.bim")
    val gmap = getGenomicMapFromBimFile(bim.getAbsolutePath)

    val tpedfiles = TPedFiles(fam = scala.io.Source.fromFile(tfamfile), tped = scala.io
      .Source.fromFile(tpedfile), '0')

    val bedresult = openFileInputStream(bed) { is =>
      openSource(fam.getCanonicalPath) { fam =>
        openSource(bim.getCanonicalPath) { bim =>
          val reader1 = SNPMajorReaders.getSNPMajorIterator(is, fam, bim, Full, Set()).prune(0.3, 5, Set(), Nil, gmap)
          (reader1.individuals, reader1.snpIterator.toList.map(x => x._1 -> x._2.toVector))
        }
      }
    }

    // println(bedresult)
    // println(bedresult._2.combinations(2).map(x =>
    //   (x(0)._1.snpName, x(1)._1.snpName) -> mybiotools.stat.RSquared.rsquared(x(0)._2.toArray.map(_.toFloat), x(1)._2.toArray.map(_.toFloat), Float.NaN)).toList)

    // val reader1 = SNPMajorReaders.getSNPMajorIterator(tpedfiles)

    bedresult.toString should equal("""(Vector(f1_1, f2_1, f3_1, f4_1, f5_1, f6_1),List((PDosageFileRowSummary(rs1,T,A,4.0,6.0,5,0.4,1),Vector(0.0, 0.0, 0.0, NaN, 2.0, 2.0)), (PDosageFileRowSummary(rs2,A,T,5.0,5.0,5,0.5,1),Vector(1.0, 1.0, 1.0, NaN, 2.0, 0.0)), (PDosageFileRowSummary(rs3,A,T,3.0,9.0,6,0.25,0),Vector(0.0, 0.0, 1.0, 2.0, 0.0, 0.0)), (PDosageFileRowSummary(rs4,A,T,5.0,7.0,6,0.41666666,0),Vector(2.0, 0.0, 1.0, 1.0, 1.0, 0.0))))""")

  }

}