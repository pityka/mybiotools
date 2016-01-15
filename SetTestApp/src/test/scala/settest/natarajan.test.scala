package settest

import mybiotools.gwascommons._
import mybiotools.stringstore._

import org.scalatest.FunSpec
import org.scalatest.Matchers

class NatarajanSpec extends FunSpec with Matchers {

  describe("simple test case") {

    val bed = io.Source.fromString("""chr1	540562	540721	chr1.1	567	.	0.0377	1.53	-1	78
|chr1	713915	714340	chr1.2	724	.	0.0849	3.33	-1	201
|chr1	752764	753181	chr1.3	635	.	0.0583	2.32	-1	163""".stripMargin)
    val geneIDs = io.Source.fromString("""geneID	name
|1	A1BG
|2	A1BG-AS1
|3	A1CF""".stripMargin)
    val assocs = io.Source.fromString("""1	c	;1;2;3;	-1
|2	c	;1;3;	-1
""".stripMargin)

    it("test") {

      val result = readNatarajansFiles(assocs, bed, geneIDs)

      val expected = Map(
        RegionName(s8"A1BG") -> Vector(Region("chr1", 540562, 540721), Region("chr1", 713915, 714340), Region("chr1", 752764, 753181)),
        RegionName(s8"A1BG-AS1") -> Vector(Region("chr1", 540562, 540721), Region("chr1", 752764, 753181))
      )

      result should equal(expected)
    }
  }

}