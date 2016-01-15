// package genotyper.tasks

// import org.scalatest._

// import mybiotools.tasks._

// import org.scalatest.FunSpec
// import org.scalatest.Matchers
// import mybiotools.gwascommons._

// object TestsPlatypus {

//   val ts = defaultTaskSystem("""
//     tasks.cacheEnabled=false
//     tasks.logFile="" 
//     tasks.fileServiceBaseFolder = "./target/"
//     """)
//   import ts._
//   implicit val fs = components.fs
//   implicit val ac = components.actorsystem

//   val exampleBamFile = new java.io.File(getClass.getResource("/").getPath + "/example.chr1:1-100000.bam")
//   val exampleBai = new java.io.File(getClass.getResource("/").getPath + "/example.chr1:1-100000.bam.bai")

//   val exampleFasta = new java.io.File(getClass.getResource("/").getPath + "/exampleFASTA.fasta")

//   val exampleFastaDict = new java.io.File(getClass.getResource("/").getPath + "/exampleFASTA.fasta.dict")

//   val exampleFastaFai = new java.io.File(getClass.getResource("/").getPath + "/exampleFASTA.fasta.fai")

//   val platypusFolder = new java.io.File(getClass.getResource("/").getPath + "/Platypus")

//   val platypus = platypusGenotype(
//     GenotypeWithPlatypusInput(platypusfolder = platypusFolder, referenceFasta = exampleFasta, referenceFai = exampleFastaFai, referenceDict = exampleFastaDict, interval = List(Region(1, 0, 100000)), outName = "examplename", expectedNumberOfBamFiles = 0, extraArgs = "", bamfiles = List(exampleBamFile -> exampleBai)),
//     memory = 500,
//     cpu = 1)

//   val r = platypus.?![GenotypeWithPlatypusOutput]

//   val vcf = r.vcf.file

//   ts.shutdown

// }

// class PlatypusTestSuite extends FunSuite with Matchers {

//   test("platypus") {

//     TestsPlatypus.vcf.canRead should equal(true)
//     // TestVariantEval.evaltaskout.canRead should be(true)

//   }

// }