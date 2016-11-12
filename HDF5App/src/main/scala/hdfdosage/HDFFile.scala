// /*
// * The MIT License
// *
// * Copyright (c) 2015 ECOLE POLYTECHNIQUE FEDERALE DE LAUSANNE, Switzerland,
// * Group Fellay
// *
// * Permission is hereby granted, free of charge, to any person obtaining
// * a copy of this software and associated documentation files (the "Software"),
// * to deal in the Software without restriction, including without limitation
// * the rights to use, copy, modify, merge, publish, distribute, sublicense,
// * and/or sell copies of the Software, and to permit persons to whom the Software
// * is furnished to do so, subject to the following conditions:
// *
// * The above copyright notice and this permission notice shall be included in all
// * copies or substantial portions of the Software.
// *
// * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// * SOFTWARE.
// */
//
// package hdfdosage
//
// import _root_.ch.systemsx.cisd.hdf5._
// import mybiotools.gwascommons.Individual
// import mybiotools.gwascommons.genotypedata.PDosageFileRowSummary
//
// object HDFFile {
//   def writeHDF(
//     hdfWriter: IHDF5Writer,
//     numberOfIndividuals: Int,
//     individuals: Seq[Individual],
//     dosageLineIterator: Iterator[Tuple2[PDosageFileRowSummary, Array[Float]]],
//     blockSize: Int
//   ): Unit = {
//
//     val compoundWriter = hdfWriter.compound
//
//     val individualCompoundType = hdfWriter.compound.getInferredType(classOf[PatientDTO])
//     val snpCompoundType = hdfWriter.compound.getInferredType(classOf[SNPDTO])
//
//     // Write patient order
//     {
//       val individualTransfer = individuals.map(x => IndividualHelpers.toDTO(x)).toArray
//
//       compoundWriter.writeArray("/individualorder", individualCompoundType, individualTransfer, HDF5GenericStorageFeatures.GENERIC_CONTIGUOUS_DELETE)
//
//     }
//
//     //Create 2D matrix in HDF5 file
//     // val blockSize = 1000 // release
//     // val blockSize = 1 // test
//     hdfWriter.float32.createMatrix("/dosagematrix", blockSize, numberOfIndividuals, blockSize, numberOfIndividuals, HDF5FloatStorageFeatures.createDeflateAndFloatScaling(1, 3))
//
//     // compoundWriter.createArray("/snporder", snpCompoundType, 1, HDF5GenericStorageFeatures.GENERIC_CHUNKED_DELETE)
//     val snpOrderBuffer = scala.collection.mutable.ArrayBuffer[SNPDTO]()
//
//     // Write line by line
//     var counter = 0
//
//     val writeAheadSize = blockSize * 100
//
//     dosageLineIterator.grouped(writeAheadSize).foreach { group =>
//
//       val writeAhead: Tuple2[Vector[SNPDTO], Vector[Array[Float]]] = group.map { elem =>
//         (PDosageFileRowSummaryHDFHelpers.toDTO(elem._1), elem._2)
//       }.toVector.unzip
//
//       val snpOrderArray = writeAhead._1.toArray
//       val dosageArray = writeAhead._2.toArray
//
//       // compoundWriter.writeArrayBlockWithOffset("/snporder", snpCompoundType, snpOrderArray, counter)
//       snpOrderBuffer ++= snpOrderArray
//
//       hdfWriter.float32.writeMatrixBlockWithOffset("/dosagematrix", dosageArray, counter, 0)
//       counter += snpOrderArray.size
//
//       if (counter % 40000 == 0) {
//         println(counter)
//         // hdfWriter.file.flush
//       }
//
//     }
//
//     hdfWriter.compound.createArray("/snporder", snpCompoundType,
//       0, 100000, HDF5GenericStorageFeatures.GENERIC_DEFLATE_MAX_DELETE)
//
//     var offset = 0
//     snpOrderBuffer.grouped(100000).foreach {
//       case part =>
//         hdfWriter.compound.writeArrayBlockWithOffset("/snporder", snpCompoundType, part.toArray, offset)
//         offset += part.size
//     }
//
//     hdfWriter.file.flush
//
//   }
// }
