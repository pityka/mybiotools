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
// import scala.io.Source
// import java.io.File
// import mybiotools.{
//   fastSplitSetSeparator,
//   fastSplitSetSeparatorIterator,
//   openFileWriter,
//   fastSplitSeparatorIterator,
//   getFileReader,
//   openFileOutputStream,
//   writeToFile
// }
// import mybiotools.gwascommons.GenomicLocation
// import mybiotools.stringstore.String8
// import _root_.ch.systemsx.cisd.hdf5._
// import java.io.{ OutputStream, BufferedOutputStream, DataOutputStream, ByteArrayOutputStream, Writer, FileOutputStream, RandomAccessFile }
//
// private[hdfdosage] object SNPBuffer {
//   def fromFile(file: File, bufferSize: Int) = {
//     val reader = HDF5Factory.openForReading(file)
//     fromReader(reader, bufferSize)
//   }
//   def fromReader(reader: IHDF5Reader, bufferSize: Int) = {
//
//     val originalSNPOrder: Map[String8, Int] = Map(HDFDosageFile.readSNPNamesOrder(reader).zipWithIndex: _*)
//
//     val originalNumberOfIndividuals = HDFDosageFile.getNumberOfIndividuals(reader)
//     new SNPBuffer(reader, originalSNPOrder, originalNumberOfIndividuals, bufferSize)
//   }
// }
//
// /** For random access reading. Missing values are NOT recoded to NaN. */
// private[hdfdosage] class SNPBuffer(reader: IHDF5Reader, snpIndex: Map[String8, Int], numberOfIndividuals: Int, maxBufferSize: Int) {
//
//   class MyLinkedMap extends java.util.LinkedHashMap[String8, Array[Float]] {
//     override def removeEldestEntry(eldest: java.util.Map.Entry[String8, Array[Float]]) = {
//       this.size > maxBufferSize
//     }
//   }
//
//   /** This is a mutable object! */
//   val buffer = new MyLinkedMap()
//
//   val reversedSNPIndex: Map[Int, String8] = snpIndex.map(_.swap)
//
//   val readSize = (reader.getDataSetInformation("/dosagematrix").tryGetChunkSizes match {
//     case x if x == null => 1000
//     case x: Array[Int] => x.apply(0)
//   }) * 1
//
//   private val maxSnpsInDataSet = reader.getDataSetInformation("/dosagematrix").getDimensions.apply(0).toInt
//
//   private val rwlock = new java.util.concurrent.locks.ReentrantReadWriteLock();
//   private val lock = new java.util.concurrent.locks.ReentrantLock();
//
//   /**
//    * This method is synchronized with a readwrite lock.
//    * This method returns raw data from the file, that is missing is -9f
//    */
//   def getSNP(snpName: String8): Array[Float] = {
//     val tmp = {
//       rwlock.readLock().lock();
//       try {
//         buffer.get(snpName)
//       } finally {
//         rwlock.readLock().unlock();
//       }
//
//     }
//
//     if (tmp != null) tmp else {
//
//       val startIdx = snpIndex(snpName)
//
//       val currentReadSize = if ((startIdx + readSize) < maxSnpsInDataSet) readSize else maxSnpsInDataSet - startIdx
//
//       val matrix = {
//         lock.lock()
//         try { reader.float32.readMatrixBlockWithOffset("/dosagematrix", currentReadSize, numberOfIndividuals, startIdx, 0) }
//         finally {
//           lock.unlock
//         }
//       }
//
//       try {
//         rwlock.writeLock().lock();
//         (0 to (currentReadSize - 1)).foreach { idx =>
//
//           val array = matrix(idx)
//
//           reversedSNPIndex.get(idx + startIdx) match {
//             case None => {}
//             case Some(snpName1) => {
//               {
//                 buffer.put(snpName1, array)
//               }
//             }
//           }
//
//         }
//       } finally {
//         rwlock.writeLock().unlock();
//       }
//
//       matrix(0)
//
//     }
//   }
//
//   def getSNPOption(snpName: String8): Array[Option[Float]] = {
//     getSNP(snpName).map { x => if (x == hdfdosage.MissingValue) None else Some(x) }
//   }
//
// }
