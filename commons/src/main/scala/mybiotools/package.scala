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

import scala.io.Source
import java.io.{ PrintWriter, BufferedWriter, FileWriter, FileInputStream, FileOutputStream, BufferedOutputStream, LineNumberReader, InputStream, BufferedReader, FileReader, BufferedInputStream }
import java.util.zip.GZIPInputStream

import java.io.EOFException
import java.io.File
import mybiotools.formatters._
import mybiotools.stringstore._
import scala.sys.process._
import scala.concurrent._
import scala.concurrent.duration._
import java.nio.charset.CodingErrorAction
import scala.io.Codec
import mybiotools.eq._
import scala.collection.mutable.ArrayBuffer
import scala.util.Try

/**
 * Provides classes and methods for general bionformatic applications.
 *
 */
package object mybiotools {

  /** Searches $PATH for the specified file */
  def searchForFileInPath(fileName: String) =
    System.getenv("PATH").split(java.util.regex.Pattern.quote(File.pathSeparator)).exists(f => new File(f, fileName).canRead)

  /**
   * Creates symbolic links with the same basename but different extensions.
   *
   * @param filesWithExtensions files with the needed extensions
   * @return A temporary File which points to the common part of the symbolic links paths.
   */
  def putBesides(filesWithExtensions: (File, String)*): File = {

    val tmp = mybiotools.TempFile.createTempFile("common")

    filesWithExtensions.foreach {
      case (file, ext) =>
        val filepath = java.nio.file.Paths.get(file.getCanonicalPath)
        val filelinkpath = java.nio.file.Paths.get(tmp.getCanonicalPath + ext)
        java.nio.file.Files.createSymbolicLink(filelinkpath, filepath)
    }

    tmp

  }

  /** Return true if the array's variance is 0. */
  def hasVariance(in: Array[Double]): Boolean = {

    @scala.annotation.tailrec
    def fold(idx: Int, head: Double, acc: Boolean): Boolean =
      if (idx == in.size || acc) acc
      else {
        val nextHead = if (in(idx).isNaN && !head.isNaN) head else in(idx)
        fold(idx + 1, nextHead, (!head.isNaN && !in(idx).isNaN) && in(idx) != head)
      }

    if (in.size == 0) false else fold(1, in(0), false)
  }

  def createSymbolicLink(from: File, to: File): File = {
    val fromP = java.nio.file.Paths.get(from.getCanonicalPath)
    val toP = java.nio.file.Paths.get(to.getCanonicalPath)
    java.nio.file.Files.createSymbolicLink(fromP, toP)
    from
  }

  implicit def string8ToString(s8: String8): String = s8.value

  /**
   * Catches all Throwables in a Left.
   *
   * Use scala.util.Try instead
   */
  def catchToLeft[T](block: => T): Either[Throwable, T] =
    try {
      Right(block)
    } catch {
      case ex: Throwable => Left(ex)
    }

  def rethrow[T](messageOnError: String, exceptionFactory: (String, Throwable) => Throwable)(block: => T): T = try {
    block
  } catch {
    case e: Throwable => throw (exceptionFactory(messageOnError, e))
  }

  def rethrow[T](messageOnError: String)(block: => T): T = rethrow(messageOnError, new RuntimeException(_: String, _: Throwable))(block)

  /** Retry the given block n times. */
  @annotation.tailrec
  def retry[T](n: Int)(fn: => T): scala.util.Try[T] = {
    scala.util.Try { fn } match {
      case x: scala.util.Success[T] => x
      case _ if n > 1 => retry(n - 1)(fn)
      case f => f
    }
  }

  type SequenceKey = String

  type FastaSequenceData = Map[SequenceKey, String]
  val FastaSequenceData = Map

  type FlatTable[A, B <: Any] = List[Map[A, B]]

  type KeyedTable[A, B] = Map[A, Map[Symbol, B]]

  /**
   * Returns the result of the block, and closes the resource.
   *
   * @param param closeable resource
   * @param f block using the resource
   */
  def useResource[A <: { def close(): Unit }, B](param: A)(f: A => B): B =
    try { f(param) } finally {
      param.close()
    }

  /** Alias for [[useResource]] */
  def using[A <: { def close(): Unit }, B] = useResource[A, B] _

  def readLines(s: String): Vector[String] = openSource(s)(_.getLines.toVector)
  def readLines(s: File): Vector[String] = readLines(s.getAbsolutePath)

  def openFileInputStreamMaybeZipped[T](file: File)(func: InputStream => T) =
    if (isGZipFile(file.getAbsolutePath)) openZippedFileInputStream(file)(func)
    else openFileInputStream(file)(func)

  /**
   * Opens a [[scala.io.Source]], executes the block, then closes the source.
   *
   * @param s Path of the file
   */
  def openSource[A](s: String)(f: io.Source => A)(implicit codec: Codec): A =
    if (isGZipFile(s)) openZippedSource(s)(f)(codec)
    else useResource(scala.io.Source.fromFile(s)(codec))(f)

  def createSource(s: File)(implicit codec: Codec) = if (isGZipFile(s)) createSourceFromZippedFile(s.getAbsolutePath)(codec) else scala.io.Source.fromFile(s)(codec)

  /**
   * Opens a [[scala.io.Source]], executes the block, then closes the source.
   *
   * @param s Path of the file
   */
  def openSource[A](s: File)(f: io.Source => A)(implicit codec: Codec): A = openSource(s.getAbsolutePath)(f)(codec)

  /** Returns a [[scala.io.Source]] from the given GZipped file. */
  def createSourceFromZippedFile(file: String, bufferSize: Int = io.Source.DefaultBufSize)(implicit codec: io.Codec): io.BufferedSource = {
    val inputStream = new BufferedInputStream(new GZIPInputStream(new FileInputStream(file), 65536 * 32), 65536 * 32)

    io.Source.createBufferedSource(
      inputStream,
      bufferSize,
      () => createSourceFromZippedFile(file, bufferSize)(codec),
      () => inputStream.close()
    )(codec) withDescription ("file:" + new java.io.File(file).getAbsolutePath)
  }

  /** Returns true if the file is GZipped. */
  def isGZipFile(file: String): Boolean = scala.util.Try(new java.util.zip.GZIPInputStream(new java.io.FileInputStream(new java.io.File(file)))).isSuccess

  /** Returns true if the file is GZipped. */
  def isGZipFile(file: File): Boolean = isGZipFile(file.getAbsolutePath)

  private[this] def openZippedSource[A](s: String)(f: io.Source => A)(codec: Codec) =
    useResource(createSourceFromZippedFile(s)(codec))(f)

  /** Writes text data to file. */
  def writeToFile(fileName: String, data: java.lang.String): Unit = useResource(new PrintWriter(new BufferedWriter(new FileWriter(fileName)))) { writer =>
    writer.write(data)
  }

  /** Writes text data to file. */
  def writeToFile(file: File, data: String): Unit = writeToFile(file.getAbsolutePath, data)

  /** Writes binary data to file. */
  def writeBinaryToFile(fileName: String, data: Array[Byte]): Unit = useResource(new BufferedOutputStream(new FileOutputStream(fileName))) { writer =>
    writer.write(data)
  }

  /** Writes binary data to file. */
  def writeBinaryToFile(file: File, data: Array[Byte]): Unit = writeBinaryToFile(file.getAbsolutePath, data)

  def writeToTempFile(data: String): File = {
    val tmp = TempFile.createTempFile("")
    writeToFile(tmp, data)
    tmp
  }

  def writeBinaryToTempFile(data: Array[Byte]): File = {
    val tmp = TempFile.createTempFile("")
    writeBinaryToFile(tmp, data)
    tmp
  }

  /** Writes the ByteBuffer's contents to a file. */
  def writeByteBufferToFile(fileName: String, buff: java.nio.ByteBuffer) = useResource(new FileOutputStream(fileName)) { writer =>
    val channel = writer.getChannel
    buff.rewind
    channel.write(buff)
    channel.close
  }

  /** Writes the ByteBuffer's contents to a file. */
  def writeByteBufferToFile(file: File, buff: java.nio.ByteBuffer): Unit = writeByteBufferToFile(file.getAbsolutePath, buff)

  @deprecated("Use scala.io.Source.getLines.map instead.", "0.0")
  def readFileByLine(fileName: String, skip: Int = 0)(func: String => Unit): Unit = useResource(new BufferedReader(new FileReader(fileName))) { reader =>
    for (i <- 0 to skip - 1) {
      reader.readLine
    }
    var line = reader.readLine
    while (line != null) {
      func(line);
      line = reader.readLine();
    }
  }

  /**
   * Returns an iterator on the InputStream's data.
   *
   * Closes the stream when read through.
   */
  def readStreamAndClose(is: java.io.InputStream) = new Iterator[Byte] {
    var s = is.read

    def hasNext = s != -1

    def next = { var x = s.toByte; s = is.read; if (!hasNext) { is.close() }; x }
  }

  /**
   * Cuts an iterator to an iterator of sequence of consecutive elements.
   *
   * Separator element is dropped.
   */
  @specialized(Byte, Char)
  def tokenizeIterator[T](i: Iterator[T], sep: T): Iterator[Seq[T]] = new Iterator[Seq[T]] {
    def hasNext = i.hasNext

    def next = i.takeWhile(_ != sep).toSeq

  }

  @deprecated("Use scala.io.Source.getLines instead.", "0.0")
  def getLineIterator(fileName: String, skip: Int = 0): Iterator[String] = {
    tokenizeReaderIterator(getFileReader(new File(fileName)), Array('\n')).drop(skip)
  }

  /**
   * Appends text to file.
   * Opens a new [[java.io.FileWriter]] on every call.
   */
  def appendToFile(fileName: String, textData: String): Unit = useResource(new BufferedWriter(new FileWriter(fileName, true))) { writer =>
    writer.write(textData)
    // buffWriter => useResource (new PrintWriter(buffWriter)) {
    //       printWriter => printWriter.print(textData)
    //     }
  }

  /**
   * Appends text to file.
   * Opens a new [[java.io.FileWriter]] on every call.
   */
  def appendToFile(file: File, data: String): Unit = appendToFile(file.getAbsolutePath, data)

  /** Returns true if file is empty. */
  def fileIsEmpty(file: File): Boolean = {
    var b: Int = 0
    openFileReader(file) { r =>
      b = r.read
    }
    b == -1
  }

  @deprecated("This never worked as intended.", "0.0")
  def redirectStdStreams[T](fileName: File, announce: Boolean = true)(f: => T): T = {
    if (announce) {
      println("Redirecting error and output steams to " + fileName.getAbsolutePath + "[.out/.err] .")
    }
    useResource(
      (new FileOutputStream(
        new File(fileName.getAbsolutePath + ".out")
      ))
    ) { streamout =>
        scala.Console.withOut(streamout) {
          useResource(
            (new FileOutputStream(
              new File(fileName.getAbsolutePath + ".err")
            ))
          ) { streamerr =>
              scala.Console.withErr(streamerr)(f)
            }
        }
      }
  }

  /** Opens a [[java.io.BufferedWriter]] on the file. Closes it after the block is executed. */
  def openFileWriter[T](fileName: File, append: Boolean = false)(func: BufferedWriter => T) = useResource(new BufferedWriter(new FileWriter(fileName, append)))(func)

  /** Opens an unbuffered [[java.io.Writer]] on the file. Closes it after the block is executed. */
  def openUnbufferedFileWriter[T](fileName: File, append: Boolean = false)(func: java.io.Writer => T) = useResource(new FileWriter(fileName, append))(func)

  /** Opens a buffered [[java.io.BufferedOutputStream]] on the file. Closes it after the block is executed. */
  def openFileOutputStream[T](fileName: File, append: Boolean = false)(func: BufferedOutputStream => T) = useResource(new BufferedOutputStream(new FileOutputStream(fileName, append)))(func)

  /** Opens a buffered [[java.io.BufferedInputStream]] on the file. Closes it after the block is executed. */
  def openFileInputStream[T](fileName: File)(func: BufferedInputStream => T) = useResource(new BufferedInputStream(new FileInputStream(fileName)))(func)

  /** Opens a buffered [[java.io.BufferedReader]] on the file. Closes it after the block is executed. */
  def openFileReader[T](fileName: File)(func: BufferedReader => T): T = {
    if (isGZipFile(fileName.getAbsolutePath)) openZippedFileReader(fileName)(func)
    else useResource(new BufferedReader(new FileReader(fileName)))(func)
  }

  /** Opens a [[java.io.BufferedReader]] on a GZipped file. Closes it after the block is executed. */
  def openZippedFileReader[T](fileName: File)(func: BufferedReader => T): T = {
    useResource(new BufferedReader(new java.io.InputStreamReader(new BufferedInputStream(new GZIPInputStream(new FileInputStream(fileName), 65536), 65536))))(func)
  }

  /** Opens a [[java.io.BufferedWriter]] which writes GZip compressed data to the given path. Closes it after the block is executed. */
  def openZippedFileWriter[T](fileName: File, append: Boolean = false)(func: java.io.Writer => T) = useResource(new BufferedWriter(new java.io.OutputStreamWriter(new java.util.zip.GZIPOutputStream(new FileOutputStream(fileName, append)))))(func)

  /** Opens a [[java.io.BufferedWriter]] which writes Block Compressed GZip  data to the given path. Closes it after the block is executed. */
  def openBlockedZippedFileWriter[T](fileName: File)(func: java.io.Writer => T) = useResource(new BufferedWriter(new java.io.OutputStreamWriter(new htsjdk.samtools.util.BlockCompressedOutputStream((fileName)))))(func)

  /** Opens a [[java.io.OutputStream]] which writes GZip compressed data to the given path. Closes it after the block is executed. */
  def openZippedFileOutputStream[T](fileName: File, append: Boolean = false)(func: java.io.OutputStream => T) = useResource(new java.util.zip.GZIPOutputStream(new FileOutputStream(fileName, append)))(func)

  /** Opens a [[java.io.OutputStream]] which writes GZip compressed data to the given path. Closes it after the block is executed. */
  def openBlockedZippedFileOutputStream[T](fileName: File)(func: java.io.OutputStream => T) = useResource(new htsjdk.samtools.util.BlockCompressedOutputStream(fileName))(func)

  /** Opens a [[java.io.OutputStream]] which writes GZip compressed data to the given path. Closes it after the block is executed. */
  def openZippedFileInputStream[T](fileName: File)(func: java.io.InputStream => T) = useResource(new java.util.zip.GZIPInputStream(new FileInputStream(fileName)))(func)

  def openBlockedZippedFileInputStream[T](fileName: File)(func: htsjdk.samtools.util.BlockCompressedInputStream => T) = useResource(new htsjdk.samtools.util.BlockCompressedInputStream(fileName))(func)

  /** Returns a [[java.io.BufferedReader]]. */
  def getFileReader(fileName: File) = new BufferedReader(new FileReader(fileName))

  /** Returns a [[java.io.BufferedWriter]]. */
  def getFileWriter(fileName: File, append: Boolean = false) = new BufferedWriter(new FileWriter(fileName, append))

  /** Returns an iterator of tokens from the Reader.*/
  def tokenizeReaderIterator(reader: java.io.Reader, sep: Array[Char]): Iterator[String] = new Iterator[String] {

    @inline
    def arrayContains(ar: Array[Int], elem: Int): Boolean = {
      var b = false
      var s = 0
      val k = ar.size
      while (!b && s < k) {
        if (elem == ar(s)) {
          b = true
        } else {
          s += 1
        }
      }
      b
    }

    val sbuilder = new scala.collection.mutable.StringBuilder

    @inline
    def readNextToken(reader: java.io.Reader, sep: Array[Int]): String = {
      var c = reader.read
      while (c != -1 && arrayContains(sep, c)) {
        c = reader.read
      }
      if (c != -1) {
        sbuilder.append(c.toChar)
        c = reader.read
      }
      while (c != -1 && !arrayContains(sep, c)) {
        sbuilder.append(c.toChar)
        c = reader.read
      }
      val r = sbuilder.toString
      sbuilder.clear
      r
    }

    val sep2 = sep.map(_.toInt).toArray

    var s = readNextToken(reader, sep2)

    @inline
    def hasNext = s.size != 0

    @inline
    def next = {
      val ret = s;
      s = readNextToken(reader, sep2)
      ret
    }
  }

  /** Returns an iterator of tokens from the Reader.*/
  def tokenizeReader(reader: java.io.Reader, sep: Char): IndexedSeq[String] = tokenizeReader(reader, Array[Char](sep))

  /** Returns an iterator of tokens from the Reader.*/
  def tokenizeReader(reader: java.io.Reader, sep: Array[Char]): IndexedSeq[String] = tokenizeReaderIterator(reader, sep).toIndexedSeq

  /** Reads file contents into a bytearray. */
  def readBinaryFile(fileName: String): Array[Byte] = {
    useResource(new BufferedInputStream(new FileInputStream(fileName))) { f =>
      readBinaryStream(f)
    }
  }

  /** Reads file contents into a bytearray. */
  def readBinaryFile(f: File): Array[Byte] = readBinaryFile(f.getAbsolutePath)

  /** Reads file contents into a bytearray. */
  def readBinaryStream(f: java.io.InputStream): Array[Byte] = {
    def read(x: List[Byte]): List[Byte] = {
      val raw = f.read
      val ch: Byte = raw.toByte
      if (raw != -1) {
        read(ch :: x)
      } else {
        x
      }
    }
    read(Nil).reverse.toArray
  }

  /** Reads file contents into a bytearray by fixed sized chunks. A block is executed on each chunk. */
  def readBinaryFileByChunks(fileName: String, chunkSize: Int)(func: Array[Byte] => Unit) {
    useResource(new BufferedInputStream(new FileInputStream(fileName))) { f =>
      val ar = new Array[Byte](chunkSize)
      var raw = f.read
      var i = 0
      while (raw != -1) {
        val ch: Byte = raw.toByte
        ar(i) = ch
        if (i == (chunkSize - 1)) {
          func(ar)
          i = 0
        } else i += 1
        raw = f.read
      }
      if (i != 0) {
        val ar2 = new Array[Byte](i)
        for (k <- 0 to i - 1) {
          ar2(k) = ar(k)
        }
        func(ar2)
      }

    }
  }

  /** Concatenates text files together, dropping header lines for each, except the first. */
  def cat(in: Iterable[File], out: File, header: Int = 0) {
    var skip = 0
    openFileWriter(out, false) { writer =>
      in.foreach { f =>
        openSource(f.getAbsolutePath) {
          _.getLines.drop(skip).foreach { line =>
            writer.write(line)
            writer.write('\n')
          }
        }
        skip = header
      }
    }
  }

  /** Computes MD5 checksum of the file. */
  def getCheckSum(fileName: String, chunk: Int = 1000 * 1000): String = {
    val digest = java.security.MessageDigest.getInstance("MD5")
    mybiotools.readBinaryFileByChunks(fileName, chunk) { x => digest.update(x) }
    digest.digest().map(0xFF & _).map { "%02x".format(_) }.foldLeft("") { _ + _ }
  }

  implicit def stringToProcess(command: String): ProcessBuilder = Process(command)
  implicit def stringSeqToProcess(command: Seq[String]): ProcessBuilder = Process(command)

  /**
   * Execute command with user function to process each line of output.
   *
   * Based on from http://www.jroller.com/thebugslayer/entry/executing_external_system_commands_in
   * Creates 3 new threads: one for the stdout, one for the stderror, and one waits for the exit code.
   * @param pb Description of the executable process
   * @param atMost Maximum time to wait for the process to complete. Default infinite.
   * @return Exit code of the process.
   */
  def exec(pb: ProcessBuilder, atMost: Duration = Duration.Inf)(stdOutFunc: String => Unit = { x: String => })(implicit stdErrFunc: String => Unit = (x: String) => ()): Int = {

    import java.util.concurrent.Executors

    val executorService = Executors.newSingleThreadExecutor

    implicit val ec = ExecutionContext.fromExecutorService(executorService)

    val process = pb.run(ProcessLogger(stdOutFunc, stdErrFunc))

    val hook = try {
      scala.sys.addShutdownHook { process.destroy() }
    } catch {
      case x: Throwable => {
        Try(process.destroy)
        Try(executorService.shutdownNow)
        throw x
      }
    }

    try {
      val f = Future { process.exitValue }
      Await.result(f, atMost = atMost)
    } finally {
      Try(process.destroy)
      Try(executorService.shutdownNow)
      Try(hook.remove)

    }
  }

  /** Execute command, returns standard output lines as list */
  def execStdOutAsList(pb: ProcessBuilder): List[String] = {
    var ls: List[String] = Nil
    exec(pb) { ln => ls = ln :: ls }()
    ls.reverse
  }

  /** Execute command, returns stdout and stderr lines as lists */
  def execStreamsAsList(pb: ProcessBuilder): Tuple2[List[String], List[String]] = {
    var ls: List[String] = Nil
    var lse: List[String] = Nil
    exec(pb) { ln => ls = ln :: ls } { ln => lse = ln :: lse }
    (ls.reverse, lse.reverse)
  }

  /** Execute command, returns true if stderr is empty */
  def execIsFinishedWithoutErrorStream(pb: ProcessBuilder)(implicit
    func1: String => Unit = { x: String => println(x) },
    func2: String => Unit = { x: String => println(x) }): Boolean = {
    var boolean = true
    exec(pb) { ln => func1(ln) } { ln => boolean = false; func2(ln) }
    boolean
  }

  /**
   * Execute command. Returns stdout and stderr as strings, and true if it was successful.
   *
   * A process is considered successful if its exit code is 0 and the error stream is empty.
   * The latter criterion can be disabled with the unsuccessfulOnErrorStream parameter.
   * @param pb The process description.
   * @param unsuccessfulOnErrorStream if true, then the process is considered as a failure if its stderr is not empty.
   * @param atMost max waiting time.
   * @return (stdout,stderr,success) triples
   */
  def execGetStreamsAndCode(pb: ProcessBuilder, unsuccessfulOnErrorStream: Boolean = true, atMost: Duration = Duration.Inf): (List[String], List[String], Boolean) = {
    var ls: List[String] = Nil
    var lse: List[String] = Nil
    var boolean = true
    val exitvalue = exec(pb, atMost) { ln => ls = ln :: ls } { ln => if (unsuccessfulOnErrorStream) { boolean = false }; lse = ln :: lse }
    (ls.reverse, lse.reverse, boolean && (exitvalue == 0))
  }

  /**
   * Execute command. Returns stdout and stderr as strings, and true if it was successful. Also writes to log.
   *
   * A process is considered successful if its exit code is 0 and the error stream is empty.
   * The latter criterion can be disabled with the unsuccessfulOnErrorStream parameter.
   * @param pb The process description.
   * @param unsuccessfulOnErrorStream if true, then the process is considered as a failure if its stderr is not empty.
   * @param atMost max waiting time.
   * @param log A logger.
   * @return (stdout,stderr,success) triples
   */
  def execGetStreamsAndCodeWithLog(pb: ProcessBuilder, unsuccessfulOnErrorStream: Boolean = true, atMost: Duration = Duration.Inf)(implicit log: {
    def info(s: String): Unit; def error(s: String): Unit
  }): (List[String], List[String], Boolean) = {
    var ls: List[String] = Nil
    var lse: List[String] = Nil
    var boolean = true
    val exitvalue = exec(pb, atMost) { ln => ls = ln :: ls; log.info(ln) } { ln => if (unsuccessfulOnErrorStream) { boolean = false }; lse = ln :: lse; if (unsuccessfulOnErrorStream) log.error(ln) else log.info(ln) }
    (ls.reverse, lse.reverse, boolean && (exitvalue == 0))
  }

  /** Return true iff stderr is empty and return code is zero */
  def execIsFinishedWithoutError(pb: ProcessBuilder)(implicit
    func1: String => Unit = { x: String => println(x) },
    func2: String => Unit = { x: String => println(x) }): Boolean = {
    var boolean = true
    val exitvalue = exec(pb) { ln => func1(ln) } { ln => boolean = false; func2(ln) }
    boolean && (exitvalue == 0)
  }

  /** Reads a fasta file. */
  def readFasta(source: Source): FastaSequenceData = {
    Map[SequenceKey, String](fastaIterator(source).toList: _*)
  }

  /** Returns an iterator on a fasta file. */
  def fastaIterator(source: Source): Iterator[Tuple2[SequenceKey, String]] = {
    tokenizeIterator(source, '>').drop(1).map { entry =>
      val spl = fastSplitSeparatorIterator(entry.mkString, '\n')
      val header = spl.next
      val seq = spl.mkString("")
      (header, seq)
    }
  }

  /** Maps fasta headers. Data is unchanged. */
  def transformFastaHeaders(input: FastaSequenceData)(func: SequenceKey => SequenceKey): FastaSequenceData = {
    for ((key, value) <- input; t = func(key)) yield {
      (t -> value)
    }
  }

  /** Formats the fasta entry as 80 char wide lines */
  def formatForFastaEntry(id: String, seq: String, lineWidth: Int = 80): List[String] = {
    val header = ">" + id
    val div = seq.size / lineWidth
    val list = for (i <- 1 to div) yield {
      seq.substring((i - 1) * lineWidth, i * lineWidth)
    }
    val x = (header :: list.toList)
    (seq.substring(div * lineWidth, seq.size) :: x.reverse).reverse
  }

  /** Renders full fasta datastring. */
  def makeFastaString(seqData: Iterable[(SequenceKey, String)], lineWidth: Int = 80): String = {
    val list = seqData.flatMap {
      case (id, seq) =>
        formatForFastaEntry(id.toString, seq, lineWidth)
    }
    return (list.mkString("\n"))
  }

  /** Writes fasta to file. Renders the whole string in memory first. */
  def writeFasta(fileName: String, seqData: Iterable[(SequenceKey, String)], lineWidth: Int): Unit = {
    writeToFile(fileName, makeFastaString(seqData, lineWidth))
  }

  /** Writes fasta to file. Does not read the whole iterator in memory at once. */
  def writeFasta(writer: java.io.Writer, fastaIter: Iterator[Tuple2[SequenceKey, String]], lineWidth: Int) {
    fastaIter.foreach { entry =>
      formatForFastaEntry(entry._1.toString, entry._2, lineWidth).foreach { line =>
        writer.write(line)
        writer.write('\n')
      }
    }
  }

  /** Writes fasta to file. Does not read the whole iterator in memory at once. */
  def writeFasta(fileName: String, fastaIter: Iterator[Tuple2[_ <: SequenceKey, String]], lineWidth: Int) {
    openFileWriter(new File(fileName))(writer => writeFasta(writer, fastaIter, lineWidth))
  }

  /** ADT representing gender. */
  sealed trait Gender
  case object Female extends Gender
  case object Male extends Gender

  /** Factory methods for Gender. */
  object Gender {
    def fromPlinkInt(i: Int): Option[Gender] = i match {
      case 1 => Some(Male)
      case 2 => Some(Female)
      case _ => None
    }
    def fromString(s: String): Option[Gender] = s match {
      case x if x.toLowerCase == "female" || x.toLowerCase == "f" || x == "2" => Some(Female)
      case x if x.toLowerCase == "male" || x.toLowerCase == "m" || x == "1" => Some(Male)
      case _ => None
    }
  }

  /**
   * Splits a string onto substrings. Very fast.
   *
   * Cuts the string into substrings and gives an iterator on them. No string copy is made, the original string is preserved in memory! (see java substring)
   */
  def fastSplit1WideSeparatorIterator8(str: SubS, sep1: Char): Iterator[SubString8] = new Iterator[SubString8] {
    private var i = 0
    private val sep = sep1.toByte

    def hasNext = i <= str.length

    def next = {
      val j = i
      while (i < str.length && (str.apply(i) !== sep)) {
        i += 1
      }
      val ret = str.substring(j, i)
      i = i + 1

      ret
    }

  }

  /**
   * Splits a string onto substrings. Very fast.
   *
   * Cuts the string into substrings and gives an iterator on them. No string copy is made, the original string is preserved in memory! (see java substring)
   */
  def fastSplit1WideSeparatorIterator(str: String, sep: Char): Iterator[String] = new Iterator[String] {
    private var i = 0

    def hasNext = i <= str.length

    def next = {
      val j = i
      while (i < str.length && str.charAt(i) != sep) {
        i += 1
      }
      val ret = str.substring(j, i)
      i = i + 1

      ret
    }

  }

  /**
   * Splits a string onto substrings. Very fast.
   *
   * Cuts the string into substrings and gives an iterator on them. No string copy is made, the original string is preserved in memory! (see java substring)
   */
  def fastSplit1WideSetSeparatorIterator8(str: SubS, sep1: Set[Char]): Iterator[SubString8] = new Iterator[SubString8] {
    private var i = 0
    val sep = sep1.map(_.toByte)

    def hasNext = i <= str.length

    def next = {
      val j = i
      while (i < str.length && !sep.contains(str.apply(i))) {
        i += 1
      }
      val ret = str.substring(j, i)
      i = i + 1

      ret
    }

  }

  /**
   * Splits a string onto substrings. Very fast.
   *
   * Cuts the string into substrings and gives an iterator on them. No string copy is made, the original string is preserved in memory! (see java substring)
   */
  def fastSplit1WideSetSeparatorIterator(str: String, sep: Set[Char]): Iterator[String] = new Iterator[String] {
    private var i = 0

    def hasNext = i <= str.length

    def next = {
      val j = i
      while (i < str.length && !sep.contains(str.charAt(i))) {
        i += 1
      }
      val ret = str.substring(j, i)
      i = i + 1

      ret
    }

  }

  /** Splits a string onto substrings and stores all substrings in the given mutable buffer. */
  def storeIterInArrayAll[T](iter: Iterator[T], destination: ArrayBuffer[T]): Unit = {
    var i = 0
    while (iter.hasNext) {
      val s = iter.next

      while (destination.size < i + 1) destination += (null).asInstanceOf[T]

      destination(i) = s

      i += 1
    }
  }

  /** Splits a string onto substrings and stores the substrings between [i,j) indices (exclusive end)  in the given mutable buffer. */
  def storeIterInArrayInterval[T](iter: Iterator[T], destination: ArrayBuffer[T], i: Int, j: Int): Unit = {
    var k = 0
    while (iter.hasNext && k < j) {
      val s = iter.next

      if (k >= i && k < j) {

        while (destination.size < k + 1 - i) destination += (null).asInstanceOf[T]

        destination(k - i) = s
      }

      k += 1
    }
  }

  /**
   * Splits string onto substrings, while continuous spans of separators are merged.
   *
   * Cuts the string into substrings and gives an iterator on them. No string copy is made, the original string is preserved in memory! (see java substring)
   */
  def fastSplitSeparatorIterator8(str: SubS, sep1: Char): Iterator[SubString8] = new Iterator[SubString8] {
    var i = 0
    val sep = sep1.toByte

    while (i < str.length && str.apply(i) === sep) {
      i = i + 1
    }

    def hasNext = i < str.length

    def next = {
      val j = i
      while (i < str.length && (str.apply(i) !== sep)) {
        i = i + 1
      }
      val ret = str.substring(j, i)
      while (i < str.length && (str.apply(i) === sep)) {
        i = i + 1
      }

      ret
    }

  }

  /** Splits a string onto substrings and converts them to an IndexedSeq[String]. */
  def fastSplit1WideSeparator(str: String, sep: Char): IndexedSeq[String] = fastSplit1WideSeparatorIterator(str, sep).toIndexedSeq

  def fastSplit1WideSetSeparator(str: String, sep: Set[Char]): IndexedSeq[String] = fastSplit1WideSetSeparatorIterator(str, sep).toIndexedSeq

  /**
   * Splits string onto substrings, while continuous spans of separators are merged.
   *
   * Cuts the string into substrings and gives an iterator on them. No string copy is made, the original string is preserved in memory! (see java substring)
   */
  def fastSplitSeparatorIterator(str: String, sep: Char): Iterator[String] = new Iterator[String] {
    var i = 0

    while (i < str.length && str.charAt(i) == sep) {
      i = i + 1
    }

    def hasNext = i < str.length

    def next = {
      val j = i
      while (i < str.length && str.charAt(i) != sep) {
        i = i + 1
      }
      val ret = str.substring(j, i)
      while (i < str.length && str.charAt(i) == sep) {
        i = i + 1
      }

      ret
    }

  }

  /**
   * Splits string onto substrings, while continuous spans of separators are merged.
   *
   * Converts to IndexedSeq.
   */
  def fastSplitSeparator(str: String, sep: Char): IndexedSeq[String] = {
    fastSplitSeparatorIterator(str, sep).toIndexedSeq
  }

  def fastSplitSetSeparator(str: String, sep: Set[Char]): IndexedSeq[String] = {
    fastSplitSetSeparatorIterator(str, sep).toIndexedSeq
  }

  /**
   * Splits string onto substrings, while continuous spans of separators are merged.
   *
   * Multiple separators are allowed.
   * Cuts the string into substrings and gives an iterator on them. No string copy is made, the original string is preserved in memory! (see java substring)
   */
  def fastSplitSetSeparatorIterator(str: String, sep: Set[Char]): Iterator[String] = new Iterator[String] {
    var i = 0

    while (i < str.length && sep.contains(str.charAt(i))) {
      i = i + 1
    }

    def hasNext = i < str.length

    def next = {
      val j = i
      while (i < str.length && !sep.contains(str.charAt(i))) {
        i = i + 1
      }
      val ret = str.substring(j, i)
      while (i < str.length && sep.contains(str.charAt(i))) {
        i = i + 1
      }

      ret
    }

  }

  /**
   * Splits string onto substrings, while continuous spans of separators are merged.
   *
   * Multiple separators are allowed.
   * Cuts the string into substrings and gives an iterator on them. No string copy is made, the original string is preserved in memory! (see java substring)
   */
  def fastSplitSetSeparatorIterator8(str: SubS, sep1: Set[Char]): Iterator[SubString8] = new Iterator[SubString8] {
    var i = 0
    val sep = sep1.map(_.toByte)

    while (i < str.length && sep.contains(str.apply(i))) {
      i = i + 1
    }

    def hasNext = i < str.length

    def next = {
      val j = i
      while (i < str.length && !sep.contains(str.apply(i))) {
        i = i + 1
      }
      val ret = str.substring(j, i)
      while (i < str.length && sep.contains(str.apply(i))) {
        i = i + 1
      }

      ret
    }

  }

  implicit class SubSWithFastSplit(string: SubS) {
    def fastSplit(s: Char) = fastSplitSeparatorIterator8(string, s).toIndexedSeq
    def fastSplit(ss: Set[Char]) = fastSplitSetSeparatorIterator8(string, ss).toIndexedSeq
    def fastSplitIterator(s: Char) = fastSplitSeparatorIterator8(string, s)
    def fastSplitIterator(ss: Set[Char]) = fastSplitSetSeparatorIterator8(string, ss)

    def fastSplit1Wide(s: Char) = fastSplit1WideSeparatorIterator8(string, s).toIndexedSeq
    def fastSplit1Wide(ss: Set[Char]) = fastSplit1WideSetSeparatorIterator8(string, ss).toIndexedSeq
    def fastSplit1WideIterator(s: Char) = fastSplit1WideSeparatorIterator8(string, s)
    def fastSplit1WideIterator(ss: Set[Char]) = fastSplit1WideSetSeparatorIterator8(string, ss)
  }

  implicit class StringWithFastSplit(string: String) {
    def fastSplit(s: Char) = fastSplitSeparator(string, s)
    def fastSplit(ss: Set[Char]) = fastSplitSetSeparator(string, ss)
    def fastSplitIterator(s: Char) = fastSplitSeparatorIterator(string, s)
    def fastSplitIterator(ss: Set[Char]) = fastSplitSetSeparatorIterator(string, ss)

    def fastSplit1Wide(s: Char) = fastSplit1WideSeparator(string, s)
    def fastSplit1Wide(ss: Set[Char]) = fastSplit1WideSetSeparator(string, ss)
    def fastSplit1WideIterator(s: Char) = fastSplit1WideSeparatorIterator(string, s)
    def fastSplit1WideIterator(ss: Set[Char]) = fastSplit1WideSetSeparatorIterator(string, ss)
  }

  /**
   * Overly simple table reading. You might want to use the [[mybiotools.tabular]] package instead.
   *
   * @param file input source
   * @param header whether the file has a header
   * @param key a special column which can be filtered on
   * @param whereKeyIn a list of value for the key column
   * @param sep separator string.
   * @param emptyFieldsIncluded if true, empty fields are included as empty strings in the return value
   */
  def readTable(
    file: Source,
    header: Boolean = true,
    key: Option[Symbol] = None,
    whereKeyIn: Option[List[String]] = None,
    sep: String = ",",
    emptyFieldsIncluded: Boolean = true
  ): FlatTable[Symbol, String] = {
    if (!key.isEmpty && !header) {
      throw new IllegalArgumentException("Key is not empty and header is false.")
    }
    if (key.isEmpty && !whereKeyIn.isEmpty) throw new IllegalArgumentException("Key is empty but whereKeyIn is not empty.")
    val separator = if (sep == " ") "\\s+" else sep
    val iterator = file.getLines
    var columnNamesIsSet = false
    var columnNames: Array[Symbol] = if (header) {
      columnNamesIsSet = true
      iterator.next.split(separator).map(x => Symbol(x.trim))
    } else {
      new Array(0)
    }
    val indexOfKeyinColumnNames = if (!whereKeyIn.isEmpty) columnNames.indexOf(key.get)
    import scala.collection.mutable.ArrayBuffer
    val returnData = new ArrayBuffer[Map[Symbol, String]]()

    while (iterator.hasNext) {
      val listOfReadLines = new ArrayBuffer[String]()
      while (listOfReadLines.size < 50000 && iterator.hasNext) {
        val s = iterator.next
        if (s != "") listOfReadLines.append(s + " ") // s+ " ": Append a whitespace to the end of the line. We will trim it anyway, and it helps split.
      }
      for (line <- listOfReadLines) {
        val splittedLine = line.split(separator).toList
        if (!columnNamesIsSet) {
          columnNamesIsSet = true
          columnNames = new Array(splittedLine.size)
          for (i <- 1 to splittedLine.size) columnNames(i - 1) = Symbol("V" + i.toString)
        }
        if (splittedLine.size != columnNames.size) throw new RuntimeException("Line and header has not equal number of fields." + splittedLine.toString + columnNames.toString)
        if (whereKeyIn.isEmpty) {
          var tmpMap = Map[Symbol, Any]()
          for (i <- 0 to splittedLine.size - 1) {
            val trimmed = splittedLine(i).trim
            if (trimmed != "" || emptyFieldsIncluded) tmpMap += (columnNames(i) -> trimmed)
          }
          returnData.asInstanceOf[ArrayBuffer[Map[Symbol, Any]]].append(tmpMap)
        } else {
          if (whereKeyIn.get.contains(splittedLine(indexOfKeyinColumnNames.asInstanceOf[Int]).trim)) {
            var tmpMap = Map[Symbol, Any]()
            for (i <- 0 to splittedLine.size - 1) {
              val trimmed = splittedLine(i).trim
              if (trimmed != "" || emptyFieldsIncluded) tmpMap += (columnNames(i) -> trimmed)
            }
            returnData.asInstanceOf[ArrayBuffer[Map[Symbol, Any]]].append(tmpMap)
          }
        }
      }
    }
    returnData.toList
  }

  /** Overly simple table reading. Consider [[mybiotools.tabular]] .*/
  def readTableAsMap[A](
    file: Source,
    key: Symbol,
    sep: String = ","
  )(implicit factory: String => A): KeyedTable[A, String] = {
    val iterator = file.getLines
    val separator = if (sep == " ") "\\s+" else sep
    val columnNames: Array[Symbol] = iterator.next.split(separator).map(x => Symbol(x.trim))
    var returnData = Map[A, Map[Symbol, String]]()
    while (iterator.hasNext) {
      val listOfReadLines = new scala.collection.mutable.ListBuffer[String]()
      while (listOfReadLines.size < 50000 && iterator.hasNext) {
        val s = iterator.next
        if (s != "") listOfReadLines.append(s + " ")
      }
      for (line <- listOfReadLines) {
        val splittedLine = line.split(separator).toList
        var tmpMap = Map[Symbol, String]()
        for (i <- 0 to splittedLine.size - 1) {
          tmpMap += (columnNames(i) -> splittedLine(i).trim)
        }
        returnData += (factory(tmpMap(key).toString) -> tmpMap)
      }
    }
    returnData
  }

  /** Returns majority consensus sequence. */
  def calculateCoverage(alignment: FastaSequenceData): Seq[Map[Char, Int]] = {
    def addSeq(s: String, sum: Seq[Map[Char, Int]]): Seq[Map[Char, Int]] = sum.zipWithIndex.map { case (freqs, idx) => freqs.updated(s(idx), freqs.getOrElse(s(idx), 0) + 1) }

    val empty: Seq[Map[Char, Int]] = alignment.values.head.map { x => Map[Char, Int]() }.toSeq
    alignment.values.map(_.toUpperCase).foldRight(empty)(addSeq(_, _))
  }

  def calculateMissingRate(alignment: FastaSequenceData)(filter: Char => Boolean): Seq[Double] = calculateCoverage(alignment).map { m =>
    val sum = m.values.sum
    val nonmiss = m.filter(x => filter(x._1)).values.sum
    nonmiss.toDouble / sum
  }

  /** Returns majority consensus sequence. */
  def makeConsensus(alignment: FastaSequenceData): String = makeConsensus(alignment, 0.0, 'X', List('*', 'N', 'X'))

  /** Returns majority consensus sequence. */
  def makeConsensus(alignment: FastaSequenceData, threshold: Double, noCall: Char, missingNucleotides: List[Char]): String = {
    def call(frequencies: Map[Char, Int]): Char = {
      val frequencies2 = frequencies.filterKeys(k => !missingNucleotides.contains(k))
      if (frequencies2.isEmpty) noCall
      else {
        val sum = frequencies2.values.sum
        val relative = frequencies2.map(x => x._1 -> x._2.toDouble / sum)
        val max = {
          val max1 = relative.maxBy(_._2)
          if (max1._1 === noCall) {
            val second = relative.toSeq.sortBy(_._2).reverse.drop(1).head
            if (second._2 >= 0.45) second
            else max1
          } else max1
        }
        if (max._2 >= threshold) max._1 else noCall
      }
    }
    calculateCoverage(alignment).map { call }.mkString
  }

  /** Returns majority consensus sequence with subthreshold traits included */
  def makeConsensusInsertFrequencies(alignment: FastaSequenceData, threshold: Double, noCall: Char, missingNucleotides: List[Char]): String = {
    def addSeq(s: String, sum: Seq[Map[Char, Int]]): Seq[Map[Char, Int]] = sum.zipWithIndex.map { case (freqs, idx) => freqs.updated(s(idx), freqs.getOrElse(s(idx), 0) + 1) }
    def call(frequencies: Map[Char, Int]): String = {
      val frequencies2 = frequencies.filterKeys(k => !missingNucleotides.contains(k))
      val sum = frequencies2.values.sum
      val relative = frequencies2.map(x => x._1 -> x._2.toDouble / sum)
      val max = relative.maxBy(_._2)
      if (max._2 >= threshold) max._1.toString else "[" + relative.toSeq.sortBy(_._2).reverse.map(x => x._1 + ":" + x._2.format(2)).mkString("/") + "]"
    }
    val empty: Seq[Map[Char, Int]] = alignment.values.head.map { x => Map[Char, Int]() }.toSeq
    alignment.values.map(_.toUpperCase).foldRight(empty)(addSeq(_, _)).flatMap { call }.mkString
  }

  /** Pads an alignment with a fill charactor such that all sequences will have equal length. */
  def padAlignment(alignment: FastaSequenceData, fill: Char = '-'): FastaSequenceData = {
    if (alignment.isEmpty) alignment else {
      val maxLength: Int = alignment.maxBy { (entry) => entry._2.length }._2.length
      var padded = Map[SequenceKey, String]()
      alignment.foreach { entry =>
        val key = entry._1
        val seq = entry._2
        var paddedseq = seq
        if (seq.length < maxLength) {
          val t: Int = (maxLength - seq.length)
          for (_ <- 1 to t) { paddedseq = paddedseq.concat(fill.toString) }
        }
        padded += (key -> paddedseq)
      }
      padded
    }
  }

  /** Deletes specific columns from alignment. */
  def deleteColumnsFromAlignment(alignment: FastaSequenceData, cols: Set[Int]): FastaSequenceData = {
    alignment.map(x => x._1 -> x._2.zipWithIndex.filterNot(x => cols.contains(x._2)).map(_._1).mkString)
  }

  /** Masks specific columns of alignment with a character. */
  def maskColumnsOfAlignment(alignment: FastaSequenceData, cols: Set[Int], fill: Char = 'X'): FastaSequenceData = {
    alignment.map(x => x._1 -> x._2.zipWithIndex.map(x => (if (cols.contains(x._2)) 'X' else x._1)).mkString)
  }

  /** Masks specific columns of alignment with a character. */
  def maskColumnsOfAlignmentNegated(alignment: FastaSequenceData, cols: Set[Int], fill: Char = 'X'): FastaSequenceData = {
    alignment.map(x => x._1 -> x._2.zipWithIndex.map(x => (if (!cols.contains(x._2)) 'X' else x._1)).mkString)
  }

  /** Selects col-th column from an alignment */
  def column(fasta: FastaSequenceData, col: Int): FastaSequenceData = fasta.mapValues(_.apply(col).toString)

  /** Selects cols-th columns from an alignment */
  def extractColumnsFromAlignment(alignment: FastaSequenceData, cols: Set[Int]): FastaSequenceData = {
    alignment.mapValues(_.zipWithIndex.filter(x => cols.contains(x._2)).map(_._1).mkString)
  }

  /** Gets those column indices where the given sequence had no gap ('-'). */
  def getNonGappyColumnsOfSequence(alignment: FastaSequenceData, key: SequenceKey): Set[Int] = {
    alignment.get(key) match {
      case None => Set[Int]()
      case Some(sequence) => sequence.zipWithIndex.filter(x => x._1 != '-').map(_._2).toSet
    }
  }

  /** Remove those column from the alignment where the key sequence has a gap. */
  def maskToSequenceKey(alignment: FastaSequenceData, key: SequenceKey): FastaSequenceData = {
    extractColumnsFromAlignment(alignment, getNonGappyColumnsOfSequence(alignment, key))
  }

  /** Horizontally concatenate several alignments, first padding them with fillChar. */
  def concatenateFastas(alignments: Seq[FastaSequenceData], fillChar: Char): FastaSequenceData = {
    def concat2(al1: FastaSequenceData, al2: FastaSequenceData) = {
      val keys = (al1.keys.toList ::: al2.keys.toList).toSet
      val padded1 = padAlignment(al1)
      val padded2 = padAlignment(al2)

      val padded1size = if (padded1.size == 0) 0 else padded1.head._2.size
      val padded2size = if (padded2.size == 0) 0 else padded2.head._2.size
      Map[SequenceKey, String](keys.map { key =>
        val string1 = padded1.get(key).getOrElse(fillChar.toString * padded1size)
        val string2 = padded2.get(key).getOrElse(fillChar.toString * padded2size)
        (key, string1 + string2)
      }.toSeq: _*)
    }
    alignments.foldLeft(Map[SequenceKey, String]())((part, next) => concat2(part, next))
  }

  /** Computes relative coverage for each columns. */
  def fastaCoverage(alignment: FastaSequenceData, missingLetters: Set[Char] = Set('-', 'X', '?')): Map[Int, Double] = {
    val length = alignment.head._2.size
    (for (i <- 0 until length) yield {
      val t = alignment.values.map(_.apply(i)).map(letter => letter match {
        case x if missingLetters.contains(x) => 0
        case _ => 1
      })
      val sum = t.sum
      val num = t.size
      i -> sum.toDouble / num
    }).toMap
  }

  /** Returns true if the chracter is a nucleotide ([AaTtGgCc]). */
  @inline
  def isNucleotide(chr: Char): Boolean = chr == 'A' || chr == 'a' || chr == 'T' || chr == 't' ||
    chr == 'G' || chr == 'g' || chr == 'c' || chr == 'C'

  /** Returns true if the chracter is an uppercase nucleotide ([ATGC]). */
  @inline
  def isNucleotideUpperCase(chr: Char): Boolean = chr == 'A' || chr == 'T' ||
    chr == 'G' || chr == 'C'

  /** Returns true if the chracter is an amino acid ([AaTtGgCcDdEeFfHhIiKkLlMmNnPpQqRrrSsVvWwYy]). */
  def isAminoAcid(chr: Char): Boolean = chr == 'A' || chr == 'a' || chr == 'T' || chr == 't' ||
    chr == 'G' || chr == 'g' || chr == 'c' || chr == 'C' || chr == 'D' || chr == 'd' || chr == 'e' || chr == 'E' || chr == 'f' || chr == 'F' || chr == 'h' || chr == 'H' || chr == 'i' || chr == 'I' || chr == 'k' || chr == 'K' || chr == 'l' || chr == 'L' || chr == 'M' || chr == 'm' || chr == 'n' || chr == 'N' || chr == 'p' || chr == 'P' || chr == 'q' || chr == 'Q' || chr == 'r' || chr == 'R' || chr == 's' || chr == 'S' || chr == 'v' || chr == 'V' || chr == 'w' || chr == 'W' || chr == 'y' || chr == 'Y'

  /** Creates 2x2 contingency table from binary data. */
  def contingencyTable[K](x1p: Map[K, Option[Boolean]], x2p: Map[K, Option[Boolean]]): ContingencyTable2x2 = {
    var a11 = 0
    var a12 = 0
    var a21 = 0
    var a22 = 0

    val x1 = x1p.filter(_._2.isDefined).mapValues(_.get)
    val x2 = x2p.filter(_._2.isDefined).mapValues(_.get)

    var length = if (x1.size < x2.size) x1.size else x2.size

    for (i <- (x1.keys.toSet intersect x2.keys.toSet)) {
      (x1(i), x2(i)) match {
        case (true, true) => a11 += 1
        case (true, false) => a21 += 1
        case (false, true) => a12 += 1
        case (false, false) => a22 += 1
        case _ => {}
      }
    }
    ContingencyTable2x2(a11, a12, a21, a22)
  }

  /**
   * Calculate Pearson's Phi coefficient
   *
   * ref: http://en.wikipedia.org/wiki/Phi_coefficient
   */
  def pearsonsPhi(ct: ContingencyTable2x2): Double = {
    import ct._
    (a11 * a22 - a12 * a21) / scala.math.sqrt(rowMargin1 * rowMargin2 * columnMargin2 * columnMargin1)
  }

  /** Created a zipped copy of the file with .gz suffix. */
  def gzipFile(file: File): Boolean = {
    val out = new File(file.getAbsolutePath + ".gz")
    gzipFile(file, out)
  }

  def gzipFile(input: File, out: File): Boolean = {
    try {
      openFileInputStream(input) { is =>
        openZippedFileOutputStream(out) { os =>
          com.google.common.io.ByteStreams.copy(is, os)
        }
      }
      true
    } catch {
      case x: java.lang.Exception => false
    }
  }

  def gunzipFile(input: File, out: File): Boolean = {
    try {
      openZippedFileInputStream(input) { is =>
        openFileOutputStream(out) { os =>
          com.google.common.io.ByteStreams.copy(is, os)
        }
      }
      true
    } catch {
      case x: java.lang.Exception => false
    }
  }

  /**
   * FDR Benjamini-Hochberg procedure
   *
   * ref: http://udel.edu/~mcdonald/statmultcomp.html
   */
  def highestSignificantPValueByFDR(Q: Float, pValues: Iterable[Float], numberOfTests: Long): Float = {
    val Qpm = Q / numberOfTests.toFloat
    pValues.toSeq.sorted.zipWithIndex.takeWhile(x => x._1 <= (x._2 + 1).toFloat * Qpm).map(_._1).lastOption match {
      case Some(x) => x
      case None => 0.0f
    }
  }

  /**
   * FDR Benjamini-Cochberg procedure
   *
   * ref: http://udel.edu/~mcdonald/statmultcomp.html
   */
  def highestSignificantPValueByFDR(Q: Double, pValues: Iterable[Double]): Double = {
    val Qpm = Q / pValues.size.toDouble
    pValues.toSeq.sorted.zipWithIndex.reverse.find(x => x._1 <= (x._2 + 1).toDouble * Qpm).map(_._1) match {
      case Some(x) => x
      case None => 0.0f
    }
  }

  /**
   * FDR Benjamini-Cochberg adjustment
   *
   */
  def adjustPValuesByFDR(pValues: IndexedSeq[Double]): IndexedSeq[Double] = {
    // This is from R
    // i <- lp:1L
    //     o <- order(p, decreasing = TRUE)
    //     ro <- order(o)
    //     pmin(1, cummin(n/i * p[o]))[ro]
    def cummin(d: List[Double], acc: List[Double]): List[Double] = d match {
      case Nil => acc.reverse
      case x :: xs if acc.isEmpty => cummin(xs, x :: Nil)
      case x :: xs if x < acc.head => cummin(xs, x :: acc)
      case x :: xs => cummin(xs, acc.head :: acc)
    }
    val i = pValues.length to 1 by -1
    val o = pValues.zipWithIndex.sortBy(x => -1 * x._1).map(_._2)
    val ro = o.zipWithIndex.sortBy(_._1).map(_._2)
    val n = pValues.size.toDouble
    val m = i zip o map {
      case (i, o) =>
        n / i * pValues(o)
    }
    val cmin = cummin(m.toList, Nil).map(x => math.min(1.0, x)).toIndexedSeq
    ro.map(ro => cmin(ro))
  }

  def adjustPValuesByFDRPairs[T](pValues: IndexedSeq[(T, Double)]): IndexedSeq[(T, Double)] = {
    val (t, p) = pValues.unzip
    t zip adjustPValuesByFDR(p)
  }

  /**
   * zips element by equality of mapped values
   *
   * @example {{{val l1 = List("aaa", "baaa", "cddd", "dddd").reverse
   * val l2 = List("b111", "c111", "d111")
   * val zipped = zipBy(l1, l2)(x => x(0))
   * expect(List("b111" -> "baaa", "c111" -> "cddd", "d111" -> "dddd"))(zipped)
   * }}}
   * @param fun Map function.
   */
  def zipBy[A, B](l1: Iterable[A], l2: Iterable[A])(fun: A => B): Iterable[Tuple2[A, A]] = {

    val longmap = if (l1.size > l2.size) l1.groupBy(fun).map(x => x._1 -> x._2.head) else l2.groupBy(fun).map(x => x._1 -> x._2.head)
    val shortlist = if (l1.size <= l2.size) l1 else l2
    (for (i <- shortlist.par) yield (i, longmap.get(fun(i)))).par.filterNot(x => x._2.isEmpty).map(x => (x._1, x._2.get)).seq

  }

  def zip[A1, A2, A3](l1: Seq[A1], l2: Seq[A2], l3: Seq[A3]): Seq[(A1, A2, A3)] = l1 zip l2 zip l3 map {
    case ((l1, l2), l3) => (l1, l2, l3)
  }

  def zip[A1, A2, A3, A4](l1: Seq[A1], l2: Seq[A2], l3: Seq[A3], l4: Seq[A4]): Seq[(A1, A2, A3, A4)] = l1 zip l2 zip l3 zip l4 map {
    case (((l1, l2), l3), l4) => (l1, l2, l3, l4)
  }

  def zip[A1, A2, A3, A4, A5](l1: Seq[A1], l2: Seq[A2], l3: Seq[A3], l4: Seq[A4], l5: Seq[A5]): Seq[(A1, A2, A3, A4, A5)] = l1 zip l2 zip l3 zip l4 zip l5 map {
    case ((((l1, l2), l3), l4), l5) => (l1, l2, l3, l4, l5)
  }

  /**
   * Binary search
   *
   * Performs binary (logarithmic search) on an ordered sequence.
   * @param a is a sequence of ordered elements.
   * @param less is a function similar to <
   * @param greater is a function similar to >
   * @param mapper comparison is made on the mapped values
   * @return an either where right contains the found element with the index and left contains the indices where the search gave up
   */
  def binarySearch[T, A](
    a: IndexedSeq[A],
    less: T => Boolean,
    greater: T => Boolean,
    mapper: A => T
  ): Either[(Int, Int), (Int, A)] = {
    def recurse(low: Int, high: Int): Either[(Int, Int), (Int, A)] = (low + high) / 2 match {
      case _ if high < low => Left(high, low)
      case mid if greater(mapper(a.apply(mid))) => recurse(low, mid - 1)
      case mid if less(mapper(a.apply(mid))) => recurse(mid + 1, high)
      case mid => Right((mid, a.apply(mid)))
    }
    recurse(0, a.size - 1)
  }

  /**
   * Binary search
   *
   * Performs binary (logarithmic search) on an ordered sequence.
   * @param a is a sequence of ordered elements.
   * @param less is a function similar to <
   * @param greater is a function similar to >
   * @return an either where right contains the found element with the index and left contains the indices where the search gave up
   */
  def binarySearch[T](
    a: IndexedSeq[T],
    less: T => Boolean,
    greater: T => Boolean
  ): Either[(Int, Int), (Int, T)] = binarySearch[T, T](a, less, greater, (x: T) => x)

  /**
   * Binary search
   *
   * Performs binary (logarithmic search) on an ordered sequence.
   * @param a is a sequence of ordered elements.
   * @param mapper comparison is made on the mapped values
   * @return an either where right contains the found element with the index and left contains the indices where the search gave up
   */
  def binarySearch[T, A](a: IndexedSeq[A], v: T, mapper: A => T)(implicit ordering: Ordering[T]): Either[(Int, Int), (Int, A)] = {
    val less = (x: T) => ordering.lt(x, v)
    val greater = (x: T) => ordering.gt(x, v)
    binarySearch(a, less, greater, mapper)
  }

  /**
   * Binary search
   *
   * Performs binary (logarithmic search) on an ordered sequence.
   * @param a is a sequence of ordered elements.
   * @return an either where right contains the found element with the index and left contains the indices where the search gave up
   */
  def binarySearch[T](a: IndexedSeq[T], v: T)(implicit ordering: Ordering[T]): Either[(Int, Int), (Int, T)] = {
    val less = (x: T) => ordering.lt(x, v)
    val greater = (x: T) => ordering.gt(x, v)
    binarySearch(a, less, greater, (x: T) => x)
  }

  /** Returns the common elements in two ordered lists in reverse order. */
  @specialized(Int, Double, Float)
  def intersectAndReverseOrderedLists[T](
    a: List[T],
    b: List[T]
  )(implicit ord: Ordering[T]): List[T] = {
    def loop(x: List[T], y: List[T], acc: List[T]): List[T] = (x, y) match {
      case (Nil, ys) => acc
      case (xs, Nil) => acc
      case (x :: xs, y :: ys) if ord.equiv(x, y) => loop(xs, ys, x :: acc)
      case (x :: xs, y :: ys) if ord.lt(x, y) => loop(xs, y :: ys, acc)
      case (x :: xs, y :: ys) if ord.gt(x, y) => loop(x :: xs, ys, acc)
    }
    loop(a, b, Nil)
  }

  /** Returns the indices of the common elements in two ordered arrays. */
  def intersectOrderedArray(
    a: Array[Int],
    b: Array[Int]
  ): Array[Int] = {

    var x = 0
    var y = 0
    var i = 0
    var nA = a.size
    var nB = b.size
    val acc1 = Array.ofDim[Int](math.min(nA, nB))
    while (x < nA && y < nB) {
      if ((a(x) == b(y))) {
        acc1(i) = a(x)
        i += 1
        x += 1
        y += 1
      } else if ((a(x) < b(y))) {
        x += 1
      } else { y += 1 }
    }
    if (i != acc1.length) {
      val ret = Array.ofDim[Int](i)
      System.arraycopy(acc1, 0, ret, 0, i)
      ret
    } else {
      acc1
    }

  }

  /**
   * Finds the first element where f is true,
   * given that the array is ordered and the first true is preceded by all falses
   */
  def jumpDirectionalSearch[T](
    a: IndexedSeq[T],
    f: T => Boolean, forward: Boolean
  ): Option[(Int, T)] = {
    if (a.isEmpty) None
    else {

      val lastpos = if (forward) a.size - 1 else 0
      val lastVal = if (forward) a.last else a.head
      val firstpos = if (forward) 0 else a.size - 1
      val firstVal = if (forward) a.head else a.last

      def overRun(y: Int) = if (forward) y >= lastpos else trf(y) <= lastpos

      def trf(y: Int) = if (forward) y else {
        a.size - y - 1
      }

      def jumpForward(pos: Int): Option[Int] = pos match {
        case x if overRun(x) && f(lastVal) => Some(lastpos)
        case x if overRun(x) && !f(lastVal) => None
        case x if f(a(trf(x))) => Some(trf(x))
        case x => jumpForward(2 * x)
      }

      if (f(firstVal)) Some((firstpos, firstVal))
      else {

        val firstWhereTrue = jumpForward(1)

        def trf2(i: Int) = if (forward) i else firstWhereTrue.get + i

        if (firstWhereTrue.isEmpty) None
        else
          (binarySearch(
            a.slice(from = if (forward) 0 else firstWhereTrue.get, until = if (forward) firstWhereTrue.get + 1 else a.size),
            less = if (forward) (!f((_: T))) else f,
            greater = if (forward) f else (!f((_: T)))
          ) match {
              case Right((pos, value)) => Some((trf2(pos), a(trf2(pos))))
              case Left((i1, i2)) => (f(a(trf2(i1))), f(a(trf2(i2)))) match {
                case (true, true) => if (forward) Some(i1, a(i1)) else (Some(trf2(i2), a(trf2(i2))))
                case (true, false) => Some(trf2(i1), a(trf2(i1)))
                case (false, true) => Some(trf2(i2), a(trf2(i2)))
                case (false, false) => None
              }
            })
      }
    }

  }

  /**
   * Merge maps with key collision
   * @param fun Handles key collision
   */
  def addMapsQuick[K, V](a: Map[K, V], b: Map[K, V])(fun: ((K, V), (K, V)) => (K, V)): Map[K, V] = {
    import collection.immutable.HashMap
    // val keys = (a.keySet.toVector ++ b.keySet.toVector).distinct
    if (a.isInstanceOf[HashMap[K, V]] && b.isInstanceOf[HashMap[K, V]]) {
      a.asInstanceOf[HashMap[K, V]].merged(b.asInstanceOf[HashMap[K, V]])(fun)
    } else {
      a ++ b.map {
        case (key, bval) =>
          val aval = a.get(key)
          val cval = aval match {
            case None => bval
            case Some(a) => fun((key, a), (key, bval))._2
          }
          (key, cval)
      }
    }
  }

  /**
   * Merge maps with key collision
   * @param fun Handles key collision
   */
  def addMaps[K, V](a: Map[K, V], b: Map[K, V])(fun: (V, V) => V): Map[K, V] = {
    a ++ b.map {
      case (key, bval) =>
        val aval = a.get(key)
        val cval = aval match {
          case None => bval
          case Some(a) => fun((a), (bval))
        }
        (key, cval)
    }
  }

  /**
   * Merge maps with key collision
   * @param fun Handles key collision
   */
  def addMaps[K, V](a: collection.Map[K, V], b: collection.Map[K, V])(fun: (V, V) => V): collection.Map[K, V] = {
    a ++ b.map {
      case (key, bval) =>
        val aval = a.get(key)
        val cval = aval match {
          case None => bval
          case Some(a) => fun((a), (bval))
        }
        (key, cval)
    }
  }

  /** Returns the string with possible 'chr', 'CHR' or 'Chr' prefix removed. */
  def trimChrPrefix(s: String): String = {
    val x = s.take(3)
    if (x == "chr" || x == "CHR" || x == "Chr") s.drop(3) else s
  }

  /**
   * Returns human chromosome number from a string.
   *
   * X -> 23, Y -> 24, XY -> 25, MT -> 26, M -> 26
   */
  def chromosomeNumberFromString(s: String) = trimChrPrefix(s) match {
    case x if x.toUpperCase == "X" => 23
    case x if x.toUpperCase == "Y" => 24
    case x if x.toUpperCase == "XY" => 25
    case x if x.toUpperCase == "MT" => 26
    case x if x.toUpperCase == "M" => 26
    case x: String => x.toInt
  }

  /**
   * Returns string representation of human chrosome numbers.
   *
   * X -> 23, Y -> 24, XY -> 25, MT -> 26, M -> 26
   */
  def chromosomeStringFromNumber(i: Int) = i match {
    case x if x == 23 => "X"
    case x if x == 24 => "Y"
    case x if x == 25 => "XY"
    case x if x == 26 => "M"
    case x => x.toString
  }

  /** Given multiple sets, return those elements that are present in at least share number of them. */
  def selectSharedElements[T](i: Seq[Set[T]], share: Int): Vector[T] = {
    i.combinations(share).map(_.reduceLeft(_ & _)).flatten.toVector.distinct
  }

  def formatDouble2DecimalTo(builder: java.lang.StringBuffer, dd: Double) {
    org.apache.xmlgraphics.util.DoubleFormatUtil.formatDouble(dd, 2, 2, builder)
  }

  def formatDouble(d: Double) = {
    val sb = new java.lang.StringBuffer(5)
    formatDouble2DecimalTo(sb, d)
    sb.toString
  }

  private val formatter = new java.text.DecimalFormat("0.###E0")
  implicit class FormatDouble(val d: Double) extends AnyVal {
    def format(prec: Int): String = formatDouble(d, prec)

    def sFormat: String = formatter.format(d)
  }

  def formatDouble(d: Double, precision: Int) = {
    val sb = new java.lang.StringBuffer(5)
    org.apache.xmlgraphics.util.DoubleFormatUtil.formatDouble(d, precision, precision, sb)
    sb.toString
  }

  def formatDouble(d: Double, decimals: Int, precision: Int) = {
    val sb = new java.lang.StringBuffer(5)
    org.apache.xmlgraphics.util.DoubleFormatUtil.formatDouble(d, decimals, precision, sb)
    sb.toString
  }

  def innerJoin2Files(
    s1: io.Source,
    s2: io.Source,
    sep1: Set[Char],
    sep2: Set[Char],
    joinIdx1: Int,
    joinIdx2: Int
  ): Iterator[IndexedSeq[String]] = {

    val map1 = s1.getLines.map { line =>
      fastSplitSetSeparator(line, sep1)
    }.filter(_.size > 0).map { spl =>
      val (before, after) = spl.splitAt(joinIdx1)
      spl(joinIdx1) -> (before ++ after.drop(1))
    }.toMap

    s2.getLines.map { line =>
      fastSplitSetSeparator(line, sep2)
    }.filter(_.size > 0).map { spl =>
      val joinField = spl(joinIdx2)
      val (before, after) = spl.splitAt(joinIdx2)
      val rest = before ++ after.drop(1)

      map1.get(joinField).map { rest1 =>
        joinField +: (rest1 ++ rest)
      }
    }.filter(_.isDefined).map(_.get)

  }

  /**
   * Given a list of iterators, collects elements with the same keys and apply a function over those.
   *
   * K keys should be unique or identical in the iterators. If duplicates are present, only the first will be processed.
   */
  def collectStreamsAndMap[K, T, R](iterators: List[Iterator[(K, T)]])(f: (K, IndexedSeq[T]) => R): Iterator[R] = {
    new Iterator[R] {

      private var mmap = Map[K, Vector[(T, Iterator[(K, T)])]]()

      private var disabledKeys = Set[K]()

      private val iteratorsSize = iterators.size

      private var restIter: List[Iterator[(K, T)]] = Nil

      private var tailIter = iterators

      // This loops the iterators list
      private def nextIterator = {
        val x = tailIter.head
        if (tailIter.tail != Nil) {
          tailIter = tailIter.tail
          restIter = x :: restIter
        } else {
          tailIter = x :: restIter
          restIter = Nil
        }
        x
      }

      private def readNext = {

        def findNextNonEmpty(current: Iterator[(K, T)]): Iterator[(K, T)] =
          if (current.hasNext) current
          else findNextNonEmpty(nextIterator)

        if (iteratorsNonEmpty) {
          // read 1 value from 1 iterator. If fills the line, then return Some(f(vs)) else None
          val source = findNextNonEmpty(nextIterator)
          val (key, value) = source.next
          if (disabledKeys.contains(key)) None
          else {
            mmap.get(key) match {
              case None => {
                mmap += (key -> Vector(value -> source))
                None
              }
              case Some(v) => {
                if (v.exists(_._2 == source)) None
                else {
                  val appended = v :+ (value -> source)
                  mmap += (key -> appended)
                  if (appended.size == iteratorsSize) {
                    mmap -= (key)
                    disabledKeys += key
                    Some(f(key, appended.map(_._1)))
                  } else None
                }
              }
            }
          }
        } else {
          // drain mmap
          val (key, values) = mmap.head
          mmap -= (key)
          Some(f(key, values.map(_._1)))

        }

      }

      private def mayHasNext = (iteratorsNonEmpty || !mmap.isEmpty)

      private def findNextTransformed(x: Option[R]): Option[R] = x match {
        case None if mayHasNext => findNextTransformed(readNext)
        case Some(x) => Some(x)
        case _ => None
      }

      private def iteratorsNonEmpty = iterators.exists(_.hasNext)

      private var nextvalue: Option[R] = if (mayHasNext) {
        findNextTransformed(readNext)
      } else None

      def hasNext = nextvalue.isDefined

      def next = {
        val r = nextvalue.get
        nextvalue = if (mayHasNext) {
          findNextTransformed(readNext)
        } else None
        r
      }

    }
  }

  def findConnectedComponentsByIntersection[K, T](
    genesets: Map[K, Set[T]],
    minimumIntersection: Int
  ): Set[Map[K, Set[T]]] = {

    var components = List[collection.mutable.Map[K, Set[T]]]()

    genesets.foreach {
      case (gsk, set) =>
        val connectedWith = genesets.filter {
          case (gsk2, set2) =>
            (set2 & set).size >= minimumIntersection
        }
        components.find(cc => connectedWith.exists(c => cc.contains(c._1))) match {
          case None => components = collection.mutable.Map(connectedWith.toSeq: _*) :: components
          case Some(component) => component ++= connectedWith
        }
    }

    components.map(_.toMap).toSet

  }

  def readGMT(source: scala.io.Source): Map[String8, Seq[String8]] = source.getLines.map { line =>
    val splitted = fastSplit1WideSeparator(line, '\t')
    val name = StringStore(splitted.head.replaceAll("/", ""))
    // val desc = new String(splitted(1))
    val rest = splitted.drop(2).map(x => StringStore(x)).toSeq

    name -> rest

  }.toMap

  def reverseMap[K, V](m: Map[K, V]): Map[V, Set[K]] = m.toSeq.groupBy(_._2).map(x => x._1 -> x._2.map(_._1).toSet)

  // This ensures that openSource is ignorant on coding errors.
  implicit private val defaultCodec = Codec("UTF-8")
  defaultCodec.onMalformedInput(CodingErrorAction.REPLACE)
  defaultCodec.onUnmappableCharacter(CodingErrorAction.REPLACE)

  def dummify[T](s: Seq[T]): Map[T, Seq[Boolean]] =
    s.distinct.drop(1).map { v =>
      v -> s.map(_ == v)
    } toMap

  def dummifyAll[T](s: Seq[T]): Map[T, Seq[Boolean]] =
    s.distinct.map { v =>
      v -> s.map(_ == v)
    } toMap

}
