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

package mybiotools.tasks

import akka.actor.{ Actor, PoisonPill, ActorRef, Props, ActorRefFactory }
import akka.actor.Actor._
import akka.pattern.ask
import akka.pattern.pipe
import scala.concurrent.{ Future, Await, ExecutionContext }
import java.lang.Class
import java.io.{ File, InputStream, FileInputStream, BufferedInputStream }
import scala.concurrent.duration._
import TaskSystemTimeouts._
import java.util.concurrent.{ TimeUnit, ScheduledFuture }
import java.nio.channels.{ WritableByteChannel, ReadableByteChannel }
import mybiotools.tasks.util._
import mybiotools.TempFile
import scala.util.{ Try, Failure, Success }
import com.google.common.hash._
import mybiotools.eq._
import scala.concurrent._
import scala.util._

sealed trait FileServiceMessage

@SerialVersionUID(1L)
case class GetListOfFilesInStorage(regexp: String) extends FileServiceMessage

@SerialVersionUID(1L)
case class NewFile(f: File, name: String, moveaway: Boolean, prefix: FileServicePrefix) extends FileServiceMessage

@SerialVersionUID(1L)
case class GetPaths(sf: SharedFile) extends FileServiceMessage

@SerialVersionUID(1L)
case class KnownPaths(paths: List[File]) extends FileServiceMessage

@SerialVersionUID(1L)
case class KnownPathsWithStorage(paths: List[File], storage: FileStorage) extends FileServiceMessage

@SerialVersionUID(1L)
case class TransferToMe(actor: ActorRef) extends FileServiceMessage

@SerialVersionUID(1L)
case class TransferFileToUser(actor: ActorRef, sf: SharedFile)

@SerialVersionUID(1L)
case class NewPath(name: SharedFile, path: File) extends FileServiceMessage

@SerialVersionUID(1L)
case object WaitingForSharedFile extends FileServiceMessage

@SerialVersionUID(1L)
case object WaitingForPath extends FileServiceMessage

@SerialVersionUID(1L)
case class FileNotFound(e: Throwable) extends FileServiceMessage

@SerialVersionUID(1L)
case class TryToDownload(storage: FileStorage) extends FileServiceMessage

@SerialVersionUID(1L)
case class TryToUpload(storage: FileStorage) extends FileServiceMessage

@SerialVersionUID(1L)
case class Uploaded(file: File, name: String, prefix: FileServicePrefix) extends FileServiceMessage

@SerialVersionUID(1L)
case class CouldNotUpload(name: String, prefix: FileServicePrefix) extends FileServiceMessage

@SerialVersionUID(1L)
case class IsInStorageAnswer(value: Boolean) extends FileServiceMessage

@SerialVersionUID(1L)
case class ErrorWhileAccessingStore(e: Throwable) extends FileServiceMessage

@SerialVersionUID(1L)
case class FileServiceActor(actor: ActorRef)

@SerialVersionUID(1L)
case class FileServicePrefix(list: Vector[String]) {
  def append(n: String) = FileServicePrefix(list :+ n)
}

class SharedFile private (
    val prefix: FileServicePrefix,
    val name: String,
    private val byteSize: Long,
    private val hash: Int
) extends Serializable with KryoSerializable {
  def canEqual(other: Any): Boolean = other.isInstanceOf[SharedFile]

  override def hashCode: Int = 41 * (41 * (41 * (name.hashCode + 41) + byteSize.hashCode) + hash.hashCode) + prefix.hashCode

  override def equals(that: Any): Boolean = that match {
    case t: SharedFile => t.canEqual(this) && t.name === this.name && t.prefix === this.prefix && t.byteSize === this.byteSize && t.hash === this.hash
    case _ => false
  }

  override def toString = s"SharedFile(${prefix.list.mkString(" / ")}/$name)"

  def file(implicit service: FileServiceActor, context: ActorRefFactory) = SharedFile.getPathToFile(this, false)

  def localFile(implicit service: FileServiceActor, context: ActorRefFactory) = SharedFile.getPathToFile(this, true)

  def openStream[R](f: InputStream => R)(implicit service: FileServiceActor, context: ActorRefFactory) = SharedFile.openStreamToFile(this)(f)

  def isAccessible(implicit service: FileServiceActor, context: ActorRefFactory) = {
    val f: File = this.file
    val fl: Long = f.length
    if (this.byteSize != fl) {
      // println("ERROR: " + name + " " + fl + " " + this.byteSize)
    }
    val r = f.canRead && (TaskSystemConstants.skipContentHashVerificationAfterCache || ((this.byteSize === fl) && this.hash === SharedFile.getContentHash(f, fl)))
    if (!r) {
      println(s"Failed isAccessible of $this . Found file: $f with size $fl. Can read: ${f.canRead}. Expected size: ${this.byteSize}, which is correct: ${this.byteSize === fl}. Size and content was checked: ${TaskSystemConstants.skipContentHashVerificationAfterCache}.")
    }
    r
  }
}

object SharedFile {

  def getByNameUnchecked(prefix: FileServicePrefix, name: String)(implicit service: FileServiceActor, context: ActorRefFactory): Option[SharedFile] = {
    Try(SharedFile.getPathToFile(new SharedFile(prefix, name, 0, 0), false)).toOption.map { f =>
      SharedFile.create(prefix, name, f)
    }

  }

  private def fileIsRelativeToNonLocalFileSystem(f: File): Boolean = {
    val nonLocalFileSystemsCanonical = TaskSystemConstants.nonLocalFileSystems.map(_.getCanonicalPath)

    val canonical = f.getCanonicalFile

    def getParents(f: File, p: List[File]): List[File] =
      if (f == null) p
      else getParents(f.getParentFile, f :: p)

    val canonicalParents = getParents(canonical, Nil).map(_.getCanonicalPath)

    (nonLocalFileSystemsCanonical)
      .exists(path => canonicalParents.contains(path))

  }

  private val isLocalStrict = (f: File) => f.canRead && !fileIsRelativeToNonLocalFileSystem(f)
  private val isLocal = (f: File) => f.canRead

  private[tasks] def createForTesting(name: String) = new SharedFile(FileServicePrefix(Vector()), name, 0, 0)
  private[tasks] def createForTesting(name: String, size: Long, hash: Int) = new SharedFile(FileServicePrefix(Vector()), name, size, hash)

  private def getContentHash(file: File, size: Long): Int = {
    val checkedSize = 1024 * 256
    val buffer = Array.fill[Byte](checkedSize)(0)
    mybiotools.openFileInputStream(file) { is =>
      mybiotools.useResource(new HashingInputStream(Hashing.crc32c, is)) { his =>
        if (size > 2 * checkedSize) {
          his.read(buffer, 0, buffer.size)
          var skipped = 0L
          while (skipped < size - checkedSize * 2) {
            val n = his.skip(size - checkedSize * 2 - skipped)
            if (n >= 0) {
              skipped += n
            }
          }
          his.read(buffer, 0, buffer.size)
        } else {
          his.read(buffer, 0, math.min(buffer.size, size.toInt))
        }
        his.hash.asInt
      }
    }
  }

  private[tasks] def create(prefix: FileServicePrefix, name: String, file: File): SharedFile = {
    var size: Long = file.length
    var hash: Int = getContentHash(file, size)
    if (size != file.length) {
      println("File's size changed during calculating hash. Recalculate the hash once. " + name + " " + file + " " + size + " " + file.length)
      size = file.length
      hash = getContentHash(file, size)
      if (size != file.length) {
        println("File's size changed during calculating hash. " + name + " " + file + " " + size + " " + file.length)
      }
    }
    new SharedFile(prefix, name, size, hash)
  }

  def getStreamToFile(sf: SharedFile)(implicit service: FileServiceActor, context: ActorRefFactory): InputStream = {

    val serviceactor = service.actor
    implicit val timout = akka.util.Timeout(1441 minutes)
    val ac = context.actorOf(Props(new FileUserStream(sf, serviceactor, isLocal)).withDispatcher("fileuser-dispatcher"))

    val f = Await.result((ac ? WaitingForPath).asInstanceOf[Future[Try[InputStream]]], atMost = 1440 minutes)
    ac ! PoisonPill
    f match {
      case Success(r) => r
      case Failure(e) => throw new RuntimeException("getStreamToFile failed. " + sf, e)
    }
  }

  def openStreamToFile[R](sf: SharedFile)(fun: InputStream => R)(implicit service: FileServiceActor, context: ActorRefFactory): R = mybiotools.useResource(getStreamToFile(sf))(fun)

  def getPathToFile(sf: SharedFile, strict: Boolean)(implicit service: FileServiceActor, context: ActorRefFactory): File = {

    val serviceactor = service.actor
    implicit val timout = akka.util.Timeout(1441 minutes)
    val ac = context.actorOf(Props(new FileUser(sf, serviceactor, if (strict) isLocalStrict else isLocal)).withDispatcher("fileuser-dispatcher"))

    val f = Await.result((ac ? WaitingForPath).asInstanceOf[Future[Try[File]]], atMost = 1440 minutes)
    ac ! PoisonPill
    f match {
      case Success(r) => r
      case Failure(e) => throw new RuntimeException("getPathToFile failed. " + sf, e)
    }
  }

  def apply(file: File, name: String, canMoveAway: Boolean)(implicit service: FileServiceActor, context: ActorRefFactory, prefix: FileServicePrefix): SharedFile = {

    val serviceactor = service.actor
    if (!file.canRead) {
      throw new java.io.FileNotFoundException("not found" + file)
    }

    implicit val timout = akka.util.Timeout(1441 minutes)

    val ac = context.actorOf(Props(new FileSender(file, name, prefix, serviceactor, canMoveAway)).withDispatcher("filesender-dispatcher"))
    val f = Await.result((ac ? WaitingForSharedFile).asInstanceOf[Future[Option[SharedFile]]], atMost = 1440 minutes).get
    ac ! PoisonPill
    f
  }

  def apply(file: File, name: String)(implicit service: FileServiceActor, context: ActorRefFactory, prefix: FileServicePrefix): SharedFile = apply(file, name, false)

  def apply(file: File)(implicit service: FileServiceActor, context: ActorRefFactory, prefix: FileServicePrefix): SharedFile = apply(file, if (TaskSystemConstants.includeFullPathInDefaultSharedName) file.getAbsolutePath.replace(File.separator, "_") else file.getName, false)

  def apply(file: File, canMoveAway: Boolean)(implicit service: FileServiceActor, context: ActorRefFactory, prefix: FileServicePrefix): SharedFile = apply(file, if (TaskSystemConstants.includeFullPathInDefaultSharedName) file.getAbsolutePath.replace(File.separator, "_") else file.getName, canMoveAway)

}

class FileUserStream(sharedFile: SharedFile, service: ActorRef, isLocal: java.io.File => Boolean) extends AbstractFileUser[InputStream](sharedFile, service, isLocal) {

  private var writeableChannel: Option[WritableByteChannel] = None

  def transfertome {
    log.debug("Unreadable")
    val pipe = java.nio.channels.Pipe.open
    writeableChannel = Some(pipe.sink)
    val transferinActor = context.actorOf(Props(new TransferIn(writeableChannel.get, self)).withDispatcher("transferin"))

    service ! TransferFileToUser(transferinActor, sharedFile)

    result = Some(Success(java.nio.channels.Channels.newInputStream(pipe.source)))
    finish
  }

  def finish {
    if (listener.isDefined) {
      listener.get ! result.get
      self ! PoisonPill
    }
  }

  def finishLocalFile(f: File) {
    log.debug("Readable")
    result = Some(Success(new BufferedInputStream(new FileInputStream(f))))
    finish
  }

  def handleCentralStorage(storage: FileStorage) {
    val stream = storage.openStream(sharedFile.name, sharedFile.prefix)
    if (stream.isSuccess) {
      result = Some(Success(stream.get))
      finish
    } else {
      log.debug(s"storage.openStream, (KnownPathsWithStorage($storage)): ${stream.toString}, $sharedFile")
      transfertome
    }
  }

  override def receive = super.receive orElse {
    case FileSaved => {
      writeableChannel.get.close
    }
  }
}

class FileUser(sharedFile: SharedFile, service: ActorRef, isLocal: java.io.File => Boolean) extends AbstractFileUser[File](sharedFile, service, isLocal) {

  private var fileUnderTransfer: Option[File] = None
  private var writeableChannel: Option[WritableByteChannel] = None

  def transfertome {
    log.debug("Unreadable")
    val fileToSave = mybiotools.TempFile.createFileInTempFolderIfPossibleWithName(sharedFile.name)
    fileUnderTransfer = Some(fileToSave)
    writeableChannel = Some(new java.io.FileOutputStream(fileToSave).getChannel)
    val transferinActor = context.actorOf(Props(new TransferIn(writeableChannel.get, self)).withDispatcher("transferin"))

    service ! TransferFileToUser(transferinActor, sharedFile)
  }

  def finishLocalFile(f: File) {
    log.debug("Readable")
    result = Some(Success(f))
    if (listener.isDefined) {
      listener.get ! result.get
      self ! PoisonPill
    }
  }

  def handleCentralStorage(storage: FileStorage) {
    val f = storage.exportFile(sharedFile.name, sharedFile.prefix)
    if (f.isSuccess && isLocal(f.get)) {
      service ! NewPath(sharedFile, f.get)
      finishLocalFile(f.get)
    } else {
      log.debug(s"storage.export, (KnownPathsWithStorage($storage)): ${f.toString}, $sharedFile")
      transfertome
    }
  }

  override def receive = super.receive orElse {
    case FileSaved => {
      writeableChannel.get.close
      service ! NewPath(sharedFile, fileUnderTransfer.get)
      finishLocalFile(fileUnderTransfer.get)
    }
  }
}

abstract class AbstractFileUser[R](sharedFile: SharedFile, service: ActorRef, isLocal: File => Boolean) extends Actor with akka.actor.ActorLogging {

  var listener: Option[ActorRef] = None
  var result: Option[Try[R]] = None
  var fileNotFound = false

  override def preStart {
    service ! GetPaths(sharedFile)
  }

  protected def transfertome: Unit
  protected def finishLocalFile(file: File): Unit
  protected def handleCentralStorage(storage: FileStorage): Unit

  private def fail(e: Throwable) {
    fileNotFound = true
    if (listener.isDefined) {
      listener.get ! Failure(e)
      self ! PoisonPill
    }
  }

  def receive = {
    case WaitingForPath => {
      listener = Some(sender)
      log.debug("listener:" + listener)
      if (result.isDefined || fileNotFound) {
        sender ! result.get
        self ! PoisonPill
      }
    }
    case FileNotFound(e) => {
      log.warning("NotFound : " + sharedFile + ". Reason: " + e.toString)
      fail(e)
    }
    case CannotSaveFile(e) => {
      log.error("CannotSaveFile : " + sharedFile + " Reason: " + e)
      fail(e)
    }
    case KnownPathsWithStorage(list, storage) => {
      log.debug("KnownPathsWithStorage")
      list.find(isLocal) match {
        case Some(file) => finishLocalFile(file)
        case None => {
          handleCentralStorage(storage)
        }
      }
    }
    case KnownPaths(list) => {
      log.debug("KnownPaths:" + list)
      list.find(isLocal) match {
        case Some(file) => finishLocalFile(file)
        case None => transfertome
      }
    }
    case TryToDownload(storage) => {
      log.debug("trytodownload")
      handleCentralStorage(storage)
    }

  }

}

class FileSender(file: File, name: String, prefix: FileServicePrefix, service: ActorRef, moveaway: Boolean) extends Actor with akka.actor.ActorLogging {

  override def preStart {
    service ! NewFile(file, name, moveaway, prefix)
  }

  var sharedFile: Option[SharedFile] = None
  var error = false
  var listener: Option[ActorRef] = None

  def receive = {
    case t: SharedFile => {
      sharedFile = Some(t)
      if (moveaway && file.canRead) {
        file.delete
      }
      if (listener.isDefined) {
        listener.get ! sharedFile
        self ! PoisonPill
      }
    }
    case TransferToMe(transferin) => {
      val readablechannel = new java.io.FileInputStream(file).getChannel
      val chunksize = mybiotools.tasks.TaskSystemConstants.fileSendChunkSize
      context.actorOf(Props(new TransferOut(readablechannel, transferin, chunksize)).withDispatcher("transferout"))
    }
    case TryToUpload(storage) => {
      log.debug("trytoupload")
      val f = storage.importFile(name, file, moveaway, prefix)
      if (f.isSuccess) {
        log.debug("uploaded")
        service ! Uploaded(f.get, name, prefix)
      } else {
        log.debug("could not upload. send could not upload and transfer")
        service ! CouldNotUpload(name, prefix)
      }

    }
    case WaitingForSharedFile => {
      listener = Some(sender)
      if (sharedFile.isDefined) {
        sender ! sharedFile
        self ! PoisonPill
      } else if (error) {
        sender ! None
        self ! PoisonPill
      }

    }
    case ErrorWhileAccessingStore(e) => {
      error = true
      log.error("ErrorWhileAccessingStore: " + e)
      listener.foreach { x =>
        x ! None
        self ! PoisonPill
      }
    }

  }

}

trait FileStorage extends Serializable {

  def contains(n: String, prefix: FileServicePrefix): Boolean

  def importFile(n: String, f: File, move: Boolean, prefix: FileServicePrefix): Try[File]

  def exportFile(n: String, prefix: FileServicePrefix): Try[File]

  def openStream(n: String, prefix: FileServicePrefix): Try[InputStream]

  def centralized: Boolean

  def list(regexp: String): List[SharedFile]

}

class FolderFileStorage(val basePath: File, val centralized: Boolean, val extendedPaths: List[File] = Nil) extends FileStorage {

  if (basePath.exists && !basePath.isDirectory) throw new IllegalArgumentException(s"$basePath exists and not a folder")
  else if (!basePath.exists) basePath.mkdirs

  if (!basePath.isDirectory) throw new RuntimeException(s"Could not create $basePath")

  override def toString = s"FolderFileStorage(basePath=$basePath, centralized=$centralized, extendedPaths=$extendedPaths)"

  private val canonicalExtendedPaths = extendedPaths.map(_.getCanonicalPath)
  private val canonicalBasePath = basePath.getCanonicalPath

  private def fileIsRelativeToBaseOrExtended(f: File): Boolean = {
    val canonical = f.getCanonicalFile

    def getParents(f: File, p: List[File]): List[File] =
      if (f == null) p
      else getParents(f.getParentFile, f :: p)

    val canonicalParents = getParents(canonical, Nil).map(_.getCanonicalPath)

    (canonicalBasePath :: canonicalExtendedPaths)
      .exists(path => canonicalParents.contains(path))

  }

  def contains(name: String, prefix: FileServicePrefix): Boolean = {
    assemblePath(basePath, prefix, name).canRead
  }

  def openStream(n: String, prefix: FileServicePrefix): Try[InputStream] =
    if (contains(n, prefix))
      Success(new FileInputStream(assemblePath(basePath, prefix, n)))
    else Failure(new RuntimeException("Storage export: can't read: " + assemblePath(basePath, prefix, n)))

  def exportFile(n: String, prefix: FileServicePrefix): Try[File] =
    if (assemblePath(basePath, prefix, n).canRead)
      Success(assemblePath(basePath, prefix, n))
    else Failure(new RuntimeException("Storage export: can't read: " + assemblePath(basePath, prefix, n)))

  private def moveOrCopyFile(name: String, prefix: FileServicePrefix, file: File, move: Boolean) = {
    val parentFolder = assemblePath(basePath, prefix, name).getParentFile
    parentFolder.mkdirs
    val tmp = new File(parentFolder, name + ".tmp")
    if (move) {
      com.google.common.io.Files.move(file, tmp)
    } else {
      com.google.common.io.Files.copy(file, tmp)
    }
    val dest = assemblePath(basePath, prefix, name)
    dest.delete
    val succ = tmp.renameTo(dest)
    if (succ) {
      tmp.delete
      dest
    } else throw new RuntimeException("can't rename file" + dest)
  }

  private def assemblePath(base: File, prefix: FileServicePrefix, name: String): File = {
    new File(base.getAbsolutePath + File.separator + prefix.list.mkString(File.separator) + File.separator + name)
  }

  def importFile(name: String, file: File, move: Boolean, prefix: FileServicePrefix): Try[File] = Try({
    if (fileIsRelativeToBaseOrExtended(file)) file
    else if ((assemblePath(basePath, prefix, name)).canRead) {
      val finalFile = assemblePath(basePath, prefix, name)
      if (com.google.common.io.Files.equal(finalFile, file)) finalFile
      else {

        def candidates(i: Int, past: List[File]): List[File] = {
          val candidate = assemblePath(basePath, prefix, name + ".old." + i)
          if (candidate.canRead) candidates(i + 1, candidate :: past)
          else past
        }

        candidates(0, Nil)
          .map(f => (f.getName.drop(name.size).drop(5).toInt + 1, f))
          .sortBy(_._1)
          .reverse
          .foreach {
            case (newversion, f) =>
              com.google.common.io.Files.move(f, assemblePath(basePath, prefix, name + ".old." + newversion))
          }

        com.google.common.io.Files.move(finalFile, assemblePath(basePath, prefix, name + ".old.0"))

        moveOrCopyFile(name, prefix, file, move)
      }

    } else {
      moveOrCopyFile(name, prefix, file, move)
    }
  })

  def list(pattern: String): List[SharedFile] = {
    import scala.collection.JavaConversions._
    val stream = java.nio.file.Files.newDirectoryStream(basePath.toPath, pattern)
    try {
      stream.toList.filter(_.toFile.isFile).map(x => SharedFile.createForTesting(x.toFile.getName))
    } catch {
      case x: Throwable => throw x
    } finally {
      stream.close
    }

  }

}

class FileService(storage: FileStorage, threadpoolsize: Int = 8, isLocal: File => Boolean = _.canRead) extends Actor with akka.actor.ActorLogging {

  val fjp = mybiotools.concurrent.newJavaForkJoinPoolWithNamePrefix("fileservice-recordtonames", threadpoolsize)
  val ec = ExecutionContext.fromExecutorService(fjp)

  import context.dispatcher

  override def postStop {
    fjp.shutdown
    log.info("FileService stopped.")
  }

  override def preStart {
    log.info("FileService will start.")
  }

  private val knownPaths = collection.mutable.AnyRefMap[(String, FileServicePrefix), List[File]]()

  // transferinactor -> (name,channel,fileinbase,filesender)
  private val transferinactors = collection.mutable.Map[ActorRef, (String, WritableByteChannel, File, ActorRef, Boolean, FileServicePrefix)]()

  private def create(file: File, name: String, prefix: FileServicePrefix): Future[SharedFile] = {
    Future {
      ((SharedFile.create(prefix, name, file)))
    }(ec)
  }

  private def recordToNames(file: File, name: String, prefix: FileServicePrefix): Unit = {
    if (knownPaths.contains((name, prefix))) {
      val oldlist = knownPaths(name -> prefix)
      knownPaths.update((name, prefix), (file.getCanonicalFile :: oldlist).distinct)
    } else {
      knownPaths.update((name, prefix), List(file))
    }

  }

  private def recordRemotePathToNames(file: File, name: String, prefix: FileServicePrefix): Unit = {
    if (knownPaths.contains(name -> prefix)) {
      val oldlist = knownPaths(name -> prefix)
      knownPaths.update((name, prefix), (file.getCanonicalFile :: oldlist).distinct)
    } else {
      knownPaths.update((name, prefix), List(file))
    }
  }

  def receive = {
    case NewFile(file, name, moveaway, prefix) => try {
      if (isLocal(file)) {

        val f = storage.importFile(name, file, moveaway, prefix).get
        recordToNames(f, name, prefix)
        val sn = create(f, name, prefix)
        if (!moveaway) {
          recordToNames(file, name, prefix)
        }
        sn.pipeTo(sender)

      } else {

        if (storage.centralized) {
          log.debug("answer trytoupload")
          sender ! TryToUpload(storage)

        } else {
          // transfer
          val savePath = TempFile.createFileInTempFolderIfPossibleWithName(name)
          val writeableChannel = new java.io.FileOutputStream(savePath).getChannel
          val transferinActor = context.actorOf(Props(new TransferIn(writeableChannel, self)).withDispatcher("transferin"))
          transferinactors.update(transferinActor, (name, writeableChannel, savePath, sender, moveaway, prefix))

          sender ! TransferToMe(transferinActor)
        }

      }
    } catch {
      case e: Exception => {
        log.error(e, "Error while accessing storage " + file + " " + name + " " + moveaway)
        sender ! ErrorWhileAccessingStore
      }
    }
    case Uploaded(file, name, prefix) => {
      log.debug("got uploaded. record")
      recordToNames(file, name, prefix)
      create(file, name, prefix) pipeTo sender
    }
    case CannotSaveFile(e) => {
      transferinactors.get(sender).foreach {
        case (name, channel, file, filesender, moveaway, prefix) =>
          channel.close
          log.error("CannotSaveFile(" + e + ")")
          filesender ! ErrorWhileAccessingStore(e)
      }
      transferinactors.remove(sender)
    }
    case FileSaved => {
      transferinactors.get(sender).foreach {
        case (name, channel, file, filesender, moveaway, prefix) =>
          channel.close
          try {
            val f = storage.importFile(name, file, moveaway, prefix).get
            recordToNames(f, name, prefix)
            create(f, name, prefix) pipeTo filesender

          } catch {
            case e: Exception => { log.error(e, "Error while accessing storage"); filesender ! ErrorWhileAccessingStore }
          }
      }
      transferinactors.remove(sender)
    }
    case CouldNotUpload(name, prefix) => {
      val savePath = TempFile.createFileInTempFolderIfPossibleWithName(name)
      val writeableChannel = new java.io.FileOutputStream(savePath).getChannel
      val transferinActor = context.actorOf(Props(new TransferIn(writeableChannel, self)).withDispatcher("transferin"))
      transferinactors.update(transferinActor, (name, writeableChannel, savePath, sender, true, prefix))

      sender ! TransferToMe(transferinActor)

    }
    case GetPaths(sharedfile) => try {
      knownPaths.get(sharedfile.name -> sharedfile.prefix) match {
        case Some(l) => {
          if (storage.centralized) {
            sender ! KnownPathsWithStorage(l, storage)
          } else {
            sender ! KnownPaths(l)
          }

        }
        case None => {
          if (storage.contains(sharedfile.name, sharedfile.prefix)) {
            if (storage.centralized) {
              sender ! TryToDownload(storage)
            } else {
              val f = storage.exportFile(sharedfile.name, sharedfile.prefix).get
              recordToNames(f, sharedfile.name, sharedfile.prefix)
              sender ! KnownPaths(List(f))
            }
          } else {
            sender ! FileNotFound(new RuntimeException(s"SharedFile not found in storage. $storage # contains(${sharedfile.name}) returned false. " + sharedfile))
          }
        }
      }
    } catch {
      case e: Exception => {
        log.error(e.toString)
        sender ! FileNotFound(e)
      }
    }
    case TransferFileToUser(transferinActor, sharedFile) => try {
      val file = knownPaths(sharedFile.name -> sharedFile.prefix).find(isLocal) match {
        case Some(x) => x
        case None => storage.exportFile(sharedFile.name, sharedFile.prefix).get
      }
      val readablechannel = new java.io.FileInputStream(file).getChannel
      val chunksize = mybiotools.tasks.TaskSystemConstants.fileSendChunkSize
      context.actorOf(Props(new TransferOut(readablechannel, transferinActor, chunksize)).withDispatcher("transferout"))

    } catch {
      case e: Exception => {
        log.error(e.toString)
        sender ! FileNotFound(e)
      }
    }
    case NewPath(sharedFile, path) => {

      recordRemotePathToNames(path, sharedFile.name, sharedFile.prefix)

    }
    case GetListOfFilesInStorage(regexp) => sender ! storage.list(regexp)
  }

}