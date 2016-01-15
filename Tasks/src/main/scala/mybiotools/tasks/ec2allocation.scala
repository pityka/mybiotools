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

import akka.actor.{ Actor, PoisonPill, ActorRef, Props, Cancellable }
import scala.concurrent.duration._
import java.util.concurrent.{ TimeUnit, ScheduledFuture }
import TaskSystemTimeouts._
import TaskAllocationConstants._
import mybiotools._
import java.net.InetSocketAddress
import akka.actor.Actor._
import akka.event.LoggingAdapter
import mybiotools.config.Config.configInstance
import scala.util._

import com.amazonaws.services.ec2.AmazonEC2;
import com.amazonaws.services.ec2.AmazonEC2Client;

import com.amazonaws.services.ec2.model.CancelSpotInstanceRequestsRequest;
import com.amazonaws.services.ec2.model.CreateTagsRequest;
import com.amazonaws.services.ec2.model.DescribeSpotInstanceRequestsRequest;
import com.amazonaws.services.ec2.model.DescribeSpotInstanceRequestsResult;
import com.amazonaws.services.ec2.model.LaunchSpecification;
import com.amazonaws.services.ec2.model.RequestSpotInstancesRequest;
import com.amazonaws.services.ec2.model.RequestSpotInstancesResult;
import com.amazonaws.services.ec2.model.SpotInstanceRequest;
import com.amazonaws.services.ec2.model.Tag;
import com.amazonaws.services.ec2.model.TerminateInstancesRequest;
import com.amazonaws.auth.InstanceProfileCredentialsProvider
import com.amazonaws.services.ec2.model.IamInstanceProfileSpecification
import com.amazonaws.services.ec2.model.SpotInstanceType
import com.amazonaws.services.ec2.model.BlockDeviceMapping
import com.amazonaws.services.ec2.model.CancelSpotInstanceRequestsRequest
import com.amazonaws.services.s3.AmazonS3Client;
import com.amazonaws.services.s3.model.CannedAccessControlList
import com.amazonaws.services.s3.transfer.TransferManager
import com.amazonaws.services.s3.model.PutObjectRequest
import com.amazonaws.services.s3.model.StorageClass
import com.amazonaws.services.ec2.model.SpotPlacement

import collection.JavaConversions._
import java.io.{ File, InputStream }

object EC2Settings {

  val endpoint: String = configInstance.getString("tasks.elastic.aws.endpoint")

  // in $
  val spotPrice: Double = configInstance.getDouble("tasks.elastic.aws.spotPrice")

  val amiID: String = configInstance.getString("tasks.elastic.aws.ami")

  val instanceType: String = configInstance.getString("tasks.elastic.aws.instanceType")

  val securityGroup: String = configInstance.getString("tasks.elastic.aws.securityGroup")

  val jarBucket: String = configInstance.getString("tasks.elastic.aws.jarBucket")

  val jarObject: String = configInstance.getString("tasks.elastic.aws.jarObject")

  val keyName = configInstance.getString("tasks.elastic.aws.keyName")

  val extraFilesFromS3: List[String] = configInstance.getStringList("tasks.elastic.aws.extraFilesFromS3").toList

  val extraStartupscript: String = configInstance.getString("tasks.elastic.aws.extraStartupScript")

  val additionalJavaCommandline = configInstance.getString("tasks.elastic.aws.extraJavaCommandline")

  val iamRole = configInstance.getString("tasks.elastic.aws.iamRole")

  val S3UpdateInterval = configInstance.getInt("tasks.elastic.aws.uploadInterval")

  val placementGroup: Option[String] = configInstance.getString("tasks.elastic.aws.placementGroup") match {
    case x if x == "" => None
    case x => Some(x)
  }

  val jvmMaxHeapFactor = configInstance.getDouble("tasks.elastic.aws.jvmMaxHeapFactor")

}

object EC2Helpers {

  val instanceTypeCPUMapping = Map(
    "m1.small" -> 0, // prevent running workers on these small instances
    "m1.medium" -> 0, // prevent running workers on these small instances
    "m1.large" -> 2,
    "m1.xlarge" -> 4,
    "m3.xlarge" -> 4,
    "m3.2xlarge" -> 8,
    "c1.medium" -> 2,
    "c1.xlarge" -> 8,
    "cc1.4xlarge" -> 8,
    "cc2.8xlarge" -> 16,
    "m2.xlarge" -> 2,
    "m2.2xlarge" -> 4,
    "m2.4xlarge" -> 8,
    "cr1.8xlarge" -> 16,
    "hi1.4xlarge" -> 16,
    "hs1.8xlarge" -> 16,
    "t1.micro" -> 1,
    "cg1.4xlarge" -> 1
  )

  val instanceTypeMemoryMapping = Map(
    "m1.small" -> 1700,
    "m1.medium" -> 3750,
    "m1.large" -> 7500,
    "m1.xlarge" -> 15000,
    "m3.xlarge" -> 15000,
    "m3.2xlarge" -> 30000,
    "c1.medium" -> 1700,
    "c1.xlarge" -> 7000,
    "cc1.4xlarge" -> 23000,
    "cc2.8xlarge" -> 60500,
    "m2.xlarge" -> 17100,
    "m2.2xlarge" -> 34200,
    "m2.4xlarge" -> 68400,
    "cr1.8xlarge" -> 244000,
    "hi1.4xlarge" -> 60500,
    "hs1.8xlarge" -> 117000,
    "t1.micro" -> 615,
    "cg1.4xlarge" -> 22500
  )
}

object EC2Operations {

  def terminateInstance(ec2: AmazonEC2Client, instanceId: String) {
    retry(5) {
      val terminateRequest = new TerminateInstancesRequest(List(instanceId));
      ec2.terminateInstances(terminateRequest);
    }
  }

  def S3contains(bucketName: String, name: String): Boolean = {
    val s3Client = new AmazonS3Client(new InstanceProfileCredentialsProvider());

    catchToLeft(s3Client.getObjectMetadata(bucketName, name)) match {
      case Right(_) => true
      // case Left(e) if  e.getErrorCode == "NoSuchKey" => false
      case _ => false // throw e
    }
  }

  def downloadFile(bucketName: String, name: String): File = {
    retry(5) {
      val s3Client = new AmazonS3Client(new InstanceProfileCredentialsProvider());
      val tm = new TransferManager(s3Client);
      val file = mybiotools.TempFile.createFileInTempFolderIfPossibleWithName(name)
      val download = tm.download(bucketName, name, file)
      download.waitForCompletion
      file
    }.get
  }

  def readMetadata(key: String): List[String] =
    scala.io.Source.fromURL("http://169.254.169.254/latest/meta-data/" + key).getLines.toList

}

trait EC2Shutdown extends ShutdownNode {

  def log: LoggingAdapter

  val ec2: AmazonEC2Client

  def shutdownRunningNode(nodeName: RunningJobId): Unit =
    EC2Operations.terminateInstance(ec2, nodeName.value)

  def shutdownPendingNode(nodeName: PendingJobId): Unit = {
    val request = new CancelSpotInstanceRequestsRequest(List(nodeName.value))
    ec2.cancelSpotInstanceRequests(request)
  }

}

trait EC2NodeRegistryImp extends Actor with GridJobRegistry {

  var counter = 0

  private def gzipBase64(str: String): String = {

    val out = new java.io.ByteArrayOutputStream();
    val gzip = new java.util.zip.GZIPOutputStream(out);
    gzip.write(str.getBytes());
    gzip.close();
    val bytes = out.toByteArray
    javax.xml.bind.DatatypeConverter.printBase64Binary(bytes)
  }

  val masterAddress: InetSocketAddress

  val ec2: AmazonEC2Client

  override def refreshPendingList: List[PendingJobId] = {
    val describeResult = ec2.describeSpotInstanceRequests();
    val spotInstanceRequests = describeResult.getSpotInstanceRequests();
    spotInstanceRequests.map(x => PendingJobId(x.getSpotInstanceRequestId)).toList
  }

  override def convertRunningToPending(p: RunningJobId): Option[PendingJobId] = {
    val describeResult = ec2.describeSpotInstanceRequests();
    val spotInstanceRequests = describeResult.getSpotInstanceRequests();

    spotInstanceRequests.filter(_.getInstanceId == p.value).headOption.map { x =>
      PendingJobId(x.getSpotInstanceRequestId)
    }

  }

  private def requestSpotInstance(size: CPUMemoryRequest): String = {
    // size is ignored, instance specification is set in configuration

    // Initializes a Spot Instance Request
    val requestRequest = new RequestSpotInstancesRequest();

    if (EC2Settings.spotPrice > 2.4) throw new RuntimeException("Spotprice too high:" + EC2Settings.spotPrice)

    requestRequest.setSpotPrice(EC2Settings.spotPrice.toString);
    requestRequest.setInstanceCount(1);
    requestRequest.setType(SpotInstanceType.OneTime)

    val launchSpecification = new LaunchSpecification();
    launchSpecification.setImageId(EC2Settings.amiID);
    launchSpecification.setInstanceType(EC2Settings.instanceType);
    launchSpecification.setKeyName(EC2Settings.keyName)

    val blockDeviceMappingSDB = new BlockDeviceMapping();
    blockDeviceMappingSDB.setDeviceName("/dev/sdb");
    blockDeviceMappingSDB.setVirtualName("ephemeral0");
    val blockDeviceMappingSDC = new BlockDeviceMapping();
    blockDeviceMappingSDC.setDeviceName("/dev/sdc");
    blockDeviceMappingSDC.setVirtualName("ephemeral1");

    launchSpecification.setBlockDeviceMappings(List(blockDeviceMappingSDB, blockDeviceMappingSDC));

    val iamprofile = new IamInstanceProfileSpecification()
    iamprofile.setName(EC2Settings.iamRole)
    launchSpecification.setIamInstanceProfile(iamprofile)

    EC2Settings.placementGroup.foreach { string =>
      val placement = new SpotPlacement();
      placement.setGroupName(string);
      launchSpecification.setPlacement(placement);
    }

    val extraFiles = EC2Settings.extraFilesFromS3.grouped(3).map { l =>
      s"python getFile.py ${l(0)} ${l(1)} ${l(2)} && chmod u+x ${l(2)} "
    }.mkString("\n")

    val javacommand = s"nohup java -Xmx${(EC2Helpers.instanceTypeMemoryMapping(EC2Settings.instanceType) * EC2Settings.jvmMaxHeapFactor).toInt}M -Dhosts.gridengine=EC2 ${EC2Settings.additionalJavaCommandline} -Dconfig.file=rendered.conf -Dhosts.master=${masterAddress.getHostName + ":" + masterAddress.getPort} -jar pipeline.jar > pipelinestdout &"

    val userdata =
      "#!/bin/bash" ::
        """
# Download myFile
cat << EOF > getFile.py
import boto;import sys;boto.connect_s3().get_bucket(sys.argv[1]).get_key(sys.argv[2]).get_contents_to_filename(sys.argv[3])
EOF

# Set up LVM on all the ephemeral disks
EPHEMERALS=`curl http://169.254.169.254/latest/meta-data/block-device-mapping/ | grep ephemeral`
DEVICES=$(for i in $EPHEMERALS
 do 
 DEVICE=`curl http://169.254.169.254/latest/meta-data/block-device-mapping/$i/` 
 umount /dev/$DEVICE
 echo /dev/$DEVICE 
done)
pvcreate $DEVICES
vgcreate vg $DEVICES
TOTALPE=`vgdisplay vg | grep "Total PE" | awk '{print $3;}'`
lvcreate -l $TOTALPE vg
mkfs -t ext4 /dev/vg/lvol0 
mount /dev/vg/lvol0 /media/ephemeral0/
mkdir -m 1777 /media/ephemeral0/tmp/
mount --bind /media/ephemeral0/tmp/ /tmp
""" ::
        s"python getFile.py ${EC2Settings.jarBucket} ${EC2Settings.jarObject} pipeline.jar" ::
        s"""cat << EOF > rendered.conf
${configInstance.root.render}
EOF""" ::
        extraFiles ::
        EC2Settings.extraStartupscript ::
        """export PATH=./:$PATH""" ::
        javacommand ::
        Nil

    launchSpecification.setUserData(gzipBase64(userdata.mkString("\n")))

    // Add the security group to the request.
    val securitygroups =
      (Set(EC2Settings.securityGroup) & EC2Operations.readMetadata("security-groups").toSet).toList
    launchSpecification.setSecurityGroups(securitygroups)

    // Add the launch specification.
    requestRequest.setLaunchSpecification(launchSpecification)

    // Call the RequestSpotInstance API.
    val requestResult = ec2.requestSpotInstances(requestRequest)

    requestResult.getSpotInstanceRequests.map(_.getSpotInstanceRequestId).head

  }

  def requestOneNewJobFromGridScheduler(request: CPUMemoryRequest): Try[Tuple2[PendingJobId, CPUMemoryAvailable]] = Try {
    val jobid = PendingJobId(requestSpotInstance(request))
    val size = CPUMemoryAvailable(
      cpu = EC2Helpers.instanceTypeCPUMapping(EC2Settings.instanceType),
      memory = EC2Helpers.instanceTypeMemoryMapping(EC2Settings.instanceType)
    )
    (jobid, size)
  }

  def initializeNode(node: Node): Unit = {
    val ac = node.launcherActor //.revive

    val ackil = context.actorOf(Props(new EC2NodeKiller(ac, node)).withDispatcher("my-pinned-dispatcher"), "nodekiller" + node.name.value.replace("://", "___"))

  }

}

class EC2NodeKiller(
    val targetLauncherActor: ActorRef,
    val targetNode: Node
) extends NodeKillerImpl with EC2Shutdown with akka.actor.ActorLogging {
  val ec2 = new AmazonEC2Client(new InstanceProfileCredentialsProvider())
  ec2.setEndpoint(EC2Settings.endpoint)
}

class EC2NodeRegistry(
    val masterAddress: InetSocketAddress,
    val targetQueue: ActorRef,
    override val unmanagedResource: CPUMemoryAvailable
) extends EC2NodeRegistryImp with NodeCreatorImpl with SimpleDecideNewNode with EC2Shutdown with akka.actor.ActorLogging {
  val ec2 = new AmazonEC2Client(new InstanceProfileCredentialsProvider())
  ec2.setEndpoint(EC2Settings.endpoint)
}

trait EC2HostConfiguration extends HostConfiguration {

  private val myPort = {
    val s = new java.net.ServerSocket(0);
    val p = s.getLocalPort()
    s.close
    p
  }

  private val myhostname = EC2Operations.readMetadata("local-hostname").head

  lazy val myAddress = new InetSocketAddress(myhostname, myPort)

  private lazy val instancetype = EC2Operations.readMetadata("instance-type").head

  lazy val availableMemory = EC2Helpers.instanceTypeMemoryMapping(instancetype)

  lazy val myCardinality = EC2Helpers.instanceTypeCPUMapping(instancetype)

}

object EC2MasterSlave extends MasterSlaveConfiguration with EC2HostConfiguration

class S3Storage(bucketName: String) extends FileStorage {

  def list(pattern: String): List[SharedFile] = ???

  def centralized = true

  @transient private var _client = new AmazonS3Client(new InstanceProfileCredentialsProvider());

  @transient private var _tm = new TransferManager(s3Client);

  private def s3Client = {
    if (_client == null) {
      _client = new AmazonS3Client(new InstanceProfileCredentialsProvider());
    }
    _client
  }

  private def tm = {
    if (_tm == null) {
      _tm = new TransferManager(s3Client);
    }
    _tm
  }

  s3Client.setBucketAcl(bucketName, CannedAccessControlList.Private)

  def contains(n: String, prefix: FileServicePrefix): Boolean = {
    catchToLeft(s3Client.getObjectMetadata(bucketName, assembleName(n, prefix))) match {
      case Right(_) => true
      // case Left(e) if  e.getErrorCode == "NoSuchKey" => false
      case _ => false // throw e
    }
  }

  private def endsWithGz(s: String) = s.takeRight(3) == ".gz"

  private def assembleName(n: String, prefix: FileServicePrefix) = prefix.list.mkString("_") + "_" + n

  def importFile(n: String, f: File, move: Boolean, prefix: FileServicePrefix): Try[File] = {
    val f2 = if (endsWithGz(n)) {
      f
    } else {
      val f2 = mybiotools.TempFile.createTempFile(".gz")
      gzipFile(f, f2)
      f2
    }

    retry(5) {

      // Asynchronous call.
      val putrequest = new PutObjectRequest(bucketName, assembleName(n, prefix), f2)
      putrequest.setStorageClass(StorageClass.ReducedRedundancy)
      putrequest.setCannedAcl(CannedAccessControlList.Private)
      val upload = tm.upload(putrequest);
      upload.waitForCompletion
    }.map(x => f)

  }

  def openStream(n: String, prefix: FileServicePrefix): Try[InputStream] = {
    val s3object = s3Client.getObject(bucketName, assembleName(n, prefix))
    scala.util.Success(s3object.getObjectContent)
  }

  def exportFile(n: String, prefix: FileServicePrefix): Try[File] = {
    val file = mybiotools.TempFile.createTempFile("")
    retry(5) {
      val download = tm.download(bucketName, assembleName(n, prefix), file)
      download.waitForCompletion
    }.map { _ =>
      if (endsWithGz(n)) file
      else {
        val namedfile = mybiotools.TempFile.createFileInTempFolderIfPossibleWithName(assembleName(n, prefix))
        mybiotools.catchToLeft {
          gunzipFile(file, namedfile)
          namedfile
        } match {
          case Left(_) => {
            com.google.common.io.Files.move(file, namedfile)
            namedfile
          }
          case Right(f) => { f }
        }

      }
    }
  }

}

class EC2SelfShutdown(val id: RunningJobId, val balancerActor: ActorRef) extends SelfShutdown with EC2Shutdown {
  val ec2 = new AmazonEC2Client(new InstanceProfileCredentialsProvider())

}

class EC2Reaper(filesToSave: List[File], bucketName: String) extends Reaper with EC2Shutdown {

  val ec2 = new AmazonEC2Client(new InstanceProfileCredentialsProvider())
  val s3Client = new AmazonS3Client(new InstanceProfileCredentialsProvider());

  def allSoulsReaped(): Unit = {
    log.debug("All souls reaped. Calling system.shutdown.")
    context.system.shutdown()
    import com.amazonaws.services.ec2.AmazonEC2Client
    import com.amazonaws.auth.InstanceProfileCredentialsProvider

    val tm = new TransferManager(s3Client);
    // Asynchronous call.
    filesToSave.foreach { f =>
      val putrequest = new PutObjectRequest(bucketName, f.getName, f)
      putrequest.setStorageClass(StorageClass.ReducedRedundancy)
      putrequest.setCannedAcl(CannedAccessControlList.Private)
      val upload = tm.upload(putrequest);
      upload.waitForCompletion
    }

    val nodename = EC2Operations.readMetadata("instance-id").head
    EC2Operations.terminateInstance(ec2, nodename)
  }
}

class S3Updater(filesToSave: List[File], bucketName: String) extends Actor with akka.actor.ActorLogging {
  case object Tick

  override def preStart {
    log.debug("S3Updater start (" + filesToSave + ")")

    import context.dispatcher

    timer =
      context.system.scheduler.schedule(
        initialDelay = 0 seconds,
        interval = EC2Settings.S3UpdateInterval seconds,
        receiver = self,
        message = Tick
      )
  }

  val s3Client = new AmazonS3Client(new InstanceProfileCredentialsProvider());
  val tm = new TransferManager(s3Client);

  private var timer: Cancellable = null

  override def postStop {
    timer.cancel
    log.info("HeartBeatActor stopped.")
  }

  def upload {
    mybiotools.catchToLeft {
      filesToSave.foreach {
        f =>

          val putrequest = new PutObjectRequest(bucketName, f.getName, f)
          putrequest.setStorageClass(StorageClass.ReducedRedundancy)
          putrequest.setCannedAcl(CannedAccessControlList.Private)
          val upload = tm.upload(putrequest);
          upload.waitForCompletion
          log.debug("Uploaded " + f.toString)
      }
    }
  }

  def receive = {
    case Tick => upload
  }

}
