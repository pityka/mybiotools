// package mybiotools.tasks.monitor

// import akka.actor.{ ActorRef, ActorSystem }
// import akka.serialization._
// import com.typesafe.config.ConfigFactory

// trait UPicklerAkkaSerializer[T <: AnyRef] extends Serializer {

//   implicit def reader: upickle.Reader[T] = implicitly[upickle.Reader[T]]
//   implicit def writer: upickle.Writer[T] = implicitly[upickle.Writer[T]]

//   def includeManifest: Boolean = false

//   def toBinary(obj: AnyRef): Array[Byte] = {
//     upickle.write[T](obj.asInstanceOf[T]).getBytes("UTF-8")
//   }

//   def fromBinary(bytes: Array[Byte],
//     clazz: Option[Class[_]]): AnyRef = {
//     upickle.read[T](new String(bytes, "UTF-8"))
//   }
// }

// object QueueStatSerializer extends UPicklerAkkaSerializer[QueueStat] {
//   def identifier = 17
// }