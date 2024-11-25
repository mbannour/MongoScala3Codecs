import akka.actor.ActorSystem
import akka.stream.scaladsl.{Sink, Source}
import com.mongodb.reactivestreams.client.{MongoClients, MongoCollection, MongoDatabase}
import io.github.mbannour.mongo.codecs
import io.github.mbannour.mongo.codecs.CodecProviderMacro
import org.bson.{BsonReader, BsonType, BsonWriter}
import org.bson.codecs.{Codec, DecoderContext, EncoderContext}
import org.bson.types.ObjectId
import org.mongodb.scala.bson.annotations.BsonProperty

import java.time.format.DateTimeFormatter
import java.time.{ZoneId, ZonedDateTime}
import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.reflect.ClassTag

enum EmploymentStatus:
  case Employed, Unemployed, Retired

case class Address(@BsonProperty("s") street: String, city: String, zipCode: Int)

case class Person(
    _id: ObjectId,
    name: String,
    @BsonProperty("ag") age: Int,
    @BsonProperty("sa") salary: Double,
    isEmployed: Boolean,
    yearsOfExperience: Long,
    nickname: Option[String],
    hobbies: List[String],
    address: Address,
    test: Int,
    employmentStatus: EmploymentStatus,
    lastUpdated: ZonedDateTime
)

@main def hello(): Unit =
  implicit val system: ActorSystem = ActorSystem("MongoActorSystem")

  import org.bson.codecs.configuration.{CodecProvider, CodecRegistry}
  import org.bson.codecs.configuration.CodecRegistries

  object EmploymentStatusCodec extends Codec[EmploymentStatus]:
    override def encode(writer: BsonWriter, value: EmploymentStatus, encoderContext: EncoderContext): Unit =
      writer.writeString(value.toString)

    override def decode(reader: BsonReader, decoderContext: DecoderContext): EmploymentStatus =
      val value = reader.readString()
      try EmploymentStatus.valueOf(value)
      catch
        case _: IllegalArgumentException =>
          throw new IllegalArgumentException(s"Invalid value for EmploymentStatus enum: $value")

    override def getEncoderClass: Class[EmploymentStatus] = classOf[EmploymentStatus]
  end EmploymentStatusCodec

  class ZonedDateTimeCodec extends Codec[ZonedDateTime]:

    private val formatter: DateTimeFormatter = DateTimeFormatter.ISO_ZONED_DATE_TIME

    override def encode(writer: BsonWriter, value: ZonedDateTime, encoderContext: EncoderContext): Unit =
      writer.writeString(value.format(formatter))

    override def decode(reader: BsonReader, decoderContext: DecoderContext): ZonedDateTime =
      val isoString = reader.readString()
      ZonedDateTime.parse(isoString, formatter)

    override def getEncoderClass: Class[ZonedDateTime] = classOf[ZonedDateTime]
  end ZonedDateTimeCodec

  val zonedDateTimeCodec = new ZonedDateTimeCodec()

  val employmentStatusCodecRegistry: CodecRegistry = CodecRegistries.fromCodecs(EmploymentStatusCodec)

//  val employmentStatusCodec = new EnumCodec[EmploymentStatus]()

  // Create CodecRegistry for Address
  val addressCodecRegistry: CodecRegistry = CodecRegistries.fromProviders(
    employmentStatusCodecRegistry,
    CodecRegistries.fromCodecs(zonedDateTimeCodec),
    CodecProviderMacro.createCodecProvider[Address](MongoClients.getDefaultCodecRegistry)
  )

  // Create CodecRegistry for Person
  val personCodecRegistry: CodecRegistry = CodecRegistries.fromProviders(
    codecs.CodecProviderMacro.createCodecProvider[Person](addressCodecRegistry)
  )

  // Combine CodecRegistries
  val codecRegistry: CodecRegistry = CodecRegistries.fromRegistries(
    personCodecRegistry,
    addressCodecRegistry,
    MongoClients.getDefaultCodecRegistry
  )

  // Setup MongoDB client and collection
  val client = MongoClients.create()
  val database: MongoDatabase = client.getDatabase("test_db").withCodecRegistry(codecRegistry)
  val collection: MongoCollection[Person] = database.getCollection("my_collection", classOf[Person])

  // Define sample data
  val address = Address("123 Elm St", "Metropolis", 12345)
  val document = Person(
    _id = new ObjectId(),
    name = "John Doe",
    age = 30,
    salary = 50000.5,
    isEmployed = true,
    yearsOfExperience = 10,
    nickname = None,
    hobbies = List("Reading", "Swimming"),
    address = address,
    test = 42,
    employmentStatus = EmploymentStatus.Employed,
    lastUpdated = ZonedDateTime.now(ZoneId.of("UTC"))
  )

  // Insert the document
  val insertFuture = Source.fromPublisher(collection.insertOne(document)).runWith(Sink.ignore)
  Await.result(insertFuture, Duration.Inf)

  // Retrieve documents from the collection
  val readFuture = Source.fromPublisher(collection.find()).runWith(Sink.seq[Person])
  val documents = Await.result(readFuture, Duration.Inf)
  documents.foreach(println)

  system.terminate()
end hello
