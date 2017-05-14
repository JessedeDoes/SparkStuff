name := "SparkStuff"
version := "1.0"


scalaVersion := "2.11.8"

val sparkVersion = "2.1.0"

libraryDependencies ++= Seq(
  "org.apache.spark" %% "spark-core" % sparkVersion,
  "org.apache.spark" %% "spark-streaming" % sparkVersion
)

// https://mvnrepository.com/artifact/postgresql/postgresql
libraryDependencies += "org.postgresql" % "postgresql" % "42.1.1"
libraryDependencies += "org.apache.spark" % "spark-sql_2.11" % sparkVersion

// https://mvnrepository.com/artifact/nl.inl.blacklab/blacklab
libraryDependencies += "nl.inl.blacklab" % "blacklab" % "1.5.0"
