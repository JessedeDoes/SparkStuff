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

//val myMergeStrategy: String => MergeStrategy = { 
assemblyMergeStrategy in assembly := {
    case x if Assembly.isConfigFile(x) =>
      MergeStrategy.concat
    case PathList(ps @ _*) if Assembly.isReadme(ps.last) || Assembly.isLicenseFile(ps.last) =>
      MergeStrategy.rename
    case PathList("META-INF", xs @ _*) =>
      (xs map {_.toLowerCase}) match {
        case ("manifest.mf" :: Nil) | ("index.list" :: Nil) | ("dependencies" :: Nil) =>
          MergeStrategy.discard
        case ps @ (x :: xs) if ps.last.endsWith(".sf") || ps.last.endsWith(".dsa") =>
          MergeStrategy.discard
        case "plexus" :: xs =>
          MergeStrategy.discard
        case "services" :: xs =>
          MergeStrategy.filterDistinctLines
        case ("spring.schemas" :: Nil) | ("spring.handlers" :: Nil) =>
          MergeStrategy.filterDistinctLines
        case _ => MergeStrategy.first
      }
    case _ => MergeStrategy.first
  }

//assemblyMergeStrategy in assembly := myMergeStrategy
//assemblyMergeStrategy in assembly := {
//  case _         => MergeStrategy.first
//}
