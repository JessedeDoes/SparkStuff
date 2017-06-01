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
libraryDependencies += "org.apache.jena"  % "jena-core"  % "3.3.0"  exclude("com.fasterxml.jackson.core", "jackson-databind")
libraryDependencies += "org.apache.jena" % "jena-arq" % "3.3.0" exclude("com.fasterxml.jackson.core", "jackson-databind")
libraryDependencies += "com.fasterxml.jackson.core" % "jackson-core" % "2.6.5" 
libraryDependencies += "com.fasterxml.jackson.core" % "jackson-databind" % "2.6.5" 

// https://mvnrepository.com/artifact/nl.inl.blacklab/blacklab
libraryDependencies += "nl.inl.blacklab" % "blacklab" % "1.5.0"

resolvers += "Local Maven Repository" at "file://"+Path.userHome.absolutePath+"/.m2/repository"
resolvers += "my private repository at xs4all" at "http://dedoes.home.xs4all.nl/maven"
// resolvers += "mavenrepo" at "https://mvnrepository.com"

libraryDependencies += "org.ivdnt.wordvectors"  % "WordVectors" % "0.0.1-SNAPSHOT"
libraryDependencies += "org.ivdnt.wsd" % "JustWSD" % "0.0.1-SNAPSHOT" 
libraryDependencies += "org.ivdnt.tagging" % "DutchTagger" % "0.0.1-SNAPSHOT"
libraryDependencies += "org.ivdnt.clariah" % "QueryNederlab" % "0.0.1-SNAPSHOT"

resolvers ++= Seq(
  "tpolecat" at "http://dl.bintray.com/tpolecat/maven",
  "Scalaz Bintray Repo" at "http://dl.bintray.com/scalaz/releases"
)

lazy val doobieVersion = "0.4.1"

libraryDependencies ++= Seq(
  "org.tpolecat"   %% "doobie-core"               % doobieVersion,
  "org.tpolecat"   %% "doobie-contrib-postgresql" % "0.3.0",
  "org.tpolecat"   %% "doobie-contrib-specs2"     % "0.3.0"
)

// https://mvnrepository.com/artifact/cc.mallet/mallet
// libraryDependencies += "cc.mallet" % "mallet" % "2.0.8"

// libraryDependencies += "graphframes" % "graphframes" % "0.4.0-spark2.1-s_2.11"

// or, for convenience:

// resolvers += Resolver.mavenLocal

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
