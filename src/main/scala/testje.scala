import impact.ee.classifier.Classifier;
import impact.ee.classifier.Dataset;
import impact.ee.classifier.FeatureSet;
import impact.ee.classifier.Feature;
import impact.ee.classifier.Instance;
import impact.ee.classifier.Distribution;
import impact.ee.classifier.StochasticFeature;
import impact.ee.classifier.libsvm.LibLinearClassifier;
import impact.ee.classifier.libsvm.LibSVMClassifier;
import impact.ee.classifier.svmlight.SVMLightClassifier;
import impact.ee.classifier.svmlight.SVMLightClassifier.TrainingMethod;

import impact.ee.tagger.features.ClusterFeature;
import word2vec.Util
import word2vec.Vectors

import wsd.features.BoCLFeature;
import wsd.features.BoWFeature;
import wsd.features.ClusterAtFeature;
import wsd.features.LemmaAtFeature;
import wsd.features.PoSAtFeature;
import wsd.features.WordAtFeature;
import wsd.WSDInstance
import scala.collection.JavaConverters._

import org.apache.spark.{SparkConf, SparkContext}

import org.apache.spark.SparkContext._
import org.apache.spark.rdd.JdbcRDD
import org.apache.spark.sql._
import org.apache.spark.sql.types._
import org.apache.spark.sql.functions._

import org.apache.log4j.Logger
import org.apache.log4j.Level

case class One(truth: String, guess: String)

case class TestResult(nItems: Int, nErrors: Int, confusion: Map[One, Int]) {
  def plus(a: Map[One, Int], b: Map[One, Int]) = {
    (a.keySet.intersect(b.keySet).map(x => ((x, a(x) + b(x)))) ++
      a.keySet.diff(b.keySet).map(x => (x, a(x))) ++
      b.keySet.diff(a.keySet).map(x => (x, b(x)))).toMap
  }

  def +(other: TestResult): TestResult = TestResult(nItems + other.nItems, nErrors + other.nErrors,
    plus(confusion, other.confusion))

  override def toString(): String = {
    val accuracy = (nItems - nErrors) / nItems.asInstanceOf[Double]
    s"accuracy: $accuracy, items: $nItems, errors: $nErrors\n" + printMatrix
  }

  def printMatrix(): String = {
    val labels = confusion.flatMap({ case (One(a, b), _) => Set(a, b) }).toSet.toList
    def count(a: String, b: String):Int = {
      confusion.get(One(a, b)) match {
        case Some(i) => i;
        case _ => 0
      }}

      def rowCounts(i: Int) = labels.map(s => count(labels(i), s))

      val row = (i: Int) => f"${labels(i)}%20s\t" + (rowCounts(i).map(j => f"$j%20d")).mkString("\t")
      val header = f"${" "}%20s\t" + labels.map(s => f"$s%20s").mkString("\t")
      (header  +: labels.indices.map(row)).mkString("\n")
    }
  }



  object tester {
    var totalItems = 0
    var totalErrors = 0
    var totalFailures = 0
    var totalMfsErrors = 0

    val minWordsinExample = 8
    val minExamplesInSense = 5
    val minAvgPerSense = 20.0
    val trivialTest = TestResult(0, 0, Map.empty)

    def leaveOneOut(wsd: wsd, df: DataFrame): Unit = {
      Console.err.println("starting...")
      val ks = df.select("lempos").distinct.collect
      Console.err.println("############### " + ks.length)
      // df.groupBy("lempos")

      val df1 = df.repartition(1000, df.col("lempos"))
      var c = 0;
      df1.foreachPartition(i => c += 1)
      Console.err.println("Aantal partities: " + c)
      // df1.foreachPartition(r => leaveOneOut(wsd,r))
    }

    def senseDistribution(instances: Seq[Concordance]) = instances.groupBy(_.meta("senseId")).mapValues(_.size).toList.sortWith((a, b) => a._2 > b._2)

    def enoughData(instances: Seq[Concordance]): Boolean = {
      val sd = senseDistribution(instances)
      val minPerSense = sd.map(_._2).reduce((a, b) => Math.min(a, b))
      val avgPerSense = sd.map(_._2).sum / sd.size.asInstanceOf[Double]
      avgPerSense > minAvgPerSense
    }

    def leaveOneOut(wsd: wsd, instances: List[Concordance]): TestResult = {
      val groupedByLempos = instances.groupBy(_.meta("lempos"))
      val allResults = groupedByLempos.par.mapValues(l => leaveOneOutOneLempos(wsd, l))
      val totalResults = allResults.values.reduce((a, b) => a + b)
      totalResults
    }

    def filterABit(all_Instances: List[Concordance]) = {
      val instancesX = all_Instances.filter(r => {
        val x = r("word");
        x.size >= minWordsinExample
      })
      val senseDistribMap = senseDistribution(instancesX).toMap
      instancesX.filter(r => {
        senseDistribMap(r.meta("senseId")) >= minExamplesInSense
      })
    }


    def leaveOneOutOneLempos(wsd: wsd, all_Instances: List[Concordance]): TestResult = {
      val retrain = !wsd.isInstanceOf[DistributionalOnly]
      var classify: Concordance => String = null


      var errors = 0
      var total = 0
      var failures = 0


      val instances = filterABit(all_Instances)

      val senseDistrib = senseDistribution(instances)
      val senses = instances.map(_.meta("senseId")).distinct


      if (senses.size < 2 || !enoughData(instances))
        return trivialTest;

      val lempos = instances.head.meta("lempos")

      System.err.println("starting work on: " + lempos + " " + senses)

      def foldingFunction(t: TestResult, x: Concordance): TestResult = {
        if (retrain || classify == null)
          classify = wsd.train(instances, Set(x.meta("id")))
        t + testResult(Set(x), classify)
      }

      val result = instances.foldLeft(trivialTest)(foldingFunction)
      Console.err.println(result)


      val accuracy = (result.nItems - result.nErrors) / (result.nItems.asInstanceOf[Double])


      val mfsProportion = senseDistrib.head._2 / (instances.size + 0.0)
      val mfsErrors = instances.size - senseDistrib.head._2

      Console.err.println(lempos + "  |senses|: " + senseDistrib.size + "  " + result.nErrors + " errors of " + result.nItems + " failures: " +
        failures + " score: " + accuracy + " distribution: " + senseDistrib + "  mfs: " + mfsProportion);

      Console.err.println("Accuracy: " + accuracy)
      result

    }


    def testOne(classify: Concordance => String, c: Concordance): TestResult = {
      val label = classify(c)
      val truth = c.meta("senseId")
      val errors = if (label == truth) 0 else 1
      TestResult(1, errors, Map(One(truth, label) -> 1))
    }

    def testResult(instances: Set[Concordance], classify: Concordance => String): TestResult = {
      val t = new Dataset("test");
      var errors = 0;
      // instances.foldLeft(0)( (a,w) => a +  (if (classify(w) == w.meta("senseId")) 0 else 1))
      instances.foldLeft(trivialTest)({ case (t, w) => t + testOne(classify, w) })
    }
  }


