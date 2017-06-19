NBDIR=~/Downloads/spark-notebook-0.7.0-scala-2.11.8-spark-2.1.0-hadoop-2.8.0/
export CLASSPATH_OVERRIDES=/home/jesse/workspace/SparkStuff/target/scala-2.11/SparkStuff-assembly-1.0.jar
cd $NBDIR
./bin/spark-notebook &
sleep 3
google-chrome http://localhost:9001
