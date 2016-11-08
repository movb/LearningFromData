import scala.util.Random

def GetRandomCoin() = {
  if (Random.nextDouble() > 0.5)
    1
  else
    0
}

def ThrowCoins(n:Int): Double = {
  Seq.fill(n)(GetRandomCoin).sum / n.toDouble
}

ThrowCoins(10)

def Experiment(n:Int) = {
  val coins = Seq.fill(n)(ThrowCoins(10))
  val randomInd = Random.nextInt(n)
  (coins(0), coins(randomInd), coins.min)
}

def Average(seq: Seq[Double]): Double = {
  seq.sum / seq.length.toDouble
}

def RunExperiment(n: Int) = {
  val results = Seq.fill(n)(Experiment(1000))
  val coin1 = results map (x => x._1)
  val coinRand = results map (x => x._2)
  val coinMin = results map (x => x._3)

  val coin1Av = Average(coin1)
  val coinRandAv = Average(coinRand)
  val coinMinAv = Average(coinMin)

  println(coin1Av + ", " + coinRandAv + ", " + coinMinAv)
}

RunExperiment(100000)
// 0.5002989999999982, 0.5007569999999968, 0.0378859999999765