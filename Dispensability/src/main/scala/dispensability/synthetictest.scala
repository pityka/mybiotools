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

// package dispensability

// import org.apache.commons.math3.random.MersenneTwister
// import mybiotools.config.Config.configInstance
// import mybiotools._
// import dispensability.tasks._
// import scala.concurrent.Future
// import scala.concurrent._
// import scala.concurrent.duration._
// import scala.concurrent.ExecutionContext.Implicits.global
// import mybiotools.plots.ScatterPlot._
// import java.awt.Color
// import mybiotools.tasks._
// import org.apache.commons.math3.random.RandomDataGenerator
// import EstimateEssentialsWithSimulation.Parameters

// class SyntheticTestRunner(ts: TaskSystem) {
//   import ts._
//   val log = ts.getLogger(this)

//   case class SummarizedParameters(fractionOfEssentials: SummaryStat, falseRate: SummaryStat, penetrance: SummaryStat)

//   def insideCIPHI(sum: SummarizedParameters, parameters: Parameters): Boolean =
//     parameters.fractionOfEssentials >= sum.fractionOfEssentials.p2_5 &&
//       parameters.fractionOfEssentials <= sum.fractionOfEssentials.p97_5

//   def insideCIPF(sum: SummarizedParameters, parameters: Parameters): Boolean =
//     parameters.falseRate >= sum.falseRate.p2_5 &&
//       parameters.falseRate <= sum.falseRate.p97_5

//   def insideCIPPE(sum: SummarizedParameters, parameters: Parameters): Boolean =
//     parameters.penetrance >= sum.penetrance.p2_5 &&
//       parameters.penetrance <= sum.penetrance.p97_5

//   def run = {

//     val samochaTable = new java.io.File(configInstance.getString("estimateessential.samochaTable"))

//     val artificalNoise = configInstance.getDouble("synthetic.noise")

//     val seed = configInstance.getInt("synthetic.seed")

//     val rnd = new MersenneTwister(seed)

//     // def noise = {
//     //   if (artificalNoise == 0.0) 1.0 else math.exp(new RandomDataGenerator(rnd).nextGaussian(0.0, artificalNoise))
//     // }

//     val unscaledProb = {
//       val neutralProbabilities: Map[String, Double] = openSource(samochaTable.getAbsolutePath)(s => Input.readTotalGeneList(s)).toSeq.map(x => x.hgnc.value -> (x.probFS + x.probStop)).toMap
//       UnscaledProbabilities(neutralProbabilities.toSeq.map(_._2).toVector)
//     }
//     println(unscaledProb.vector.size)

//     val bootstrap = configInstance.getInt("synthetic.bootstrap")
//     val syntheticreplicas = configInstance.getInt("synthetic.replicas")

//     val useTrueParameter = configInstance.getBoolean("synthetic.useTrueParameter")

//     val knownParameters: Seq[Parameters] = List(
//       Parameters(0.3, 0.0, 0.8),
//       Parameters(0.4, 0.0, 0.9),
//       Parameters(0.2, 0.0, 0.9),
//       Parameters(0.1, 0.0, 1.0),
//       Parameters(0.0, 0.0, 1.0),
//       Parameters(0.25, 0.1, 0.9)
//     )

//     val numberOfVariants: List[Int] = List(41745, 150000)

//     val errorFixed = true

//     val numberOfReferencePoints = configInstance.getInt("synthetic.numberOfReferencePoints")

//     knownParameters.foreach {
//       case (knownparameter) =>
//         val simulatedData = EstimateEssentialsWithSimulation.simulationGeneCounts(
//           fullNeutralDensity = unscaledProb.rescale,
//           fractionOfEssentials = knownparameter.fractionOfEssentials,
//           penetranceOfEssentials = knownparameter.penetrance,
//           falseRate = knownparameter.falseRate,
//           replicasOfEssentialDice = syntheticreplicas,
//           replicasOfVariantDice = 1,
//           variants = numberOfVariants,
//           commonsrnd = rnd)

//         val estimates: Future[Seq[(Int, Seq[Seq[(Parameters)]])]] = Future.sequence(simulatedData.toSeq.map {
//           case (variantcount, replicates) =>
//             Future.sequence(replicates.map { synthetic =>

//               val syntheticUnscaledWithCounts = UnscaledProbabilitiesWithCounts((synthetic.map(x => ObservedTruncationCount(x)) zip unscaledProb.vector).toVector)

//               val downsampleVariants = {
//                 val minimumDownSample = 15000
//                 (minimumDownSample to variantcount by (variantcount - minimumDownSample) / numberOfReferencePoints).toList
//               }

//               bootstrapFromCounts(BootstrapInputFromCounts(
//                 Some(syntheticUnscaledWithCounts),
//                 Some(bootstrap),
//                 Some(downsampleVariants),
//                 Some(seed)), 1000).?[BootstrappedReferenceData]
//                 .flatMap {
//                   case BootstrappedReferenceData(bootstrapAndSubSamples) =>
//                     Future.sequence(bootstrapAndSubSamples.map {
//                       case (bootstrappedcounts, downsampledreference) =>

//                         val totalModelEstimate: Future[Parameters] = {
//                           fit2D(EstimateWithSimulationInput(
//                             fullNeutralDensity = Some(bootstrappedcounts.probabilities),
//                             parameters = Some((if (errorFixed) Parameters(0, knownparameter.falseRate, Double.NaN) else Parameters(0, Double.NaN, knownparameter.penetrance)) :: Nil),
//                             replicasOfEssentialDice = Some(0),
//                             replicasOfVariantDice = Some(0),
//                             observed = Some(downsampledreference),
//                             analytical = Some(true)), 1000).?[OptimalParameter].map(_.optimum)

//                         }

//                         totalModelEstimate.map { y =>
//                           y
//                         }

//                     })

//                 }
//             }).map(x => variantcount -> x)

//         })
//         val wait = estimates.map { (estimates: Seq[(Int, Seq[Seq[(Parameters)]])]) =>
//           val summarized: Seq[(Int, Seq[(SummarizedParameters)])] = estimates.map(x => x._1 -> x._2.map { y =>
//             val totalPHI = SummaryStat(y.map(_.fractionOfEssentials))
//             val totalPF = SummaryStat(y.map(_.falseRate))
//             val totalPPE = SummaryStat(y.map(_.penetrance))

//             SummarizedParameters(totalPHI, totalPF, totalPPE)
//           })
//           val summaryText = summarized.map {
//             case (totalcount, replicates) =>
//               val totalModelMatchPHI = replicates.count(x => insideCIPHI(x, knownparameter))
//               val totalModelMatchPF = replicates.count(x => insideCIPF(x, knownparameter))
//               val totalModelMatchPPE = replicates.count(x => insideCIPPE(x, knownparameter))
//               val totalModelMeanDeviationPHI = SummaryStat(replicates.map(_.fractionOfEssentials.mean - knownparameter.fractionOfEssentials))
//               val totalModelMeanDeviationPF = SummaryStat(replicates.map(_.falseRate.mean - knownparameter.falseRate))
//               val totalModelMeanDeviationPPE = SummaryStat(replicates.map(_.penetrance.mean - knownparameter.penetrance))

//               s""" 
//             |  Data size: $totalcount
//             |  
//             |    # of times the true value is inside the marginal 95% CI of the bootstrap estimates for the summed model:
//             |      PHI: $totalModelMatchPHI / ${replicates.size}
//             |      PF : $totalModelMatchPF / ${replicates.size}
//             |      PPE: $totalModelMatchPPE / ${replicates.size}
//             |    Summary of the mean of the bootstrap estimates minus the known parameter for the summed model:
//             |      PHI: ${totalModelMeanDeviationPHI.mean} ${totalModelMeanDeviationPHI.stddev} ${totalModelMeanDeviationPHI.count} 
//             |      PF : ${totalModelMeanDeviationPF.mean} ${totalModelMeanDeviationPF.stddev} ${totalModelMeanDeviationPF.count}
//             |      PPE: ${totalModelMeanDeviationPPE.mean} ${totalModelMeanDeviationPPE.stddev} ${totalModelMeanDeviationPPE.count}
//             |    
//             """.stripMargin

//           }.mkString(s"\nTrue parameters: $knownparameter", "", "")
//           log.info(summaryText)

//         }
//         Await.result(wait, atMost = 168 hours)

//     }
//   }

// }

// object SyntheticTestApp extends App {
//   val ts = defaultTaskSystem
//   if (ts.hostConfig.myRole == mybiotools.tasks.MASTER) {
//     ts.registerApplicationFileLogger(new java.io.File("synthetic.logfile"))
//     val runner = new SyntheticTestRunner(ts)
//     runner.run
//     ts.shutdown
//   }

// }