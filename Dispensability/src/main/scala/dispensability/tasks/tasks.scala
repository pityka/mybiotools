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

package dispensability.tasks

import mybiotools._
import scala.concurrent.Future
import akka.actor.{ ActorRef, Actor, ActorRefFactory, Props }
import akka.actor.Actor._
import mybiotools.tasks._
import scala.concurrent.duration._
import mybiotools.mapreduce.MapReduceTraversal._
import org.apache.commons.math3.random.RandomDataGenerator
import org.apache.commons.math3.random.Well19937c
import org.apache.commons.math3.random.MersenneTwister
import dispensability._

case class OptimalParameter[P](optimum: P, residual: Double) extends Result

case class ModelInput[D, P](
  data: Option[D],
  model: Option[ModelProxy[D, P, _]]
) extends SimplePrerequisitive[ModelInput[D, P]]

object fitModel {

  def apply[D, P](
    in: ModelInput[D, P],
    memory: Int,
    update: UpdatePrerequisitive[ModelInput[D, P]] = identity[ModelInput[D, P]]
  )(implicit components: TaskSystemComponents) =
    newTask(
      in, update, CPUMemoryRequest(cpu = 1, memory = memory)
    ) {
      case (ModelInput(
        Some(data),
        Some(modelproxy)), ce) =>
        import ce._

        modelproxy.model.cdfFit(data)

    }

}

object fitModelML {

  def apply[D, P](
    in: ModelInput[D, P],
    memory: Int,
    update: UpdatePrerequisitive[ModelInput[D, P]] = identity[ModelInput[D, P]]
  )(implicit components: TaskSystemComponents) =
    newTask(
      in, update, CPUMemoryRequest(cpu = 1, memory = memory)
    ) {
      case (ModelInput(
        Some(data),
        Some(modelproxy)), ce) =>
        import ce._

        modelproxy.model.maximumLikelihood(data)

    }

}

object fitModelDownsampling {

  def apply[D, P](
    in: ModelInput[D, P],
    memory: Int,
    update: UpdatePrerequisitive[ModelInput[D, P]] = identity[ModelInput[D, P]]
  )(implicit components: TaskSystemComponents) =
    newTask(
      in, update, CPUMemoryRequest(cpu = 1, memory = memory)
    ) {
      case (ModelInput(
        Some(data),
        Some(modelproxy)), ce) =>
        import ce._

        val rnd = new MersenneTwister

        modelproxy.model.downsamplingFit(data, rnd)

    }

}

object fitModelNoPenetrance {

  def apply[D, P](
    in: ModelInput[D, P],
    memory: Int,
    update: UpdatePrerequisitive[ModelInput[D, P]] = identity[ModelInput[D, P]]
  )(implicit components: TaskSystemComponents) =
    newTask(
      in, update, CPUMemoryRequest(cpu = 1, memory = memory)
    ) {
      case (ModelInput(
        Some(data),
        Some(modelproxy)), ce) =>
        import ce._

        modelproxy.model.cdfFitNoPenetrance(data)

    }

}

case class BootstrapInputFromCounts(
  data: Option[DataForAbsoluteModel],
  replicas: Option[Int],
  seed: Option[Int]
) extends SimplePrerequisitive[BootstrapInputFromCounts]

case class BootstrappedData(list: List[DataForAbsoluteModel]) extends Result

object bootstrapFromCounts {

  def apply(
    in: BootstrapInputFromCounts,
    memory: Int,
    update: UpdatePrerequisitive[BootstrapInputFromCounts] = identity[BootstrapInputFromCounts]
  )(implicit components: TaskSystemComponents) =
    newTask(
      in, update, CPUMemoryRequest(cpu = 1, memory = memory)
    ) {
      case (BootstrapInputFromCounts(
        Some(data),
        Some(replicas),
        Some(seed)
        ), ce) =>
        import ce._
        val commonsrandom = new MersenneTwister(seed)

        val bootstrap = data.makeBootstrapReplicas(replicas, commonsrandom)

        BootstrappedData(bootstrap)

    }
}

case class PlotModelInput[Data <: PerGeneCountData, Param <: HasFractionOfEssentials with ParamForPlot, Indep <: IntVal](
  data: Option[Data],
  model: Option[ModelProxy[Data, Param, Indep]],
  parameters: Option[Seq[OptimalParameter[Param]]],
  parametersNoNoise: Option[Seq[OptimalParameter[Param]]],
  xAxisPoints: Option[Seq[Indep]],
  shortXAxisPoints: Option[Seq[Indep]],
  uniformData: Option[Data],
  homozygousData: Option[Data],
  severeData: Option[Data],
  seed: Option[Int],
  outname: Option[String],
  xlab: Option[String]
) extends SimplePrerequisitive[PlotModelInput[Data, Param, Indep]]

case class PlotResult(list: List[SharedFile]) extends ResultWithSharedFiles(list: _*)

object plotModel {

  def apply[Data <: PerGeneCountData, Param <: HasFractionOfEssentials with ParamForPlot, Indep <: IntVal](
    in: PlotModelInput[Data, Param, Indep],
    memory: Int,
    update: UpdatePrerequisitive[PlotModelInput[Data, Param, Indep]] = identity[PlotModelInput[Data, Param, Indep]]
  )(implicit components: TaskSystemComponents) =
    newTask(
      in, update, CPUMemoryRequest(cpu = 1, memory = memory)
    ) {
      case (PlotModelInput(
        Some(data),
        Some(modelproxy),
        Some(parameters),
        Some(parametersNoNoise),
        Some(xAxisPoints),
        Some(shortXAxisPoints),
        Some(uniformData),
        Some(homozygousData),
        Some(severeData),
        Some(seed),
        Some(outname),
        Some(xlab)), ce) =>
        import ce._

        PlotResult(Plots.plotModel(
          model = modelproxy.model,
          data = data,
          parameters = parameters,
          parametersWithoutNoise = parametersNoNoise,
          xAxisPoints = xAxisPoints,
          shortXAxisPoints = shortXAxisPoints,
          uniformData = uniformData,
          homozygousData = homozygousData,
          severeData = severeData,
          seed = seed,
          xlab = xlab
        ).map {
          case (name, file) =>

            SharedFile(file, outname + "." + name + ".pdf")
        }.toList)

    }

}

