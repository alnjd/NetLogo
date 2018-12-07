// (C) Uri Wilensky. https://github.com/NetLogo/NetLogo

package org.nlogo.fileformat

import java.nio.file.Path

import org.nlogo.core.{ CompilationEnvironment, Dialect, ExtensionManager,
  LiteralParser, Model, Plot, SourceRewriter }
import org.nlogo.api.AutoConvertable

object PlotConverter {
  def allPlotNames(model: Model): Seq[String] =
    model.plots.flatMap(_.display).filterNot(_.isEmpty)


  def allLocalMapPenNames(model: Model): Seq[(String, Seq[String])] = {
    val plotList = model.plots.map((cas: Plot) => (cas.display.getOrElse(""), cas.pens.toSeq.map(_.display)))
    plotList.filterNot(_._1.isEmpty)
  }

  def allLocalPenNames(model: Model): Seq[Seq[String]] =
    model.plots.map(_.pens.toSeq.map(_.display))

  private[fileformat] def determineMapRenames(names: Seq[(String, Seq[String])]): Seq[(String, Seq[(String, String)])] = {
    names.map(c => (c._1, c._2.groupBy(cas => cas.toUpperCase) // Map[K, Repr]
      .toSeq
      .map {
        case (upper, originals) => upper -> originals //.distinct // no duplicates
      }
      .flatMap { // each entry is a groupBy
        case (upper, originals) if originals.length > 1 =>
          // Put lower-case names before uppercase names
          val sortedOriginals = originals.sorted.reverse
          sortedOriginals.zipWithIndex.map {
            case (original, index) if index == 0 => (original, original)
            case (original, index) => (original, s"${original}_${index}")
          }
        case _ => Seq.empty[(String, String)]
      }
    )).filterNot(_._2.isEmpty)
  }

  private[fileformat] def determineRenames(names: Seq[String]): Seq[(String, String)] = {
    names.groupBy(_.toUpperCase)
      .toSeq
      .map {
        case (upper, originals) => upper -> originals.distinct // no duplicates
      }
      .flatMap { // each entry is a groupBy
        case (upper, originals) if originals.length > 1 =>
          // Put lower-case names before uppercase names
          val sortedOriginals = originals.sorted.reverse
          sortedOriginals.zipWithIndex.map {
            case (original, index) if index == 0 => (original, original)
            case (original, index) => (original, s"${original}_${index}")
          }
        case _ => Seq.empty[(String, String)]
      }
  }

  private def clarifyProcedureBody(renamePairs: Seq[(String, String)]): String = {
    val nameMap = renamePairs.map(p => s"""["${p._1}" "${p._2}"]""").mkString("[", " ", "]")
    s"""|  let name-map ${nameMap}
        |  let replacement filter [ rename -> first rename = name] name-map
        |  let reported-name name
        |  if not empty? replacement [
        |    set reported-name item 1 replacement
        |  ]
        |  report reported-name""".stripMargin
  }

  private def buildConversionSet(
    description: String,
    reporterName: String,
    renames: Seq[(String, String)],
    conversionTarget: String
  ): Seq[ConversionSet] = {
    if (renames.nonEmpty) {
      val codeTabTransformation: SourceRewriter => String =
        _.addReporterProcedure(reporterName, Seq("name"), clarifyProcedureBody(renames))
      val sharedTransformations = Seq[SourceRewriter => String](
        _.replaceToken(conversionTarget, s"$conversionTarget $reporterName"))
      Seq(ConversionSet(description,
        codeTabTransformation +: sharedTransformations,
        sharedTransformations,
        Seq(conversionTarget)))
    } else
      Seq.empty[ConversionSet]
  }

  def plotCodeConversions(model: Model): Seq[ConversionSet] = {
    val plotConversions =
      buildConversionSet("Make plots case-insensitive",
        "_clarify-duplicate-plot-name",
        determineRenames(allPlotNames(model)),
        "set-current-plot")

    val penConversions =
      buildConversionSet("Make pens case-insensitive",
        "_clarify-duplicate-plot-pen-name",
        determineMapRenames(allLocalMapPenNames(model)).map(_._2).flatten.distinct,
        "set-current-plot-pen")

    plotConversions ++ penConversions
  }
}

class PlotConverter(
  extensionManager:      ExtensionManager,
  compilationEnv:        CompilationEnvironment,
  literalParser:         LiteralParser,
  baseDialect:           Dialect,
  components:            Seq[AutoConvertable]) extends
  ModelConverter(
    extensionManager,
    compilationEnv,
    literalParser,
    baseDialect,
    components,
    PlotConverter.plotCodeConversions _) {
      import PlotConverter._

      override def apply(model: Model, modelPath: Path): ConversionResult = {
        val plotRenames = determineRenames(allPlotNames(model))
        val penRenames: Seq[(String, Seq[(String,String)])] = determineMapRenames(allLocalMapPenNames(model))
        (plotRenames, penRenames) match {
          case (Seq(), Seq()) => super.apply(model,modelPath)
          case _ => { val conversion = super.apply(model, modelPath)
            val convertPandPN: Model = convertPlotAndPenNames(conversion.model, plotRenames.toMap, penRenames.toMap)
            conversion.updateModel(convertPandPN)
          }
        }
      }

   private def convertPlotAndPenNames( // you need to create a pipeline
        model:       Model,               // Seq[Seq[String]] -> Seq[(String, String)] -> Complete
        plotRenames: Map[String, String], // Apply each set of changes to its corresponding list
        penRenames:  Map[String, Seq[(String, String)]]): Model = { // I need penRenames with list of list
                                                     // List[Map[String,String]], possibly
                                                     // this would need to be generated as we go
       val updatedWidgets = model.widgets.map{
         case p: Plot => {
           val newDisplay = p.display.map(currentName => plotRenames.getOrElse(currentName, currentName))
           val localRenames = penRenames.getOrElse((p.display.getOrElse("")), Seq()).toMap
           val newPens = p.pens.map {
             pen =>
             val newPenName = localRenames.getOrElse(pen.display, pen.display)
             pen.copy(display = newPenName)

           }
           p.copy(display = newDisplay, pens = newPens)
         }
         case w => w
       }
       model.copy(widgets = updatedWidgets)
    }
}
