package org.nlogo.fileformat

import org.nlogo.core.{ Model }
import org.nlogo.core.{ Pen, Plot, View }
import org.scalatest.{ FunSuite, DiagrammedAssertions }

class PlotConverterTests extends FunSuite with ConversionHelper with DiagrammedAssertions {

  /*************
   * Plot Name *
   *************/

  test("if the plot names are not initialized"){
    val model = Model(widgets = Seq( View()
                                   , Plot(display = None, pens = List(Pen(display = "dep", setupCode = "plot e 10")))
                                   , Plot(display = None, pens = List(Pen(display = "DeP", setupCode = "plot e 10")))
                                   , Plot(display = None, pens = List(Pen(display = "Dep", setupCode = "plot e 10")))))

    val result = PlotConverter.allPlotNames(model)
    assertResult(List())(result)
  }

  test("if the plot names are initialized"){
    val model = Model(widgets = Seq( View()
                                   , Plot(display = Some("MegaLazer"), pens = List(Pen(display = "dep", setupCode = "plot e 10")))
                                   , Plot(display = Some("Teser"), pens = List(Pen(display = "DeP", setupCode = "plot e 10")))
                                   , Plot(display = Some("Test"), pens = List(Pen(display = "Dep", setupCode = "plot e 10")))))

    val result = PlotConverter.allPlotNames(model)
    assertResult(List("MegaLazer", "Teser", "Test"))(result)
  }

   /**********************
   * All Local Pens Name *
   ***********************/

  test("if the plot pen names are initialized"){
    val model = Model(widgets = Seq( View()
                                   , Plot(display = Some("MegaLazer"), pens = List(Pen(display = "dep", setupCode = "plot e 10")))
                                   , Plot(display = Some("Teser"), pens = List(Pen(display = "DeP", setupCode = "plot e 10")))
                                   , Plot(display = Some("Test"), pens = List(Pen(display = "Dep", setupCode = "plot e 10")))))

    val result = PlotConverter.allLocalPenNames(model).flatten
    assertResult(List("dep", "DeP", "Dep"))(result)
  }

  test("if the plot pen names are searching locally for each plot"){
    val model = Model(widgets = Seq( View()
                                   , Plot(display = Some("MegaLazer"), pens = List(Pen(display = "dep", setupCode = "plot e 10")))
                                   , Plot(display = Some("Teser"), pens = List(Pen(display = "DeP", setupCode = "plot e 10")))
                                   , Plot(display = Some("Test"), pens = List(Pen(display = "Dep", setupCode = "plot e 10")))))

    val result = PlotConverter.allLocalPenNames(model)
    assertResult(List(List("dep"), List("DeP"), List("Dep")))(result)
  }

  test("if the local plot pen names are renamed from a list with similarities"){
    val model = Model(widgets = Seq( View()
                                   , Plot(display = Some("MegaLazer"), pens = List(Pen(display = "dep", setupCode = "plot e 10")))
                                   , Plot(display = Some("Teser"), pens = List(Pen(display = "DeP", setupCode = "plot e 10")))
                                   , Plot(display = Some("Test"), pens = List(Pen(display = "Dep", setupCode = "plot e 10"),
                                                                              Pen(display = "DEP", setupCode = "plot e 10")))))

    val result = PlotConverter.determineMapRenames(PlotConverter.allLocalMapPenNames(model))
    assertResult(Seq(("Test", Seq(("Dep", "Dep"), ("DEP", "DEP_1")))))(result)
  }

  test("if the local2 plot pen names are renamed from a list with no similarities"){
    val model = Model(widgets = Seq( View()
                                   , Plot(display = Some("MegaLazer"), pens = List(Pen(display = "dep", setupCode = "plot e 10")))
                                   , Plot(display = Some("Teser"), pens = List(Pen(display = "DeP", setupCode = "plot e 10")))
                                   , Plot(display = Some("Test"), pens = List(Pen(display = "Dep", setupCode = "plot e 10")))))

    val result = PlotConverter.determineMapRenames(PlotConverter.allLocalMapPenNames(model))
    assertResult(Seq())(result)
  }

  test("if the global plot pen names are renamed from a list with similarities"){
    val model = Model(widgets = Seq( View()
                                   , Plot(display = Some("MegaLazer"), pens = List(Pen(display = "dep", setupCode = "plot e 10")))
                                   , Plot(display = Some("Teser"), pens = List(Pen(display = "DeP", setupCode = "plot e 10")))
                                   , Plot(display = Some("Test"), pens = List(Pen(display = "Dep", setupCode = "plot e 10"),
                                                                              Pen(display = "DEP", setupCode = "plot e 10")))))

    val result = PlotConverter.determineMapRenames(PlotConverter.allLocalMapPenNames(model))
    assertResult(Seq(("Test",Seq(("Dep","Dep"), ("DEP","DEP_1")))))(result)
  }

  test("if the global plot pen names are renamed from a list with similarities and a duplicate pen name"){
    val model = Model(widgets = Seq( View()
                                   , Plot(display = Some("MegaLazer"), pens = List(Pen(display = "DEP", setupCode = "plot e 10")))
                                   , Plot(display = Some("Teser"), pens = List(Pen(display = "DeP", setupCode = "plot e 10")))
                                   , Plot(display = Some("Test"), pens = List(Pen(display = "Dep", setupCode = "plot e 10"),
                                                                              Pen(display = "DEP", setupCode = "plot e 10")))))

    val result = PlotConverter.determineMapRenames(PlotConverter.allLocalMapPenNames(model))
    assertResult(Seq(("Test", Seq(("Dep","Dep"), ("DEP","DEP_1")))))(result)
  }

  test("if all plots have duplicate pen name"){
    val model = Model(widgets = Seq( View()
                                   , Plot(display = Some("MegaLazer"), pens = List(Pen(display = "DEP", setupCode = "plot e 10")))
                                   , Plot(display = Some("Teser"), pens = List(Pen(display = "DEP", setupCode = "plot e 10")))
                                   , Plot(display = Some("Test"), pens = List(Pen(display = "DEP", setupCode = "plot e 10"),
                                                                              Pen(display = "DEP", setupCode = "plot e 10")))))
    val result = PlotConverter.determineMapRenames(PlotConverter.allLocalMapPenNames(model))
    assertResult(List(("Test", Seq(("DEP","DEP"), ("DEP","DEP_1")))))(result)
  }

  test("if all plots have duplicate pen name with one field"){
    val model = Model(widgets = Seq( View()
                                   , Plot(display = Some("MegaLazer"), pens = List(Pen(display = "DEP", setupCode = "plot e 10")))
                                   , Plot(display = Some("Teser"), pens = List(Pen(display = "DEP", setupCode = "plot e 10")))
                                   , Plot(display = Some("Test"), pens = List(Pen(display = "DEP", setupCode = "plot e 10")))))

    val result = PlotConverter.determineMapRenames(PlotConverter.allLocalMapPenNames(model))
    assertResult(Seq())(result)
  }

  test("if only one plot doesn't have duplicate pen name"){
    val model = Model(widgets = Seq( View()
                                   , Plot(display = Some("MegaLazer"), pens = List(Pen(display = "DEP", setupCode = "plot e 10")))
                                   , Plot(display = Some("Teser"), pens = List(Pen(display = "DeP", setupCode = "plot e 10")))
                                   , Plot(display = Some("Test"), pens = List(Pen(display = "DEP", setupCode = "plot e 10")))))

    val listOfNames = PlotConverter.allLocalMapPenNames(model)
    val result = PlotConverter.determineMapRenames(listOfNames)
    assertResult(Seq())(result)
  }

  test("if only one plot doesn't have duplicate pen name: local pens"){
    val model = Model(widgets = Seq( View()
                                   , Plot(display = Some("MegaLazer"), pens = List(Pen(display = "DEP", setupCode = "plot e 10")))
                                   , Plot(display = Some("Teser"), pens = List(Pen(display = "DEP", setupCode = "plot e 10")))
                                   , Plot(display = Some("Teser"), pens = List(Pen(display = "DeP", setupCode = "plot e 10")))
                                   , Plot(display = Some("Test"), pens = List(Pen(display = "DEP", setupCode = "plot e 10")))))

    val result = PlotConverter.determineMapRenames(PlotConverter.allLocalMapPenNames(model))
    assertResult(Seq())(result)
  }

  test("if only one plot doesn't have duplicate pen name: local traversal"){
    val model = Model(widgets = Seq( View()
                                   , Plot(display = Some("MegaLazer"), pens = List(Pen(display = "DEP", setupCode = "plot e 10")))
                                   , Plot(display = Some("Teser"), pens = List(Pen(display = "DEP", setupCode = "plot e 10")))
                                   , Plot(display = Some("Teser"), pens = List(Pen(display = "DEP", setupCode = "plot e 10"), Pen(display = "DeP", setupCode = "plot e 10")))
                                   , Plot(display = Some("Tesuer"), pens = List(Pen(display = "DEP", setupCode = "plot e 10"), Pen(display = "DEp", setupCode = "plot e 10")))
                                   , Plot(display = Some("Test"), pens = List(Pen(display = "DEP", setupCode = "plot e 10")))))

    val listOfNames = PlotConverter.allLocalMapPenNames(model)
    val result = PlotConverter.determineMapRenames(listOfNames)
    assertResult(Seq(("Teser", Seq(("DeP", "DeP"), ("DEP", "DEP_1")))
                ,("Tesuer", Seq(("DEp", "DEp"), ("DEP", "DEP_1")))))(result)
  }

  test("if only one plot doesn't have duplicate pen name: local traversal with duplicates"){
    val model = Model(widgets = Seq( View()
                                   , Plot(display = Some("MegaLazer"), pens = List(Pen(display = "DEP", setupCode = "plot e 10")))
                                   , Plot(display = Some("Teser"), pens = List(Pen(display = "DEP", setupCode = "plot e 10")))
                                   , Plot(display = Some("Teser"), pens = List(Pen(display = "DEP", setupCode = "plot e 10"), Pen(display = "DEp", setupCode = "plot e 10")))
                                   , Plot(display = Some("Tesuer"), pens = List(Pen(display = "DEP", setupCode = "plot e 10"), Pen(display = "DEp", setupCode = "plot e 10")))
                                   , Plot(display = Some("Test"), pens = List(Pen(display = "DEP", setupCode = "plot e 10")))))

    val listOfNames = PlotConverter.allLocalMapPenNames(model)
    val result = PlotConverter.determineMapRenames(listOfNames)
    assertResult(Seq(("Teser",  Seq(("DEp", "DEp"), ("DEP", "DEP_1")))
                    ,("Tesuer", Seq(("DEp", "DEp"), ("DEP", "DEP_1"))))
    )(result)
  }

  test("if only one plot doesn't have duplicate pen name: local traverse"){
    val model = Model(widgets = Seq( View()
                                   , Plot(display = Some("MegaLazer"), pens = List(Pen(display = "SPORKS", setupCode = "plot e 10"), Pen(display = "sPORKS", setupCode = "plot e 10")))
                                   , Plot(display = Some("Tseser"), pens = List(Pen(display = "SPORKS", setupCode = "plot e 10"), Pen(display = "SPORKS_1", setupCode = "plot e 10")))))

    val listOfNames = PlotConverter.allLocalMapPenNames(model)
    val result = PlotConverter.determineMapRenames(listOfNames)
    assertResult(Seq(("MegaLazer", Seq(("sPORKS", "sPORKS"), ("SPORKS", "SPORKS_1")))))(result)
  }
}
