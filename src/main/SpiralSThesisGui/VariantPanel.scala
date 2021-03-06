package SpiralSThesisGui


import javax.swing.event.ChangeListener

import org.jfree.chart.axis.NumberAxis
import org.jfree.chart.event.{ChartChangeListener, ChartProgressEvent, ChartProgressListener}
import org.jfree.chart.plot._
import org.jfree.chart.renderer.xy._
import org.jfree.chart.{ChartFactory, ChartPanel}
import org.jfree.data.xy._
import org.jfree.ui._

import scala.swing.{Component, Label, _}

/**
  * Created by rayda on 06-Jan-17.
  */
class VariantPanel extends scala.swing.BoxPanel(Orientation.Vertical) with ChangeListener with ChartProgressListener with ChartChangeListener {
  self =>

  import java.awt.Color;
  val xAxis: NumberAxis = new NumberAxis("x Axis")
  val yAxis: NumberAxis = new NumberAxis("y Axis")
  //val renderer:XYSplineRenderer = new XYSplineRenderer();
  val renderer = new XYLineAndShapeRenderer();

  val dataset: XYSeriesCollection = new XYSeriesCollection()

  val series: XYSeries = new XYSeries("jtransform")


  dataset.addSeries(series)

  val chart1 = ChartFactory.createScatterPlot("Performance of Variants",
    "Variant id", "GFlops/s", dataset, PlotOrientation.VERTICAL, true, true, false);


  //val plot: XYPlot = new XYPlot(dataset, xAxis, yAxis, renderer)
  val plot = chart1.getPlot().asInstanceOf[XYPlot];

  plot.setBackgroundPaint(Color.lightGray);
  plot.setDomainGridlinePaint(Color.white);
  plot.setRangeGridlinePaint(Color.white);

  plot.setDomainCrosshairVisible(true)
  plot.setRangeCrosshairVisible(true)
  plot.setAxisOffset(new RectangleInsets(4, 4, 4, 4));

  //val chart = scalax.chart.XYChart(plot, title = "", legend = true)


  chart1.addChangeListener(this)
  chart1.addProgressListener(this)

  val chartpanel = new ChartPanel(chart1)
  val label = new Label("nothing smart yet")
  //override val contents = Vector(chartpanel).toSeq
  contents.append(Component.wrap(chartpanel))
  contents.append(label)

  def stateChanged(ev: javax.swing.event.ChangeEvent): Unit = {
    println(ev)
  }

  def chartChanged(ev: org.jfree.chart.event.ChartChangeEvent): Unit = {
    println(ev)
    //update something
    /*val xx = plot.getDomainCrosshairValue()
    val yy = plot.getRangeCrosshairValue()
    label.text_=(s"x = $xx y = $yy")*/
  }

  def chartProgress(event: ChartProgressEvent): Unit = {
    if (event.getType() != ChartProgressEvent.DRAWING_FINISHED) {
      return;
    } else {
      //update something
      val xx = plot.getDomainCrosshairValue()
      val yy = plot.getRangeCrosshairValue()
      label.text_=(s"x = $xx y = $yy")
    }
  }

}