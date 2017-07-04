package SpiralSThesisGui

import scala.swing.SimpleSwingApplication
import java.io._
import javax.swing.{JPanel, JSlider}
import javax.swing.event.ChangeListener

import scala.swing._
import scalaswingcontrib.event.TreeNodeSelected
import scalaswingcontrib.tree.{InternalTreeModel, Tree}
import scala.xml.{Node, XML}
import scala.swing.{Action, BorderPanel, Button, Component, Dimension, GridPanel, Label, MainFrame, ScrollPane, SimpleSwingApplication, Swing, TabbedPane}
import Swing.{Icon, pair2Dimension}
import scalaswingcontrib.tree.{ExternalTreeModel, InternalTreeModel, Tree, TreeModel}
import scalaswingcontrib.event.TreeNodeSelected
import scala.collection.mutable
import Tree.{Editor, Renderer}
import scala.swing._
import scala.swing.event._
import scala.swing.Swing._
import scala.swing.ListView._
import scife.enumeration.dependent.Depend
import scife.enumeration.{dependent, memoization}
import dependent._
import memoization._
import org.jfree.chart.{ChartFactory, ChartPanel}
import org.jfree.chart.axis.NumberAxis
import org.jfree.chart.event.{ChartChangeListener, ChartProgressEvent, ChartProgressListener}
import org.jfree.chart.plot.PlotOrientation
import org.jfree.chart.renderer.xy.{XYLineAndShapeRenderer, XYSplineRenderer}

import scala.swing.TabbedPane.Page
import TabbedPane.Page
import BorderPanel.Position._
import scalax.chart.module.XYChartFactories
import SpiralSThesis._

/**
  * Created by rayda on 05-Jan-17.
  */
object GuiThesis extends EnumTree with scalax.chart.module.Charting {

  var defradix: scala.collection.mutable.Map[Int, Int] = scala.collection.mutable.Map(
    4 -> 2
    , 8 -> 2
    , 16 -> 2
    , 32 -> 2
    , 64 -> 2
    , 128 -> 2
    , 256 -> 2
    , 512 -> 2
    , 1024 -> 2
    , 2048 -> 2
    , 4096 -> 2
    , 8192 -> 2
    , 16384 -> 2
    , 32768 -> 2
    , 65536 -> 2
  ).withDefaultValue(256)

  /*
   scala.collection.mutable.Map(
        4 -> 2
      , 8 -> 4
      , 16 -> 4
      , 32 -> 16
      , 64 -> 16
      , 128 -> 16
      , 256 -> 16
      , 512 -> 16
      , 1024 -> 16
      , 2048 -> 16
      , 4096 -> 16
      , 8192 -> 16
      , 16384 -> 256
      , 32768 -> 256
      , 65536 -> 256
    ).withDefaultValue(256)

   */
  object SmallEnum {
    var basecase_min = 4
    var basecase_max = 8
    var basecase_default = 0
    val default_dft_size = 3

    val radix_enum = BreakDown.RadixFreedom()

    //2^n
    var cur_dft_size = default_dft_size

    var breakdown_enum = BreakDown.getBreakdown(Some(basecase_min, basecase_max), basecase_default, None)
    var dft_variants = radix_enum(8)
    //breakdown_enum((Math.pow(2, cur_dft_size).toInt, false))
    var cur_variant = dft_variants(0)


    val radio_dft = new RadioButton("DFT")
    val radio_wht = new RadioButton("WHT")
    val mutex_transformtype = new ButtonGroup(radio_dft, radio_wht)

    val checkbox_threading = new CheckBox("Threading")

    val boxpanel_parallelism = new BoxPanel(Orientation.Horizontal) {
      border = CompoundBorder(TitledBorder(EtchedBorder, "Parallelism"), EmptyBorder(5, 5, 5, 10))
      contents += checkbox_threading
    }


    val radio_format_complex = new RadioButton("Complex Class")
    val radio_format_interleaved = new RadioButton("Interleaved Complex")
    val radio_format_splitcomplex = new RadioButton("Split Complex")
    val mutex_dataformat = new ButtonGroup(radio_format_complex, radio_format_interleaved) //, radio_format_splitcomplex)


    val boxpanel_dataformat = new BoxPanel(Orientation.Horizontal) {
      border = CompoundBorder(TitledBorder(EtchedBorder, "Data layout"), EmptyBorder(5, 5, 5, 10))
      contents ++= mutex_dataformat.buttons
    }


    val radio_stat_size = new RadioButton("Static Input size")
    val radio_dyn_size = new RadioButton("General Input size")
    val mutex_size = new ButtonGroup(radio_stat_size, radio_dyn_size)


    val boxpanel_statvdyn = new BoxPanel(Orientation.Horizontal) {
      border = CompoundBorder(TitledBorder(EtchedBorder, "Fixed vs general sized input"), EmptyBorder(5, 5, 5, 10))
      contents ++= mutex_size.buttons
    }


    /*val textfield_basecase_default = new TextField {
      text = basecase_default.toString
      horizontalAlignment = Alignment.Left
    }*/

    val slider_basecase_default = new Slider() {
      min = 1
      value = 3
      max = 6
      majorTickSpacing = 1
      paintLabels = true
      paintTicks = true
      orientation = Orientation.Vertical
    }

    val boxpanel_basecase_default = new BoxPanel(Orientation.Vertical) {
      border = CompoundBorder(TitledBorder(EtchedBorder, "Default base case size 2^ ( ) "), EmptyBorder(5, 5, 5, 10))
      contents.append(slider_basecase_default)
    }


    val radio_twiddle_onthefly = new RadioButton("Compute on the fly")
    val radio_twiddle_precompute = new RadioButton("Precompute")
    val mutex_twiddle_default = new ButtonGroup(radio_twiddle_onthefly, radio_twiddle_precompute)

    val checkbox_twiddle_inline = new CheckBox("Inline Twiddles @ unrolled code")

    val boxpanel_twiddle = new BoxPanel(Orientation.Horizontal) {
      border = CompoundBorder(TitledBorder(EtchedBorder, "Twiddle config"), EmptyBorder(5, 5, 5, 10))
      contents.append(checkbox_twiddle_inline)
      contents ++= mutex_twiddle_default.buttons
    }


    val checkbox_validate = new CheckBox("Validate Code ")
    val checkbox_inplace = new CheckBox("Inplace")

    val leftconfig = new BoxPanel(Orientation.Horizontal) {
      contents.append(boxpanel_statvdyn, boxpanel_dataformat, boxpanel_parallelism, checkbox_validate, checkbox_inplace)
    }

    val radix_sliders = makeRadixSliders(16)

    val rightconfig = new BoxPanel(Orientation.Horizontal) {
      contents.append(
        new BoxPanel(Orientation.Vertical) {
          contents.append(boxpanel_basecase_default, boxpanel_twiddle)
        })
      radix_sliders.zipWithIndex.map(p => {
        //contents.append(new Label(s"Radix 2^( ) for ${Math.pow(2,p._2+3)}"))
        contents.append(new BoxPanel(Orientation.Vertical) {
          contents.append(new Label(s"${p._2 + 3}"))
          contents.append(p._1)
        })
      })

    }

    val config = new BoxPanel(Orientation.Vertical) {
      border = CompoundBorder(TitledBorder(EtchedBorder, "Config"), EmptyBorder(5, 5, 5, 10))
      contents.append(leftconfig, rightconfig)
    }

    def makeRadixSliders(max: Int = 16): Vector[Slider] = {
      val t = for (i <- 3 to max) yield
        new Slider() {
          min = 1
          value = 1
          max = i - 1
          majorTickSpacing = 1
          paintLabels = true
          paintTicks = true
          orientation = Orientation.Vertical
        }
      t.toVector
    }

    //Radix Choices

    //-------------------------------------------------Variants
    val dft_size =
      new Slider() {
        min = 1
        value = default_dft_size
        max = 18
        majorTickSpacing = 1
        paintLabels = true
        paintTicks = true
      }
    val slider_variant =
      new Slider() {
        min = 0
        value = 0
        max = dft_variants.size - 1
        majorTickSpacing = (dft_variants.size - 1) / 10
        paintLabels = true
        paintTicks = true
      }

    //val displaytree = getInternalBreakdownTree(dft_variants(0))


    //val scpanel = new ScrollPane(displaytree)
    //scpanel.preferredSize_=((200, 768): Dimension)


    //val data = for (i <- 1 to 5) yield (i, i)
    //val chart = XYLineChart(data)

    val variantplot = new VariantPanel()


    val plotting = new BoxPanel(Orientation.Horizontal) {

      contents.append(variantplot)
    }


    val variants = new BoxPanel(Orientation.Vertical) {
      contents.append(new Label("DFT size"))
      contents.append(dft_size)
      contents.append(new Label("DFT breakdown variant #"))
      contents.append(slider_variant)
      contents.append(plotting)

    }


    //-----------------------------------------------Buttons




    def jtransform2(): Double = {
      import org.jtransforms.fft.DoubleFFT_1D
      import org.jtransforms.utils.{CommonUtils, IOUtils}
      import org.scalameter._

      var sizes1D: Array[Long] = Array(Math.pow(2, cur_dft_size).toLong)
      var nsize: Int = sizes1D.size
      var niter: Int = 100;
      var x: Array[Double] = null
      val doWarmup: Boolean = true
      val times_without_constructor = new Array[Double](nsize)
      val times_with_constructor = new Array[Double](nsize)

      var fft = new DoubleFFT_1D(sizes1D(0))
      val i = 0
      val t = for (j <- 0 until 10) yield {
        val repeats: Int = 10000

        val standardConfig = org.scalameter.config(
          Key.exec.minWarmupRuns -> 100,
          Key.exec.maxWarmupRuns -> 1000,
          Key.exec.benchRuns -> repeats, //(1000*1000/sizes1D(i)).toLong,
          Key.verbose -> false
        ) withWarmer (new Warmer.Default)
        println("starting measurment")

        var elapsedTime = System.nanoTime

        x = new Array[Double]((2 * sizes1D(i)).toInt)
        IOUtils.fillMatrix_1D(2 * sizes1D(i), x)
        var min_time = standardConfig measure {
          fft.complexForward(x)
        }
        IOUtils.fillMatrix_1D(2 * sizes1D(i), x)


        val n = sizes1D(i)
        val flops: Double = 5 * n * (Math.log10(n) / Math.log10(2.0))
        val y: Double = ((flops / (min_time * 1000000)))
        println("adding flops" + flops + "/ " + min_time + " -> " + y)


        x = null
        y
      }
      t.max
    }

    def ms2gflops(d: Double): Double = {
      val n = Math.pow(2, cur_dft_size)
      val flops = 5 * n * (Math.log10(n) / Math.log10(2))
      val y: Double = ((flops / (d)))
      y
    }


    def makecode(): CorewGlue = {
      new CorewGlue(cur_dft_size,
        defradix.toMap.withDefaultValue(defradix(3)),
        if (radio_dyn_size.selected) None else Some((Math.pow(2, cur_dft_size).toInt)),
        radio_format_interleaved.selected,
        checkbox_threading.selected,
        Math.pow(2, slider_basecase_default.value).toInt,
        //textfield_basecase_default.text.toInt,
        checkbox_twiddle_inline.selected,
        radio_twiddle_precompute.selected,
        checkbox_validate.selected,
        checkbox_inplace.selected

        //checkbox_default_config.selected
      )
    }

    val buttons = new FlowPanel {
      border = Swing.EmptyBorder(5, 5, 5, 5)
      contents += new Button(Action("Generate Code") {
        val dsl = makecode()
        dsl.codeexport()
        //dsl.codeexport_java()
      })

      contents += new Button(Action("gen timing") {
        val t = new Thread(new Runnable {
          def run() {
            val series: XYSeries = new XYSeries(s"basecase 8")
            variantplot.dataset.addSeries(series)
            val series2: XYSeries = new XYSeries(s"base case 16")
            variantplot.dataset.addSeries(series2)
            val series3: XYSeries = new XYSeries(s"basecase 32")
            variantplot.dataset.addSeries(series3)
            val series4: XYSeries = new XYSeries(s"base case 64")
            variantplot.dataset.addSeries(series4)
            var curradix = Map(2048 -> 16, 128 -> 8, 32 -> 2, 8 -> 4, 4096 -> 64, 256 -> 16, 1024 -> 2, 64 -> 4, 4 -> 2, 16 -> 4, 8192 -> 128, 512 -> 2)

            val max = 30


            val base8 = new Array[Double](max);
            val base16 = new Array[Double](max);
            val base32 = new Array[Double](max);
            val base64 = new Array[Double](max);



            for (size <- 2 until 30) {
              println(s"Size: $size")
              val fsize = Math.pow(2, size).toInt


              val trypar = if (fsize < 4096) false else true
              def xms2gflops(d: Double): Double = {
                val n = Math.pow(2, size)
                val flops = 5 * n * (Math.log10(n) / Math.log10(2))
                val y: Double = ((flops / (d)))
                y
              }

              for (basesize <- Vector(8,16,32,64)) {
                for (gen <- Vector(true)) {
                  val dsl = new CorewGlue(
                    testsize = size,
                    radix_choice = curradix.withDefaultValue(16),
                    static_size = if (gen) None else Some(fsize),
                    interleaved = true,
                    thread = trypar, //threading
                    base_default = basesize,
                    twid_inline = true,
                    twid_default_precomp = true,
                    validate = true,
                    inplace = false
                    //checkbox_default_config.selected
                  )

                  var elapsedTime = System.nanoTime
                  val f = dsl.codeexport()
                  elapsedTime = System.nanoTime - elapsedTime

                  basesize match{
                    case 8 => base8(size) = elapsedTime
                    case 16 => base16(size) = elapsedTime
                    case 32 => base32(size) = elapsedTime
                    case 64 => base64(size) = elapsedTime
                  }

                }
              }
            }
            println(scala.runtime.ScalaRunTime.stringOf(base8))
            println(scala.runtime.ScalaRunTime.stringOf(base16))
            println(scala.runtime.ScalaRunTime.stringOf(base32))
            println(scala.runtime.ScalaRunTime.stringOf(base64))
          }
        })
        t.start()

      })

      contents += new Button(Action("best found") {
        val t = new Thread(new Runnable {
          def run() {
            val series: XYSeries = new XYSeries(s"General Size basecase 16")
            variantplot.dataset.addSeries(series)
            val series2: XYSeries = new XYSeries(s"Fixed Size base case 16")
            variantplot.dataset.addSeries(series2)
            val series3: XYSeries = new XYSeries(s"General Size basecase 32")
            variantplot.dataset.addSeries(series3)
            val series4: XYSeries = new XYSeries(s"Fixed Size base case 32")
            variantplot.dataset.addSeries(series4)
            //var curradix = Map(2048 -> 16, 128 -> 8, 32 -> 2, 8 -> 4, 4096 -> 64, 256 -> 16, 1024 -> 2, 64 -> 4, 4 -> 2, 16 -> 4, 8192 -> 128, 512 -> 2)
            var curradix = Map(128 -> 2, 2048 -> 8, 32 -> 8, 8 -> 4, 256 -> 16, 4096 -> 64, 16384 -> 64, 64 -> 4, 1024 -> 2, 4 -> 2, 16 -> 4, 512 -> 2, 8192 -> 512, 32768 -> 2048).withDefaultValue(2048)

            var parsize = 4096

            val gen16 = new Array[Double](20);
            val stat16 = new Array[Double](20);
            val gen32 = new Array[Double](20);
            val stat32 = new Array[Double](20);



            for (size <- 2 until 17) {
              println(s"Size: $size")
              val fsize = Math.pow(2, size).toInt


              val trypar = if (fsize < 4096) false else true
              def xms2gflops(d: Double): Double = {
                val n = Math.pow(2, size)
                val flops = 5 * n * (Math.log10(n) / Math.log10(2))
                val y: Double = ((flops / (d)))
                y
              }

              for (basesize <- Vector(32)) {
                for (gen <- Vector(true,false)) {
                  val dsl = new CorewGlue(
                    testsize = size,
                    radix_choice = curradix.withDefaultValue(16),
                    static_size = if (gen) None else Some(fsize),
                    interleaved = true,
                    thread = trypar, //threading
                    base_default = basesize,
                    twid_inline = true,
                    twid_default_precomp = true,
                    validate = true,
                    inplace = false
                    //checkbox_default_config.selected
                  )
                  val f = dsl.compile()
                  val perf = f();

                  if (basesize == 16) {
                    if (!gen) {
                      stat16(size) = xms2gflops(perf)
                      series2.add(size, xms2gflops(perf))
                    }
                    else {
                      gen16(size) = xms2gflops(perf)
                      series.add(size, xms2gflops(perf))
                    }
                  } else if (!gen) {
                    stat32(size) = xms2gflops(perf)
                    series4.add(size, xms2gflops(perf))
                  }
                  else {
                    gen32(size) = xms2gflops(perf)
                    series3.add(size, xms2gflops(perf))
                  }

                }
              }
            }
            println(scala.runtime.ScalaRunTime.stringOf(gen16))
            println(scala.runtime.ScalaRunTime.stringOf(stat16))
            println(scala.runtime.ScalaRunTime.stringOf(gen32))
            println(scala.runtime.ScalaRunTime.stringOf(stat32))
          }
        })
        t.start()

      })

      contents += new Button(Action("DynProgramming") {
        val t = new Thread(new Runnable {
          def run() {
            val series: XYSeries = new XYSeries(s"DynProg sequential")
            variantplot.dataset.addSeries(series)
            val series2: XYSeries = new XYSeries(s"DynProg parallel")
            variantplot.dataset.addSeries(series2)
            var curradix: scala.collection.mutable.Map[Int, Int] = scala.collection.mutable.Map(
              4 -> 2
              , 8 -> 4
              , 16 -> 4
            )

            var parsize = 0
            for (size <- 3 until 20) {
              println(s"Size: $size")
              val fsize = Math.pow(2, size).toInt
              val pairs = Bla.DivisorPairs(fsize)
              var alwaysparnow = false

              val trypar = if (size < 10) Vector(false) else if (alwaysparnow) Vector(true) else Vector(false,true)
              def xms2gflops(d: Double): Double = {
                val n = Math.pow(2, size)
                val flops = 5 * n * (Math.log10(n) / Math.log10(2))
                val y: Double = ((flops / (d)))
                y
              }
              for (par <- trypar) {
                val perfs = pairs.foldLeft(Map.empty[Double, (Int,Boolean)])(
                  (acc, rchoice) => {
                    curradix(fsize) = rchoice._1
                    val dsl = new CorewGlue(
                      testsize = size,
                      radix_choice = curradix.toMap.withDefaultValue(8),
                      static_size = None,//if (fsize < 512) Some(fsize) else None, //None else Some(fsize),
                      interleaved = true,
                      thread = par, //threading
                      base_default = 32,
                      twid_inline = true,
                      twid_default_precomp = true,
                      validate = false,
                      inplace = false
                      //checkbox_default_config.selected
                    )
                    val f = dsl.compile()
                    val perf = f();


                    if (par)
                      series2.add(size, xms2gflops(perf))
                    else
                      series.add(size, xms2gflops(perf))
                    println(s" time ${perf} performance ${xms2gflops(perf)} ${rchoice._1} $par")
                    acc + (perf -> (rchoice._1, par))
                  })
                curradix(fsize) = perfs(perfs.keySet.min)._1

                if (perfs(perfs.keySet.min)._2) {
                  alwaysparnow = true
                  if (alwaysparnow)parsize = fsize
                }
                println(s"selected radix ${curradix(fsize)}")
                println(curradix)
              }
            }
            println(s"parallelize at $parsize")
          }
        })
        t.start()

      })


      contents += new Button(Action("CT vs split") {
        val t = new Thread(new Runnable {
          def run() {
            val series: XYSeries = new XYSeries(s"Split Radix")
            variantplot.dataset.addSeries(series)
            val series2: XYSeries = new XYSeries(s"CT")
            variantplot.dataset.addSeries(series2)
            var curradix: scala.collection.mutable.Map[Int, Int] = scala.collection.mutable.Map(
              4 -> 2
              , 8 -> 4
              , 16 -> 4
            )

            var parsize = 0
            for (size <- 2 until 7) {
              println(s"Size: $size")
              val fsize = Math.pow(2, size).toInt
              val pairs = Bla.DivisorPairs(fsize)
              var alwaysparnow = false

              val trypar = Vector(false,true)
              def xms2gflops(d: Double): Double = {
                val n = Math.pow(2, size)
                val flops = 5 * n * (Math.log10(n) / Math.log10(2))
                val y: Double = ((flops / (d)))
                y
              }
              for (par <- trypar) {
                val perfs = pairs.foldLeft(Map.empty[Double, (Int,Boolean)])(
                  (acc, rchoice) => {
                    curradix(fsize) = rchoice._1
                    val dsl = new CorewGlue(
                      testsize = size,
                      radix_choice = curradix.toMap.withDefaultValue(8),
                      static_size = Some(fsize),
                      interleaved = true,
                      thread = false, //threading
                      base_default = 64,
                      twid_inline = true,
                      twid_default_precomp = true,
                      validate = true,
                      inplace = false,
                      splitradix = par
                      //checkbox_default_config.selected
                    )
                    val f = dsl.compile()
                    val perf = f();


                    if (!par)
                      series2.add(size, xms2gflops(perf))
                    else
                      series.add(size, xms2gflops(perf))
                    println(s" time ${perf} performance ${xms2gflops(perf)} ${rchoice._1} $par")
                    acc + (perf -> (rchoice._1, par))
                  })
                curradix(fsize) = perfs(perfs.keySet.min)._1

                if (perfs(perfs.keySet.min)._2) {
                  alwaysparnow = true
                  if (alwaysparnow)parsize = fsize
                }
                println(s"selected radix ${curradix(fsize)}")
                println(curradix)
              }
            }
            println(s"parallelize at $parsize")
          }
        })
        t.start()

      })

      contents += new Button(Action("Time JTransform") {
        val t = new Thread(new Runnable {
          def run() {
            val series: XYSeries = new XYSeries(s"JTransform")
            val array = new Array[Double](20)
            variantplot.dataset.addSeries(series)
            for (j <- 2 until 20) {
              cur_dft_size = j
              val gflops = jtransform2()
              //variantplot.series.add(0.0,gflops)
              array(j) = gflops
              series.add(j, gflops)
            }
            println(scala.runtime.ScalaRunTime.stringOf(array))
          }
        })
        t.start()

      })
      contents += new Button(Action("Generate and Time Code") {
        val t = new Thread(new Runnable {
          def run() {
            val dsl = makecode()
            val f = dsl.compile()
            val perf = f();
            variantplot.series.add(-1.0, ms2gflops(perf))

          }
        })
        t.start()

      })
      contents += new Button {
        text = "Gen and time all"
        reactions += {
          case ButtonClicked(_) => {
            val t = new Thread(new Runnable {
              def run() {
                val series: XYSeries = new XYSeries(s"${slider_basecase_default.value}")
                variantplot.dataset.addSeries(series)
                for (i <- 0 until dft_variants.size) {

                  println("Variant " + i + " of " + dft_variants.size)
                  println(defradix)
                  slider_variant.value_=(i)
                  val dsl = makecode()
                  val f = dsl.compile()
                  val perf = f();

                  series.add(i, ms2gflops(perf))
                }
              }
            })
            t.start()
          }
        }
      }
      contents += new Button {
        text = "Clear Plot"
        reactions += {
          case ButtonClicked(_) => {
            variantplot.series.clear()
            variantplot.plot.clearRangeMarkers()
          }
        }
      }


    }

    ///////////////////////////////////// DEFAULT Config

    radio_dyn_size.selected_=(true)
    radio_stat_size.selected_=(false)
    radio_wht.selected_=(false)
    radio_dft.selected_=(true)
    radio_format_complex.selected_=(false);
    radio_format_interleaved.selected_=(true)

    checkbox_validate.selected_=(true)
    checkbox_inplace.selected_=(false)
    checkbox_twiddle_inline.selected_=(true)
    radio_twiddle_precompute.selected_=(true);
    radio_twiddle_onthefly.selected_=(false)
    checkbox_threading.selected_=(false)


    //Refresh the tree
    breakdown_enum = BreakDown.getBreakdown(None, basecase_default, None)
    //dft_variants = breakdown_enum((Math.pow(2, default_dft_size).toInt, false))
    cur_variant = dft_variants(0)
    //scpanel.viewportView_=(getInternalBreakdownTree(cur_variant))

    slider_variant.max_=(dft_variants.size - 1)
    if (dft_variants.size < 20) {
      slider_variant.majorTickSpacing_=(if ((dft_variants.size - 1) < 5) dft_variants.size - 1 else 5)
      slider_variant.minorTickSpacing_=(1)
    } else {
      slider_variant.majorTickSpacing_=((dft_variants.size - 1) / 10)
      slider_variant.minorTickSpacing_=((dft_variants.size - 1) / 5)
    }

    slider_variant.paintLabels_=(true)
    slider_variant.paintTicks_=(true)


    val smallenum = new Page("Exhaustive enumeration for small sizes",
      new BoxPanel(Orientation.Vertical) {
        contents.append(config)
        contents.append(variants)
        contents.append(buttons)

        listenTo(slider_variant)
        listenTo(dft_size)
        radix_sliders.map(s => listenTo(s))
        reactions += {
          case ValueChanged(`slider_variant`) => {
            cur_variant = dft_variants(slider_variant.value)
            cur_variant.zipWithIndex.map(p => radix_sliders(p._2).value_=(Integer.numberOfTrailingZeros(p._1)))
            //val newtree = getInternalBreakdownTree(cur_variant)
            //scpanel.viewportView_=(newtree)
          }
          case ValueChanged(`dft_size`) => {
            cur_dft_size = dft_size.value
            dft_variants = radix_enum(dft_size.value)
            slider_variant.paintLabels_=(false)
            slider_variant.paintTicks_=(false)


            slider_variant.max_=(dft_variants.size - 1)
            if (dft_variants.size < 20) {
              slider_variant.majorTickSpacing_=(if ((dft_variants.size - 1) < 5) dft_variants.size - 1 else 5)
              slider_variant.minorTickSpacing_=(1)
              slider_variant.peer.setLabelTable(slider_variant.peer.createStandardLabels(1));
            } else {
              slider_variant.majorTickSpacing_=((dft_variants.size - 1) / 10)
              slider_variant.minorTickSpacing_=((dft_variants.size - 1) / 5)
              slider_variant.peer.setLabelTable(slider_variant.peer.createStandardLabels(slider_variant.peer.getMajorTickSpacing()));
            }

            slider_variant.paintLabels_=(true)
            slider_variant.paintTicks_=(true)
            slider_variant.value_=(0)
          }
        }
        radix_sliders.zipWithIndex.map(p => {
          val (slider, nr) = p
          reactions += {
            case ValueChanged(`slider`) => {
              defradix.update(Math.pow(2, nr + 3).toInt, Math.pow(2, slider.value).toInt)
            }
          }
        })

      }

    )


  }


  /*


            val nlogn = size * Math.log10(size) / Math.log10(size)

            val perf: Double = nlogn/endtime

            println(s"GFlops: $perf")
   */




  //Heuristic.jtransform()


  def top = new MainFrame {
    title = "SpiralS"
    size = (2 * 1024, 2 * 768): Dimension

    contents = new TabbedPane {
      pages += SmallEnum.smallenum

    }
  }


}
