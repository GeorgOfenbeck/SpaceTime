package Filter2


import org.scala_lang.virtualized.SourceContext
import scala.lms._
import scala.lms.internal._
import scala.lms.ops._
import scala.lms.targets.graphviz.GraphVizExport
import scala.lms.targets.scalalike._

class Core extends FilterHeader {
  self =>
  val emitGraph = new GraphVizExport {
    override val IR: self.type = self
  }
  override val codegen = new ScalaCodegen with EmitHeadInternalFunctionAsClass with ScalaGenPrimitivOps with ScalaGenSort_DSL with ScalaGenBooleanOps with ScalaGenIfThenElse {
    val IR: self.type = self
  }

  case class MaybeSFunction(f: Either[StagedFunction[DynFilterHeader, Rep[ImageH]], DynFilterHeader => Rep[ImageH]]) {
    def apply(dyn: DynFilterHeader): Rep[ImageH] = f.fold(fa => fa(dyn), fb => fb(dyn))
  }

  object MaybeSFunction {
    def apply(f: StagedFunction[DynFilterHeader, Rep[ImageH]]): MaybeSFunction = MaybeSFunction(Left(f))

    def apply(f: (DynFilterHeader => Rep[ImageH])): MaybeSFunction = MaybeSFunction(Right(f))
  }

  def compute_type_symetrie(stat: StatFilterHeader): StatFilterHeader = ???


  def filter_core(mix: MixFilterHeader, xindex: Exp[Int], yindex: Exp[Int], outimg: Rep[ImageH]): Rep[ImageH] = {
    import mix._

    val ina: Rep[Int] = {
      import matrix.r1.c1._
      val z = evnum.zero
      a match {
        case `z` => Const(0)
        case _ => getImage[Int](image_in, int_minus(xindex, Const(1)), int_minus(yindex, Const(1)))
      }
    }
    val inb: Rep[Int] = {
      import matrix.r1.c2._
      val z = evnum.zero
      a match {
        case `z` => Const(0)
        case _ => getImage[Int](image_in, xindex, int_minus(yindex, Const(1)))
      }
    }
    val inc: Rep[Int] = {
      import matrix.r1.c3._
      val z = evnum.zero
      a match {
        case `z` => Const(0)
        case _ => getImage[Int](image_in, int_plus(xindex, Const(1)), int_minus(yindex, Const(1)))
      }
    }
    val ind: Rep[Int] = {
      import matrix.r2.c1._
      val z = evnum.zero
      a match {
        case `z` => Const(0)
        case _ => getImage[Int](image_in, int_minus(xindex, Const(1)), yindex)
      }
    }
    val ine: Rep[Int] = {
      import matrix.r2.c2._
      val z = evnum.zero
      a match {
        case `z` => Const(0)
        case _ => getImage[Int](image_in, xindex, yindex)
      }
    }
    val inf: Rep[Int] = {
      import matrix.r2.c3._
      val z = evnum.zero
      a match {
        case `z` => Const(0)
        case _ => getImage[Int](image_in, int_plus(xindex, Const(1)), yindex)
      }
    }
    val ing: Rep[Int] = {
      import matrix.r3.c1._
      val z = evnum.zero
      a match {
        case `z` => Const(0)
        case _ => getImage[Int](image_in, int_minus(xindex, Const(1)), int_plus(yindex, Const(1)))
      }
    }
    val inh: Rep[Int] = {
      import matrix.r3.c2._
      val z = evnum.zero
      a match {
        case `z` => Const(0)
        case _ => getImage[Int](image_in, xindex, int_plus(yindex, Const(1)))
      }
    }
    val ini: Rep[Int] = {
      import matrix.r3.c3._
      val z = evnum.zero
      a match {
        case `z` => Const(0)
        case _ => getImage[Int](image_in, int_plus(xindex, Const(1)), int_plus(yindex, Const(1)))
      }
    }


    /*int rgb = inPixels[ioffset+ix];
    a += f * ((rgb >> 24) & 0xff);
    r += f * ((rgb >> 16) & 0xff);
    g += f * ((rgb >> 8) & 0xff);
    b += f * (rgb & 0xff);*/

    def short(o: OneEntry, in: Exp[Int]) = {
      import o._
      implicit val nev = evnum
      implicit val tev = evtyp
      val pixel = in
      val pa = pixelgetAlpha(pixel)
      val pr = pixelgetRed(pixel)
      val pg = pixelgetGreen(pixel)
      val pb = pixelgetBlue(pixel)
      val ra = toDouble(gentimes[T](fromInt(pa), ev.toRep(a)))
      val rr = toDouble(gentimes[T](fromInt(pr), ev.toRep(a)))
      val rg = toDouble(gentimes[T](fromInt(pg), ev.toRep(a)))
      val rb = toDouble(gentimes[T](fromInt(pb), ev.toRep(a)))
      (ra, rr, rg, rb)
    }



    val (aa, ar, ag, ab) = short(matrix.r1.c1, ina)
    val (ba, br, bg, bb) = short(matrix.r1.c2, inb)
    val (ca, cr, cg, cb) = short(matrix.r1.c3, inc)
    val (da, dr, dg, db) = short(matrix.r2.c1, ind)
    val (ea, er, eg, eb) = short(matrix.r2.c2, ine)
    val (fa, fr, fg, fb) = short(matrix.r2.c3, inf)
    val (ga, gr, gg, gb) = short(matrix.r3.c1, ing)
    val (ha, hr, hg, hb) = short(matrix.r3.c2, inh)
    val (ia, ir, ig, ib) = short(matrix.r3.c3, ini)

    def wcast(o: OneEntry, ids: Vector[Int]) = {
      import o._
      implicit val nev = evnum
      implicit val tev = evtyp

      val mapped = ids.map(id => {
        val pixel = id2inint(id)
        val pa = pixelgetAlpha(pixel)
        val pr = pixelgetRed(pixel)
        val pg = pixelgetGreen(pixel)
        val pb = pixelgetBlue(pixel)
        (pa, pr, pg, pb)
      })
      val summed = mapped.reduce((l, r) => {
        val (la, lr, lg, lb) = l
        val (ra, rr, rg, rb) = l
        (genplus(la, ra), genplus(lr, rr), genplus(lg, rg), genplus(lb, rb))
      })
      summed
    }

    def id2oentry(id: Int): OneEntry = id match {
      case 1 => matrix.r1.c1
      case 2 => matrix.r1.c2
      case 3 => matrix.r1.c3
      case 4 => matrix.r2.c1
      case 5 => matrix.r2.c2
      case 6 => matrix.r2.c3
      case 7 => matrix.r3.c1
      case 8 => matrix.r3.c2
      case 9 => matrix.r3.c3
    }

    def id2inint(id: Int): Rep[Int] = id match {
      case 1 => ina
      case 2 => inb
      case 3 => inc
      case 4 => ind
      case 5 => ine
      case 6 => inf
      case 7 => ing
      case 8 => inh
      case 9 => ini
    }

    val symsum = mix.sym.valuesym.map(p => {
      val (facid, inids) = p
      val (ga, gr, gg, gb) = wcast(id2oentry(facid), inids)
      (ga, gr, gg, gb, id2oentry(facid))
    })

    val scaledsymsum = symsum.map(p => {
      val (la, lr, lg, lb, o) = p
      import o._
      implicit val nev = evnum
      implicit val tev = evtyp
      val ra = gentimes[T](fromInt(la), ev.toRep(a))
      val rr = gentimes[T](fromInt(lr), ev.toRep(a))
      val rg = gentimes[T](fromInt(lg), ev.toRep(a))
      val rb = gentimes[T](fromInt(lb), ev.toRep(a))
      (ra, rr, rg, rb, o)
    })
    val orderbytyp = scaledsymsum.foldLeft(Map.empty[Numeric[_], Vector[(Exp[_], Exp[_], Exp[_], Exp[_], OneEntry)]]) {
      (acc, ele) => {
        val (la, lr, lg, lb, o) = ele

        if (acc.contains(o.evnum))
          acc + (o.evnum -> (acc(o.evnum) ++ Vector((la, lr, lg, lb, o))))
        else
          acc + (o.evnum -> Vector((la, lr, lg, lb, o)))
      }
    }

    val sumbytype = orderbytyp.map(p => {
      val (num, ele) = p
      ele.reduce((l, r) => {
        val o = l._5
        val (la, lr, lg, lb, lo) = l.asInstanceOf[(Exp[o.T], Exp[o.T], Exp[o.T], Exp[o.T], OneEntry)]
        val (ra, rr, rg, rb, ro) = r.asInstanceOf[(Exp[o.T], Exp[o.T], Exp[o.T], Exp[o.T], OneEntry)]
        import o._
        implicit val nev = evnum
        implicit val tev = evtyp
        val res = (genplus(la, ra), genplus(lr, rr), genplus(lg, rg), genplus(lb, rb), o)
        res.asInstanceOf[(Exp[_], Exp[_], Exp[_], Exp[_], OneEntry)]
      })
    })

    val dzero = Const(implicitly[Numeric[Double]].zero)
    val sumtotal = sumbytype.foldLeft((dzero, dzero, dzero, dzero))(
      (acc, ele) => {
        val o = ele._5
        import o._
        val (la, lr, lg, lb, lo) = ele.asInstanceOf[(Exp[o.T], Exp[o.T], Exp[o.T], Exp[o.T], OneEntry)]
        implicit val nev = evnum
        implicit val tev = evtyp
        val (ra, rr, rg, rb) = acc
        val res = (genplus[Double](toDouble(la), ra), genplus[Double](toDouble(lr), rr), genplus[Double](toDouble(lg), rg), genplus[Double](toDouble(lb), rb))
        /*val norm = Const(0.11111111111d)
        val res = (gentimes(genplus[Double](toDouble(la), ra),norm), gentimes(genplus[Double](toDouble(lr), rr),norm), gentimes(genplus[Double](toDouble(lg), rg),norm), gentimes(genplus[Double](toDouble(lb), rb),norm))*/
        res
      })
    val (suma, sumr, sumg, sumb) = sumtotal
    val outPixel = combinePixel(suma, sumr, sumg, sumb)

    val out = setImage(outimg, xindex, yindex, outPixel)
    out
  }


  def multiply_core(stat: StatFilterHeader): MaybeSFunction = {
    val exposarg: ExposeRep[DynFilterHeader] = exposeDynHeader(stat)
    implicit val exposeret = exposeRepFromRep[ImageH]
    val stageme: (DynFilterHeader => Rep[ImageH]) = (dyn: DynFilterHeader) => {

      val mix = MixFilterHeader(stat, dyn)
      import mix._
      val iterations_x = image.xsize.ev.div(image.xsize.a, image.xsize.ev.const(blocking.blockingx)) //dyn
      val iterations_y = image.ysize.ev.div(image.ysize.a, image.ysize.ev.const(blocking.blockingy)) //dyn

      val rest_x = image.xsize.ev.mod(image.xsize.a, image.xsize.ev.const(blocking.blockingx)) //dyn
      val rest_y = image.ysize.ev.mod(image.ysize.a, image.ysize.ev.const(blocking.blockingy)) //dyn

      val itblockedx = blocking.blockingx / blocking.unrollx
      val itblockedy = blocking.blockingy / blocking.unrolly


      val xe = image.xsize.ev
      val xzero = xe.const(0)
      val xrange = xe.unt(xzero, iterations_x)
      xe.rangefold(xrange, image_out, exposeret)({
        case (arrayx, i) => {
          val ye = image.ysize.ev
          val yzero = ye.const(0)
          val yrange = ye.unt(yzero, iterations_y)
          ye.rangefold(yrange, arrayx, exposeret)({
            case (arrayy, j) => {
              val iirange = cRep.unt(cRep.const(0), cRep.const(itblockedx))
              cRep.rangefold(iirange, arrayy, exposeret)({
                case (arrayii, ii) => {
                  val jjrange = cRep.unt(cRep.const(0), cRep.const(itblockedy))
                  cRep.rangefold(jjrange, arrayii, exposeret)({
                    case (arrayjj, jj) => {

                      val s1 = xe.gtimes(i, xe.const(blocking.blockingx))
                      val s2 = gentimes(ii, Const(blocking.unrollx))
                      val xoffset = genplus(xe.toRep(s1), s2)


                      val iiirange = cNoRep.unt(cNoRep.const(0), cNoRep.const(blocking.unrollx))
                      cNoRep.rangefold(iiirange, arrayjj, exposeret)({
                        case (arrayiii, iii) => {

                          val sy1 = ye.gtimes(j, ye.const(blocking.blockingy))
                          val sy2 = gentimes(jj, Const(blocking.unrolly))
                          val yoffset = genplus(ye.toRep(sy1), sy2)


                          val jjjrange = cNoRep.unt(cNoRep.const(0), cNoRep.const(blocking.unrolly))
                          cNoRep.rangefold(jjjrange, arrayiii, exposeret)({
                            case (arrayjjj, jjj) => {
                              val xindex = int_plus(xoffset, Const(iii))
                              val yindex = int_plus(yoffset, Const(jjj))
                              filter_core(mix, xindex, yindex, arrayjjj)
                            }
                          })
                        }
                      })


                    }
                  })
                }
              })
              //setImage(arrayy,xe.toRep(i),ye.toRep(j),Const(0))
            }
          })
        }
      })
    }
    if (stat.inline.inline) {
      MaybeSFunction(stageme)
    } else {
      val t: StagedFunction[DynFilterHeader, Rep[ImageH]] = doGlobalLambda(stageme, Some("FilterMultCore" + stat.genSig()), Some("FilterMultCore" + stat.genSig()))(exposarg, exposeret)
      MaybeSFunction(t)
    }
  }

  def multiply(stat: StatFilterHeader): MaybeSFunction = {
    val exposarg: ExposeRep[DynFilterHeader] = exposeDynHeader(stat)
    implicit val exposeret = exposeRepFromRep[ImageH]
    val stageme: (DynFilterHeader => Rep[ImageH]) = (dyn: DynFilterHeader) => {
      def checkreplace(pos: Int, pmix: MixFilterHeader, check: Int): Rep[ImageH] = {
        import pmix._
        def id2oentry(id: Int): OneEntry = id match {
          case 1 => matrix.r1.c1
          case 2 => matrix.r1.c2
          case 3 => matrix.r1.c3
          case 4 => matrix.r2.c1
          case 5 => matrix.r2.c2
          case 6 => matrix.r2.c3
          case 7 => matrix.r3.c1
          case 8 => matrix.r3.c2
          case 9 => matrix.r3.c3
        }

        val o = id2oentry(pos+1)
        import o._
        val checkval: o.A[o.T] = ev.const[o.T](o.evnum.fromInt(check))(o.evtyp)
        val ninline = pmix.inlineInfo.copy(spezialize_done = pmix.inlineInfo.spezialize_done + 1)
        val bmix = pmix.cpy(inlineInfo = ninline)
        ev._if(ev.equiv(a,checkval)(o.evnum,o.evtyp.mf), {
          //val ninline = pmix.inlineInfo.copy(spezialize_done = pmix.inlineInfo.spezialize_done + 1)
          val nmix: MixFilterHeader = pos match {
            case 0 => pmix.cpy(inlineInfo = ninline, a = Some(pmix.matrix.r1.c1.evnum.fromInt(check)))
            case 1 => pmix.cpy(inlineInfo = ninline, b = Some(pmix.matrix.r1.c2.evnum.fromInt(check)))
            case 2 => pmix.cpy(inlineInfo = ninline, c = Some(pmix.matrix.r1.c3.evnum.fromInt(check)))
            case 3 => pmix.cpy(inlineInfo = ninline, d = Some(pmix.matrix.r2.c1.evnum.fromInt(check)))
            case 4 => pmix.cpy(inlineInfo = ninline, e = Some(pmix.matrix.r2.c2.evnum.fromInt(check)))
            case 5 => pmix.cpy(inlineInfo = ninline, f = Some(pmix.matrix.r2.c3.evnum.fromInt(check)))
            case 6 => pmix.cpy(inlineInfo = ninline, g = Some(pmix.matrix.r3.c1.evnum.fromInt(check)))
            case 7 => pmix.cpy(inlineInfo = ninline, h = Some(pmix.matrix.r3.c2.evnum.fromInt(check)))
            case 8 => pmix.cpy(inlineInfo = ninline, i = Some(pmix.matrix.r3.c3.evnum.fromInt(check)))
            case _ => pmix.cpy(inlineInfo = ninline)
          }
          val (nstat, ndyn) = nmix.split()
          val f = multiply(nstat)
          f(ndyn)
        }, {
          //val binline = pmix.inlineInfo.copy(spezialize_done = pmix.inlineInfo.spezialize_done + 1)
          //val bmix = pmix //pmix.cpy(inlineInfo = binline) //,inlineInfo = ninline)
          val (nstat, ndyn) = bmix.split()
          val f = multiply(nstat)
          f(ndyn)
        } )
      }

      val mix = MixFilterHeader(stat, dyn)
      import mix._



      if (inlineInfo.specialize) {
        if (inlineInfo.spezialize_done < 9)
          checkreplace(pos = inlineInfo.spezialize_done,pmix = mix,0)
        else multiply_core(stat)(dyn)
      }
      else {
        val nmix = mix.cpy(image_in = image_in, image_out = image_out)
        val (nstat, ndyn) = nmix.split()
        val f = multiply_core(nstat)
        f(ndyn)
      }
    }
    if (stat.inline.inline) {
      MaybeSFunction(stageme)
    } else {
      val t: StagedFunction[DynFilterHeader, Rep[ImageH]] = doGlobalLambda(stageme, Some("FilterMult" + stat.genSig()), Some("FilterMult" + stat.genSig()))(exposarg, exposeret)
      MaybeSFunction(t)
    }
  }

  def entry(stat: StatFilterHeader): (DynFilterHeader => Rep[ImageH]) = {
    val exposarg: ExposeRep[DynFilterHeader] = exposeDynHeader(stat)
    implicit val exposeret = exposeRepFromRep[ImageH]
    val stageme: (DynFilterHeader => Rep[ImageH]) = (dyn: DynFilterHeader) => {

      val mix = MixFilterHeader(stat, dyn)
      import mix._
      val (nstat, ndyn) = mix.split()
      val f = multiply(nstat)
      val x = f(ndyn)
      x
    }
    stageme
  }

  def codeexport() = {
    val stream2 = new java.io.PrintWriter(new java.io.FileOutputStream("C:\\Phd\\git\\code\\deleteme\\src\\main\\TestFilter.scala"))
    stream2.println(codefrag)
    val inline = InlineInfo(inline = false, maxfunctions = 0, compareinline = false, consider_inline = false,specialize = false, spezialize_done = 0)
    val ini: StatFilterHeader = StatFilterHeader[Int, Int, Int, Int, Int, Int, Int, Int, Int](inlineInfo = inline)//, a = Some(0), b= Some(0), c= Some(0), d= Some(0), e = Some(0), f= Some(0), g= Some(0), h=Some(0), i=Some(0))
    //val ini = StatFilterHeader[Int, Rep, Rep, Rep, Rep, Rep, Rep, Rep, Rep, Rep](Const(-1), Const(-1), Const(-1), Const(-1), Const(-1), Const(-1), Const(-1), Const(-1), Const(-1), inline)
    //val ini: StatKaratsubaHeader[Rep,Rep,Rep,Rep,Rep,Rep] = StatKaratsubaHeader[Rep,Rep,Rep,Rep,Rep,Rep](Const(-1), Const(-1), Const(-1),Const(-1))
    val esc = codegen.emitSource(entry(ini), "testClass", stream2)(exposeDynHeader(ini), exposeRepFromRep[ImageH])
    stream2.println("\n}\n")
    stream2.flush()
    stream2.close()
  }

  val codefrag = "\npackage Filter2\n\n/*class ImageH(val ele: Int) {\n  def get(x: Int, y: Int): Int = ele\n\n  def set(x: Int, y: Int, p: Int) = this\n}*/\n\ncase class ImageH(pixels: Array[Int], width: Int, height: Int) {\n  def get(x: Int, y: Int): Int = if (x < 0 || x >= height || y < 0 || y >= width) 0\n  else {\n    val size = pixels.length\n    if ((width * x + y) >= size) {\n      //println(\"wtf\")\n      0\n    }\n    else\n      pixels(width * x + y)\n  }\n\n  def set(x: Int, y: Int, p: Int): ImageH = {\n    val size = pixels.length\n    if ((width * x + y) >= size) {\n      //println(\"wtf\")\n      this\n    }\n    else\n      pixels(width * x + y) = p\n\n    this\n  }\n}\n\n\nimport com.sksamuel.scrimage._\nimport java.io.{File, FileInputStream, FileOutputStream}\n\n\n/**\n  * Created by rayda on 05-Nov-16.\n  */\nobject ImageTest extends App {\n  val file = new File(\"C:\\\\PhD\\\\git\\\\code\\\\DualDev\\\\us.jpg\")\n  val file2 = new File(\"C:\\\\PhD\\\\git\\\\code\\\\DualDev\\\\us_out3.jpg\")\n  val fileIn = new FileInputStream(file)\n  //val fileOut = new FileOutputStream(file2)\n  val image = Image(fileIn)\n\n  val res = BufferedImage_filter(image.awt, null)\n\n  val out = Image(res)\n  out.output(file2)\n  //image.output(file2)\n\n\n  import java.awt._\n  import java.awt.image._\n  import java.awt.geom._\n\n  def BufferedImage_filter(src: BufferedImage, dstp: BufferedImage): BufferedImage = {\n    var dst: BufferedImage = dstp\n    val width = src.getWidth();\n    val height = src.getHeight();\n    if (dst == null)\n      dst = createCompatibleDestImage(src, null);\n    var inPixels: Array[Int] = new Array[Int](width * height)\n    var outPixels: Array[Int] = new Array[Int](width * height)\n    getRGB(src, 0, 0, width, height, inPixels);\n\n    val in_imageh = ImageH(inPixels, width, height)\n    val out_imageh = ImageH(outPixels, width, height)\n    val repeats = 1000\n    val timing = new Array[Array[Long]](10)\n    for (i <- 0 until timing.size) timing(i) = new Array[Long](repeats)\n\n\n    val conv = new testClass\n\n    val filter = Array(2,2,2,-2,2,2,-2,2,2)\n    val out = (0 until 1).foldLeft(in_imageh) {\n      (iacc, pos) => {\n        if (pos != 0)\n          filter(pos-1) = 0\n        println(\"Pos: \" + pos)\n        (0 until repeats).foldLeft(in_imageh) {\n          (acc, ele) => {\n            val t0 = System.nanoTime()\n            val out = conv.apply((in_imageh, out_imageh, height, width,\n              filter(2),\n              filter(3),\n              filter(4),\n              filter(5),\n              filter(6),\n              filter(7),\n              filter(8)))\n            val t1 = System.nanoTime()\n            timing(pos)(ele) = t1 - t0\n            out\n          }\n        }\n      }\n    }\n\n    timing.map(p => p.sum/p.length).map(p => println(p))\n\n    //convolve(kernel, inPixels, outPixels, width, height, alpha, edgeAction);\n    //setRGB(dst, 0, 0, width, height, inPixels);\n    setRGB(dst, 0, 0, width, height, out.pixels);\n    return dst\n  }\n\n  /**\n    * A convenience method for getting ARGB pixels from an image. This tries to avoid the performance\n    * penalty of BufferedImage.getRGB unmanaging the image.\n    *\n    * @param image  a BufferedImage object\n    * @param x      the left edge of the pixel block\n    * @param y      the right edge of the pixel block\n    * @param width  the width of the pixel arry\n    * @param height the height of the pixel arry\n    * @param pixels the array to hold the returned pixels. May be null.\n    * @return the pixels\n    * @see #setRGB\n    */\n  def getRGB(image: BufferedImage, x: Int, y: Int, width: Int, height: Int, pixels: Array[Int]): Array[Int] = {\n    var typ: Int = image.getType()\n    if (typ == BufferedImage.TYPE_INT_ARGB || typ == BufferedImage.TYPE_INT_RGB)\n      return (image.getRaster().getDataElements(x, y, width, height, pixels)).asInstanceOf[Array[Int]]\n    else\n      return image.getRGB(x, y, width, height, pixels, 0, width);\n  }\n\n  /**\n    * A convenience method for setting ARGB pixels in an image. This tries to avoid the performance\n    * penalty of BufferedImage.setRGB unmanaging the image.\n    *\n    * @param image  a BufferedImage object\n    * @param x      the left edge of the pixel block\n    * @param y      the right edge of the pixel block\n    * @param width  the width of the pixel arry\n    * @param height the height of the pixel arry\n    * @param pixels the array of pixels to set\n    * @see #getRGB\n    */\n  def setRGB(image: BufferedImage, x: Int, y: Int, width: Int, height: Int, pixels: Array[Int]) {\n    val typ: Int = image.getType();\n    if (typ == BufferedImage.TYPE_INT_ARGB || typ == BufferedImage.TYPE_INT_RGB)\n      image.getRaster().setDataElements(x, y, width, height, pixels);\n    else\n      image.setRGB(x, y, width, height, pixels, 0, width);\n  }\n\n\n  def createCompatibleDestImage(src: BufferedImage, dstCMp: ColorModel): BufferedImage = {\n    var dstCM = dstCMp\n    if (dstCM == null)\n      dstCM = src.getColorModel();\n    return new BufferedImage(dstCM, dstCM.createCompatibleWritableRaster(src.getWidth(), src.getHeight()), dstCM.isAlphaPremultiplied(), null);\n  }\n}\n\n\nobject PixelUtils {\n  /**\n    * Clamp a value to the range 0..255\n    */\n  def clamp(c: Int): Int = {\n    if (c < 0)\n      return 0;\n    if (c > 255)\n      return 255;\n    return c;\n  }\n}"

}