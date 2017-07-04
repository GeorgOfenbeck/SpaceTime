package Filter2

import com.sksamuel.scrimage._
import java.io.{File, FileInputStream, FileOutputStream}


/**
  * Created by rayda on 05-Nov-16.
  */
object ImageTest extends App {
  val file = new File("C:\\PhD\\git\\code\\DualDev\\gibson.jpg")
  val file2 = new File("C:\\PhD\\git\\code\\DualDev\\gibson_out3.jpg")
  val fileIn = new FileInputStream(file)
  //val fileOut = new FileOutputStream(file2)
  val image = Image(fileIn)

  val res = BufferedImage_filter(image.awt,null)

  val out = Image(res)
  out.output(file2)
  //image.output(file2)



  import java.awt._
  import java.awt.image._
  import java.awt.geom._
  def BufferedImage_filter(src: BufferedImage, dstp: BufferedImage): BufferedImage = {
    var dst: BufferedImage = dstp
    val width = src.getWidth();
    val height = src.getHeight();
    if (dst == null)
      dst = createCompatibleDestImage(src, null);
    var inPixels: Array[Int] = new Array[Int](width * height)
    var outPixels: Array[Int] = new Array[Int](width * height)
    getRGB(src, 0, 0, width, height, inPixels);

    //convolve(kernel, inPixels, outPixels, width, height, alpha, edgeAction);
    //setRGB(dst, 0, 0, width, height, inPixels);
    setRGB(dst, 0, 0, width, height, outPixels);
    return src//dst
  }

  /**
    * A convenience method for getting ARGB pixels from an image. This tries to avoid the performance
    * penalty of BufferedImage.getRGB unmanaging the image.
    *
    * @param image  a BufferedImage object
    * @param x      the left edge of the pixel block
    * @param y      the right edge of the pixel block
    * @param width  the width of the pixel arry
    * @param height the height of the pixel arry
    * @param pixels the array to hold the returned pixels. May be null.
    * @return the pixels
    * @see #setRGB
    */
  def getRGB(image: BufferedImage, x: Int, y: Int, width: Int, height: Int, pixels: Array[Int]): Array[Int] = {
    var typ: Int = image.getType()
    if (typ == BufferedImage.TYPE_INT_ARGB || typ == BufferedImage.TYPE_INT_RGB)
      return (image.getRaster().getDataElements(x, y, width, height, pixels)).asInstanceOf[Array[Int]]
    else
      return image.getRGB(x, y, width, height, pixels, 0, width);
  }

  /**
    * A convenience method for setting ARGB pixels in an image. This tries to avoid the performance
    * penalty of BufferedImage.setRGB unmanaging the image.
    *
    * @param image  a BufferedImage object
    * @param x      the left edge of the pixel block
    * @param y      the right edge of the pixel block
    * @param width  the width of the pixel arry
    * @param height the height of the pixel arry
    * @param pixels the array of pixels to set
    * @see #getRGB
    */
  def setRGB(image: BufferedImage, x: Int, y: Int, width: Int, height: Int, pixels: Array[Int]) {
    val typ: Int = image.getType();
    if (typ == BufferedImage.TYPE_INT_ARGB || typ == BufferedImage.TYPE_INT_RGB)
      image.getRaster().setDataElements(x, y, width, height, pixels);
    else
      image.setRGB(x, y, width, height, pixels, 0, width);
  }


  def createCompatibleDestImage(src: BufferedImage, dstCMp: ColorModel): BufferedImage = {
    var dstCM = dstCMp
    if (dstCM == null)
      dstCM = src.getColorModel();
    return new BufferedImage(dstCM, dstCM.createCompatibleWritableRaster(src.getWidth(), src.getHeight()), dstCM.isAlphaPremultiplied(), null);
  }
}

