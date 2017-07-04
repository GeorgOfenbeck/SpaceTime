package Filter2

import scala.swing._
import java.awt.image.BufferedImage
import java.net.URL
import javax.imageio.ImageIO


class ImagePanel(rows0: Int, cols0: Int) extends GridPanel(rows0, cols0) {
  private var _imagePath = ""
  private var buf = Option.empty[BufferedImage]

  def imagePath = _imagePath
  def imagePath_=(value: String): Unit = {
    _imagePath = value
    buf.foreach(_.flush()); buf = None
    buf = Some(ImageIO.read(new URL(value)))
    repaint()
  }

  override def paintComponent(g: Graphics2D): Unit = {
    super.paintComponent(g)
    buf.foreach(g.drawImage(_, 0, 0, null))
  }
}

object GenMain extends App {

  val dsl = new Core
  dsl.codeexport()
  //dsl.graphvizexport()

}
