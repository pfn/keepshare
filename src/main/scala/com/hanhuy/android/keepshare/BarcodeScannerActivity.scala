package com.hanhuy.android.keepshare

import android.graphics._
import android.hardware.Camera
import android.os.Bundle
import android.support.v7.app.AppCompatActivity
import android.text.TextPaint
import android.view.WindowManager
import android.widget.Toast
import com.google.android.gms.common.{GoogleApiAvailability, ConnectionResult}
import com.google.android.gms.vision.Detector.Detections
import com.google.android.gms.vision.{Tracker, MultiProcessor}
import com.google.android.gms.vision.barcode.{Barcode, BarcodeDetector}
import com.hanhuy.android.common.Futures

import Futures._
import com.hanhuy.android.keepshare.camera.{GraphicOverlay, CameraSource}
import iota.std.Configurations._

/**
  * @author pfnguyen
  */
class BarcodeScannerActivity extends AppCompatActivity with TypedFindView with PermissionManager {
  lazy val preview = findView(TR.preview)
  lazy val overlay: GraphicOverlay[BarcodeGraphic] = findViewById(R.id.graphicOverlay).asInstanceOf[GraphicOverlay[BarcodeGraphic]]

  var cameraSource = Option.empty[CameraSource]

  override def onCreate(savedInstanceState: Bundle) = {
    super.onCreate(savedInstanceState)
    setContentView(R.layout.barcode_scanner)

    requestPermission(android.Manifest.permission.CAMERA, R.string.camera_permission, preview) onSuccessMain { case _ =>
        createCameraSource()
    }
  }

  def createCameraSource(): Unit = {
    val detector = new BarcodeDetector.Builder(getApplicationContext)
      .setBarcodeFormats(Barcode.QR_CODE)
      .build()
    val barcodeFactory = new BarcodeTrackerFactory(overlay)
    detector.setProcessor(new MultiProcessor.Builder(barcodeFactory).build())
    if (!detector.isOperational) {
      Toast.makeText(this, "Barcode detector is not operational, please try again", Toast.LENGTH_LONG).show()
    }

    import iota.std.Contexts._
    val size = new Point
    systemService[WindowManager].getDefaultDisplay.getSize(size)

    // because Camera.Parameters is not a static class...
    val FOCUS_MODE = (null: Camera).Parameters.FOCUS_MODE_CONTINUOUS_PICTURE
    val builder = new CameraSource.Builder(getApplicationContext, detector)
      .setFacing(CameraSource.CAMERA_FACING_BACK)
      .setRequestedPreviewSize(size.x, size.y)
      .setFocusMode(FOCUS_MODE)
      .setRequestedFps(15.0f)

    cameraSource = Option(builder.build())
  }

  def startCameraSource(): Unit = {
    val code = GoogleApiAvailability.getInstance().isGooglePlayServicesAvailable(
      getApplicationContext)
    if (code != ConnectionResult.SUCCESS) {
      val dlg = GoogleApiAvailability.getInstance().getErrorDialog(this, code, 0)
      dlg.show()
    }

    for {
      source  <- cameraSource
    } {
      try {
        preview.start(source, overlay)
      } catch {
        case e: Exception =>
          Toast.makeText(this, "Failed to start preview: " + e, Toast.LENGTH_LONG).show()
          source.release()
          cameraSource = None
      }
    }
  }

  override def onResume(): Unit = {
    super.onResume()
    startCameraSource()
  }
  override def onPause(): Unit = {
    super.onPause()
    preview.stop()
  }

  override def onDestroy(): Unit = {
    super.onDestroy()
    preview.release()
  }
}

class BarcodeTrackerFactory(overlay: GraphicOverlay[BarcodeGraphic]) extends MultiProcessor.Factory[Barcode] {
  override def create(t: Barcode) = new BarcodeGraphicTracker(overlay, new BarcodeGraphic(overlay))
}

class BarcodeGraphicTracker(overlay: GraphicOverlay[BarcodeGraphic], graphic: BarcodeGraphic) extends Tracker[Barcode] {
  override def onDone() = overlay.remove(graphic)
  override def onMissing(detections: Detections[Barcode]) = overlay.remove(graphic)
  override def onNewItem(id: Int, item: Barcode) = graphic.id = id

  override def onUpdate(detections: Detections[Barcode], item: Barcode) = {
    overlay.add(graphic)
    graphic.item = item
  }
}
class BarcodeGraphic(overlay: GraphicOverlay[BarcodeGraphic]) extends GraphicOverlay.Graphic(overlay) {
  val COLORS = Vector(Color.BLUE, Color.CYAN, Color.GREEN)
  implicit val context = Application.instance
  lazy val textPaint = {
    val paint = new TextPaint
    paint.setTextSize(18.dp)
    paint
  }

  lazy val rectPaint = {
    val paint = new Paint()
    paint.setStyle(Paint.Style.STROKE)
    paint.setStrokeWidth(2.dp)
    paint
  }
  override def draw(canvas: Canvas) = {
    item foreach { barcode =>
      val rect = new RectF(barcode.getBoundingBox)
      rect.left = translateX(rect.left)
      rect.top = translateY(rect.top)
      rect.right = translateX(rect.right)
      rect.bottom = translateY(rect.bottom)
      canvas.drawRect(rect, rectPaint)

      canvas.drawText(barcode.rawValue, rect.left, rect.bottom, textPaint)
    }
  }

  var _id = Option.empty[Int]
  def id_=(i: Int) = {
    _id = Some(i)
    val color = COLORS(math.abs(i) % COLORS.size)
    textPaint.setColor(color)
    rectPaint.setColor(color)
  }
  def id = _id
  var _item = Option.empty[Barcode]
  def item_=(item: Barcode) = {
    _item = Option(item)
    postInvalidate()
  }
  def item = _item
}
