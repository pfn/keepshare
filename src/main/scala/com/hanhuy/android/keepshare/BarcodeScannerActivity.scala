package com.hanhuy.android.keepshare

import android.content.Intent
import android.graphics._
import android.hardware.Camera
import android.net.Uri
import android.os.Bundle
import android.support.v7.app.AppCompatActivity
import android.view.{MenuItem, WindowManager}
import android.widget.Toast
import com.google.android.gms.common.{GoogleApiAvailability, ConnectionResult}
import com.google.android.gms.vision.Detector.Detections
import com.google.android.gms.vision.{Tracker, MultiProcessor}
import com.google.android.gms.vision.barcode.{Barcode, BarcodeDetector}
import com.hanhuy.android.common.Futures

import Futures._
import com.hanhuy.android.keepshare.camera.{GraphicOverlay, CameraSource}
import iota.std.Configurations._

import com.hanhuy.android.common._

import scala.util.Try

/**
  * @author pfnguyen
  */
class BarcodeScannerActivity extends AppCompatActivity with TypedFindView with PermissionManager {
  lazy val text = findView(TR.text)
  lazy val preview = findView(TR.preview)
  lazy val overlay: GraphicOverlay[BarcodeGraphic] = findViewById(R.id.graphicOverlay).asInstanceOf[GraphicOverlay[BarcodeGraphic]]

  var cameraSource = Option.empty[CameraSource]
  var lastBarcode = Option.empty[String]

  override def onCreate(savedInstanceState: Bundle) = {
    super.onCreate(savedInstanceState)
    setContentView(R.layout.barcode_scanner)
    getSupportActionBar.setDisplayHomeAsUpEnabled(true)

    requestPermission(android.Manifest.permission.CAMERA, R.string.camera_permission, preview) onSuccessMain { case _ =>
        createCameraSource()
    }
  }

  override def onOptionsItemSelected(item: MenuItem) = item.getItemId match {
    case android.R.id.home =>
      finish()
      overridePendingTransition(android.R.anim.slide_in_left, android.R.anim.slide_out_right)
      true
    case _ =>
      super.onOptionsItemSelected(item)
  }

  override def onBackPressed() = {
    super.onBackPressed()
    overridePendingTransition(android.R.anim.slide_in_left, android.R.anim.slide_out_right)
  }

  def createCameraSource(): Unit = {
    val detector = new BarcodeDetector.Builder(getApplicationContext)
      .setBarcodeFormats(Barcode.QR_CODE)
      .build()
    val barcodeFactory = new BarcodeTrackerFactory(overlay)
    detector.setProcessor(new MultiProcessor.Builder(barcodeFactory).build())
    if (!detector.isOperational) {
      Toast.makeText(this, "Barcode detector is not available, please try again", Toast.LENGTH_LONG).show()
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

    cameraSource foreach { source =>
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
  class BarcodeTrackerFactory(overlay: GraphicOverlay[BarcodeGraphic]) extends MultiProcessor.Factory[Barcode] {
    override def create(t: Barcode) = {
      onBarcodeDetected(t)
      BarcodeGraphicTracker(overlay, new BarcodeGraphic(overlay))
    }
    case class BarcodeGraphicTracker(overlay: GraphicOverlay[BarcodeGraphic], graphic: BarcodeGraphic) extends Tracker[Barcode] {
      override def onDone() = overlay.remove(graphic)
      override def onMissing(detections: Detections[Barcode]) = overlay.remove(graphic)
      override def onNewItem(id: Int, item: Barcode) = ()

      override def onUpdate(detections: Detections[Barcode], item: Barcode) = {
        overlay.add(graphic)
        graphic.item = item
      }
    }
  }

  def onBarcodeDetected(barcode: Barcode): Unit = {
    if (!lastBarcode.contains(barcode.rawValue)) {
      if (!barcode.rawValue.startsWith("otpauth:")) UiBus.post {
        text.setText("Invalid code: " + barcode.rawValue)
        UiBus.handler.removeCallbacks(reset)
        UiBus.handler.removeCallbacks(launch)
        UiBus.handler.postDelayed(reset, 5000)
      } else if (barcode.rawValue.startsWith("otpauth:")) UiBus.post {
        text.setText("Detected " + barcode.rawValue)
        UiBus.handler.removeCallbacks(reset)
        UiBus.handler.removeCallbacks(launch)
        UiBus.handler.postDelayed(launch, 1000)
      }
    }
    lastBarcode = barcode.rawValue.?
  }

  val reset: Runnable = () => {
    text.setText(R.string.scanning_for_otp)
    lastBarcode = None
  }
  val launch: Runnable = () => {
    val intentUri = for {
      code <- lastBarcode
      uri <- Try(Uri.parse(code)).toOption
    } yield uri

    intentUri match {
      case Some(uri) =>
        val intent = new Intent(Intent.ACTION_VIEW)
        intent.setData(uri)
        startActivity(intent)
        finish()
        overridePendingTransition(R.anim.slide_in_right, R.anim.slide_out_left)
      case None =>
        text.setText("Malformed OTP code: " + lastBarcode.getOrElse("null"))
        UiBus.handler.removeCallbacks(reset)
        UiBus.handler.removeCallbacks(launch)
        UiBus.handler.postDelayed(reset, 5000)
    }
  }
}

class BarcodeGraphic(overlay: GraphicOverlay[BarcodeGraphic]) extends GraphicOverlay.Graphic(overlay) {
  implicit val context = Application.instance
  lazy val rectPaint = {
    val paint = new Paint()
    paint.setStyle(Paint.Style.STROKE)
    paint.setStrokeWidth(2.dp)
    paint.setColor(context.getResources.getColor(R.color.colorPrimary))
    paint
  }
  override def draw(canvas: Canvas) = {
    item foreach { barcode =>
      if (barcode.cornerPoints.nonEmpty) {
        val first = barcode.cornerPoints.head
        val last = barcode.cornerPoints.reduceLeft { (ac, p) =>
          canvas.drawLine(translateX(ac.x), translateY(ac.y), translateX(p.x), translateY(p.y), rectPaint)
          p
        }
        canvas.drawLine(translateX(last.x), translateY(last.y), translateX(first.x), translateY(first.y), rectPaint)
      }
    }
  }

  var _item = Option.empty[Barcode]
  def item_=(item: Barcode) = {
    _item = Option(item)
    postInvalidate()
  }
  def item = _item
}
