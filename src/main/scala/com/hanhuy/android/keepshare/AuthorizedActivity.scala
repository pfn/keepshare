package com.hanhuy.android.keepshare

import android.app.{Activity, ProgressDialog}
import android.content.Intent
import android.os.Bundle
import android.support.v7.app.AppCompatActivity
import android.view.{MenuItem, Menu}
import android.widget.Toast
import com.hanhuy.android.common._
import com.hanhuy.keepassj.PwDatabase

import scala.concurrent.{Promise, Future}

import Futures._

import scala.util.Try

/**
 * @author pfnguyen
 */
class AuthorizedActivity extends AppCompatActivity with EventBus.RefOwner {
  private implicit val TAG = LogcatTag("AuthorizedActivity")
  lazy val settings = Settings(this)
  lazy val km = new KeyManager(this, settings)
  private var running = false

  private var dbFuture = Option.empty[Future[PwDatabase]]
  private val readyPromise = Promise[Unit]()
  def ready = readyPromise.isCompleted
  def database = dbFuture getOrElse openDatabase()
  override def onCreate(savedInstanceState: Bundle) = {
    super.onCreate(savedInstanceState)
    if (settings.get(Settings.FIRST_RUN)) {
      startActivityForResult(SetupActivity.intent, RequestCodes.REQUEST_SETUP)
    } else if (!km.ready) {
      if (settings.get(Settings.NEEDS_PIN) && PINHolderService.instance.isEmpty)
        PINEntryActivity.requestPIN(this)
      else
        startActivityForResult(SetupActivity.intent, RequestCodes.REQUEST_SETUP)
    } else {
      readyPromise.trySuccess()
      val p = ProgressDialog.show(this, getString(R.string.loading_key),
        getString(R.string.please_wait), true, false)
      km.config flatMap {
        case Left(_) =>
          startActivityForResult(
            SetupActivity.intent, RequestCodes.REQUEST_SETUP)
          Future.failed(KeyError.LoadFailed("need setup"))
        case Right(_) => database
      } onCompleteMain(_=> if (!isFinishing && p.isShowing) p.dismiss())
    }
  }

  override def onActivityResult(requestCode: Int, resultCode: Int, data: Intent) = {
    val success = requestCode match {
      case RequestCodes.REQUEST_SIGN_IN =>
        recreate()
        false
      case RequestCodes.REQUEST_SETUP =>
        if (resultCode == Activity.RESULT_OK) {
          true
        } else {
          false
        }
      case RequestCodes.REQUEST_PIN => resultCode == Activity.RESULT_OK
      case RequestCodes.REQUEST_SETUP_PIN => invalidateOptionsMenu()
        true
    }
    if (!success)
      finish()
    else {
      readyPromise.trySuccess()
      dbFuture foreach (_.onFailure { case _ => dbFuture = None })
    }
  }
  override def onResume() = {
    super.onResume()
    running = true
  }
  override def onPause() = {
    super.onPause()
    running = false
  }

  override def onCreateOptionsMenu(menu: Menu) = {
    super.onCreateOptionsMenu(menu)
    getMenuInflater.inflate(R.menu.authorized, menu)
    if (settings.get(Settings.NEEDS_PIN)) {
      menu.findItem(R.id.menu_setup_pin).setVisible(false)
    }
    true
  }
  override def onOptionsItemSelected(item: MenuItem) = item.getItemId match {
    case R.id.menu_setup =>
      startActivity(new Intent(this, classOf[SetupActivity]))
      true
    case R.id.menu_setup_pin =>
      if (km.ready) {
        startActivityForResult(new Intent(this, classOf[PINSetupActivity]),
          RequestCodes.REQUEST_SETUP_PIN)
      } else {
        Toast.makeText(this,
          R.string.setup_required_for_pin, Toast.LENGTH_SHORT).show()
      }
      true
    case _ => super.onOptionsItemSelected(item)
  }
  ServiceBus += {
    case DatabaseClosed =>
      dbFuture = None
    case PINServiceExit  =>
      dbFuture = None
      if (running)
        Toast.makeText(this, "Your KeepShare session has timed out, exiting",
          Toast.LENGTH_LONG).show()
      finish()
  }

  private def openDatabase(): Future[PwDatabase] = {
    if (!readyPromise.isCompleted) {
      Future.failed(KeyError.NotReady)
    } else if (Database.isOpen)
      Future.successful(Database.pwdatabase)
    else {

      val f = km.config flatMap {
        case Left(err) => err match {
          case KeyError.NeedPin =>
            // ignore this case as it should be handled earlier by readyPromise
            Future.failed(KeyError.NeedPin)
          case _ =>
            UiBus.post {
              Toast.makeText(this,
                R.string.failed_verify, Toast.LENGTH_LONG).show()
            }
            startActivityForResult(SetupActivity.intent,
              RequestCodes.REQUEST_SETUP)
            Future.failed(KeyError.NeedSetup)
        }
        case Right((db, pw, keyf)) =>
          val b = new Bundle
          Database.open(db, Option(pw), Option(keyf)) recoverWith {
            case e =>
              UiBus.post {
                Toast.makeText(this, getString(R.string.failed_to_open) +
                  e.getMessage, Toast.LENGTH_LONG).show()
                RichLogger.e(e.getMessage, e)
              }
              startActivityForResult(SetupActivity.intent,
                RequestCodes.REQUEST_SETUP)
              Future.failed(KeyError.NeedSetup)
          }
      }
      if (!f.isCompleted) {
        val p = ProgressDialog.show(this, getString(R.string.loading_database),
          getString(R.string.please_wait), true, false)
        f onCompleteMain (_ => if (running && p.isShowing) p.dismiss())
      }
      dbFuture = Some(f)
      f
    }
  }
}
