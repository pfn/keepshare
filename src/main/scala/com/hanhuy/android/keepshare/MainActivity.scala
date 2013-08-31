package com.hanhuy.android.keepshare

import AndroidConversions._
import RichLogger._

import collection.JavaConversions._

import android.app.Activity
import android.os.Bundle
import com.google.api.services.drive.{Drive, DriveScopes}
import com.google.api.client.googleapis.extensions.android.gms.auth._
import android.content.Intent
import android.accounts.AccountManager
import android.view.View
import com.google.api.client.extensions.android.http.AndroidHttp
import com.google.api.client.json.gson.GsonFactory
import com.google.api.services.drive.model.{ParentReference, File}
import com.google.api.client.http.{GenericUrl, ByteArrayContent}
import java.security.{MessageDigest, SecureRandom}
import java.nio.ByteBuffer

class MainActivity extends Activity with TypedViewHolder {
  implicit val TAG = LogcatTag("MainActivity")

  val REQUEST_ACCOUNT_PICKER = 1
  val REQUEST_AUTHORIZATION  = 2

  val EXTRA_STATE = "com.hanhuy.android.keepshare.extra.STATE"
  val KEY_FILE = "keepass-share.key"

  val STATE_SAVE = "save"
  val STATE_LOAD = "load"

  lazy val sha1 = MessageDigest.getInstance("SHA1")
  def sha1(b: Array[Byte]): String = {
    sha1.digest(b).map { byte => "%02X" format (byte & 0xff) }.mkString
  }

  lazy val settings = Settings(this)
  lazy val credential = GoogleAccountCredential.usingOAuth2(this,
    Seq(DriveScopes.DRIVE_APPDATA))
  lazy val drive = new Drive.Builder(AndroidHttp.newCompatibleTransport,
    new GsonFactory, credential).build

  def loadKey() {
    async {
      val req = drive.files.list
      req.setQ("'appdata' in parents")
      try {
        val files = req.execute()
        files.getItems find (_.getTitle == KEY_FILE) map {
          file =>
            val resp = drive.getRequestFactory.buildGetRequest(
              new GenericUrl(file.getDownloadUrl)).execute
            val buf = Array.ofDim[Byte](32)
            val in = resp.getContent
            val b = ByteBuffer.allocate(32)
            Stream.continually(in.read(buf)).takeWhile(_ != -1) foreach { r =>
              b.put(buf, 0, r)
            }
            b.flip()
            if (b.remaining != 32)
              throw new IllegalStateException(
                "wrong buffer size: " + b.remaining)
            b.get(buf)
            val hash = settings.get(Settings.CLOUD_KEY_HASH)
            if (sha1(buf) != hash) {
              throw new IllegalStateException("cloud key has changed")
            }
            v("Read: " + b)
        } getOrElse saveKey()
        v(files.toString)
        v("token: " + files.getNextPageToken)
      } catch {
        case e: UserRecoverableAuthIOException => requestAuthz(e, STATE_LOAD)
      }
    }
  }
  def saveKey() {
    val random = new SecureRandom
    val keybuf = Array.ofDim[Byte](32)
    random.nextBytes(keybuf)
    settings.set(Settings.CLOUD_KEY_HASH, sha1(keybuf))

    val content = new ByteArrayContent("application/octet-stream", keybuf)
    val f = new File
    f.setTitle(KEY_FILE)
    f.setParents(Seq(new ParentReference().setId("appdata")))
    async {
      try {
        val r = drive.files.insert(f, content).execute()
        UiBus.post {
          findView(TR.new_setup_container).setVisibility(View.GONE)
          findView(TR.setup_container).setVisibility(View.VISIBLE)
        }
      } catch {
        case e: UserRecoverableAuthIOException => requestAuthz(e, STATE_SAVE)
      }
    }
  }

  private def requestAuthz(e: UserRecoverableAuthIOException, state: String) {
    val i = e.getIntent
    i.putExtra(EXTRA_STATE, state)
    startActivityForResult(i, REQUEST_AUTHORIZATION)
  }
  private def pickAccount() {
    settings.set(Settings.GOOGLE_USER, null)
    startActivityForResult(
      credential.newChooseAccountIntent(), REQUEST_ACCOUNT_PICKER)
  }

  override def onCreate(savedInstanceState: Bundle) {
    super.onCreate(savedInstanceState)
    setContentView(R.layout.main)

    val user = Option(settings.get(Settings.GOOGLE_USER))
    val connectVis = if (user.isEmpty)
      (View.VISIBLE,View.GONE) else (View.GONE,View.VISIBLE)
    val connect = findView(TR.connect)
    findView(TR.new_setup_container).setVisibility(connectVis._1)
    findView(TR.setup_container).setVisibility(connectVis._2)
    findView(TR.save) onClick {
      finish()
    }
    connect onClick {
      pickAccount()
    }

    user foreach { name =>
      credential.setSelectedAccountName(name)
      loadKey()
    }
  }

  override def onActivityResult(request: Int, result: Int, data: Intent) {
    request match {
      case REQUEST_ACCOUNT_PICKER =>
        if (result == Activity.RESULT_OK) {
          Option(data.getStringExtra(AccountManager.KEY_ACCOUNT_NAME)) foreach {
            n =>
            credential.setSelectedAccountName(n)
            settings.set(Settings.GOOGLE_USER, n)
            saveKey()
          }
        } else finish()
      case REQUEST_AUTHORIZATION =>
        if (result == Activity.RESULT_OK) {
          findView(TR.new_setup_container).setVisibility(View.GONE)
          findView(TR.setup_container).setVisibility(View.VISIBLE)
          data.getStringExtra(EXTRA_STATE) match {
            case STATE_LOAD =>
            case STATE_SAVE => saveKey()
          }
        } else {
          pickAccount()
        }
    }
  }
}
