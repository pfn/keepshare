package com.hanhuy.android.keepshare

import android.app.{ActionBar, Activity, NotificationManager}

import android.content.Intent
import android.content.Context
import android.content.BroadcastReceiver
import android.content.res.Configuration
import android.os.Build
import android.os.Handler
import android.os.Looper
import android.view.View
import android.view.KeyEvent
import android.view.MotionEvent
import android.widget.AdapterView
import android.widget.TextView
import android.widget.CheckBox
import android.content.DialogInterface
import android.view.LayoutInflater
import android.text.{SpannableString, SpannableStringBuilder, Spanned}
import android.text.style.{StyleSpan, ForegroundColorSpan}
import android.graphics.Typeface
import scala.annotation.tailrec
import android.util.Log

object AndroidConversions {
  val icsAndNewer =
    Build.VERSION.SDK_INT >= Build.VERSION_CODES.ICE_CREAM_SANDWICH
  val honeycombAndNewer =
    Build.VERSION.SDK_INT >= Build.VERSION_CODES.HONEYCOMB
  val gingerbreadAndNewer =
    Build.VERSION.SDK_INT >= Build.VERSION_CODES.GINGERBREAD
  val jellybeanAndNewer =
    Build.VERSION.SDK_INT >= Build.VERSION_CODES.JELLY_BEAN

  implicit def toBroadcastReceiver(f: (Context, Intent) => Unit) =
    new BroadcastReceiver() {
      def onReceive(c: Context, i: Intent) = f(c, i)
    }

  implicit def toOnNavigationListener(f: (Int, Long) => Boolean) =
    new ActionBar.OnNavigationListener() {
      override def onNavigationItemSelected(pos: Int, id: Long) = f(pos, id)
    }

  implicit def toViewOnClickListener1[A](f: () => A) =
    new View.OnClickListener() { def onClick(v: View) = f() }

  implicit def toViewOnClickListener[A](f: View => A) =
    new View.OnClickListener() { def onClick(v: View) = f(v) }

  implicit def toDialogInterfaceOnClickListener(
      f: (DialogInterface, Int) => Unit) =
    new DialogInterface.OnClickListener() {
      def onClick(d: DialogInterface, id: Int) = f(d, id)
    }

  implicit def toDialogInterfaceOnClickListener1(f: () => Unit) =
    new DialogInterface.OnClickListener() {
      def onClick(d: DialogInterface, id: Int) = f()
    }

  implicit def toDialogInterfaceOnShowListener(f: () => Unit) =
    new DialogInterface.OnShowListener() {
      def onShow(d: DialogInterface) = f()
    }

  implicit def toAdapterViewOnItemClickListener(
      f: (AdapterView[_], View, Int, Long) => Unit) =
    new AdapterView.OnItemClickListener() {
      def onItemClick(av: AdapterView[_], v: View, pos: Int, id: Long) =
        f(av, v, pos, id)
    }

  implicit def toAdapterViewOnItemClickListener2(
      f: (Int) => Unit) =
    new AdapterView.OnItemClickListener() {
      def onItemClick(av: AdapterView[_], v: View, pos: Int, id: Long) =
        f(pos)
    }

  implicit def toViewOnKeyListener(f: (View, Int, KeyEvent) => Boolean) =
    new View.OnKeyListener() {
      def onKey(v: View, key: Int, e: KeyEvent) = f(v, key, e)
    }

  implicit def toViewOnTouchListener(f: (View, MotionEvent) => Boolean) =
    new View.OnTouchListener() {
      def onTouch(v: View, e: MotionEvent) = f(v, e)
    }

  implicit def toTextViewOnEditorAction(f: (View, Int, KeyEvent) => Boolean) =
    new TextView.OnEditorActionListener() {
      def onEditorAction(v: TextView, action: Int, e: KeyEvent) =
        f(v, action, e)
    }

  implicit def toRunnable(f: () => Unit) = new Runnable() { def run() = f() }

  def async(r: Runnable) = _threadpool.execute(r)

  // ok, param: => T can only be used if called directly, no implicits
  def async[A](f: => A): Unit = async(byNameToRunnable(f))

  def byNameToRunnable(f: => Unit) = new Runnable() { def run() = f }

  implicit def toUncaughtExceptionHandler(f: (Thread, Throwable) => Unit) =
    new Thread.UncaughtExceptionHandler {
      override def uncaughtException(t: Thread, e: Throwable) = f(t, e)
    }

  implicit def toString(c: CharSequence) = if (c == null) null else c.toString

  implicit def toString(t: TextView): String = t.getText

  implicit def toInt(t: TextView) = {
    val s: String = t.getText
    if (s == null || s == "") -1 else Integer.parseInt(s)
  }

  implicit def toBoolean(c: CheckBox) = c.isChecked

  implicit def toRichView(v: View) = RichView(v)
  implicit def toRichContext(c: Context) = RichContext(c)
  implicit def toRichActivity(a: Activity) = new RichActivity(a)
  implicit def toRichHandler(h: Handler) = RichHandler(h)
  implicit def toSpannedGenerator(s: String) = SpannedGenerator(s)

  lazy val _threadpool = {
    if (honeycombAndNewer) android.os.AsyncTask.THREAD_POOL_EXECUTOR
    else { // basically how THREAD_POOL_EXECUTOR is defined in api11+
      import java.util.concurrent._
      import java.util.concurrent.atomic._
      // initial, max, keep-alive time
      new ThreadPoolExecutor(5, 128, 1, TimeUnit.SECONDS,
        new LinkedBlockingQueue[Runnable](10),
        new ThreadFactory() {
          val count = new AtomicInteger(1)
          override def newThread(r: Runnable) =
            new Thread(r,
              "AsyncPool #" + count.getAndIncrement)
        })
    }
  }
  def isMainThread = Looper.getMainLooper.getThread == Thread.currentThread
}

case class SystemService[T](name: String)
object SystemService {
  import Context._
  implicit val ls = SystemService[LayoutInflater](LAYOUT_INFLATER_SERVICE)
  implicit val ns = SystemService[NotificationManager](NOTIFICATION_SERVICE)
}
case class RichContext(context: Context) {
  def systemService[T](implicit s: SystemService[T]): T =
    context.getSystemService(s.name).asInstanceOf[T]
}
case class RichView(view: View) extends TypedViewHolder {
  import AndroidConversions._

  def findViewById(id: Int): View = view.findViewById(id)

  def findView[A <: View](id: Int): A =
    view.findViewById(id).asInstanceOf[A]

  def onClick[A](f: => A) = view.setOnClickListener { () => f }
  def onClick[A](f: View => A) = view.setOnClickListener(f)
}
// can't be case class because of inheritance :-/
class RichActivity(activity: Activity) extends RichContext(activity)
with TypedViewHolder {
  import Configuration._
  lazy val config = activity.getResources.getConfiguration

  def findView[A <: View](id: Int): A =
    activity.findViewById(id).asInstanceOf[A]

  def findViewById(id: Int): View = activity.findViewById(id)

  private def atLeast(size: Int) =
    (config.screenLayout & SCREENLAYOUT_SIZE_MASK) >= size
  lazy val isLargeScreen  = atLeast(SCREENLAYOUT_SIZE_LARGE)
  lazy val isXLargeScreen = atLeast(SCREENLAYOUT_SIZE_XLARGE)
}

case class RichHandler(handler: Handler) {
  def delayed(delay: Long)(f: => Unit) = handler.postDelayed(
    AndroidConversions.byNameToRunnable(f), delay)
}

object SpannedGenerator {
  def span(style: Object, text: CharSequence) = {
    val s = new SpannableString(text)
    s.setSpan(style, 0, text.length, Spanned.SPAN_INCLUSIVE_EXCLUSIVE)
    s
  }
  def textColor(color: Int, text: CharSequence) =
    span(new ForegroundColorSpan(color), text)

  def bold(text: CharSequence) = span(new StyleSpan(Typeface.BOLD) , text)

  def italics(text: CharSequence) = span(new StyleSpan(Typeface.ITALIC), text)

  val DIGITS = Set('0','1','2','3','4','5','6','7','8','9')
}

case class SpannedGenerator(fmt: String) {
  def formatSpans(items: CharSequence*): Spanned = {
    val builder = new SpannableStringBuilder()
    val idx = fmt indexOf "%"

    formatNext(builder, fmt, 0, idx, items)

    builder
  }

  @tailrec
  private def formatNext(s: SpannableStringBuilder, fmt: String,
                 cur: Int, next: Int, items: Seq[CharSequence]) {
    if (next == -1) {
      s.append(fmt.substring(cur, fmt.length))
    } else {
      s.append(fmt.substring(cur, next))
      val space = fmt.indexWhere((!SpannedGenerator.DIGITS(_)), next + 1)
      val number = fmt.substring(next + 1,
        if (space < 0) fmt.length else space).toInt
      s.append(Option(items(number - 1)) getOrElse "")
      if (space > 0)
        formatNext(s, fmt, space, fmt indexOf ("%", space), items)
    }
  }

}

case class LogcatTag(tag: String)

object RichLogger {
  def d(msg: String)(implicit tag: LogcatTag) = Log.d(tag.tag, msg)
  def d(msg: String, e: Exception)(implicit tag: LogcatTag) =
    Log.d(tag.tag, msg, e)
  def d(msg: String, args: String*)(implicit tag: LogcatTag) =
    Log.d(tag.tag, msg format(args:_*))
  def v(msg: String)(implicit tag: LogcatTag) = Log.v(tag.tag, msg)
  def v(msg: String, e: Exception)(implicit tag: LogcatTag) =
    Log.v(tag.tag, msg, e)
  def v(msg: String, args: Any*)(implicit tag: LogcatTag) =
    Log.v(tag.tag, msg format(args:_*))
  def i(msg: String)(implicit tag: LogcatTag) = Log.i(tag.tag, msg)
  def i(msg: String, e: Exception)(implicit tag: LogcatTag) =
    Log.i(tag.tag, msg, e)
  def i(msg: String, args: Any*)(implicit tag: LogcatTag) =
    Log.i(tag.tag, msg format(args:_*))
  def w(msg: String)(implicit tag: LogcatTag) = Log.w(tag.tag, msg)
  def w(msg: String, e: Exception)(implicit tag: LogcatTag) =
    Log.w(tag.tag, msg, e)
  def w(msg: String, args: Any*)(implicit tag: LogcatTag) =
    Log.w(tag.tag, msg format(args:_*))
  def e(msg: String)(implicit tag: LogcatTag) = Log.e(tag.tag, msg)
  def e(msg: String, e: Exception)(implicit tag: LogcatTag) =
    Log.e(tag.tag, msg, e)
  def e(msg: String, args: Any*)(implicit tag: LogcatTag) =
    Log.e(tag.tag, msg format(args:_*))
}
