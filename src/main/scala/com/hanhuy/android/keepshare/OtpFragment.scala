package com.hanhuy.android.keepshare

import android.animation.Animator.AnimatorListener
import android.animation.ObjectAnimator
import android.app.FragmentManager
import android.os.Bundle
import android.view.{View, Gravity}
import android.view.ViewGroup.LayoutParams._
import android.view.animation.LinearInterpolator
import android.widget.{Toast, LinearLayout, ProgressBar, TextView}
import com.google.common.io.BaseEncoding
import com.hanhuy.android.common.UiBus
import com.hanhuy.keepassj._
import iota._
import iota.pure.PureFragment

import scala.util.Try

import OtpFragment._
/**
  * @author pfnguyen
  */
object OtpFragment {
  def show(fm: FragmentManager, entry: PwEntry): Unit = {
    val f = new OtpFragment
    val b = new Bundle
    b.putString(EntryViewActivity.EXTRA_ENTRY_ID, entry.getUuid.ToHexString)
    f.setArguments(b)
    f.show(fm, "otp-view")
  }
  case class TimeOtpModel(key: Array[Byte], t0: Long, step: Int, size: Int, icon: Int, title: Option[String], username: Option[String])
}
class OtpFragment extends AlertDialogFragment with PureFragment[Option[TimeOtpModel]] {
  override def hasPositiveButton = false
  override def title = "OTP"

  override def initialState(savedState: Option[Bundle], arguments: Option[Bundle]) = {
    val entry = for {
      args <- arguments
      id   <- Option(args.getString(EntryViewActivity.EXTRA_ENTRY_ID))
      root <- Database.rootGroup
    } yield root.FindEntry(new PwUuid(KeyManager.bytes(id)), true)
    entry map { e =>
      val strings = e.getStrings
      def getKey(k: String, f: String => Array[Byte]): Option[Array[Byte]] =
        Option(strings.Get(k)).map(ps => f(ps.ReadString()))
      def getValue[A](k: String, default: A)(f: String => Option[A]): A =
        Option(strings.Get(k)).flatMap(ps => f(ps.ReadString())).getOrElse(default)
      val key =
        getKey("HmacOtp-Secret",        s => s.getBytes(StrUtil.Utf8))                    orElse
          getKey("HmacOtp-Secret-Hex",    s => MemUtil.HexStringToByteArray(s.toUpperCase)) orElse
          getKey("HmacOtp-Secret-Base32", s => MemUtil.ParseBase32(s.toUpperCase))          orElse
          getKey("HmacOtp-Secret-Base64", s => BaseEncoding.base64().decode(s.toUpperCase)) getOrElse
          Array.ofDim[Byte](0)

      val t0 = getValue("TimeOtp-T0",     0l)(s => Try(s.toLong).toOption)
      val step = getValue("TimeOtp-Step", 30)(s => Try(s.toInt).toOption)
      val size = getValue("TimeOtp-Size",  6)(s => Try(s.toInt).toOption)
      val title = Option(strings.Get(PwDefs.TitleField)).map(_.ReadString)
      val username = Option(strings.Get(PwDefs.UserNameField)).map(_.ReadString)
      val icon = Database.Icons(e.getIconId.ordinal)
      TimeOtpModel(key, t0, step, size, icon, title, username)
    }
  }

  lazy val otpfield = new TextView(getActivity)
  lazy val progressBar = new ProgressBar(getActivity, null, android.R.attr.progressBarStyleHorizontal)
  lazy val animator = ObjectAnimator.ofInt(progressBar, "progress", 3000, 0)

  override def applyState[T](s: FragmentState[T]) = s match {
    case OnResume(state) => s(IO {
      animator.setInterpolator(new LinearInterpolator)
      animator.removeAllListeners()
      if (state.isEmpty) {
        Toast.makeText(getActivity, "No OTP info", Toast.LENGTH_SHORT).show()
        dismiss()
      }

      def animate(): Unit = {
        val rem = 3000 - System.currentTimeMillis / 10 % 3000
        animator.setIntValues(if (rem <= 0) 3000 else rem.toInt, 0)
        animator.setDuration(rem * 10)
        UiBus.post(animator.start())

        val t = System.currentTimeMillis / 1000
        state foreach { st =>
          otpfield.setText(HmacOtp.Generate(st.key,
            (t - st.t0) / st.step, st.size, false, -1))
        }
      }
      val listener: AnimatorListener = BrowseActivity.animationEnd { _ =>
        animate()
      }
      animator.addListener(listener)
      animate()
    })
    case OnPause(state) => s(IO(
      animator.cancel()
    ))
    case OnCreateView(state, inflater, container) => s.applyResult(
      l[LinearLayout](
        IO(otpfield) >>=
          k.backgroundColor(getResources.getColor(android.R.color.darker_gray)) >>=
          k.text("OTP HERE") >>=
          k.gravity(Gravity.CENTER) >>=
          k.textAppearance(getActivity, R.style.TextAppearance_AppCompat_Large) >>=
          lp(MATCH_PARENT, 48.dp),
        IO(progressBar) >>= kestrel { p =>
          p.setIndeterminate(false)
          p.setMax(3000)
          p.setProgress(0)
        } >>= lp(MATCH_PARENT, WRAP_CONTENT),
        w[StandardFieldView] >>= kestrel { v =>
          val title = state.flatMap(_.title).filter(_.trim.nonEmpty)
          v.first = true
          v.text = title.getOrElse("")
          v.hint = "Title"
          v.icon = state.map(_.icon).getOrElse(R.mipmap.i00_password)
          if (title.isEmpty)
            v.setVisibility(View.GONE)
        },
        w[StandardFieldView] >>= kestrel { v =>
          val user = state.flatMap(_.username).filter(_.trim.nonEmpty)
          v.first = true
          v.text = user.getOrElse("")
          v.hint = "Username"
          v.icon = R.drawable.ic_account_box_black_36dp
          if (user.isEmpty)
            v.setVisibility(View.GONE)
        }
      ) >>= k.orientation(LinearLayout.VERTICAL)
    )
    case x => defaultApplyState(x)
  }
}
