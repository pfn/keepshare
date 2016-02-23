package com.hanhuy.android.keepshare

import android.app.DialogFragment
import android.content.{ClipData, ClipboardManager, DialogInterface, Context}
import android.os.{Build, Bundle}
import android.view.{View, Gravity, ViewGroup}
import android.widget._
import iota.pure.PureFragment

import PasswordGeneratorFragment._
/**
  * @author pfnguyen
  */
object PasswordGeneratorFragment {
  object PasswordGeneratorModel {
    def empty = PasswordGeneratorModel()
  }
  case class PasswordGeneratorModel(length: Int = 8,
                                    lower: Boolean = true,
                                    upper: Boolean = true,
                                    num: Boolean = true,
                                    sym: Boolean = true,
                                    minUpper: Int = 1,
                                    minNum: Int = 1,
                                    minSym: Int = 1)

  val UPPER = 'A' to 'Z'
  val LOWER = 'a' to 'z'
  val NUMS = '0' to '9'
  val SYMS = '!' :: '@' :: '#' :: '$' :: '%' :: '^' :: '&' :: '*' :: '+' :: '=' :: '?' :: '/' :: '\\' :: Nil
}
class PasswordGeneratorFragment extends AlertDialogFragment with PureFragment[PasswordGeneratorModel] {
  import iota._
  import ViewGroup.LayoutParams._
  lazy val lengthSlider = {
    val s = new SeekBar(getActivity)
    s.setMax(24)
    s
  }
  lazy val generatedPassword = new TextView(getActivity)
  lazy val lengthLabel = new TextView(getActivity)
  lazy val minUppLabel = new TextView(getActivity)
  lazy val minNumLabel = new TextView(getActivity)
  lazy val minSymLabel = new TextView(getActivity)
  lazy val uppSlider = new SeekBar(getActivity)
  lazy val numSlider = new SeekBar(getActivity)
  lazy val symSlider = new SeekBar(getActivity)
  def stdH[A <: View]: Kestrel[A] = c[LinearLayout](lp(MATCH_PARENT, 48.dp): Kestrel[A])
  def checkbox(implicit ctx: Context) = if (Build.VERSION.SDK_INT >= 21)
    new CheckBox(ctx) else new android.support.v7.widget.AppCompatCheckBox(ctx)
  val MODEL = "keepshare.generator.model"
  setStyle(DialogFragment.STYLE_NORMAL, android.R.style.Theme_DeviceDefault_Light_Dialog)
  override def applyState[T](s: FragmentState[T]) = s match {
    case OnCreateView(state, inflater, container) => s.applyResult(l[LinearLayout](
      l[FrameLayout](
        IO(generatedPassword) >>= k.text(generate(state)) >>=
          k.textAppearance(getActivity, android.R.style.TextAppearance_Medium) >>=
          k.gravity(Gravity.CENTER) >>=
          k.backgroundColor(getResources.getColor(android.R.color.darker_gray)) >>= lp(MATCH_PARENT, MATCH_PARENT),
        IO(new ImageButton(getActivity, null, R.attr.borderlessButtonStyle)) >>= k.imageResource(R.drawable.ic_refresh_black_24dp) >>= lp(WRAP_CONTENT, WRAP_CONTENT, Gravity.LEFT | Gravity.CENTER) >>=
        hook0.click(for {
          st <- transformState(identity)
          _  <- IO(generatedPassword) >>= k.text(generate(st))
        } yield ()),
        IO(new ImageButton(getActivity, null, R.attr.borderlessButtonStyle)) >>= k.imageResource(R.drawable.ic_content_copy_black_24dp) >>= lp(WRAP_CONTENT, WRAP_CONTENT, Gravity.RIGHT | Gravity.CENTER) >>=
        hook0.click(IO {
          systemService[ClipboardManager].setPrimaryClip(ClipData.newPlainText("", generatedPassword.getText))
          Toast.makeText(getActivity, "Copied to clipboard", Toast.LENGTH_SHORT).show()
        })
      ) >>= stdH >>= condK(v(21) ? k.elevation(12.dp)),
      l[ScrollView](
        l[LinearLayout](
          IO(lengthLabel) >>= k.text(getString(R.string.minimum_length, state.length.asInstanceOf[Integer])),
          IO(lengthSlider) >>= stdH >>= k.progress(state.length - 8) >>=
            hookM.seekBarChange.onProgressChanged((sb: SeekBar, p: Int, user: Boolean) => for {
              st <- transformState(_.copy(length = p + 8))
              _  <- IO(lengthLabel) >>= k.text(getString(R.string.minimum_length, st.length.asInstanceOf[Integer]))
              _  <- IO(uppSlider) >>= k.max(st.length) >>= k.progress(math.min(st.minUpper, math.max(st.length - st.minNum - st.minSym, 0)))
              _  <- IO(numSlider) >>= k.max(st.length) >>= k.progress(math.min(st.minNum, math.max(st.length - st.minUpper - st.minSym, 0)))
              _  <- IO(symSlider) >>= k.max(st.length) >>= k.progress(math.min(st.minSym, math.max(st.length - st.minUpper - st.minNum, 0)))
            } yield ()),
          IO(checkbox) >>= stdH >>= k.text("Lowercase (a-z)") >>= k.checked(state.lower) >>=
            hookM.checkedChange.onCheckedChanged((v: CompoundButton, b: Boolean) => transformState(_.copy(lower = b))),
          IO(checkbox) >>= stdH >>= k.text("Uppercase (A-Z)") >>= k.checked(state.upper) >>=
            hookM.checkedChange.onCheckedChanged((v: CompoundButton, b: Boolean) => enable(uppSlider, b, transformState(_.copy(upper = b)))),
          IO(minUppLabel) >>= k.text(getString(R.string.minimum_uppercase, state.minUpper.asInstanceOf[Integer])),
          IO(uppSlider) >>= stdH >>= k.max(state.length) >>= k.progress(state.minUpper) >>= k.enabled(state.upper) >>=
            hookM.seekBarChange.onProgressChanged((sb: SeekBar, p: Int, user: Boolean) => for {
              st <- transformState(_.copy(minUpper = p))
              _  <- IO(minUppLabel) >>= k.text(getString(R.string.minimum_uppercase, p.asInstanceOf[Integer]))
              _  <- IO(numSlider) >>= k.progress(math.min(st.minNum, math.max(st.length - st.minUpper - st.minSym, 0)))
              _  <- IO(symSlider) >>= k.progress(math.min(st.minSym, math.max(st.length - st.minUpper - st.minNum, 0)))
            } yield ()),
          IO(checkbox) >>= stdH >>= k.text("Numbers (0-9)") >>= k.checked(state.num) >>=
            hookM.checkedChange.onCheckedChanged((v: CompoundButton, b: Boolean) => enable(numSlider, b, transformState(_.copy(num = b)))),
          IO(minNumLabel) >>= k.text(getString(R.string.minimum_numbers, state.minNum.asInstanceOf[Integer])),
          IO(numSlider) >>= stdH >>= k.max(state.length) >>= k.progress(state.minNum) >>= k.enabled(state.num) >>=
            hookM.seekBarChange.onProgressChanged((sb: SeekBar, p: Int, user: Boolean) => for {
              st <- transformState(_.copy(minNum = p))
              _  <- IO(minNumLabel) >>= k.text(getString(R.string.minimum_numbers, p.asInstanceOf[Integer]))
              _  <- IO(uppSlider) >>= k.progress(math.min(st.minUpper, math.max(st.length - st.minNum - st.minSym, 0)))
              _  <- IO(symSlider) >>= k.progress(math.min(st.minSym, math.max(st.length - st.minUpper - st.minNum, 0)))
            } yield ()),
          IO(checkbox) >>= stdH >>= k.text("Symbols (!@#$%^&*+=?/\\)") >>= k.checked(state.sym) >>=
            hookM.checkedChange.onCheckedChanged((v: CompoundButton, b: Boolean) => enable(symSlider, b, transformState(_.copy(sym = b)))),
          IO(minSymLabel) >>= k.text(getString(R.string.minimum_symbols, state.minSym.asInstanceOf[Integer])),
          IO(symSlider) >>= stdH >>= k.max(state.length) >>= k.progress(state.minSym) >>= k.enabled(state.sym) >>=
            hookM.seekBarChange.onProgressChanged((sb: SeekBar, p: Int, user: Boolean) => for {
              st <- transformState(_.copy(minSym = p))
              _  <- IO(minSymLabel) >>= k.text(getString(R.string.minimum_symbols, p.asInstanceOf[Integer]))
              _  <- IO(uppSlider) >>= k.progress(math.min(st.minUpper, math.max(st.length - st.minNum - st.minSym, 0)))
              _  <- IO(numSlider) >>= k.progress(math.min(st.minNum, math.max(st.length - st.minUpper - st.minSym, 0)))
            } yield ())
        ) >>= k.orientation(LinearLayout.VERTICAL) >>= padding(all = 16.dp)
      ) >>= lp(MATCH_PARENT, WRAP_CONTENT, 1)
    ) >>= k.orientation(LinearLayout.VERTICAL)
    )
    case SaveState(state, bundle) => s(IO(bundle.putSerializable(MODEL, state)))
    // TODO save settings to shared prefs
    case TransformState(st,_) => s(IO(generatedPassword) >>= k.text(generate(st)))
    case x => defaultApplyState(x)
  }

  def enable(slider: SeekBar, enabled: Boolean, state: IO[PasswordGeneratorModel]): IO[Unit] = for {
    st <- state
    _  <- IO(slider) >>= k.enabled(enabled)
  } yield ()

  def generate(state: PasswordGeneratorModel): CharSequence = {
    val random = new java.security.SecureRandom
    val ALL =
      (if (state.lower)   LOWER else Nil) ++
        (if (state.upper) UPPER else Nil) ++
        (if (state.num)   NUMS  else Nil) ++
        (if (state.sym)   SYMS  else Nil)
    if (ALL.isEmpty) {
      "UNABLE TO GENERATE"
    } else {
      val required = (if (state.upper) {
        (0 until state.minUpper) map (_ => UPPER(random.nextInt(UPPER.size)))
      } else Nil) ++ (if (state.num) {
        (0 until state.minNum) map (_ => NUMS(random.nextInt(NUMS.size)))
      } else Nil) ++ (if (state.sym) {
        (0 until state.minSym) map (_ => SYMS(random.nextInt(SYMS.size)))
      } else Nil)
      val remainder = (0 until math.max(0, state.length - required.size)) map (_ => ALL(random.nextInt(ALL.size)))

      EntryViewActivity.colorPassword(util.Random.shuffle(required ++ remainder).mkString)
    }
  }
  override def title = getString(R.string.generate_password)

  // TODO load settings from shared prefs
  override def initialState(savedState: Option[Bundle], arguments: Option[Bundle]) =
    savedState.fold(PasswordGeneratorModel.empty)(_.getSerializable(MODEL).asInstanceOf[PasswordGeneratorModel])

  override def onPositiveClick() = getActivity.asInstanceOf[EntryViewActivity].saveGeneratedPassword(generatedPassword.getText)
}

abstract class AlertDialogFragment extends DialogFragment {
  def title: String
  def onPositiveClick() = ()
  def onNegativeClick() = ()
  def positiveLabel = getString(R.string.save)
  def negativeLabel = getString(R.string.cancel)

  override def onCreateDialog(savedInstanceState: Bundle) = {
    import com.hanhuy.android.conversions._
    new android.app.AlertDialog(getActivity, R.style.DialogTheme) {
      setTitle(title)
      setButton(DialogInterface.BUTTON_POSITIVE, positiveLabel, (dlg: DialogInterface, b: Int) => {
        onPositiveClick()
      })
      setButton(DialogInterface.BUTTON_NEGATIVE, negativeLabel, (dlg: DialogInterface, b: Int) => {
        onNegativeClick()
      })

      override def setContentView(view: View) = setView(view)
    }
  }
}
