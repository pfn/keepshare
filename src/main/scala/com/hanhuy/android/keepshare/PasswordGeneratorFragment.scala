package com.hanhuy.android.keepshare

import android.annotation.TargetApi
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
  def chartype(ct: EntryViewActivity.CharType) = Stream.continually(ct)
  val UPPER = ('A' to 'Z').zip(chartype(EntryViewActivity.Uppercase)).toVector
  val LOWER = ('a' to 'z').zip(chartype(EntryViewActivity.Lowercase)).toVector
  val NUMS  = ('0' to '9').zip(chartype(EntryViewActivity.Digit)).toVector
  val SYMS  = ('!' ::
    '@' :: '#' :: '$' :: '%' :: '^' :: '&' ::
    '*' :: '+' :: '=' :: '?' :: '/' :: '\\' ::
    Nil).zip(chartype(EntryViewActivity.Symbol)).toVector
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
  lazy val uppSlider   = new SeekBar(getActivity)
  lazy val numSlider   = new SeekBar(getActivity)
  lazy val symSlider   = new SeekBar(getActivity)

  def stdH[A <: View]: Kestrel[A] = c[LinearLayout](lp(MATCH_PARENT, 48.dp): Kestrel[A])
  def checkbox(implicit ctx: Context) = if (Build.VERSION.SDK_INT >= 21)
    new CheckBox(ctx) else new android.support.v7.widget.AppCompatCheckBox(ctx)
  val MODEL = "keepshare.generator.model"

  setStyle(DialogFragment.STYLE_NORMAL, android.R.style.Theme_DeviceDefault_Light_Dialog)

  @TargetApi(21)
  def generatedBar(state: PasswordGeneratorModel) = l[FrameLayout](
    IO(generatedPassword) >>= k.text(generate(state)) >>=
      k.textAppearance(getActivity, android.R.style.TextAppearance_Medium) >>=
      k.gravity(Gravity.CENTER) >>=
      k.backgroundColor(getResources.getColor(android.R.color.darker_gray)) >>= lp(MATCH_PARENT, MATCH_PARENT),
    IO(new ImageButton(getActivity, null, R.attr.borderlessButtonStyle)) >>=
      k.imageResource(R.drawable.ic_refresh_black_24dp) >>=
      lp(WRAP_CONTENT, WRAP_CONTENT, Gravity.LEFT | Gravity.CENTER) >>=
      hook0.click(for {
        st <- transformState(identity)
        _  <- IO(generatedPassword) >>= k.text(generate(st))
      } yield ()),
    IO(new ImageButton(getActivity, null, R.attr.borderlessButtonStyle)) >>=
      k.imageResource(R.drawable.ic_content_copy_black_24dp) >>=
      lp(WRAP_CONTENT, WRAP_CONTENT, Gravity.RIGHT | Gravity.CENTER) >>=
      hook0.click(IO {
        systemService[ClipboardManager].setPrimaryClip(ClipData.newPlainText("", generatedPassword.getText))
        Toast.makeText(getActivity, R.string.copied_to_clipboard, Toast.LENGTH_SHORT).show()
      })
  ) >>= stdH >>= condK(v(21) ? k.elevation(12.dp))

  case class SeekBarInfo(bar: SeekBar, min: PasswordGeneratorModel => Int, max: PasswordGeneratorModel => Int)
  lazy val upperBar = SeekBarInfo(uppSlider, _.minUpper, s => s.minSym + s.minNum)
  lazy val numBar   = SeekBarInfo(numSlider, _.minNum,   s => s.minSym + s.minUpper)
  lazy val symBar   = SeekBarInfo(symSlider, _.minSym,   s => s.minNum + s.minUpper)
  def newSlider(state: PasswordGeneratorModel, labelres: Int, label: TextView, countRes: Int,
                slider: SeekBar, enableTransform: (PasswordGeneratorModel, Boolean) => PasswordGeneratorModel,
                isEnabled: PasswordGeneratorModel => Boolean, min: PasswordGeneratorModel => Int,
                updateMin: (PasswordGeneratorModel,Int) => PasswordGeneratorModel,
                seek1: SeekBarInfo, seek2: SeekBarInfo) = {
    c[LinearLayout](l[LinearLayout](
      IO(checkbox) >>= stdH >>= k.text(labelres) >>= k.checked(isEnabled(state)) >>=
        hookM.checkedChange.onCheckedChanged((v: CompoundButton, b: Boolean) => enable(slider, b, transformState(enableTransform(_,b)))),

      IO(label) >>= k.text(getString(countRes, min(state).asInstanceOf[Integer])),

      IO(slider) >>= stdH >>= k.max(state.length) >>= k.progress(min(state)) >>= k.enabled(isEnabled(state)) >>=
        hookM.seekBarChange.onProgressChanged((sb: SeekBar, p: Int, user: Boolean) => for {
          st <- transformState(updateMin(_,p))
          _ <- IO(label) >>= k.text(getString(countRes, p.asInstanceOf[Integer]))
          _ <- IO(seek1.bar) >>= k.progress(math.min(seek1.min(st), math.max(st.length - seek1.max(st), 0)))
          _ <- IO(seek2.bar) >>= k.progress(math.min(seek2.min(st), math.max(st.length - seek2.max(st), 0)))
        } yield ())
    ) >>= k.orientation(LinearLayout.VERTICAL) >>= lp(MATCH_PARENT, WRAP_CONTENT))
  }

  override def applyState[T](s: FragmentState[T]) = s match {
    case OnCreateView(state, inflater, container) => s.applyResult(l[LinearLayout](
      generatedBar(state),
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
          IO(checkbox) >>= stdH >>= k.text(R.string.lowercase_enable) >>= k.checked(state.lower) >>=
            hookM.checkedChange.onCheckedChanged((v: CompoundButton, b: Boolean) => transformState(_.copy(lower = b))),
          newSlider(state, R.string.uppercase_enable, minUppLabel, R.string.minimum_uppercase,
            uppSlider, (s,b) => s.copy(upper = b), _.upper, _.minUpper, (s,c) => s.copy(minUpper = c),
            numBar, symBar),
          newSlider(state, R.string.numbers_enable, minNumLabel, R.string.minimum_numbers,
            numSlider, (s,b) => s.copy(num = b), _.num, _.minNum, (s,c) => s.copy(minNum = c),
            symBar, upperBar),
          newSlider(state, R.string.symbols_enable, minSymLabel, R.string.minimum_symbols,
            symSlider, (s,b) => s.copy(sym = b), _.sym, _.minSym, (s,c) => s.copy(minSym = c),
            numBar, upperBar)
        ) >>= k.orientation(LinearLayout.VERTICAL) >>= padding(all = 16.dp)
      ) >>= lp(MATCH_PARENT, WRAP_CONTENT, 1)
    ) >>= k.orientation(LinearLayout.VERTICAL))

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
    import util.Random._
    val random = new java.security.SecureRandom
    val ALL =
      (if (state.lower)   LOWER else Nil) ++
        (if (state.upper) UPPER else Nil) ++
        (if (state.num)   NUMS  else Nil) ++
        (if (state.sym)   SYMS  else Nil)
    if (ALL.isEmpty) {
      getString(R.string.unable_to_generate_password)
    } else {
      val required = (if (state.upper) {
        (0 until state.minUpper) map (_ => UPPER(random.nextInt(UPPER.size)))
      } else Nil) ++ (if (state.num) {
        (0 until state.minNum)   map (_ =>  NUMS(random.nextInt(NUMS.size)))
      } else Nil) ++ (if (state.sym) {
        (0 until state.minSym)   map (_ =>  SYMS(random.nextInt(SYMS.size)))
      } else Nil)

      // drop from required list when found, reduces disproportionate occurance with minimum
      @annotation.tailrec
      def assemblePW(reqs: List[(Char,EntryViewActivity.CharType)], selected: List[(Char,EntryViewActivity.CharType)]): List[(Char,EntryViewActivity.CharType)] = {
        if (reqs.size + selected.size >= state.length)
          reqs ++ selected
        else {
          val c = ALL(random.nextInt(ALL.size))
          val (xs, ys) = reqs.span(_._2 != c._2)
          assemblePW(xs ++ ys.drop(1), c :: selected)
        }
      }

      EntryViewActivity.colorPassword(random.shuffle(assemblePW(required.toList, Nil).map(_._1)).mkString)
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
      setButton(DialogInterface.BUTTON_POSITIVE, positiveLabel,
        (dlg: DialogInterface, b: Int) => onPositiveClick())
      setButton(DialogInterface.BUTTON_NEGATIVE, negativeLabel,
        (dlg: DialogInterface, b: Int) => onNegativeClick())

      override def setContentView(view: View) = setView(view)
    }
  }
}
