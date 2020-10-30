package org.http4s
package parsley.instructions

import parsley.{UnsafeOption, XCompat}
import parsley.instructions.Stack.push

import scala.annotation.tailrec
import scala.language.implicitConversions
import scala.collection.mutable

private [parsley] final class Perform[-A, +B](f: A => B) extends Instr
{
    private [Perform] val g = f.asInstanceOf[Any => B]
    override def apply(ctx: Context): Unit =
    {
        ctx.stack.exchange(g(ctx.stack.upeek))
        ctx.inc()
    }
    override def toString: String = "Perform(?)"
}

private [parsley] final class Exchange[A](private [Exchange] val x: A) extends Instr
{
    override def apply(ctx: Context): Unit =
    {
        ctx.stack.exchange(x)
        ctx.inc()
    }
    override def toString: String = s"Ex($x)"
}

private [parsley] class Newline(_expected: UnsafeOption[String]) extends CharTok('\n', _expected)
{
    override def apply(ctx: Context): Unit =
    {
        if (ctx.moreInput && ctx.nextChar == '\n')
        {
            ctx.stack.push(ac)
            ctx.offset += 1
            ctx.col = 1
            ctx.line += 1
            ctx.inc()
        }
        else ctx.fail(expected)
    }
}

private [parsley] class Tab(_expected: UnsafeOption[String]) extends CharTok('\t', _expected)
{
    override def apply(ctx: Context): Unit =
    {
        if (ctx.moreInput && ctx.nextChar == '\t')
        {
            ctx.stack.push(ac)
            ctx.offset += 1
            ctx.col += 4 - ((ctx.col - 1) & 3)
            ctx.inc()
        }
        else ctx.fail(expected)
    }
}

private [parsley] class CharTokFastPerform protected (protected final val c: Char, protected final val f: Char => Any, _expected: UnsafeOption[String]) extends Instr
{
    protected val expected: String = if (_expected == null) "\"" + c.toString + "\"" else _expected
    protected final val fc: Any = f(c)
    override def apply(ctx: Context): Unit =
    {
        if (ctx.moreInput && ctx.nextChar == c)
        {
            ctx.stack.push(fc)
            ctx.offset += 1
            ctx.col += 1
            ctx.inc()
        }
        else ctx.fail(expected)
    }
    override final def toString: String = s"ChrPerform($c, ?)"
}

private [parsley] final class NewlineFastPerform(g: Char => Any, _expected: UnsafeOption[String]) extends CharTokFastPerform('\n', g, _expected)
{
    override def apply(ctx: Context): Unit =
    {
        if (ctx.moreInput && ctx.nextChar == '\n')
        {
            ctx.stack.push(fc)
            ctx.offset += 1
            ctx.col = 1
            ctx.line += 1
            ctx.inc()
        }
        else ctx.fail(expected)
    }
}

private [parsley] final class TabFastPerform(g: Char => Any, _expected: UnsafeOption[String]) extends CharTokFastPerform('\t', g, _expected)
{
    override def apply(ctx: Context): Unit =
    {
        if (ctx.moreInput && ctx.nextChar == '\t')
        {
            ctx.stack.push(fc)
            ctx.offset += 1
            ctx.col += 4 - ((ctx.col - 1) & 3)
            ctx.inc()
        }
        else ctx.fail(expected)
    }
}

private [parsley] final class StringTokFastPerform(s: String, f: String => Any, _expected: UnsafeOption[String]) extends Instr
{
    protected val expected: String = if (_expected == null) "\"" + s + "\"" else _expected
    private [this] val cs = s.toCharArray
    private [this] val sz = cs.length
    private [this] val fs: Any = f(s)
    private [this] val adjustAtIndex = new Array[(Int => Int, Int => Int)](s.length + 1)
    def makeAdjusters(col: Int, line: Int, tabprefix: Option[Int]): (Int => Int, Int => Int) =
        if (line > 0) ((_: Int) => col, (x: Int) => x + line)
        else (tabprefix match
        {
            case Some(prefix) =>
                val outer = 4 + col + prefix
                val inner = prefix - 1
                (x: Int) => outer + x - ((x + inner) & 3)
            case None => (x: Int) => x + col
        }, (x: Int) => x)
    @tailrec def compute(cs: Array[Char], i: Int = 0, col: Int = 0, line: Int = 0)(implicit tabprefix: Option[Int] = None): Unit =
    {
        adjustAtIndex(i) = makeAdjusters(col, line, tabprefix)
        if (i < cs.length) cs(i) match
        {
            case '\n' => compute(cs, i+1, 1, line + 1)(Some(0))
            case '\t' if tabprefix.isEmpty => compute(cs, i+1, 0, line)(Some(col))
            case '\t' => compute(cs, i+1, col + 4 - ((col-1) & 3), line)
            case _ => compute(cs, i+1, col + 1, line)
        }
    }
    compute(cs)
    override def apply(ctx: Context): Unit =
    {
        val strsz = this.sz
        val inputsz = ctx.inputsz
        val input = ctx.input
        var i = ctx.offset
        var j = 0
        val cs = this.cs
        if (inputsz != i)
        {
            while (j < strsz)
            {
                val c = cs(j)
                if (i == inputsz || input(i) != c)
                {
                    ctx.offset = i
                    val (colAdjust, lineAdjust) = adjustAtIndex(j)
                    ctx.col = colAdjust(ctx.col)
                    ctx.line = lineAdjust(ctx.line)
                    ctx.fail(expected)
                    return
                }
                i += 1
                j += 1
            }
            val (colAdjust, lineAdjust) = adjustAtIndex(j)
            ctx.col = colAdjust(ctx.col)
            ctx.line = lineAdjust(ctx.line)
            ctx.offset = i
            ctx.stack.push(fs)
            ctx.inc()
        }
        else ctx.fail(expected)
    }
    override def toString: String = s"StrPerform($s, ?)"
}

private [parsley] final class SatisfyExchange[A](f: Char => Boolean, x: A, expected: UnsafeOption[String]) extends Instr
{
    override def apply(ctx: Context): Unit =
    {
        if (ctx.moreInput)
        {
            val c = ctx.nextChar
            if (f(ctx.nextChar))
            {
                ctx.stack.push(x)
                ctx.offset += 1
                c match
                {
                    case '\n' => ctx.line += 1; ctx.col = 1
                    case '\t' => ctx.col += 4 - ((ctx.col - 1) & 3)
                    case _ => ctx.col += 1
                }
                ctx.inc()
            }
            else ctx.fail(expected)
        }
        else ctx.fail(expected)
    }
    override def toString: String = s"SatEx(?, $x)"
}

private [parsley] final class JumpGoodAttempt(var label: Int) extends JumpInstr
{
    override def apply(ctx: Context): Unit =
    {
        if (ctx.status eq Good)
        {
            ctx.states = ctx.states.tail
            ctx.handlers = ctx.handlers.tail
            ctx.pc = label
        }
        else
        {
            val state = ctx.states.head
            ctx.states = ctx.states.tail
            ctx.offset = state.offset
            ctx.line = state.line
            ctx.col = state.col
            ctx.regs = state.regs
            ctx.status = Good
            ctx.inc()
        }
    }
    override def toString: String = s"JumpGood'($label)"
}

private [parsley] final class RecoverWith[A](x: A) extends Instr
{
    override def apply(ctx: Context): Unit =
    {
        if (ctx.offset != ctx.checkStack.head) ctx.fail()
        else
        {
            ctx.status = Good
            ctx.stack.push(x)
            ctx.inc()
        }
        ctx.checkStack = ctx.checkStack.tail
    }
    override def toString: String = s"Recover($x)"
}

private [parsley] final class AlwaysRecoverWith[A](x: A) extends Instr
{
    override def apply(ctx: Context): Unit =
    {
        if (ctx.status eq Good)
        {
            ctx.states = ctx.states.tail
            ctx.handlers = ctx.handlers.tail
            ctx.inc()
        }
        else
        {
            val state = ctx.states.head
            ctx.states = ctx.states.tail
            ctx.offset = state.offset
            ctx.line = state.line
            ctx.col = state.col
            ctx.regs = state.regs
            ctx.status = Good
            ctx.stack.push(x)
            ctx.inc()
        }
    }
    override def toString: String = s"AlwaysRecover($x)"
}

private [parsley] final class JumpTable(prefixes: List[Char], labels: List[Int], private [this] var default: Int, _expecteds: List[UnsafeOption[String]]) extends Instr
{
    private [this] var defaultPreamble: Int = _
    private [this] val jumpTable = mutable.LongMap(prefixes.map(_.toLong).zip(labels): _*)
    val expecteds = prefixes.zip(_expecteds).map{case (c, expected) => if (expected == null) "\"" + c + "\"" else expected}

    override def apply(ctx: Context): Unit =
    {
        if (ctx.moreInput)
        {
            val dest = jumpTable.getOrElseUpdate(ctx.nextChar, default)
            ctx.pc = dest
            if (dest == default) addErrors(ctx)
            else
            {
                ctx.checkStack = push(ctx.checkStack, ctx.offset)
                ctx.handlers = push(ctx.handlers, new Handler(ctx.depth, defaultPreamble, ctx.stack.usize))
            }
        }
        else
        {
            addErrors(ctx)
            ctx.pc = default
        }
    }

    private def addErrors(ctx: Context): Unit =
    {
        if (ctx.offset > ctx.erroffset)
        {
            ctx.erroffset = ctx.offset
            ctx.errcol = ctx.col
            ctx.errline = ctx.line
            ctx.unexpected = if (ctx.offset < ctx.inputsz) "\"" + ctx.nextChar + "\"" else "end of input"
            ctx.expected = if (ctx.errorOverride == null) expecteds else ctx.errorOverride::Nil
            ctx.raw = Nil
            ctx.unexpectAnyway = false
        }
        else if (ctx.offset == ctx.erroffset)
        {
            if (ctx.errorOverride == null) ctx.expected = ctx.expected reverse_::: expecteds
            else ctx.expected ::= ctx.errorOverride
        }
    }

    private [parsley] def relabel(labels: Array[Int]): Unit =
    {
        XCompat.mapValuesInPlace(jumpTable)((_, v) => labels(v))
        default = labels(default)
        defaultPreamble = default - 1
    }
    override def toString: String = s"JumpTable(${jumpTable.map{case (k, v) => k.toChar -> v}.mkString(", ")}, _ -> $default)"
}

private [parsley] object CharTokFastPerform
{
    def apply[A >: Char, B](c: Char, f: A => B, expected: UnsafeOption[String]): CharTokFastPerform = c match
    {
        case '\n' => new NewlineFastPerform(f, expected)
        case '\t' => new TabFastPerform(f, expected)
        case _ => new CharTokFastPerform(c, f, expected)
    }
}

private [parsley] object Exchange
{
    def unapply[A](ex: Exchange[A]): Option[A] = Some(ex.x)
}
