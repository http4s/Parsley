import parsley.{Failure, Parsley, Success, Var, runParser}
import parsley.Parsley._
import parsley.Char.{char, satisfy}
import parsley.Implicits.{charLift, stringLift}

class CoreTests extends ParsleyTest
{
    private val add: (Int, Int) => Int = _+_
    private val add1 = (x: Int) => x + 1
    private val mult5 = (x: Int) => x * 5
    private val toUpper = (c: Char) => c.toUpper

    "Character parsers" should "fail on no input" in
    {
        runParser('a', "") should not be a [Success[_]]
    }

    they should "fail when given incorrect input" in
    {
        runParser('a', "b") should not be a [Success[_]]
    }

    they should "succeed when given correct input" in
    {
        runParser('a', "a") should be (Success('a'))
    }

    they must "only consume a single character of input at most" in
    {
        var res = runParser(satisfy(_ == 'a') *> 'b', "aaaaaa")
        res shouldBe a [Failure]
        res match
        {
            case Failure(err) => err should startWith ("(line 1, column 2)")
            case _ =>
        }
        res = runParser('a' *> 'b', "bc")
        res shouldBe a [Failure]
        res match
        {
            case Failure(err) => err should startWith ("(line 1, column 1)")
            case _ =>
        }
    }

    "Pure parsers" should "not require input" in
    {
        runParser(unit, "") should not be a [Failure]
    }

    they must "result in their correct value" in
    {
        runParser(pure(42), "") should be (Success(42))
        runParser(pure("hello world"), "") should be (Success("hello world"))
        runParser(pure(add), "") should be (Success(add))
    }

    they must "not consume any input" in
    {
        runParser(pure('a') *> 'a', "a") should be (Success('a'))
    }

    // APPLICATIVE LAWS
    they must "obey the homomorphism law: pure f <*> pure x = pure (f x)" in
    {
        runParser(pure(add1) <*> pure(42), "") should be (Success(43))
    }

    they must "obey the fmap law: pure f <*> p = f <$> p" in
    {
        runParser(pure(toUpper) <*> 'a', "a") should equal (runParser(toUpper <#> 'a', "a"))
        runParser(pure(toUpper) <*> ('a' <|> 'b'), "a") should equal (runParser(toUpper <#> ('a' <|> 'b'), "a"))
        runParser(pure(toUpper) <*> ('a' <|> 'b'), "b") should equal (runParser(toUpper <#> ('a' <|> 'b'), "b"))
    }

    they must "obey the interchange law: u <*> pure x = ($x) <$> u" in
    {
        runParser(('a' #> add1) <*> pure(41), "a") should equal (runParser(((f: Int => Int) => f(41)) <#> ('a' #> add1), "a"))
        runParser((('a' <|> 'b') #> add1) <*> pure(41), "a") should equal (runParser(((f: Int => Int) => f(41)) <#> (('a' <|> 'b') #> add1), "a"))
        runParser((('a' <|> 'b') #> add1) <*> pure(41), "b") should equal (runParser(((f: Int => Int) => f(41)) <#> (('a' <|> 'b') #> add1), "b"))
    }

    they must "obey the composition law: pure (.) <*> u <*> v <*> w = u <*> (v <*> w)" in
    {
        val u: Parsley[Char, Int => Int] = 'a' #> add1
        val v: Parsley[Char, Int => Int] = 'b' #> mult5
        val w: Parsley[Char, Int] = 'c' #> 7
        val compose: (Int => Int) => (Int => Int) => Int => Int = f => g => f.compose(g)
        runParser(pure(compose) <*> u <*> v <*> w, "abc") should equal (runParser(u <*> (v <*> w), "abc"))
    }

    // MONAD LAWS
    they must "obey the left identity law: pure x >>= f = f x" in
    {
        runParser(pure('a') >>= char, "a") should equal (runParser('a', "a"))
    }
    "Parsers" must "obey the right identity law: m >>= pure = m" in
    {
        runParser('a' >>= pure, "a") should equal (runParser('a', "a"))
    }
    they must "obey the associativity law: (m >>= f) >>= g = m >>= (x => f x >>= g)" in
    {
        val f: Int => Parsley[Any, Int] = x => pure(x + 1)
        val g: Int => Parsley[Any, Int] = x => pure(x/3)
        val m = '1' #> 4
        runParser((m >>= f) >>= g, "1") should equal (runParser(m >>= (x => f(x) >>= g), "1"))
    }

    "mzero parsers" should "always fail" in
    {
        runParser(Parsley.empty *> 'a', "a") shouldBe a [Failure]
        runParser(Parsley.fail("") *> 'a', "a") shouldBe a [Failure]
        runParser(Parsley.unexpected("") *> 'a', "a") shouldBe a [Failure]
        runParser(('a' ! (_ => "")) *> 'b', "ab") shouldBe a [Failure]
        runParser('a'.unexpected(_ => "") *> 'b', "ab") shouldBe a [Failure]
    }

    "<|>" should "not try the second alternative if the first succeeded" in
    {
        runParser('a' <|> Parsley.fail("wrong!"), "a") should not be a [Failure]
    }
    it should "only try second alternative if the first failed without consuming input" in
    {
        runParser('a' <|> 'b', "b") should not be a [Failure]
    }
    it should "not try the second alternative if the first failed after consuming input" in
    {
        runParser("ab" <|> "ac", "ac") shouldBe a [Failure]
    }

    "attempt" should "cause <|> to try second alternative even if input consumed" in
    {
        runParser(attempt("ab") <|> "ac", "ac") should not be a [Failure]
    }

    "lookAhead" should "consume no input on success" in
    {
        runParser(lookAhead('a'), "a") should not be a [Failure]
        runParser(lookAhead('a') *> 'b', "ab") should be (Failure("(line 1, column 1):\n  unexpected \"a\"\n  expected \"b\""))
    }
    it must "fail when input is consumed, and input is consumed" in
    {
        runParser(lookAhead("ab"), "ac") shouldBe a [Failure]
    }
    "lookAhead" should "not affect the state of the registers on success" in
    {
        runParser(put(Var(0), 5) *> lookAhead(put(Var(0), 7) *> 'a') *> get[Int](Var(0)), "a") should be
        {
            Success(5)
        }
        runParser(put(Var(0), 5) *> (lookAhead(put(Var(0), 7) *> 'a') <|> 'b') *> get[Int](Var(0)), "b") should be
        {
            Success(7)
        }
    }

    "many" should "crash when given a parser that does not consume input" in
    {
       an [Exception] should be thrownBy runParser(many(pure(5)), "")
    }

    "stateful parsers" should "allow for persistent state" in
    {
        val r1 = Var(0)
        val r2 = Var(1)
        val p = (put(r1, 5)
              *> put(r2, 7)
              *> put(r1, lift2[Any, Int, Int, Int](_+_, get[Int](r1), get[Int](r2)))
              *> (get[Int](r1) <~> get[Int](r2)))
        runParser(p, "") should be (Success((12, 7)))
    }
    they should "be modifiable" in
    {
        val r1 = Var(0)
        val p = put(r1, 5) *> modify[Int](r1, _+1) *> get[Int](r1)
        runParser(p, "") should be (Success(6))
    }
    they should "provide localised context" in
    {
        val r1 = Var(0)
        val p = put(r1, 5) *> (local(r1, (x: Int) => x+1, get[Int](r1)) <~> get[Int](r1))
        runParser(p, "") should be (Success((6, 5)))
    }

    "ternary parsers" should "function correctly" in
    {
        val p = pure(true)
        runParser(p ?: ('a', 'b'), "a") should be (Success('a'))
    }

    "stack overflows" should "not occur" in
    {
        def repeat[Tok](n: Int, p: Parsley[Tok, Char]): Parsley[Tok, Char] =
        {
            if (n > 0) p *> repeat(n-1, p)
            else p
        }
        noException should be thrownBy runParser(repeat(4000, 'a'), "a")
    }
    they should "not be thrown by recursive parsers" in
    {
        lazy val p: Parsley[Any, Int] = p.map((x: Int) => x+1)
        def many_[Tok, A](p: Parsley[Tok, A]): Parsley[Tok, List[A]] =
        {
            lazy val manyp: Parsley[Tok, List[A]] = (p <::> manyp) </> Nil
            manyp
        }
        noException should be thrownBy runParser(many_('a' *> p), "")
    }
    they should "not be caused by bind optimisation" in
    {
        lazy val uhoh: Parsley[Char, Unit] = 'a' >>= (_ => uhoh)
        noException should be thrownBy runParser(uhoh, "a")
    }

    "subroutines" should "function correctly" in
    {
        val p = satisfy(_ => true) *> satisfy(_ => true) *> satisfy(_ => true)
        runParser('a' *> +p <* 'b' <* +p <* 'c', "a123b123c") should be (Success('3'))
    }
}
