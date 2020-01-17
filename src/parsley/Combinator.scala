package parsley

import parsley.Parsley._
import scala.annotation.tailrec

object Combinator
{
    /**`choice(ps)` tries to apply the parsers in the list `ps` in order, until one of them succeeds.
      *  Returns the value of the succeeding parser.*/
    def choice[Tok, A](ps: Parsley[Tok, A]*): Parsley[Tok, A] = ps.reduceLeftOption(_<|>_).getOrElse(empty)

    /**`attemptChoice(ps)` tries to apply the parsers in the list `ps` in order, until one of them succeeds.
      *  Returns the value of the succeeding parser. Utilises `<\>` vs choice's `<|>`.*/
    def attemptChoice[Tok, A](ps: Parsley[Tok, A]*): Parsley[Tok, A] = ps.reduceLeftOption(_<\>_).getOrElse(empty)

    /** `repeat(n, p)` parses `n` occurrences of `p`. If `n` is smaller or equal to zero, the parser is
      *  `pure(Nil)`. Returns a list of `n` values returned by `p`.*/
    def repeat[Tok, A](n: Int, p: =>Parsley[Tok, A]): Parsley[Tok, List[A]] =
    {
        lazy val _p = p
        sequence((for (_ <- 1 to n) yield _p): _*)
    }

    /**`option(p)` tries to apply parser `p`. If `p` fails without consuming input, it returns
      * `None`, otherwise it returns `Some` of the value returned by `p`.*/
    def option[Tok, A](p: =>Parsley[Tok, A]): Parsley[Tok, Option[A]] = p.map(Some(_)).getOrElse(None)

    /**`decide(p)` removes the option from inside parser `p`, and if it returned `None` will fail.*/
    def decide[Tok, A](p: =>Parsley[Tok, Option[A]]): Parsley[Tok, A] = for (opt <- p; if opt.isDefined) yield opt.get

    /**`decide(p, q)` removes the option from inside parser `p`, if it returned `None` then `q` is executed.*/
    def decide[Tok, A](p: =>Parsley[Tok, Option[A]], q: =>Parsley[Tok, A]): Parsley[Tok, A] =
        select(p <#>
        {
            case Some(x) => Right(x)
            case None => Left(())
        }, q.map[Unit => A](x => _ => x))

    /**optional(p) tries to apply parser `p`. It will parse `p` or nothing. It only fails if `p`
      * fails after consuming input. It discards the result of `p`.*/
    def optional[Tok](p: =>Parsley[Tok, _]): Parsley[Tok, Unit] = void(p).getOrElse(())

    /**`between(open, close, p)` parses `open`, followed by `p` and `close`. Returns the value returned by `p`.*/
    def between[Tok, A](open: =>Parsley[Tok, _],
                        close: =>Parsley[Tok, _],
                        p: =>Parsley[Tok, A]): Parsley[Tok, A] = open *> p <* close

    /**`some(p)` applies the parser `p` *one* or more times. Returns a list of the returned values of `p`.*/
    def some[Tok, A](p: =>Parsley[Tok, A]): Parsley[Tok, List[A]] = manyN(1, p)

    /**`manyN(n, p)` applies the parser `p` *n* or more times. Returns a list of the returned values of `p`.*/
    def manyN[Tok, A](n: Int, p: =>Parsley[Tok, A]): Parsley[Tok, List[A]] =
    {
        lazy val _p = p
        @tailrec def go(n: Int, acc: Parsley[Tok, List[A]] = many(_p)): Parsley[Tok, List[A]] =
        {
            if (n == 0) acc
            else go(n-1, _p <::> acc)
        }
        go(n)
    }

    /**`skipSome(p)` applies the parser `p` *one* or more times, skipping its result.*/
    def skipSome[Tok, A](p: => Parsley[Tok, A]): Parsley[Tok, Unit] = skipManyN(1, p)

    /**`skipManyN(n, p)` applies the parser `p` *n* or more times, skipping its result.*/
    def skipManyN[Tok, A](n: Int, p: =>Parsley[Tok, A]): Parsley[Tok, Unit] =
    {
        lazy val _p = p
        @tailrec def go(n: Int, acc: Parsley[Tok, Unit] = skipMany(_p)): Parsley[Tok, Unit] =
        {
            if (n == 0) acc
            else go(n-1, _p *> acc)
        }
        go(n)
    }

    /**`sepBy(p, sep)` parses *zero* or more occurrences of `p`, separated by `sep`. Returns a list
      * of values returned by `p`.*/
    def sepBy[Tok, A, B](p: =>Parsley[Tok, A], sep: =>Parsley[Tok, B]): Parsley[Tok, List[A]] = sepBy1(p, sep).getOrElse(Nil)

    /**`sepBy1(p, sep)` parses *one* or more occurrences of `p`, separated by `sep`. Returns a list
      *  of values returned by `p`.*/
    def sepBy1[Tok, A, B](p: =>Parsley[Tok, A], sep: =>Parsley[Tok, B]): Parsley[Tok, List[A]] =
    {
        lazy val _p = p
        lazy val _sep = sep
        _p <::> many(_sep *> _p)
    }

    /**`sepEndBy(p, sep)` parses *zero* or more occurrences of `p`, separated and optionally ended
      * by `sep`. Returns a list of values returned by `p`.*/
    def sepEndBy[Tok, A, B](p: =>Parsley[Tok, A], sep: =>Parsley[Tok, B]): Parsley[Tok, List[A]] = sepEndBy1(p, sep).getOrElse(Nil)

    /**`sepEndBy1(p, sep)` parses *one* or more occurrences of `p`, separated and optionally ended
      * by `sep`. Returns a list of values returned by `p`.*/
    def sepEndBy1[Tok, A, B](p: =>Parsley[Tok, A], sep: =>Parsley[Tok, B]): Parsley[Tok, List[A]] = new DeepEmbedding.SepEndBy1(p, sep)

    /**`endBy(p, sep)` parses *zero* or more occurrences of `p`, separated and ended by `sep`. Returns a list
      * of values returned by `p`.*/
    def endBy[Tok, A, B](p: =>Parsley[Tok, A], sep: =>Parsley[Tok, B]): Parsley[Tok, List[A]] = many(p <* sep)

    /**`endBy1(p, sep)` parses *one* or more occurrences of `p`, separated and ended by `sep`. Returns a list
      * of values returned by `p`.*/
    def endBy1[Tok, A, B](p: =>Parsley[Tok, A], sep: =>Parsley[Tok, B]): Parsley[Tok, List[A]] = some(p <* sep)

    /**`chainr(p, op, x)` parses *zero* or more occurrences of `p`, separated by `op`. Returns a value
      * obtained by a right associative application of all functions return by `op` to the values
      * returned by `p`. If there are no occurrences of `p`, the value `x` is returned.*/
    def chainr[Tok, A](p: =>Parsley[Tok, A], op: =>Parsley[Tok, (A, A) => A], x: A): Parsley[Tok, A] = chainr1(p, op).getOrElse(x)

    /**`chainl(p, op, x)` parses *zero* or more occurrences of `p`, separated by `op`. Returns a value
      * obtained by a left associative application of all functions returned by `op` to the values
      * returned by `p`. If there are no occurrences of `p`, the value `x` is returned.*/
    def chainl[Tok, A](p: =>Parsley[Tok, A], op: =>Parsley[Tok, (A, A) => A], x: A): Parsley[Tok, A] = chainl1(p, op).getOrElse(x)

    /**`chainr1(p, op)` parses *one* or more occurrences of `p`, separated by `op`. Returns a value
      * obtained by a right associative application of all functions return by `op` to the values
      * returned by `p`.*/
    def chainr1[Tok, A](p: =>Parsley[Tok, A], op: =>Parsley[Tok, (A, A) => A]): Parsley[Tok, A] = new DeepEmbedding.Chainr(p, op)

    /**`chainPre(op, p)` parses many prefixed applications of `op` onto a single final result of `p`*/
    def chainPre[Tok, A](op: =>Parsley[Tok, A => A], p: =>Parsley[Tok, A]): Parsley[Tok, A] = new DeepEmbedding.ChainPre(p, op)

    /**chainl1(p, op) parses *one* or more occurrences of `p`, separated by `op`. Returns a value
      * obtained by a left associative application of all functions return by `op` to the values
      * returned by `p`. This parser can for example be used to eliminate left recursion which
      * typically occurs in expression grammars.*/
    def chainl1[Tok, A](p: =>Parsley[Tok, A], op: =>Parsley[Tok, (A, A) => A]): Parsley[Tok, A] = new DeepEmbedding.Chainl(+p, op)

    /**`chainPost(p, op)` parses one occurrence of `p`, followed by many postfix applications of `op`
      * that associate to the left.*/
    def chainPost[Tok, A](p: =>Parsley[Tok, A], op: =>Parsley[Tok, A => A]): Parsley[Tok, A] = new DeepEmbedding.ChainPost(p, op)

    /**This parser only succeeds at the end of the input. This is a primitive parser.*/
    val eof: Parsley[Any, Unit] = new DeepEmbedding.*>(new DeepEmbedding.Eof, unit)

    /**This parser only succeeds if there is still more input.*/
    val more: Parsley[Any, Unit] = notFollowedBy(eof)

    /**`notFollowedBy(p)` only succeeds when parser `p` fails. This parser does not consume any input.
      * This parser can be used to implement the 'longest match' rule. For example, when recognising
      * keywords, we want to make sure that a keyword is not followed by a legal identifier character,
      * in which case the keyword is actually an identifier. We can program this behaviour as follows:
      * {{{attempt(kw *> notFollowedBy(alphaNum))}}}*/
    def notFollowedBy[Tok](p: Parsley[Tok, _]): Parsley[Tok, Unit] = new DeepEmbedding.*>(new DeepEmbedding.NotFollowedBy(p), unit)

    /**`manyUntil(p, end)` applies parser `p` zero or more times until the parser `end` succeeds.
      * Returns a list of values returned by `p`. This parser can be used to scan comments.*/
    def manyUntil[Tok, A, B](p: =>Parsley[Tok, A], end: =>Parsley[Tok, B]): Parsley[Tok, List[A]] =
    {
        new DeepEmbedding.ManyUntil(end #> DeepEmbedding.ManyUntil.Stop <|> p)
    }

    /**`someUntil(p, end)` applies parser `p` one or more times until the parser `end` succeeds.
      * Returns a list of values returned by `p`.*/
    def someUntil[Tok, A, B](p: =>Parsley[Tok, A], end: =>Parsley[Tok, B]): Parsley[Tok, List[A]] =
    {
        lazy val _p = p
        lazy val _end = end
        notFollowedBy(_end) *> (_p <::> manyUntil(_p, _end))
    }

    /** `when(p, q)` will first perform `p`, and if the result is `true` will then execute `q` or else return unit.
      * @param p The first parser to parse
      * @param q If `p` returns `true` then this parser is executed
      * @return ()
      */
    def when[Tok](p: =>Parsley[Tok, Boolean], q: =>Parsley[Tok, Unit]): Parsley[Tok, Unit] = p ?: (q, unit)

    /** `whileP(p)` will continue to run `p` until it returns `false`. This is often useful in conjunction with stateful
      * parsers.
      * @param p The parser to continuously execute
      * @return ()
      */
    def whileP[Tok](p: =>Parsley[Tok, Boolean]): Parsley[Tok, Unit] =
    {
        lazy val whilePP: Parsley[Tok, Unit] = when(p, whilePP)
        whilePP
    }

    /** `forP(v, init, cond, step, body)` behaves much like a traditional for loop using variable `v` as the loop
      * variable and `init`, `cond`, `step` and `body` as parsers which control the loop itself. This is useful for
      * performing certain context sensitive tasks. For instance, to read an equal number of as, bs and cs you can do:
      *
      * {{{
      * put(v1, 0) *>
      * many('a' *> modify[Int](v1, _+1)) *>
      * forP[Int](v2, get[Int](v1), pure(_ != 0), pure(_ - 1), 'b') *>
      * forP[Int](v2, get[Int](v1), pure(_ != 0), pure(_ - 1), 'c')
      * }}}
      *
      * The value of `v` is reset on exiting this parser. This is to preserve the limited register numbers.
      *
      * @param v The address the induction variable is stored in
      * @param init The initial value that register v should take
      * @param cond The condition by which the loop terminates
      * @param step The change in induction variable on each iteration
      * @param body The body of the loop performed each iteration
      * @return ()
      */
    def forP[Tok, A](v: Var, init: =>Parsley[Tok, A], cond: =>Parsley[Tok, A => Boolean], step: =>Parsley[Tok, A => A], body: =>Parsley[Tok, _]): Parsley[Tok, Unit] =
    {
        val _cond = gets(v, cond)
        val _step = put(v, gets(v, step))
        local(v, init, when(_cond, whileP(body *> _step *> _cond)))
    }
}