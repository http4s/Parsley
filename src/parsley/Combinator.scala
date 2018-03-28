package parsley

import parsley.Parsley._

import scala.annotation.tailrec

object Combinator
{
    /**`choice(ps)` tries to apply the parsers in the list `ps` in order, until one of them succeeds.
      *  Returns the value of the succeeding parser.*/
    def choice[A](ps: List[Parsley[A]]): Parsley[A] = ps.reduceLeftOption(_<|>_).getOrElse(empty)

    /**`attemptChoice(ps)` tries to apply the parsers in the list `ps` in order, until one of them succeeds.
      *  Returns the value of the succeeding parser. Utilises <\> vs choice's <|>.*/
    def attemptChoice[A](ps: List[Parsley[A]]): Parsley[A] = ps.reduceLeftOption(_<\>_).getOrElse(empty)

    /** `count(n, p)` parses `n` occurrences of `p`. If `n` is smaller or equal to zero, the parser is
      *  `pure(List())`. Returns a list of `n` values returned by `p`.*/
    def count[A](n: Int, p: Parsley[A]): Parsley[List[A]] = sequence(for (_ <- 1 to n) yield p)

    /**`option(p)` tries to apply parser `p`. If `p` fails without consuming input, it returns
      * `None`, otherwise it returns `Some` of the value returned by `p`.*/
    def option[A](p: =>Parsley[A]): Parsley[Option[A]] = p.map(Some(_)).getOrElse(None)

    /**`decide(p)` removes the option from inside parser `p`, and if it returned `None` will fail.**/
    def decide[A](p: =>Parsley[Option[A]]): Parsley[A] = for (opt <- p; if opt.isDefined) yield opt.get

    /**`optional(p)` tries to apply parser `p`. It will parse `p` or nothing. It only fails if `p`
      * fails after consuming input. It discards the result of `p`.*/
    // CODO This could be a Nothing parser, with an instrinsic-esque approach that code gens {InputCheck; p; Pop; JumpGood} with no actual branch
    // TODO The above would obsoleted by the peephole optimisation of; p <* (q <|> pure _) => p; InputCheck; q; Pop; JumpGood
    //                                                                 (q <|> pure _) *> p => InputCheck; q; Pop; JumpGood; p
    def optional[A](p: =>Parsley[A]): Parsley[Unit] = (p *> unit).getOrElse(())

    /**`between(open, close, p)` parses `open`, followed by `p` and `close`. Returns the value returned by `p`.*/
    def between[O, C, A](open: =>Parsley[O],
                         close: =>Parsley[C],
                         p: =>Parsley[A]): Parsley[A] = open *> p <* close

    /**`some(p)` applies the parser `p` *one* or more times. Returns a list of the returned values of `p`.*/
    def some[A](p: =>Parsley[A]): Parsley[List[A]] = manyN(1, p)

    /**`manyN(n, p)` applies the parser `p` *n* or more times. Returns a list of the returned values of `p`.*/
    def manyN[A](n: Int, _p: =>Parsley[A]): Parsley[List[A]] =
    {
        lazy val p = _p
        @tailrec def go(n: Int, acc: Parsley[List[A]] = many(p)): Parsley[List[A]] =
        {
            if (n == 0) acc
            else go(n-1, p <::> acc)
        }
        go(n)
    }

    /**`skipSome(p)` applies the parser `p` *one* or more times, skipping its result.*/
    def skipSome[A](p: => Parsley[A]): Parsley[Unit] = skipManyN(1, p)

    /**`skipManyN(n, p)` applies the parser `p` *n* or more times, skipping its result.*/
    def skipManyN[A](n: Int, _p: =>Parsley[A]): Parsley[Unit] =
    {
        lazy val p = _p
        @tailrec def go(n: Int, acc: Parsley[Unit] = skipMany(p)): Parsley[Unit] =
        {
            if (n == 0) acc
            else go(n-1, p *> acc)
        }
        go(n)
    }

    /**`sepBy(p, sep)` parses *zero* or more occurrences of `p`, separated by `sep`. Returns a list
      * of values returned by `p`.*/
    def sepBy[A, B](p: =>Parsley[A], sep: =>Parsley[B]): Parsley[List[A]] = sepBy1(p, sep).getOrElse(Nil)

    /**`sepBy1(p, sep)` parses *one* or more occurrences of `p`, separated by `sep`. Returns a list
      *  of values returned by `p`.*/
    def sepBy1[A, B](_p: =>Parsley[A], _sep: =>Parsley[B]): Parsley[List[A]] =
    {
        lazy val p = _p
        lazy val sep = _sep
        p <::> many(sep *> p)
    }

    /**`sepEndBy(p, sep)` parses *zero* or more occurrences of `p`, separated and optionally ended
      * by `sep`. Returns a list of values returned by `p`.*/
    def sepEndBy[A, B](p: => Parsley[A], sep: =>Parsley[B]): Parsley[List[A]] = sepEndBy1(p, sep).getOrElse(Nil)

    /**`sepEndBy1(p, sep)` parses *one* or more occurrences of `p`, separated and optionally ended
      * by `sep`. Returns a list of values returned by `p`.*/
    def sepEndBy1[A, B](p: => Parsley[A], _sep: =>Parsley[B]): Parsley[List[A]] =
    {
        lazy val sep = _sep
        sepBy1(p, sep) <* optional(sep)
    }

    /**`endBy(p, sep)` parses *zero* or more occurrences of `p`, separated and ended by `sep`. Returns a list
      * of values returned by `p`.*/
    def endBy[A, B](p: =>Parsley[A], sep: =>Parsley[B]): Parsley[List[A]] = many(p <* sep)

    /**`endBy1(p, sep)` parses *one* or more occurrences of `p`, separated and ended by `sep`. Returns a list
      * of values returned by `p`.*/
    def endBy1[A, B](p: =>Parsley[A], sep: =>Parsley[B]): Parsley[List[A]] = some(p <* sep)

    /**`chainr(p, op, x)` parses *zero* or more occurrences of `p`, separated by `op`. Returns a value
      * obtained by a right associative application of all functions return by `op` to the values
      * returned by `p`. If there are no occurrences of `p`, the value `x` is returned.*/
    def chainr[A](p: =>Parsley[A], op: =>Parsley[A => A => A], x: A): Parsley[A] = chainr1(p, op).getOrElse(x)

    /**`chainl(p, op, x)` parses *zero* or more occurrences of `p`, separated by `op`. Returns a value
      * obtained by a left associative application of all functions returned by `op` to the values
      * returned by `p`. If there are no occurrences of `p`, the value `x` is returned.*/
    def chainl[A](p: =>Parsley[A], op: =>Parsley[A => A => A], x: A): Parsley[A] = chainl1(p, op).getOrElse(x)

    /**`chainr1(p, op)` parses *one* or more occurrences of `p`, separated by `op`. Returns a value
      * obtained by a right associative application of all functions return by `op` to the values
      * returned by `p`.*/
    def chainr1[A](_p: =>Parsley[A], op: =>Parsley[A => A => A]): Parsley[A] =
    {
        lazy val p = _p
        chainPre(p, attempt(p <**> op))
    }

    /**`chainPre(p, op)` parses many prefixed applications of `op` onto a single final result of `p`*/
    // TODO: Intrinsic
    def chainPre[A](p: =>Parsley[A], op: =>Parsley[A => A]): Parsley[A] =
    {
        lift2[List[A => A], A, A](_.foldRight(_)(_(_)), many(op), p)
    }

    /**chainl1(p, op) parses *one* or more occurrences of `p`, separated by `op`. Returns a value
      * obtained by a left associative application of all functions return by `op` to the values
      * returned by `p`. This parser can for example be used to eliminate left recursion which
      * typically occurs in expression grammars.*/
    def chainl1[A](p: =>Parsley[A], op: =>Parsley[A => A => A]): Parsley[A] = chainl1_(p, op.map(f => (x: A) => (y: A) => f(y)(x)))

    /**`chainl1_(p, op)` parses *one* or more occurrences of `p`, separated by `op`. Returns a value
      * obtained by a left associative application of all functions return by `op` to the values
      * returned by `p`. This parser can for example be used to eliminate left recursion which
      * typically occurs in expression grammars. NOTE: the op should either be commutative or should
      * be flipped. This is more efficient than the manual flip performed by `chainl1`.*/
    def chainl1_[A](_p: =>Parsley[A], op: =>Parsley[A => A => A]): Parsley[A] =
    {
        lazy val p = _p
        chainPost(p, op <*> p)
    }

    /**`chainPost(p, op)` parses one occurrence of `p`, followed by many postfix applications of `op`
      * that associate to the left.*/
    def chainPost[A](p: =>Parsley[A], op: =>Parsley[A => A]) = new DeepEmbedding.Chainl(p, op)

    /**The parser `anyChar()` accepts any kind of character. It is for example used to implement `eof`.
      * Returns the accepted character.*/
    val anyChar: Parsley[Char] = satisfy(_ => true)

    /**This parser only succeeds at the end of the input. This is not a primitive parser but it is
      * defined using `notFollowedBy`.*/
    val eof: Parsley[Unit] = notFollowedBy(anyChar) ? "end of input"

    /**`notFollowedBy(p)` only succeeds when parser `p` fails. This parser does not consume any input.
      * This parser can be used to implement the 'longest match' rule. For example, when recognising
      * keywords, we want to make sure that a keyword is not followed by a legal identifier character,
      * in which case the keyword is actually an identifier. We can program this behaviour as follows:
      * {{{attempt(kw *> notFollowedBy(alphaNum))}}}*/
    def notFollowedBy(p: Parsley[_]): Parsley[Unit] = attempt(p).unexpected("\"" + _.toString + "\"").orElse[Unit](unit)

    /**`manyTill(p, end)` applies parser `p` zero or more times until the parser `end` succeeds.
      * Returns a list of values returned by `p`. This parser can be used to scan comments.*/
    // TODO Candidate for intrinsic; manyTill(p, term@(end #> End)) => Label(0); InputCheck(1); end; Label(1); JumpGood(2); p; Label(2); ManyTill(0) { tos == End }
    def manyTill[A, B](_p: =>Parsley[A], _end: Parsley[B]): Parsley[List[A]] =
    {
        lazy val p = _p
        lazy val end = _end
        lazy val scan: Parsley[List[A]] = (end #> Nil) <|> (p <::> scan)
        scan
    }

    /**`many1Till(p, end)` applies parser `p` one or more times until the parser `end` succeeds.
      * Returns a list of values returned by `p`.*/
    def many1Till[A, B](_p: =>Parsley[A], _end: Parsley[B]): Parsley[List[A]] =
    {
        lazy val p = _p
        lazy val end = _end
        notFollowedBy(end) *> (p <::> manyTill(p, end))
    }
}