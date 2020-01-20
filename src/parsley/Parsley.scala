package parsley

import parsley.ContOps._
import parsley.DeepToken._
import parsley.instructions._

import scala.annotation.tailrec
import scala.collection.mutable
import scala.language.{existentials, higherKinds, reflectiveCalls}
    
// User API
object Parsley
{
    implicit final class LazyParsley[P, -Tok, +A](p: =>P)(implicit con: P => Parsley[Tok, A])
    {
        /**
          * This is the functorial map operation for parsers. When the invokee produces a value, this value is fed through
          * the function `f`.
          *
          * @note This is subject to aggressive optimisations assuming purity; the compiler is permitted to optimise such
          * that the application of `f` actually only happens once at compile time. In order to preserve the behaviour of
          * impure functions, consider using the `unsafe` method before map; `p.unsafe.map(f)`.
          * @param f The mutator to apply to the result of previous parse
          * @return A new parser which parses the same input as the invokee but mutated by function `f`
          */
        def map[B](f: A => B): Parsley[Tok, B] = (pure(f): Parsley[Tok, A => B]) <*> p
        /**This combinator is an alias for `map`*/
        def <#>[B](f: A => B): Parsley[Tok, B] = map(f)
        /**
          * This is the Applicative application parser. The type of `pf` is `Parsley[A => B]`. Then, given a
          * `Parsley[A]`, we can produce a `Parsley[B]` by parsing `pf` to retrieve `f: A => B`, then parse `px`
          * to receive `x: A` then return `f(x): B`.
          *
          * @note `pure(f) <*> p` is subject to the same aggressive optimisations as `map`. When using impure functions
          * the optimiser may decide to cache the result of the function execution, be sure to use `unsafe` in order to
          * prevent these optimisations.
          * @param px A parser of type A, where the invokee is A => B
          * @return A new parser which parses `pf`, then `px` then applies the value returned by `px` to the function
          *         returned by `pf`
          */
        def <*>[Tok_ <: Tok, B, C](px: =>Parsley[Tok_, B])(implicit ev: P <:< Parsley[Tok_, B=>C]): Parsley[Tok_, C] = new DeepEmbedding.<*>[Tok_, B, C](p, px)
        /**
          * This is the traditional Monadic binding operator for parsers. When the invokee produces a value, the function
          * `f` is used to produce a new parser that continued the computation.
          *
          * @note There is significant overhead for using flatMap; if possible try to write parsers in an applicative
          * style otherwise try and use the intrinsic parsers provided to replace the flatMap.
          * @param f A function that produces the next parser
          * @return The parser produces from the application of `f` on the result of the last parser
          */
        def flatMap[Tok_ <: Tok, B](f: A => Parsley[Tok_, B]): Parsley[Tok_, B] = new DeepEmbedding.>>=(p, f)
        /**This combinator is an alias for `flatMap(identity)`.*/
        def flatten[Tok_ <: Tok, B](implicit ev: A <:< Parsley[Tok_, B]): Parsley[Tok_, B] = flatMap[Tok_, B](ev)

        /**This combinator is an alias for `flatMap`*/
        def >>=[Tok_ <: Tok, B](f: A => Parsley[Tok_, B]): Parsley[Tok_, B] = flatMap(f)
        /**This combinator is defined as `lift2((x, f) => f(x), p, f)`. It is pure syntactic sugar.*/
        def <**>[Tok_ <: Tok, B](pf: =>Parsley[Tok_, A => B]): Parsley[Tok_, B] = lift2[Tok_, A, A=>B, B]((x, f) => f(x), p, pf)
        /**
          * This is the traditional Alternative choice operator for parsers. Following the parsec semantics precisely,
          * this combinator first tries to parse the invokee. If this is successful, no further action is taken. If the
          * invokee failed *without* consuming input, then `q` is parsed instead. If the invokee did parse input then the
          * whole parser fails. This is done to prevent space leaks and to give good error messages. If this behaviour
          * is not desired, use the `<\>` combinator (or `attempt(this) <|> q`) to parse `q` regardless of how the
          * invokee failed.
          * @param q The parser to run if the invokee failed without consuming input
          * @return The value produced by the invokee if it was successful, or if it failed without consuming input, the
          *         possible result of parsing q.
          */
        def <|>[Tok_ <: Tok, B >: A](q: =>Parsley[Tok_, B]): Parsley[Tok_, B] = new DeepEmbedding.<|>(p, q)
        /**This combinator is defined as `p <|> pure(x)`. It is pure syntactic sugar.*/
        def </>[B >: A](x: B): Parsley[Tok, B] = this <|> pure(x)
        /**This combinator is an alias for `<|>`.*/
        def orElse[Tok_ <: Tok, B >: A](q: =>Parsley[Tok_, B]): Parsley[Tok_, B] = this <|> q
        /**This combinator is an alias for `</>`.*/
        def getOrElse[B >: A](x: B): Parsley[Tok, B] = p </> x
        /**This combinator is defined as `attempt(p) <|> q`. It is pure syntactic sugar.*/
        def <\>[Tok_ <: Tok, B >: A](q: Parsley[Tok_, B]): Parsley[Tok_, B] = attempt(p) <|> q
        /**
          * This is the parser that corresponds to a more optimal version of `p.map(_ => x => x) <*> q`. It performs
          * the parse action of both parsers, in order, but discards the result of the invokee.
          * @param q The parser whose result should be returned
          * @return A new parser which first parses `p`, then `q` and returns the result of `q`
          */
        def *>[Tok_ <: Tok, A_ >: A, B](q: =>Parsley[Tok_, B]): Parsley[Tok_, B] = new DeepEmbedding.*>[Tok_, A_, B](p, q)
        /**
          * This is the parser that corresponds to a more optimal version of `p.map(x => _ => x) <*> q`. It performs
          * the parse action of both parsers, in order, but discards the result of the second parser.
          * @param q The parser who should be executed but then discarded
          * @return A new parser which first parses `p`, then `q` and returns the result of the `p`
          */
        def <*[Tok_ <: Tok, B](q: =>Parsley[Tok_, B]): Parsley[Tok_, A] = new DeepEmbedding.<*(p, q)
        /**
          * This is the parser that corresponds to `p *> pure(x)` or a more optimal version of `p.map(_ => x)`.
          * It performs the parse action of the invokee but discards its result and then results the value `x` instead
          * @param x The value to be returned after the execution of the invokee
          * @return A new parser which first parses the invokee, then results `x`
          */
        def #>[B](x: B): Parsley[Tok, B] = this *> pure(x)
        /**This combinator is an alias for `*>`*/
        def >>[Tok_ <: Tok, B](q: Parsley[Tok_, B]): Parsley[Tok_, B] = this *> q
        /**This parser corresponds to `lift2(_+:_, p, ps)`.*/
        def <+:>[Tok_ <: Tok, B >: A](ps: =>Parsley[Tok_, Seq[B]]): Parsley[Tok_, Seq[B]] = lift2[Tok_, A, Seq[B], Seq[B]](_ +: _, p, ps)
        /**This parser corresponds to `lift2(_::_, p, ps)`.*/
        def <::>[Tok_ <: Tok, B >: A](ps: =>Parsley[Tok_, List[B]]): Parsley[Tok_, List[B]] = lift2[Tok_, A, List[B], List[B]](_ :: _, p, ps)
        /**This parser corresponds to `lift2((_, _), p, q)`. For now it is sugar, but in future may be more optimal*/
        def <~>[Tok_ <: Tok, A_ >: A, B](q: =>Parsley[Tok_, B]): Parsley[Tok_, (A_, B)] = lift2[Tok_, A_, B, (A_, B)]((_, _), p, q)
        /** Filter the value of a parser; if the value returned by the parser matches the predicate `pred` then the
          * filter succeeded, otherwise the parser fails with an empty error
          * @param pred The predicate that is tested against the parser result
          * @return The result of the invokee if it passes the predicate
          */
        def filter(pred: A => Boolean): Parsley[Tok, A] = new DeepEmbedding.Ensure(p, pred)
        def withFilter(pred: A => Boolean): Parsley[Tok, A] = filter(pred)
        /** Similar to `filter`, except the error message desired is also provided. This allows you to name the message
          * itself.
          * @param pred The predicate that is tested against the parser result
          * @param msg The message used for the error if the input failed the check
          * @return The result of the invokee if it passes the predicate
          */
        def guard(pred: A => Boolean, msg: String): Parsley[Tok, A] = new DeepEmbedding.Guard(p, pred, msg)
        /** Similar to `filter`, except the error message desired is also provided. This allows you to name the message
          * itself. The message is provided as a generator, which allows the user to avoid otherwise expensive
          * computation.
          * @param pred The predicate that is tested against the parser result
          * @param msggen Generator function for error message, generating a message based on the result of the parser
          * @return The result of the invokee if it passes the predicate
          */
        def guard(pred: A => Boolean, msggen: A => String): Parsley[Tok, A] = new DeepEmbedding.FastGuard(p, pred, msggen)
        /**Alias for guard combinator, taking a fixed message.*/
        def >?>(pred: A => Boolean, msg: String): Parsley[Tok, A] = guard(pred, msg)
        /**Alias for guard combinator, taking a dynamic message generator.*/
        def >?>(pred: A => Boolean, msggen: A => String): Parsley[Tok, A] = guard(pred, msggen)
        /**Sets the expected message for a parser. If the parser fails then `expected msg` will added to the error*/
        def ?(msg: String): Parsley[Tok, A] = new DeepEmbedding.ErrorRelabel(p, msg)
        /** Same as `fail`, except allows for a message generated from the result of the failed parser. In essence, this
          * is equivalent to `p >>= (x => fail(msggen(x))` but requires no expensive computations from the use of `>>=`.
          * @param msggen The generator function for error message, creating a message based on the result of invokee
          * @return A parser that fails if it succeeds, with the given generator used to produce the error message
          */
        def !(msggen: A => String): Parsley[Tok, Nothing] = new DeepEmbedding.FastFail(p, msggen)
        /** Same as `unexpected`, except allows for a message generated from the result of the failed parser. In essence,
          * this is equivalent to `p >>= (x => unexpected(x))` but requires no expensive computations from the use of
          * `>>=`
          * @param msggen The generator function for error message, creating a message based on the result of invokee
          * @return A parser that fails if it succeeds, with the given generator used to produce an unexpected message
          */
        def unexpected(msggen: A => String): Parsley[Tok, Nothing] = new DeepEmbedding.FastUnexpected(p, msggen)
        /** Transforms this parser into a subroutine; instead of inlining this parser into every use-site (performing
          * optimisations and repeatedly generating code), produces a subroutine-like parser which is jumped to when
          * required. This will introduce runtime overhead, but it is fairly cheap and speeds up the compilation
          * of parsers that are very big and used many times considerably.
          * @return The same parser, but wrapped in a subroutine call
          */
        def unary_+ : Parsley[Tok, A] = new DeepEmbedding.Subroutine(p)
        /**
          * Using this method enables debugging functionality for this parser. When it is entered a snapshot is taken and
          * presented on exit. It will signify when a parser is entered and exited as well. Use the break parameter to halt
          * execution on either entry, exit, both or neither.
          * @param name The name to be assigned to this parser
          * @param break The breakpoint properties of this parser, defaults to NoBreak
          */
        def debug[A_ >: A](name: String, break: Breakpoint = NoBreak): Parsley[Tok, A_] = new DeepEmbedding.Debug[Tok, A_](p, name, break)
        /**
          * A fold for a parser: `p.foldRight(k)(f)` will try executing `p` many times until it fails, combining the
          * results with right-associative application of `f` with a `k` at the right-most position
          *
          * @example {{{p.foldRight(Nil)(_::_) == many(p) //many is more efficient, however}}}
          *
          * @param k base case for iteration
          * @param f combining function
          * @return the result of folding the results of `p` with `f` and `k`
          */
        def foldRight[B](k: B)(f: (A, B) => B): Parsley[Tok, B] = Combinator.chainPre(map(f.curried), pure(k))
        /**
          * A fold for a parser: `p.foldLeft(k)(f)` will try executing `p` many times until it fails, combining the
          * results with left-associative application of `f` with a `k` on the left-most position
          *
          * @example {{{val natural: Parsley[Int] = digit.foldLeft(0)((x, d) => x * 10 + d.toInt)}}}
          *
          * @param k base case for iteration
          * @param f combining function
          * @return the result of folding the results of `p` with `f` and `k`
          */
        def foldLeft[B](k: B)(f: (B, A) => B): Parsley[Tok, B] = Combinator.chainPost(pure(k), map(x => y => f(y, x)))
    }
    implicit final class LazyMapParsley[Tok, A, +B](f: A => B)
    {
        /**This combinator is an alias for `map`*/
        def <#>(p: =>Parsley[Tok, A]): Parsley[Tok, B] = p.map(f)
    }
    implicit final class LazyChooseParsley[Tok, P, +A](pq: =>(P, P))(implicit con: P => Parsley[Tok, A])
    {
        private lazy val (p, q) = pq
        /**
          * This is an if statement lifted to the parser level. Formally, this is a selective functor operation,
          * equivalent to (branch b.map(boolToEither) (p.map(const)) (q.map(const))).
          * Note: due to Scala operator associativity laws, this is a right-associative operator, and must be properly
          * bracketed, technically the invokee is the rhs...
          * @param b The parser that yields the condition value
          * @return The result of either `p` or `q` depending on the return value of the invokee
          */
        def ?:(b: =>Parsley[Tok, Boolean]): Parsley[Tok, A] = new DeepEmbedding.Ternary(b, p, q)
    }

    /** This is the traditional applicative pure function (or monadic return) for parsers. It consumes no input and
      * does not influence the state of the parser, but does return the value provided. Useful to inject pure values
      * into the parsing process.
      * @param x The value to be returned from the parser
      * @return A parser which consumes nothing and returns `x`
      */
    def pure[A](x: A): Parsley[Any, A] = new DeepEmbedding.Pure(x)

    /** Traditionally, `lift2` is defined as `lift2(f, p, q) = p.map(f) <*> q`. However, `f` is actually uncurried,
      * so it's actually more exactly defined as; read `p` and then read `q` then provide their results to function
      * `f`. This is designed to bring higher performance to any curried operations that are not themselves
      * intrinsic.
      * @param f The function to apply to the results of `p` and `q`
      * @param p The first parser to parse
      * @param q The second parser to parse
      * @return `f(x, y)` where `x` is the result of `p` and `y` is the result of `q`.
      */
    def lift2[Tok, A, B, C](f: (A, B) => C, p: =>Parsley[Tok, A], q: =>Parsley[Tok, B]): Parsley[Tok, C] = new DeepEmbedding.Lift2(f, p, q)
    /** Traditionally, `lift2` is defined as `lift3(f, p, q, r) = p.map(f) <*> q <*> r`. However, `f` is actually uncurried,
      * so it's actually more exactly defined as; read `p` and then read `q` and then read 'r' then provide their results
      * to function `f`. This is designed to bring higher performance to any curried operations that are not themselves
      * intrinsic.
      * @param f The function to apply to the results of `p` and `q`
      * @param p The first parser to parse
      * @param q The second parser to parse
      * @param r The third parser to parse
      * @return `f(x, y, z)` where `x` is the result of `p`, `y` is the result of `q` and `z` is the result of `r`.
      */
    def lift3[Tok, A, B, C, D](f: (A, B, C) => D, p: =>Parsley[Tok, A], q: =>Parsley[Tok, B], r: =>Parsley[Tok, C]): Parsley[Tok, D] = new DeepEmbedding.Lift3(f, p, q, r)

    /** This is one of the core operations of a selective functor. It will conditionally execute one of `p` and `q`
      * depending on the result from `b`. This can be used to implement conditional choice within a parser without
      * relying on expensive monadic operations.
      * @param b The first parser to parse
      * @param p If `b` returns `Left` then this parser is executed with the result
      * @param q If `b` returns `Right` then this parser is executed with the result
      * @return Either the result from `p` or `q` depending on `b`.
      */
    def branch[Tok, A, B, C](b: =>Parsley[Tok, Either[A, B]], p: =>Parsley[Tok, A => C], q: =>Parsley[Tok, B => C]): Parsley[Tok, C] =
        // TODO This should be converted to use Case instruction from Haskell Parsley, this is too inefficient right now
        // We can then apply some laws and optimisations to it...
        b >>= {
            case Left(x) => p <*> pure(x)
            case Right(y) => q <*> pure(y)
        }
    /** This is one of the core operations of a selective functor. It will conditionally execute one of `q` depending on
      * whether or not `p` returns a `Left`. It can be used to implement `branch` and other selective operations, however
      * it is more efficiently implemented with `branch` itself.
      * @param p The first parser to parse
      * @param q If `p` returns `Left` then this parser is executed with the result
      * @return Either the result from `p` if it returned `Left` or the result of `q` applied to the `Right` from `p`
      */
    def select[Tok, A, B](p: =>Parsley[Tok, Either[A, B]], q: =>Parsley[Tok, A => B]): Parsley[Tok, B] = branch(p, q, pure(identity[B](_)))
    /**This function is an alias for `_.flatten`. Provides namesake to Haskell.*/
    def join[Tok, A](p: =>Parsley[Tok, Parsley[Tok, A]]): Parsley[Tok, A] = p.flatten
    /** Given a parser `p`, attempts to parse `p`. If the parser fails, then `attempt` ensures that no input was
      * consumed. This allows for backtracking capabilities, disabling the implicit cut semantics offered by `<|>`.
      * @param p The parser to run
      * @return The result of `p`, or if `p` failed ensures the parser state was as it was on entry.
      */
    def attempt[Tok, A](p: =>Parsley[Tok, A]): Parsley[Tok, A] = new DeepEmbedding.Attempt(p)
    /** Parses `p` without consuming any input. If `p` fails and consumes input then so does `lookAhead(p)`. Combine with
      * `attempt` if this is undesirable.
      * @param p The parser to look ahead at
      * @return The result of the lookahead
      */
    def lookAhead[Tok, A](p: =>Parsley[Tok, A]): Parsley[Tok, A] = new DeepEmbedding.Look(p)
    /**Alias for `p ? msg`.*/
    def label[Tok, A](p: Parsley[Tok, A], msg: String): Parsley[Tok, A] = p ? msg
    /** The `fail(msg)` parser consumes no input and fails with `msg` as the error message */
    def fail(msg: String): Parsley[Any, Nothing] = new DeepEmbedding.Fail(msg)
    /** The `empty` parser consumes no input and fails softly (that is to say, no error message) */
    val empty: Parsley[Any, Nothing] = new DeepEmbedding.Empty
    /** The `unexpected(msg)` parser consumes no input and fails with `msg` as an unexpected error */
    def unexpected(msg: String): Parsley[Any, Nothing] = new DeepEmbedding.Unexpected(msg)
    /** Returns `()`. Defined as `pure(())` but aliased for sugar*/
    val unit: Parsley[Any, Unit] = pure(())
    /** converts a parser's result to () */
    def void[Tok](p: Parsley[Tok, _]): Parsley[Tok, Unit] = p *> unit
    /** `many(p)` executes the parser `p` zero or more times. Returns a list of the returned values of `p`. */
    def many[Tok, A](p: =>Parsley[Tok, A]): Parsley[Tok, List[A]] = new DeepEmbedding.Many(p)
    /** `skipMany(p)` executes the parser `p` zero or more times and ignores the results. Returns `()` */
    def skipMany[Tok, A](p: =>Parsley[Tok, A]): Parsley[Tok, Unit] = new DeepEmbedding.*>(new DeepEmbedding.SkipMany(p), unit)
    /**
      * Evaluate each of the parsers in `ps` sequentially from left to right, collecting the results.
      * @param ps Parsers to be sequenced
      * @return The list containing results, one from each parser, in order
      */
    def sequence[Tok, A](ps: Parsley[Tok, A]*): Parsley[Tok, List[A]] = ps.foldRight[Parsley[Tok, List[A]]](pure[List[A]](Nil))(_ <::> _)
    /**
      * Like `sequence` but produces a list of parsers to sequence by applying the function `f` to each
      * element in `xs`.
      * @param f The function to map on each element of `xs` to produce parsers
      * @param xs Values to generate parsers from
      * @return The list containing results formed by executing each parser generated from `xs` and `f` in sequence
      */
    def traverse[Tok, A, B](f: A => Parsley[Tok, B], xs: A*): Parsley[Tok, List[B]] = sequence(xs.map(f): _*)
    /**
      * Evaluate each of the parsers in `ps` sequentially from left to right, ignoring the results.
      * @param ps Parsers to be performed
      */
    def skip[Tok](ps: Parsley[Tok, _]*): Parsley[Tok, Unit] = ps.foldRight[Parsley[Tok, Unit]](unit)(_ *> _)
    /**
      * This parser consumes no input and returns the current line number reached in the input stream
      * @return The line number the parser is currently at
      */
    val line: Parsley[Any, Int] = DeepEmbedding.Line
    /**
      * This parser consumes no input and returns the current column number reached in the input stream
      * @return The column number the parser is currently at
      */
    val col: Parsley[Any, Int] = DeepEmbedding.Col
    /**
      * This parser consumes no input and returns the current position reached in the input stream
      * @return Tuple of line and column number that the parser has reached
      */
    val pos: Parsley[Any, (Int, Int)] = line <~> col
    /**
      * Consumes no input and returns the value stored in one of the parser registers.
      * @note There are only 4 registers at present.
      * @param v The index of the register to collect from
      * @tparam S The type of the value in register `v` (this will result in a runtime type-check)
      * @return The value stored in register `v` of type `S`
      */
    def get[S](v: Var)(implicit ev: S =!= Nothing): Parsley[Any, S] = new DeepEmbedding.Get(v)
    /**
      * Consumes no input and returns the value stored in one of the parser registers after applying a function.
      * @note There are only 4 registers at present.
      * @param v The index of the register to collect from
      * @param f The function used to transform the value in the register
      * @tparam S The type of the value in register `v` (this will result in a runtime type-check)
      * @tparam A The desired result type
      * @return The value stored in register `v` applied to `f`
      */
    def gets[S, A](v: Var, f: S => A): Parsley[Any, A] = gets(v, pure(f))
    /**
      * Returns the value stored in one of the parser registers after applying a function obtained from given parser.
      * @note There are only 4 registers at present. The value is fetched before `pf` is executed
      * @param v The index of the register to collect from
      * @param pf The parser which provides the function to transform values
      * @tparam S The type of the value in register `v` (this will result in a runtime type-check)
      * @tparam A The desired result type
      * @return The value stored in register `v` applied to `f` from `pf`
      */
    def gets[Tok, S, A](v: Var, pf: Parsley[Tok, S => A]): Parsley[Tok, A] = get[S](v) <**> pf
    /**
      * Consumes no input and places the value `x` into register `v`.
      * @note There are only 4 registers at present.
      * @param v The index of the register to place the value in
      * @param x The value to place in the register
      */
    def put[S](v: Var, x: S): Parsley[Any, Unit] = put(v, pure(x))
    /**
      * Places the result of running `p` into register `v`.
      * @note There are only 4 registers at present.
      * @param v The index of the register to place the value in
      * @param p The parser to derive the value from
      */
    def put[Tok, S](v: Var, p: =>Parsley[Tok, S]): Parsley[Tok, Unit] = new DeepEmbedding.*>(new DeepEmbedding.Put(v, p), unit)
    /**
      * Modifies the value contained in register `v` using function `f`. It is left to the users responsibility to
      * ensure the types line up. There is no compile-time type checking enforced!
      * @note There are only 4 registers at present.
      * @param v The index of the register to modify
      * @param f The function used to modify the register
      * @tparam S The type of value currently assumed to be in the register
      */
    def modify[S](v: Var, f: S => S): Parsley[Any, Unit] = new DeepEmbedding.*>(new DeepEmbedding.Modify(v, f), unit)
    /**
      * For the duration of parser `p` the state stored in register `v` is instead set to `x`. The change is undone
      * after `p` has finished.
      * @note There are only 4 registers at present.
      * @param v The index of the register to modify
      * @param x The value to place in the register `v`
      * @param p The parser to execute with the adjusted state
      * @return The parser that performs `p` with the modified state
      */
    def local[Tok, R, A](v: Var, x: R, p: =>Parsley[Tok, A]): Parsley[Tok, A] = local(v, pure(x), p)
    /**
      * For the duration of parser `q` the state stored in register `v` is instead set to the return value of `p`. The
      * change is undone after `q` has finished.
      * @note There are only 4 registers at present.
      * @param v The index of the register to modify
      * @param p The parser whose return value is placed in register `v`
      * @param q The parser to execute with the adjusted state
      * @return The parser that performs `q` with the modified state
      */
    def local[Tok, R, A](v: Var, p: =>Parsley[Tok, R], q: =>Parsley[Tok, A]): Parsley[Tok, A] = new DeepEmbedding.Local(v, p, q)
    /**
      * For the duration of parser `p` the state stored in register `v` is instead modified with `f`. The change is undone
      * after `p` has finished.
      * @note There are only 4 registers at present.
      * @param v The index of the register to modify
      * @param f The function used to modify the value in register `v`
      * @param p The parser to execute with the adjusted state
      * @return The parser that performs `p` with the modified state
      */
    def local[Tok, R, A](v: Var, f: R => R, p: =>Parsley[Tok, A]): Parsley[Tok, A] = local(v, get[R](v).map(f), p)
    /** Reads a character from the head of the input stream if and only if it satisfies the given predicate. Else it
      * fails without consuming the character.
      * @param f The function to test the character on
      * @return `c` if `f(c)` is true.
      */
    def satisfy[Tok](f: Tok => Boolean): Parsley[Tok, Tok] = new DeepEmbedding.Satisfy(f)
    /** Reads a token from the input stream and returns it, else fails if the token is not found at the head
      * of the stream.
      * @param t The token to search for
      * @return `t` if it can be found at the head of the input
      */
    def token[Tok](t: Tok): Parsley[Tok, Tok] = new DeepEmbedding.CharTok(t)
    /**`oneOf(ts)` succeeds if the current token is in the supplied sequence of tokens `ts`.
      * Returns the parsed token. See also `satisfy`.*/
    def oneOf[Tok](ts: Set[Tok]): Parsley[Tok, Tok] = satisfy(ts.contains)
    /**As the dual of `oneOf`, `noneOf(ts)` succeeds if the current token is not in the supplied
      * sequence of tokens `ts`. Returns the parsed token.*/
    def noneOf[Tok](ts: Set[Tok]): Parsley[Tok, Tok] = satisfy(!ts.contains(_))
    /**`oneOf(ts)` succeeds if the current token is in the supplied sequence of tokens `ts`.
      * Returns the parsed token. See also `satisfy`.*/
    def oneOf[Tok](ts: Tok*): Parsley[Tok, Tok] = oneOf(ts.toSet)
    /**As the dual of `oneOf`, `noneOf(ts)` succeeds if the current token is not in the supplied
      * sequence of tokens `ts`. Returns the parsed token.*/
    def noneOf[Tok](ts: Tok*): Parsley[Tok, Tok] = noneOf(ts.toSet)
    /**The parser `item` accepts any kind of token. Returns the accepted token.*/
    def item[Tok]: Parsley[Tok, Tok] = satisfy[Tok](_ => true) ? "any token"
}

// Internals
private [parsley] class CodeGenState
{
    import CodeGenState.CodeGenSubQueueNode
    private [this] var current = 0
    private [this] var queue: CodeGenSubQueueNode = _
    val map: mutable.Map[Parsley[_, _], Int] = mutable.Map.empty
    def freshLabel(): Int =
    {
        val next = current
        current += 1
        next
    }
    def nlabels: Int = current

    def getSubLabel(p: Parsley[_, _]) =
    {
        map.getOrElseUpdate(p,
        {
            queue = new CodeGenSubQueueNode(p, queue)
            freshLabel()
        })
    }

    def nextSub(): Parsley[_, _] =
    {
        val p = queue.p
        queue = queue.tail
        p
    }

    def more: Boolean = queue != null
}
private [parsley] object CodeGenState
{
    private [CodeGenState] class CodeGenSubQueueNode(val p: Parsley[_, _], val tail: CodeGenSubQueueNode)
}

private [parsley] class LetFinderState
{
    private val _recs = mutable.Set.empty[Parsley[_, _]]
    private val _preds = mutable.Map.empty[Parsley[_, _], Int]
    private val before = mutable.Set.empty[Parsley[_, _]]

    def addPred(p: Parsley[_, _]): Unit = _preds += p -> (_preds.getOrElseUpdate(p, 0) + 1)
    def addRec(p: Parsley[_, _]): Unit = _recs += p
    def doNotProcessAgain(p: Parsley[_, _]): Unit = before += p
    def notProcessedBefore(p: Parsley[_, _]): Boolean = !before.contains(p)

    def lets: Map[Parsley[_, _], Parsley[_, _]] =
    {
        (for ((k, v) <- _preds;
              if v >= 2 && !_recs.contains(k))
         yield k -> new DeepEmbedding.Subroutine(k)).toMap
    }
    def recs: Set[Parsley[_, _]] = _recs.toSet
}

private [parsley] class SubMap(val subMap: Map[Parsley[_, _], Parsley[_, _]]) extends AnyVal
{
    def apply[Tok, A](p: Parsley[Tok, A]): Parsley[Tok, A] = subMap.getOrElse(p, p).asInstanceOf[Parsley[Tok, A]]
}

/**
  * This is the class that encapsulates the act of parsing and running an object of this class with `runParser` will
  * parse the string given as input to `runParser`.
  *
  * Note: In order to construct an object of this class you must use the combinators; the class itself is abstract
  *
  * @author Jamie Willis
  * @version 1
  */
abstract class Parsley[-Tok, +A] private [parsley]
{
    final protected type InstrBuffer = ResizableArray[Instr]
    final protected type T = Any
    final protected type U = Any
    final protected type V = Any
    /**
      * Using this method signifies that the parser it is invoked on is impure and any optimisations which assume purity
      * are disabled.
      */
    final def unsafe(): Unit = safe = false

    /**
      * Forces the compilation of a parser as opposed to the regular lazy evaluation.
      */
    final def force(): Unit = instrs

    /**
      *
      * Provides an indicator that this parser is likely to stack-overflow
      */
    final def overflows(): Unit = cps = true

    final private [parsley] def pretty: String = instrs.mkString("; ")

    // Internals
    final private [parsley] def findLets[Cont[_, _]](implicit seen: Set[Parsley[_, _]], state: LetFinderState, ops: ContOps[Cont]): Cont[Unit, Unit] =
    {
        state.addPred(this)
        if (seen(this)) result(state.addRec(this))
        else if (state.notProcessedBefore(this)) for (_ <- findLetsAux(seen + this, state, ops)) yield state.doNotProcessAgain(this)
        else result(())
    }
    final private [parsley] def optimised[Cont[_, _], Tok_ <: Tok, A_ >: A](implicit seen: Set[Parsley[_, _]], sub: SubMap, label: UnsafeOption[String], ops: ContOps[Cont]): Cont[Parsley[_, _], Parsley[Tok_, A_]] =
    {
        for (p <- this.fix.preprocess[Cont, Tok_, A_](seen + this, sub, label, ops)) yield p.optimise
    }
    final private [parsley] var safe = true
    final private [parsley] var cps = false
    final private [parsley] var size: Int = 1

    final private def computeInstrs(implicit ops: GenOps): Array[Instr] =
    {
        val instrs: InstrBuffer = new ResizableArray()
        val state = new CodeGenState
        val letFinderState = new LetFinderState
        perform(findLets(Set.empty, letFinderState, ops))
        perform(perform(optimised(Set.empty, new SubMap(letFinderState.lets), null, ops).asInstanceOf[({type C[_, _]})#C[Parsley[_, _], Parsley[_, _]]]).codeGen(instrs, state, ops))
        if (state.map.nonEmpty)
        {
            val end = state.freshLabel()
            instrs += new instructions.Jump(end)
            val map = state.map
            while (state.more)
            {
                val p = state.nextSub()
                val label = map(p)
                instrs += new instructions.Label(label)
                perform(p.codeGen(instrs, state, ops))
                instrs += instructions.Return
            }
            instrs += new instructions.Label(end)
        }
        val instrsOversize = instrs.toArray
        val labelMapping = new Array[Int](state.nlabels)
        @tailrec def findLabels(instrs: Array[Instr], labels: Array[Int], n: Int, i: Int, off: Int, nopop: Int): Int = if (i + off < n) instrs(i + off) match
        {
            case label: Label => instrs(i + off) = null; labels(label.i) = i; findLabels(instrs, labels, n, i, off + 1, nopop)
            case _: NoPush => findLabels(instrs, labels, n, i + 1, off, nopop + 1)
            case instructions.Pop if nopop != 0 => instrs(i + off) = null; findLabels(instrs, labels, n, i, off + 1, nopop - 1)
            case instructions.Exchange(x) if nopop != 0 => instrs(i + off) = new instructions.Push(x); findLabels(instrs, labels, n, i + 1, off, nopop - 1)
            case _ => findLabels(instrs, labels, n, i + 1, off, nopop)
        }
        else i
        @tailrec def applyLabels(srcs: Array[Instr], labels: Array[Int], dests: Array[Instr], n: Int, i: Int, off: Int): Unit = if (i < n) srcs(i + off) match
        {
            case null => applyLabels(srcs, labels, dests, n, i, off + 1)
            case jump: JumpInstr =>
                jump.label = labels(jump.label)
                dests(i) = jump
                applyLabels(srcs, labels, dests, n, i + 1, off)
            case table: JumpTable =>
                table.relabel(labels)
                dests(i) = table
                applyLabels(srcs, labels, dests, n, i + 1, off)
            case instr =>
                dests(i) = instr
                applyLabels(srcs, labels, dests, n, i + 1, off)
        }
        val size = findLabels(instrsOversize, labelMapping, instrs.length, 0, 0, 0)
        val instrs_ = new Array[Instr](size)
        applyLabels(instrsOversize, labelMapping, instrs_, instrs_.length, 0, 0)
        instrs_
    }

    final private [parsley] lazy val instrs: Array[Instr] = if (cps) computeInstrs(Cont.ops.asInstanceOf[GenOps]) else safeCall(computeInstrs(_))
    final private [this] lazy val pindices: Array[Int] =
    {
        val linstrs = instrs
        val sz = linstrs.length
        var i: Int = 0
        val buff: ResizableArray[Int] = new ResizableArray[Int]()
        while (i < sz)
        {
            // We need to check for calls here too, unlike a call copy.
            if (linstrs(i).isInstanceOf[Stateful] || linstrs(i).isInstanceOf[Call]) buff += i
            i += 1
        }
        buff.toArray
    }
    final private [parsley] def threadSafeInstrs: Array[Instr] =
    {
        val nstateful = pindices.length
        if (nstateful != 0)
        {
            val linstrs = instrs.clone
            val lpindices = pindices
            var i: Int = 0
            while (i < nstateful)
            {
                val j = lpindices(i)
                linstrs(j) = linstrs(j).copy
                i += 1
            }
            linstrs
        }
        else instrs
    }
    final private [parsley] def fix(implicit seen: Set[Parsley[_, _]], sub: SubMap, label: UnsafeOption[String]): Parsley[Tok, A] =
    {
        // We use the seen set here to prevent cascading sub-routines
        val self = sub(this)
        if (seen(this))
        {
            if (self == this) new DeepEmbedding.Fixpoint(() => this, label)
            else this
        }
        else self
    }

    // Abstracts
    // Sub-tree optimisation and fixpoint calculation - Bottom-up
    protected def preprocess[Cont[_, _], Tok_ <: Tok, A_ >: A](implicit seen: Set[Parsley[_, _]], sub: SubMap, label: UnsafeOption[String], ops: ContOps[Cont]): Cont[Parsley[_, _], Parsley[Tok_, A_]]
    // Let-finder recursion
    protected def findLetsAux[Cont[_, _]](implicit seen: Set[Parsley[_, _]], state: LetFinderState, ops: ContOps[Cont]): Cont[Unit, Unit]
    // Optimisation - Bottom-up
    private [parsley] def optimise: Parsley[Tok, A] = this
    // Peephole optimisation and code generation - Top-down
    private [parsley] def codeGen[Cont[_, _]](implicit instrs: InstrBuffer, state: CodeGenState, ops: ContOps[Cont]): Cont[Unit, Unit]
}

private [parsley] object DeepEmbedding
{
    // Core Embedding
    private [parsley] final class Pure[A](private [Pure] val x: A) extends Parsley[Any, A]
    {
        override def preprocess[Cont[_, _], Tok_ <: Any, A_ >: A](implicit seen: Set[Parsley[_, _]], sub: SubMap, label: UnsafeOption[String], ops: ContOps[Cont]): Cont[Parsley[_, _], Parsley[Tok_, A_]] = result(this)
        override def findLetsAux[Cont[_, _]](implicit seen: Set[Parsley[_, _]], state: LetFinderState, ops: ContOps[Cont]): Cont[Unit, Unit] = result(())
        override def codeGen[Cont[_, _]](implicit instrs: InstrBuffer, state: CodeGenState, ops: ContOps[Cont]): Cont[Unit, Unit] =
        {
            result(instrs += new instructions.Push(x))
        }
    }
    private [parsley] final class <*>[Tok, A, B](_pf: =>Parsley[Tok, A => B], _px: =>Parsley[Tok, A]) extends Parsley[Tok, B]
    {
        private [<*>] var pf: Parsley[Tok, A => B] = _
        private [<*>] var px: Parsley[Tok, A] = _
        override def preprocess[Cont[_, _], Tok_ <: Tok, B_ >: B](implicit seen: Set[Parsley[_, _]], sub: SubMap, label: UnsafeOption[String], ops: ContOps[Cont]): Cont[Parsley[_, _], Parsley[Tok_, B_]] =
            if (label == null && pf != null) result(this) else for (pf <- _pf.optimised[Cont, Tok, A => B]; px <- _px.optimised[Cont, Tok, A]) yield
            {
                if (label == null)
                {
                    this.pf = pf
                    this.px = px
                    this.size = pf.size + px.size + 1
                    this
                }
                else <*>(pf, px)
            }
        override def findLetsAux[Cont[_, _]](implicit seen: Set[Parsley[_, _]], state: LetFinderState, ops: ContOps[Cont]): Cont[Unit, Unit] =
            for (_ <- _pf.findLets; _ <- _px.findLets) yield ()
        override def optimise: Parsley[Tok, B] = (pf, px) match
        {
            // Fusion laws
            case (uf, Pure(x)) if uf.isInstanceOf[Pure[_]] || uf.isInstanceOf[<*>[Tok, _, _]] && uf.safe => uf match
            {
                // first position fusion
                case Pure(f) => new Pure(f(x))
                // second position fusion
                case Pure(f: (T => A => B) @unchecked) <*> (uy: Parsley[Tok, T]) =>
                    pf = new Pure((y: T) => f(y)(x))
                    px = uy.asInstanceOf[Parsley[Tok, A]]
                    this
                // third position fusion
                case Pure(f: (T => U => A => B) @unchecked) <*> (uy: Parsley[Tok, T]) <*> (uz: Parsley[Tok, U]) =>
                    pf = <*>(new Pure((y: T) => (z: U) => f(y)(z)(x)), uy)
                    px = uz.asInstanceOf[Parsley[Tok, A]]
                    this
                // interchange law: u <*> pure y == pure ($y) <*> u == ($y) <$> u (single instruction, so we benefit at code-gen)
                case _ =>
                    pf = new Pure((f: A => B) => f(x)).asInstanceOf[Parsley[Tok, A => B]]
                    px = uf.asInstanceOf[Parsley[Tok, A]]
                    this
            }
            // functor law: fmap f (fmap g p) == fmap (f . g) p where fmap f p = pure f <*> p from applicative
            case (Pure(f), Pure(g: (T => A) @unchecked) <*> (u: Parsley[Tok, T])) => <*>(new Pure(f.compose(g)), u)
            // TODO: functor law with lift2!
            // right absorption law: mzero <*> p = mzero
            case (z: MZero[Tok], _) => z
            /* RE-ASSOCIATION LAWS */
            // re-association law 1: (q *> pf) <*> px = q *> (pf <*> px)
            case (q *> uf, ux) => *>(q, <*>(uf, ux).optimise)
            case (uf, cont: Cont[Tok, _, _]) => cont match
            {
                // re-association law 2: pf <*> (px <* q) = (pf <*> px) <* q
                case ux <* v => <*(<*>(uf, ux).optimise, v).optimise
                // re-association law 3: p *> pure x = pure x <* p
                // consequence of re-association law 3: pf <*> (q *> pure x) = (pf <*> pure x) <* q
                case v *> (ux: Pure[_]) => <*(<*>(uf, ux).optimise, v).optimise
                case _ => this
            }
            // consequence of left zero law and monadic definition of <*>, preserving error properties of pf
            case (u, z: MZero[Tok]) => *>(u, z)
            // interchange law: u <*> pure y == pure ($y) <*> u == ($y) <$> u (single instruction, so we benefit at code-gen)
            case (uf, Pure(x)) =>
                pf = new Pure((f: A => B) => f(x)).asInstanceOf[Parsley[Tok, A => B]]
                px = uf.asInstanceOf[Parsley[Tok, A]]
                this
            case _ => this
        }
        override def codeGen[Cont[_, _]](implicit instrs: InstrBuffer, state: CodeGenState, ops: ContOps[Cont]): Cont[Unit, Unit] = pf match
        {
            // pure f <*> p = f <$> p
            case Pure(f) => px match
            {
                case ct@CharTok(c) => result(instrs += instructions.CharTokFastPerform[Tok, B](c.asInstanceOf[Tok], f.asInstanceOf[Tok => B], ct.expected))
                case st@StringTok(s) => result(instrs += new instructions.StringTokFastPerform(s, f.asInstanceOf[String => B], st.expected))
                case _ =>
                    px.codeGen |>
                    (instrs += new instructions.Perform(f))
            }
            case _ =>
                pf.codeGen >>
                px.codeGen |>
                (instrs += instructions.Apply)
        }
    }
    private [parsley] final class <|>[Tok, A, B](_p: =>Parsley[Tok, A], _q: =>Parsley[Tok, B]) extends Parsley[Tok, B]
    {
        private [<|>] var p: Parsley[Tok, A] = _
        private [<|>] var q: Parsley[Tok, B] = _
        override def preprocess[Cont[_, _], Tok_ <: Tok, B_ >: B](implicit seen: Set[Parsley[_, _]], sub: SubMap, label: UnsafeOption[String], ops: ContOps[Cont]): Cont[Parsley[_, _], Parsley[Tok_, B_]] =
            if (label == null && p != null) result(this) else for (p <- _p.optimised[Cont, Tok, A]; q <- _q.optimised[Cont, Tok, B]) yield
            {
                if (label == null)
                {
                    this.p = p
                    this.q = q
                    this.size = p.size + q.size + 3
                    this
                }
                else <|>(p, q)
            }
        override def findLetsAux[Cont[_, _]](implicit seen: Set[Parsley[_, _]], state: LetFinderState, ops: ContOps[Cont]): Cont[Unit, Unit] =
            for (_ <- _p.findLets; _ <- _q.findLets) yield ()
        override def optimise: Parsley[Tok, B] = (p, q) match
        {
            // left catch law: pure x <|> p = pure x
            case (u: Pure[B], _) => u
            // alternative law: empty <|> p = p
            case (e: Empty, v) if e.expected == null => v
            // alternative law: p <|> empty = p
            case (u: Parsley[Tok, B], e: Empty) if e.expected == null => u
            // associative law: (u <|> v) <|> w = u <|> (v <|> w)
            case ((u: Parsley[Tok, T]) <|> (v: Parsley[Tok, A]), w) =>
                p = u.asInstanceOf[Parsley[Tok, A]]
                q = <|>[Tok, A, B](v, w).optimise
                this
            case _ => this
        }
        override def codeGen[Cont[_, _]](implicit instrs: InstrBuffer, state: CodeGenState, ops: ContOps[Cont]): Cont[Unit, Unit] = tablify(this, Nil) match
        {
            // If the tablified list is single element, that implies that this should be generated as normal!
            case _::Nil => p match
            {
                case Attempt(u) => q match
                {
                    case Pure(x) =>
                        val handler = state.freshLabel()
                        instrs += new instructions.PushHandler(handler)
                        u.codeGen |>
                        {
                            instrs += new instructions.Label(handler)
                            instrs += new instructions.AlwaysRecoverWith[B](x)
                        }
                    case v =>
                        val handler = state.freshLabel()
                        val skip = state.freshLabel()
                        instrs += new instructions.PushHandler(handler)
                        u.codeGen >>
                        {
                            instrs += new instructions.Label(handler)
                            instrs += new instructions.JumpGoodAttempt(skip)
                            v.codeGen |>
                            (instrs += new instructions.Label(skip))
                        }
                }
                case u => q match
                {
                    case Pure(x) =>
                        val handler = state.freshLabel()
                        val skip = state.freshLabel()
                        instrs += new instructions.InputCheck(handler)
                        u.codeGen |>
                        {
                            instrs += new instructions.JumpGood(skip)
                            instrs += new instructions.Label(handler)
                            instrs += new instructions.RecoverWith[B](x)
                            instrs += new instructions.Label(skip)
                        }
                    case v =>
                        val handler = state.freshLabel()
                        val skip = state.freshLabel()
                        instrs += new instructions.InputCheck(handler)
                        u.codeGen >>
                        {
                            instrs += new instructions.JumpGood(skip)
                            instrs += new instructions.Label(handler)
                            instrs += instructions.Catch
                            v.codeGen |>
                            (instrs += new instructions.Label(skip))
                        }
                }

            }
            // In case of None'd list, the codeGen cont continues by codeGenning that p, else we are done for this tree, call cont!
            case tablified =>
                // This list is backwards :)
                val needsDefault = tablified.head._2.isDefined
                val end = state.freshLabel()
                val default = state.freshLabel()
                val (roots, leads, ls, expecteds) = foldTablified(tablified, state, mutable.Map.empty, Nil, Nil, Nil)
                instrs += new instructions.JumpTable(leads, ls, default, expecteds)
                codeGenRoots(roots, ls, end) >>
                {
                    instrs += instructions.Catch
                    instrs += new instructions.Label(default)
                    if (needsDefault)
                    {
                        instrs += new instructions.Empty(null)
                        result(instrs += new instructions.Label(end))
                    }
                    else
                    {
                        tablified.head._1.codeGen |>
                        (instrs += new instructions.Label(end))
                    }
                }
        }
        def codeGenRoots[Cont[_, _]](roots: List[List[Parsley[Tok, _]]], ls: List[Int], end: Int)(implicit instrs: InstrBuffer, state: CodeGenState, ops: ContOps[Cont]): Cont[Unit, Unit] = roots match
        {
            case root::roots_ =>
                instrs += new instructions.Label(ls.head)
                codeGenAlternatives(root) >>
                {
                    instrs += new instructions.JumpGood(end)
                    codeGenRoots(roots_, ls.tail, end)
                }
            case Nil => result(())
        }
        def codeGenAlternatives[Cont[_, _]](alts: List[Parsley[Tok, _]])(implicit instrs: InstrBuffer, state: CodeGenState, ops: ContOps[Cont]): Cont[Unit, Unit] = (alts: @unchecked) match
        {
            case alt::Nil => alt.codeGen
            case Attempt(alt)::alts_ =>
                val handler = state.freshLabel()
                val skip = state.freshLabel()
                instrs += new instructions.PushHandler(handler)
                alt.codeGen >>
                {
                    instrs += new instructions.Label(handler)
                    instrs += new instructions.JumpGoodAttempt(skip)
                    ops.|>(codeGenAlternatives(alts_), instrs += new instructions.Label(skip))
                }
            case alt::alts_ =>
                val handler = state.freshLabel()
                val skip = state.freshLabel()
                instrs += new instructions.InputCheck(handler)
                alt.codeGen >>
                {
                    instrs += new instructions.JumpGood(skip)
                    instrs += new instructions.Label(handler)
                    instrs += instructions.Catch
                    ops.|>(codeGenAlternatives(alts_), instrs += new instructions.Label(skip))
                }
        }
        @tailrec def foldTablified(tablified: List[(Parsley[Tok, _], Option[Parsley[Tok, _]])], labelGen: CodeGenState,
                                   roots: mutable.Map[Char, List[Parsley[Tok, _]]],
                                   leads: List[Char],
                                   labels: List[Int],
                                   expecteds: List[UnsafeOption[String]]):
            (List[List[Parsley[Tok, _]]], List[Char], List[Int], List[UnsafeOption[String]]) = tablified match
        {
            case (_, None)::tablified_ => foldTablified(tablified_, labelGen, roots, leads, labels, expecteds)
            case (root, Some(lead))::tablified_ =>
                val (c, expected) = lead match
                {
                    //case ct@CharTok(d) => (d, ct.expected) //TODO What do we do about this?
                    case st@StringTok(s) => (s.head, if (st.expected == null) "\"" + s + "\"" else st.expected)
                    case kw@Keyword(k) => (k.head, if (kw.expected == null) k else kw.expected)
                    case op@Operator(o) => (o.head, if (op.expected == null) o else op.expected)
                    case op@MaxOp(o) => (o.head, if (op.expected == null) o else op.expected)
                    case sl: StringLiteral => ('"', if (sl.expected == null) "string" else sl.expected)
                    case rs: RawStringLiteral => ('"', if (rs.expected == null) "string" else rs.expected)
                }
                if (roots.contains(c))
                {
                    roots.update(c, root::roots(c))
                    foldTablified(tablified_, labelGen, roots, leads, labelGen.freshLabel() :: labels, expected :: expecteds)
                }
                else
                {
                    roots.update(c, root::Nil)
                    foldTablified(tablified_, labelGen, roots, c::leads, labelGen.freshLabel() :: labels, expected :: expecteds)
                }
            case Nil => (leads.map(roots(_)), leads, labels, expecteds)
        }
        @tailrec private def tablable(p: Parsley[Tok, _]): Option[Parsley[Tok, _]] = p match
        {
            // CODO: Numeric parsers by leading digit (This one would require changing the foldTablified function a bit)
            case t@(/*_: CharTok |*/ _: StringTok | _: Keyword | _: StringLiteral | _: RawStringLiteral | _: Operator | _: MaxOp) => Some(t)
            case Attempt(t) => tablable(t)
            case (_: Pure[_]) <*> t => tablable(t)
            case Lift2(_, t, _) => tablable(t)
            case Lift3(_, t, _, _) => tablable(t)
            case t <*> _ => tablable(t)
            case t *> _ => tablable(t)
            case t <* _ => tablable(t)
            case _ => None
        }
        @tailrec private [DeepEmbedding] def tablify(p: Parsley[Tok, _], acc: List[(Parsley[Tok, _], Option[Parsley[Tok, _]])]): List[(Parsley[Tok, _], Option[Parsley[Tok, _]])] = p match
        {
            case u <|> v =>
                val leading = tablable(u)
                if (leading.isDefined) tablify(v, (u, leading)::acc)
                else (p, None)::acc
            case _ => (p, tablable(p))::acc
        }
    }
    private [parsley] final class >>=[Tok, A, B](_p: =>Parsley[Tok, A], private [>>=] var f: A => Parsley[Tok, B], val expected: UnsafeOption[String] = null) extends Parsley[Tok, B]
    {
        private [>>=] var p: Parsley[Tok, A] = _
        override def preprocess[Cont[_, _], Tok_ <: Tok, B_ >: B](implicit seen: Set[Parsley[_, _]], sub: SubMap, label: UnsafeOption[String], ops: ContOps[Cont]): Cont[Parsley[_, _], Parsley[Tok_, B_]] =
            if (label == null && p != null) result(this) else for (p <- _p.optimised[Cont, Tok, A]) yield
            {
                if (label == null)
                {
                    this.p = p
                    this.size = p.size + 1
                    this
                }
                else >>=(p, f, label)
            }
        override def findLetsAux[Cont[_, _]](implicit seen: Set[Parsley[_, _]], state: LetFinderState, ops: ContOps[Cont]): Cont[Unit, Unit] = _p.findLets
        override def optimise: Parsley[Tok, B] = p match
        {
            // CODO: We need to try and identify the fixpoints in the optimised binds, so we can remove the call instructions
            // monad law 1: pure x >>= f = f x
            case Pure(x) if safe => new Fixpoint(() => f(x), expected)
            // char/string x = char/string x *> pure x and monad law 1
            case p@CharTok(c) => *>(p, new Fixpoint(() => f(c.asInstanceOf[A]), expected))
            case p@StringTok(s) => *>(p, new Fixpoint(() => f(s.asInstanceOf[A]), expected))
            // (q *> p) >>= f = q *> (p >>= f)
            case u *> v => *>(u, >>=(v, f, expected).optimise)
            // monad law 3: (m >>= g) >>= f = m >>= (\x -> g x >>= f) Note: this *could* help if g x ended with a pure, since this would be optimised out!
            case (m: Parsley[Tok, T] @unchecked) >>= (g: (T => A) @unchecked) =>
                p = m.asInstanceOf[Parsley[Tok, A]]
                f = (x: T) => >>=(g(x), f, expected).optimise
                this
            // monadplus law (left zero)
            case z: MZero[Tok] => z
            case _ => this
        }
        override def codeGen[Cont[_, _]](implicit instrs: InstrBuffer, state: CodeGenState, ops: ContOps[Cont]): Cont[Unit, Unit] =
        {
            p.codeGen |>
            (instrs += new instructions.DynCall[A](x => f(x).instrs, expected))
        }
    }
    private [parsley] final class Satisfy[Tok](private [Satisfy] val f: Tok => Boolean, val expected: UnsafeOption[String] = null) extends Parsley[Tok, Tok]
    {
        override def preprocess[Cont[_, _], Tok_ <: Tok, Tok__ >: Tok](implicit seen: Set[Parsley[_, _]], sub: SubMap, label: UnsafeOption[String], ops: ContOps[Cont]): Cont[Parsley[_, _], Parsley[Tok_, Tok__]] =
        {
            if (label == null) result(this)
            else result(new Satisfy(f, label))
        }
        override def findLetsAux[Cont[_, _]](implicit seen: Set[Parsley[_, _]], state: LetFinderState, ops: ContOps[Cont]): Cont[Unit, Unit] = result(())
        override def codeGen[Cont[_, _]](implicit instrs: InstrBuffer, state: CodeGenState, ops: ContOps[Cont]): Cont[Unit, Unit] =
        {
            result(instrs += new instructions.Satisfies(f.asInstanceOf[Any => Boolean], expected))
        }
    }
    private [parsley] abstract class Cont[Tok, A, +B] extends Parsley[Tok, B]
    {
        def result: Parsley[Tok, B]
        def discard: Parsley[Tok, A]
        def copy[B_ >: B](prev: Parsley[Tok, A], next: Parsley[Tok, B_]): Cont[Tok, A, B_]
    }
    private [parsley] final class *>[Tok, A, B](_p: =>Parsley[Tok, A], _q: =>Parsley[Tok, B]) extends Cont[Tok, A, B]
    {
        private [*>] var p: Parsley[Tok, A] = _
        private [*>] var q: Parsley[Tok, B] = _
        override def preprocess[Cont[_, _], Tok_ <: Tok, B_ >: B](implicit seen: Set[Parsley[_, _]], sub: SubMap, label: UnsafeOption[String], ops: ContOps[Cont]): Cont[Parsley[_, _], Parsley[Tok_, B_]] =
            if (label == null && p != null) ops.wrap(this) else for (p <- _p.optimised[Cont, Tok, A]; q <- _q.optimised[Cont, Tok, B]) yield
            {
                if (label == null)
                {
                    this.p = p
                    this.q = q
                    this.size = p.size + q.size + 1
                    this
                }
                else *>(p, q)
            }
        override def findLetsAux[Cont[_, _]](implicit seen: Set[Parsley[_, _]], state: LetFinderState, ops: ContOps[Cont]): Cont[Unit, Unit] =
            for (_ <- _p.findLets; _ <- _q.findLets) yield ()
        @tailrec override def optimise: Parsley[Tok, B] = p match
        {
            // pure _ *> p = p
            case _: Pure[_] => q
            // p *> pure _ *> q = p *> q
            case u *> (_: Pure[_]) =>
                p = u.asInstanceOf[Parsley[Tok, A]]
                optimise
            /*case CharTok(c) if q.isInstanceOf[CharTok] || q.isInstanceOf[StringTok] => q match
            {
                // char(c) *> char(d) = string(cd) *> pure(d)
                case CharTok(d) =>
                    p = new StringTok(c.toString + d).asInstanceOf[Parsley[Tok, A]]
                    q = new Pure(d).asInstanceOf[Parsley[Tok, B]]
                    optimise
                // char(c) *> string(s) = string(cs) *> pure(s)
                case StringTok(s) =>
                    p = new StringTok(c.toString + s).asInstanceOf[Parsley[Tok, A]]
                    q = new Pure(s).asInstanceOf[Parsley[Tok, B]]
                    optimise
            }*/
            case StringTok(s) if /*q.isInstanceOf[CharTok] ||*/ q.isInstanceOf[StringTok] => q match
            {
                // string(s) *> char(c) = string(sc) *> pure(c)
                case CharTok(c) =>
                    p = new StringTok(s + c).asInstanceOf[Parsley[Tok, A]]
                    q = new Pure(c).asInstanceOf[Parsley[Tok, B]]
                    optimise
                // string(s) *> string(t) = string(st) *> pure(t)
                case StringTok(t) =>
                    p = new StringTok(s + t).asInstanceOf[Parsley[Tok, A]]
                    q = new Pure(t).asInstanceOf[Parsley[Tok, B]]
                    optimise
            }
            // mzero *> p = mzero (left zero and definition of *> in terms of >>=)
            case z: MZero[Tok] => z
            case u => q match
            {
                // re-association - normal form of Then chain is to have result at the top of tree
                case v *> w =>
                    p = *>(u, v).asInstanceOf[Parsley[Tok, A]].optimise
                    q = w
                    optimise
                case _ => this
            }
        }
        override def codeGen[Cont[_, _]](implicit instrs: InstrBuffer, state: CodeGenState, ops: ContOps[Cont]): Cont[Unit, Unit] = q match
        {
            case Pure(x) => p match
            {
                case ct@CharTok(c) => ops.wrap(instrs += instructions.CharTokFastPerform[Any, B](c, _ => x, ct.expected))
                case st@StringTok(s) => ops.wrap(instrs += new instructions.StringTokFastPerform(s, _ => x, st.expected))
                case st@Satisfy(f) => ops.wrap(instrs += new instructions.SatisfyExchange(f.asInstanceOf[Any => Boolean], x, st.expected))
                case u =>
                    u.codeGen |>
                    (instrs += new instructions.Exchange(x))
            }
            case v =>
                p.codeGen >>
                {
                    instrs += instructions.Pop
                    v.codeGen
                }
        }
        override def discard: Parsley[Tok, A] = p
        override def result: Parsley[Tok, B] = q
        override def copy[B_ >: B](prev: Parsley[Tok, A], next: Parsley[Tok, B_]): *>[Tok, A, B_] = *>(prev, next)
    }
    private [parsley] final class <*[Tok, A, B](_p: =>Parsley[Tok, A], _q: =>Parsley[Tok, B]) extends Cont[Tok, B, A]
    {
        private [<*] var p: Parsley[Tok, A] = _
        private [<*] var q: Parsley[Tok, B] = _
        override def preprocess[Cont[_, _], Tok_ <: Tok, A_ >: A](implicit seen: Set[Parsley[_, _]], sub: SubMap, label: UnsafeOption[String], ops: ContOps[Cont]): Cont[Parsley[_, _], Parsley[Tok_, A_]] =
            if (label == null && p != null) ops.wrap(this) else for (p <- _p.optimised[Cont, Tok, A]; q <- _q.optimised[Cont, Tok, B]) yield
            {
                if (label == null)
                {
                    this.p = p
                    this.q = q
                    this.size = p.size + q.size + 1
                    this
                }
                else <*(p, q)
            }
        override def findLetsAux[Cont[_, _]](implicit seen: Set[Parsley[_, _]], state: LetFinderState, ops: ContOps[Cont]): Cont[Unit, Unit] =
            for (_ <- _p.findLets; _ <- _q.findLets) yield ()
        @tailrec override def optimise: Parsley[Tok, A] = q match
        {
            // p <* pure _ = p
            case _: Pure[_] => p
            // p <* (q *> pure _) = p <* q
            case v *> (_: Pure[_]) =>
                q = v.asInstanceOf[Parsley[Tok, B]]
                optimise
            /*case CharTok(d) if p.isInstanceOf[CharTok] || p.isInstanceOf[StringTok] => p match
            {
                // char(c) <* char(d) = string(cd) *> pure(c)
                case CharTok(c) => *>(new StringTok(c.toString + d), new Pure(c)).asInstanceOf[Parsley[Tok, A]]
                // string(s) <* char(d) = string(sd) *> pure(s)
                case StringTok(s) => *>(new StringTok(s + d), new Pure(s)).asInstanceOf[Parsley[Tok, A]]
            }*/
            case StringTok(t) if /*p.isInstanceOf[CharTok] || */p.isInstanceOf[StringTok]  => p match
            {
                // char(c) <* string(t) = string(ct) *> pure(c)
                case CharTok(c) => *>(new StringTok(c.toString + t), new Pure(c)).asInstanceOf[Parsley[Tok, A]]
                // string(s) <* string(t) = string(st) *> pure(s)
                case StringTok(s) => *>(new StringTok(s + t), new Pure(s)).asInstanceOf[Parsley[Tok, A]]
            }
            // p <* mzero = p *> mzero (by preservation of error messages and failure properties) - This moves the pop instruction after the failure
            case z: MZero[Tok] => *>(p, z)
            case w => p match
            {
                // re-association law 3: pure x <* p = p *> pure x
                case u: Pure[_] => *>(w, u).optimise
                // mzero <* p = mzero (left zero law and definition of <* in terms of >>=)
                case z: MZero[Tok] => z
                // re-association - normal form of Prev chain is to have result at the top of tree
                case u <* v =>
                    p = u
                    q = <*(v, w).asInstanceOf[Parsley[Tok, B]].optimise
                    optimise
                case _ => this
            }
        }
        override def codeGen[Cont[_, _]](implicit instrs: InstrBuffer, state: CodeGenState, ops: ContOps[Cont]): Cont[Unit, Unit] = (p, q) match
        {
            case (Pure(x), ct@CharTok(c)) => ops.wrap(instrs += instructions.CharTokFastPerform[Any, A](c, _ => x, ct.expected))
            case (Pure(x), st@StringTok(s)) => ops.wrap(instrs += new instructions.StringTokFastPerform(s, _ => x, st.expected))
            case (Pure(x), st@Satisfy(f)) => ops.wrap(instrs += new instructions.SatisfyExchange(f.asInstanceOf[Any => Boolean], x, st.expected))
            case (Pure(x), v) =>
                v.codeGen |>
                (instrs += new instructions.Exchange(x))
            case _ =>
                p.codeGen >>
                q.codeGen |>
                (instrs += instructions.Pop)
        }
        override def discard: Parsley[Tok, B] = q
        override def result: Parsley[Tok, A] = p
        override def copy[A_ >: A](prev: Parsley[Tok, B], next: Parsley[Tok, A_]): <*[Tok, A_, B] = <*(next, prev)
    }
    private [parsley] final class Attempt[Tok, A](_p: =>Parsley[Tok, A]) extends Parsley[Tok, A]
    {
        private [Attempt] var p: Parsley[Tok, A] = _
        override def preprocess[Cont[_, _], Tok_ <: Tok, A_ >: A](implicit seen: Set[Parsley[_, _]], sub: SubMap, label: UnsafeOption[String], ops: ContOps[Cont]): Cont[Parsley[_, _], Parsley[Tok_, A_]] =
            if (label == null && p != null) result(this) else for (p <- _p.optimised[Cont, Tok, A]) yield
            {
                if (label == null)
                {
                    this.p = p
                    this.size = p.size + 2
                    this
                }
                else Attempt(p)
            }
        override def findLetsAux[Cont[_, _]](implicit seen: Set[Parsley[_, _]], state: LetFinderState, ops: ContOps[Cont]): Cont[Unit, Unit] = _p.findLets
        override def codeGen[Cont[_, _]](implicit instrs: InstrBuffer, state: CodeGenState, ops: ContOps[Cont]): Cont[Unit, Unit] =
        {
            val handler = state.freshLabel()
            instrs += new instructions.PushHandler(handler)
            p.codeGen |>
            {
                instrs += new instructions.Label(handler)
                instrs += instructions.Attempt
            }
        }
    }
    private [parsley] final class Look[Tok, A](_p: =>Parsley[Tok, A]) extends Parsley[Tok, A]
    {
        private [Look] var p: Parsley[Tok, A] = _
        override def preprocess[Cont[_, _], Tok_ <: Tok, A_ >: A](implicit seen: Set[Parsley[_, _]], sub: SubMap, label: UnsafeOption[String], ops: ContOps[Cont]): Cont[Parsley[_, _], Parsley[Tok_, A_]] =
            if (label == null && p != null) result(this) else for (p <- _p.optimised[Cont, Tok, A]) yield
            {
                if (label == null)
                {
                    this.p = p
                    this.size = p.size + 3
                    this
                }
                else Look(p)
            }
        override def findLetsAux[Cont[_, _]](implicit seen: Set[Parsley[_, _]], state: LetFinderState, ops: ContOps[Cont]): Cont[Unit, Unit] = _p.findLets
        override def codeGen[Cont[_, _]](implicit instrs: InstrBuffer, state: CodeGenState, ops: ContOps[Cont]): Cont[Unit, Unit] =
        {
            val handler = state.freshLabel()
            instrs += new instructions.PushHandler(handler)
            p.codeGen |>
            {
                instrs += new instructions.Label(handler)
                instrs += instructions.Look
            }
        }
    }
    private [parsley] sealed trait MZero[Tok] extends Parsley[Tok, Nothing]
    private [parsley] class Empty(val expected: UnsafeOption[String] = null) extends MZero[Any]
    {
        override def preprocess[Cont[_, _], Tok_ <: Any, N >: Nothing](implicit seen: Set[Parsley[_, _]], sub: SubMap, label: UnsafeOption[String], ops: ContOps[Cont]): Cont[Parsley[_, _], Parsley[Tok_, N]] =
        {
            if (label == null) result(this)
            else result(new Empty(label))
        }
        override def findLetsAux[Cont[_, _]](implicit seen: Set[Parsley[_, _]], state: LetFinderState, ops: ContOps[Cont]): Cont[Unit, Unit] = result(())
        override def codeGen[Cont[_, _]](implicit instrs: InstrBuffer, state: CodeGenState, ops: ContOps[Cont]): Cont[Unit, Unit] =
        {
            result(instrs += new instructions.Empty(expected))
        }
    }
    private [parsley] final class Fail(private [Fail] val msg: String, val expected: UnsafeOption[String] = null) extends MZero[Any]
    {
        override def preprocess[Cont[_, _], Tok_ <: Any, N >: Nothing](implicit seen: Set[Parsley[_, _]], sub: SubMap, label: UnsafeOption[String], ops: ContOps[Cont]): Cont[Parsley[_, _], Parsley[Tok_, N]] =
        {
            if (label == null) result(this)
            else result(new Fail(msg, label))
        }
        override def findLetsAux[Cont[_, _]](implicit seen: Set[Parsley[_, _]], state: LetFinderState, ops: ContOps[Cont]): Cont[Unit, Unit] = result(())
        override def codeGen[Cont[_, _]](implicit instrs: InstrBuffer, state: CodeGenState, ops: ContOps[Cont]): Cont[Unit, Unit] =
        {
            result(instrs += new instructions.Fail(msg, expected))
        }
    }
    private [parsley] final class Unexpected(private [Unexpected] val msg: String, val expected: UnsafeOption[String] = null) extends MZero[Any]
    {
        override def preprocess[Cont[_, _], Tok_ <: Any, N >: Nothing](implicit seen: Set[Parsley[_, _]], sub: SubMap, label: UnsafeOption[String], ops: ContOps[Cont]): Cont[Parsley[_, _], Parsley[Tok_, N]] =
        {
            if (label == null) result(this)
            else result(new Unexpected(msg, label))
        }
        override def findLetsAux[Cont[_, _]](implicit seen: Set[Parsley[_, _]], state: LetFinderState, ops: ContOps[Cont]): Cont[Unit, Unit] = result(())
        override def codeGen[Cont[_, _]](implicit instrs: InstrBuffer, state: CodeGenState, ops: ContOps[Cont]): Cont[Unit, Unit] =
        {
            result(instrs += new instructions.Unexpected(msg, expected))
        }
    }
    private [parsley] final class Fixpoint[Tok, A](var _p: ()=>Parsley[Tok, A], val expected: UnsafeOption[String] = null) extends Parsley[Tok, A]
    {
        private [Fixpoint] lazy val p = _p()
        override def preprocess[Cont[_, _], Tok_ <: Tok, A_ >: A](implicit seen: Set[Parsley[_, _]], sub: SubMap, label: UnsafeOption[String], ops: ContOps[Cont]): Cont[Parsley[_, _], Parsley[Tok_, A_]] =
        {
            if (label == null) result(this)
            else result(new Fixpoint(_p, label))
        }
        override def findLetsAux[Cont[_, _]](implicit seen: Set[Parsley[_, _]], state: LetFinderState, ops: ContOps[Cont]): Cont[Unit, Unit] = result(())
        override def codeGen[Cont[_, _]](implicit instrs: InstrBuffer, state: CodeGenState, ops: ContOps[Cont]): Cont[Unit, Unit] =
        {
            result(instrs += new instructions.Call(p, expected))
        }
    }
    private [parsley] final class Subroutine[Tok, A](_p: =>Parsley[Tok, A], val expected: UnsafeOption[String] = null) extends Parsley[Tok, A]
    {
        private [Subroutine] var p: Parsley[Tok, A] = _

        override def preprocess[Cont[_, _], Tok_ <: Tok, A_ >: A](implicit seen: Set[Parsley[_, _]], sub: SubMap, label: UnsafeOption[String], ops: ContOps[Cont]): Cont[Parsley[_, _], Parsley[Tok_, A_]] =
        {
            if (p == null) for (p <- _p.optimised[Cont, Tok, A](seen, sub, null, ops)) yield
            {
                if (label == null)
                {
                    this.p = p
                    this
                }
                else Subroutine(p, label)
            }
            else if (label == null) result(this)
            else result(Subroutine(p, label))
        }
        override def findLetsAux[Cont[_, _]](implicit seen: Set[Parsley[_, _]], state: LetFinderState, ops: ContOps[Cont]): Cont[Unit, Unit] = result(())
        override def optimise = if (p.size == 1) p else this
        override def codeGen[Cont[_, _]](implicit instrs: InstrBuffer, state: CodeGenState, ops: ContOps[Cont]): Cont[Unit, Unit] =
        {
            val label = state.getSubLabel(p)
            result(instrs += new instructions.GoSub(label, expected))
        }
    }
    // Intrinsic Embedding
    private [parsley] final class CharTok[Tok](private [CharTok] val t: Tok, val expected: UnsafeOption[String] = null) extends Parsley[Tok, Tok]
    {
        override def preprocess[Cont[_, _], Tok_ <: Tok, Tok__ >: Tok](implicit seen: Set[Parsley[_, _]], sub: SubMap, label: UnsafeOption[String], ops: ContOps[Cont]): Cont[Parsley[_, _], Parsley[Tok_, Tok__]] =
        {
            if (label == null) result(this)
            else result(new CharTok(t, label))
        }
        override def findLetsAux[Cont[_, _]](implicit seen: Set[Parsley[_, _]], state: LetFinderState, ops: ContOps[Cont]): Cont[Unit, Unit] = result(())
        override def codeGen[Cont[_, _]](implicit instrs: InstrBuffer, state: CodeGenState, ops: ContOps[Cont]): Cont[Unit, Unit] =
        {
            result(instrs += instructions.Token(t, expected))
        }
    }
    private [parsley] final class StringTok(private [StringTok] val s: String, val expected: UnsafeOption[String] = null) extends Parsley[Char, String]
    {
        override def preprocess[Cont[_, _], Tok_ <: Char, S >: String](implicit seen: Set[Parsley[_, _]], sub: SubMap, label: UnsafeOption[String], ops: ContOps[Cont]): Cont[Parsley[_, _], Parsley[Tok_, S]] =
        {
            if (label == null) result(this)
            else result(new StringTok(s, label))
        }
        override def findLetsAux[Cont[_, _]](implicit seen: Set[Parsley[_, _]], state: LetFinderState, ops: ContOps[Cont]): Cont[Unit, Unit] = result(())
        override def optimise = s match
        {
            case "" => new Pure("")
            case _ => this
        }
        override def codeGen[Cont[_, _]](implicit instrs: InstrBuffer, state: CodeGenState, ops: ContOps[Cont]): Cont[Unit, Unit] =
        {
            result(instrs += new instructions.StringTok(s, expected))
        }
    }
    // TODO: Perform applicative fusion optimisations
    private [parsley] final class Lift2[Tok, A, B, +C](private [Lift2] val f: (A, B) => C, _p: =>Parsley[Tok, A], _q: =>Parsley[Tok, B]) extends Parsley[Tok, C]
    {
        private [Lift2] var p: Parsley[Tok, A] = _
        private [Lift2] var q: Parsley[Tok, B] = _
        override def preprocess[Cont[_, _], Tok_ <: Tok, C_ >: C](implicit seen: Set[Parsley[_, _]], sub: SubMap, label: UnsafeOption[String], ops: ContOps[Cont]): Cont[Parsley[_, _], Parsley[Tok_, C_]] =
            if (label == null && p != null) result(this) else for (p <- _p.optimised[Cont, Tok, A]; q <- _q.optimised[Cont, Tok, B]) yield
            {
                if (label == null)
                {
                    this.p = p
                    this.q = q
                    this.size = p.size + q.size + 1
                    this
                }
                else Lift2(f, p, q)
            }
        override def findLetsAux[Cont[_, _]](implicit seen: Set[Parsley[_, _]], state: LetFinderState, ops: ContOps[Cont]): Cont[Unit, Unit] =
            for (_ <- _p.findLets; _ <- _q.findLets) yield ()
        override def codeGen[Cont[_, _]](implicit instrs: InstrBuffer, state: CodeGenState, ops: ContOps[Cont]): Cont[Unit, Unit] =
        {
            p.codeGen >>
            q.codeGen |>
            (instrs += new instructions.Lift2(f))
        }
    }
    private [parsley] final class Lift3[Tok, A, B, C, +D](private [Lift3] val f: (A, B, C) => D, _p: =>Parsley[Tok, A], _q: =>Parsley[Tok, B], _r: =>Parsley[Tok, C]) extends Parsley[Tok, D]
    {
        private [Lift3] var p: Parsley[Tok, A] = _
        private [Lift3] var q: Parsley[Tok, B] = _
        private [Lift3] var r: Parsley[Tok, C] = _
        override def preprocess[Cont[_, _], Tok_ <: Tok, D_ >: D](implicit seen: Set[Parsley[_, _]], sub: SubMap, label: UnsafeOption[String], ops: ContOps[Cont]): Cont[Parsley[_, _], Parsley[Tok_, D_]] =
            if (label == null && p != null) result(this) else for (p <- _p.optimised[Cont, Tok, A]; q <- _q.optimised[Cont, Tok, B]; r <- _r.optimised[Cont, Tok, C]) yield
            {
                if (label == null)
                {
                    this.p = p
                    this.q = q
                    this.r = r
                    this.size = p.size + q.size + r.size + 1
                    this
                }
                else Lift3(f, p, q, r)
            }
        override def findLetsAux[Cont[_, _]](implicit seen: Set[Parsley[_, _]], state: LetFinderState, ops: ContOps[Cont]): Cont[Unit, Unit] =
            for (_ <- _p.findLets; _ <- _q.findLets; _ <- _r.findLets) yield ()
        override def codeGen[Cont[_, _]](implicit instrs: InstrBuffer, state: CodeGenState, ops: ContOps[Cont]): Cont[Unit, Unit] =
        {
            p.codeGen >>
            q.codeGen >>
            r.codeGen |>
            (instrs += new instructions.Lift3(f))
        }
    }
    private [parsley] final class FastFail[Tok, A](_p: =>Parsley[Tok, A], private [FastFail] val msggen: A => String, val expected: UnsafeOption[String] = null) extends MZero[Tok]
    {
        private [FastFail] var p: Parsley[Tok, A] = _
        override def preprocess[Cont[_, _], Tok_ <: Tok, N >: Nothing](implicit seen: Set[Parsley[_, _]], sub: SubMap, label: UnsafeOption[String], ops: ContOps[Cont]): Cont[Parsley[_, _], Parsley[Tok_, N]] =
            if (label == null && p != null) result(this) else for (p <- _p.optimised[Cont, Tok, A]) yield
            {
                if (label == null)
                {
                    this.p = p
                    this.size = p.size + 1
                    this
                }
                else FastFail(p, msggen, label)
            }
        override def findLetsAux[Cont[_, _]](implicit seen: Set[Parsley[_, _]], state: LetFinderState, ops: ContOps[Cont]): Cont[Unit, Unit] = _p.findLets
        override def optimise = p match
        {
            case Pure(x) => new Fail(msggen(x))
            case z: MZero[Tok] => z
            case _ => this
        }
        override def codeGen[Cont[_, _]](implicit instrs: InstrBuffer, state: CodeGenState, ops: ContOps[Cont]): Cont[Unit, Unit] =
        {
            p.codeGen |>
            (instrs += new instructions.FastFail(msggen, expected))
        }
    }
    private [parsley] final class FastUnexpected[Tok, A](_p: =>Parsley[Tok, A], private [FastUnexpected] val msggen: A => String, val expected: UnsafeOption[String] = null) extends MZero[Tok]
    {
        private [FastUnexpected] var p: Parsley[Tok, A] = _
        override def preprocess[Cont[_, _], Tok_ <: Tok, N >: Nothing](implicit seen: Set[Parsley[_, _]], sub: SubMap, label: UnsafeOption[String], ops: ContOps[Cont]): Cont[Parsley[_, _], Parsley[Tok_, N]] =
            if (label == null && p != null) result(this) else for (p <- _p.optimised[Cont, Tok, A]) yield
            {
                if (label == null)
                {
                    this.p = p
                    this.size = p.size + 1
                    this
                }
                else FastUnexpected(p, msggen, label)
            }
        override def findLetsAux[Cont[_, _]](implicit seen: Set[Parsley[_, _]], state: LetFinderState, ops: ContOps[Cont]): Cont[Unit, Unit] = _p.findLets
        override def optimise = p match
        {
            case Pure(x) => new Unexpected(msggen(x))
            case z: MZero[Tok] => z
            case _ => this
        }
        override def codeGen[Cont[_, _]](implicit instrs: InstrBuffer, state: CodeGenState, ops: ContOps[Cont]): Cont[Unit, Unit] =
        {
            p.codeGen |>
            (instrs += new instructions.FastUnexpected(msggen, expected))
        }
    }
    private [parsley] final class Ensure[Tok, A](_p: =>Parsley[Tok, A], private [Ensure] val pred: A => Boolean, val expected: UnsafeOption[String] = null) extends Parsley[Tok, A]
    {
        private [Ensure] var p: Parsley[Tok, A] = _
        override def preprocess[Cont[_, _], Tok_ <: Tok, A_ >: A](implicit seen: Set[Parsley[_, _]], sub: SubMap, label: UnsafeOption[String], ops: ContOps[Cont]): Cont[Parsley[_, _], Parsley[Tok_, A_]] =
            if (label == null && p != null) result(this) else for (p <- _p.optimised[Cont, Tok, A]) yield
            {
                if (label == null)
                {
                    this.p = p
                    this.size = p.size + 1
                    this
                }
                else Ensure(p, pred, label)
            }
        override def findLetsAux[Cont[_, _]](implicit seen: Set[Parsley[_, _]], state: LetFinderState, ops: ContOps[Cont]): Cont[Unit, Unit] = _p.findLets
        override def optimise = p match
        {
            case px@Pure(x) => if (pred(x)) px else new Empty
            case _ => this
        }
        override def codeGen[Cont[_, _]](implicit instrs: InstrBuffer, state: CodeGenState, ops: ContOps[Cont]): Cont[Unit, Unit] =
        {
            p.codeGen |>
            (instrs += new instructions.Ensure(pred, expected))
        }
    }
    private [parsley] final class Guard[Tok, A](_p: =>Parsley[Tok, A], private [Guard] val pred: A => Boolean, private [Guard] val msg: String, val expected: UnsafeOption[String] = null) extends Parsley[Tok, A]
    {
        private [Guard] var p: Parsley[Tok, A] = _
        override def preprocess[Cont[_, _], Tok_ <: Tok, A_ >: A](implicit seen: Set[Parsley[_, _]], sub: SubMap, label: UnsafeOption[String], ops: ContOps[Cont]): Cont[Parsley[_, _], Parsley[Tok_, A_]] =
            if (label == null && p != null) result(this) else for (p <- _p.optimised[Cont, Tok, A]) yield
            {
                if (label == null)
                {
                    this.p = p
                    this.size = p.size + 1
                    this
                }
                else Guard(p, pred, msg, label)
            }
        override def findLetsAux[Cont[_, _]](implicit seen: Set[Parsley[_, _]], state: LetFinderState, ops: ContOps[Cont]): Cont[Unit, Unit] = _p.findLets
        override def optimise = p match
        {
            case px@Pure(x) => if (pred(x)) px else new Fail(msg)
            case _ => this
        }
        override def codeGen[Cont[_, _]](implicit instrs: InstrBuffer, state: CodeGenState, ops: ContOps[Cont]): Cont[Unit, Unit] =
        {
            p.codeGen |>
            (instrs += new instructions.Guard(pred, msg, expected))
        }
    }
    private [parsley] final class FastGuard[Tok, A](_p: =>Parsley[Tok, A], private [FastGuard] val pred: A => Boolean, private [FastGuard] val msggen: A => String, val expected: UnsafeOption[String] = null) extends Parsley[Tok, A]
    {
        private [FastGuard] var p: Parsley[Tok, A] = _
        override def preprocess[Cont[_, _], Tok_ <: Tok, A_ >: A](implicit seen: Set[Parsley[_, _]], sub: SubMap, label: UnsafeOption[String], ops: ContOps[Cont]): Cont[Parsley[_, _], Parsley[Tok_, A_]] =
            if (label == null && p != null) result(this) else for (p <- _p.optimised[Cont, Tok, A]) yield
            {
                if (label == null)
                {
                    this.p = p
                    this.size = p.size + 1
                    this
                }
                else FastGuard(p, pred, msggen, label)
            }
        override def findLetsAux[Cont[_, _]](implicit seen: Set[Parsley[_, _]], state: LetFinderState, ops: ContOps[Cont]): Cont[Unit, Unit] = _p.findLets
        override def optimise = p match
        {
            case px@Pure(x) => if (pred(x)) px else new Fail(msggen(x))
            case _ => this
        }
        override def codeGen[Cont[_, _]](implicit instrs: InstrBuffer, state: CodeGenState, ops: ContOps[Cont]): Cont[Unit, Unit] =
        {
            p.codeGen |>
            (instrs += new instructions.FastGuard(pred, msggen, expected))
        }
    }
    private [parsley] final class Many[Tok, A](_p: =>Parsley[Tok, A]) extends Parsley[Tok, List[A]]
    {
        private [Many] var p: Parsley[Tok, A] = _
        override def preprocess[Cont[_, _], Tok_ <: Tok, L >: List[A]](implicit seen: Set[Parsley[_, _]], sub: SubMap, label: UnsafeOption[String], ops: ContOps[Cont]): Cont[Parsley[_, _], Parsley[Tok_, L]] =
            if (label == null && p != null) result(this) else for (p <- _p.optimised[Cont, Tok, A]) yield
            {
                if (label == null)
                {
                    this.p = p
                    this.size = p.size + 2
                    this
                }
                else Many(p)
            }
        override def findLetsAux[Cont[_, _]](implicit seen: Set[Parsley[_, _]], state: LetFinderState, ops: ContOps[Cont]): Cont[Unit, Unit] = _p.findLets
        override def optimise = p match
        {
            case _: Pure[A] => throw new Exception("many given parser which consumes no input")
            case _: MZero[Tok] => new Pure(Nil)
            case _ => this
        }
        override def codeGen[Cont[_, _]](implicit instrs: InstrBuffer, state: CodeGenState, ops: ContOps[Cont]): Cont[Unit, Unit] =
        {
            val body = state.freshLabel()
            val handler = state.freshLabel()
            instrs += new instructions.InputCheck(handler)
            instrs += new instructions.Label(body)
            p.codeGen |>
            {
                instrs += new instructions.Label(handler)
                instrs += new instructions.Many(body)
            }
        }
    }
    private [parsley] final class SkipMany[Tok, A](_p: =>Parsley[Tok, A]) extends Parsley[Tok, Nothing]
    {
        private [SkipMany] var p: Parsley[Tok, A] = _
        override def preprocess[Cont[_, _], Tok_ <: Tok, N >: Nothing](implicit seen: Set[Parsley[_, _]], sub: SubMap, label: UnsafeOption[String], ops: ContOps[Cont]): Cont[Parsley[_, _], Parsley[Tok_, N]] =
            if (label == null && p != null) result(this) else for (p <- _p.optimised[Cont, Tok, A]) yield
            {
                if (label == null)
                {
                    this.p = p
                    this.size = p.size + 2
                    this
                }
                else SkipMany(p)
            }
        override def findLetsAux[Cont[_, _]](implicit seen: Set[Parsley[_, _]], state: LetFinderState, ops: ContOps[Cont]): Cont[Unit, Unit] = _p.findLets
        override def optimise = p match
        {
            case _: Pure[A] => throw new Exception("skipMany given parser which consumes no input")
            case _: MZero[Tok] => new Pure(()).asInstanceOf[Parsley[Tok, Nothing]]
            case _ => this
        }
        override def codeGen[Cont[_, _]](implicit instrs: InstrBuffer, state: CodeGenState, ops: ContOps[Cont]): Cont[Unit, Unit] =
        {
            val body = state.freshLabel()
            val handler = state.freshLabel()
            instrs += new instructions.InputCheck(handler)
            instrs += new instructions.Label(body)
            p.codeGen |>
            {
                instrs += new instructions.Label(handler)
                instrs += new instructions.SkipMany(body)
            }
        }
    }
    private [parsley] final class ChainPost[Tok, A](_p: =>Parsley[Tok, A], _op: =>Parsley[Tok, A => A]) extends Parsley[Tok, A]
    {
        private [ChainPost] var p: Parsley[Tok, A] = _
        private [ChainPost] var op: Parsley[Tok, A => A] = _
        override def preprocess[Cont[_, _], Tok_ <: Tok, A_ >: A](implicit seen: Set[Parsley[_, _]], sub: SubMap, label: UnsafeOption[String], ops: ContOps[Cont]): Cont[Parsley[_, _], Parsley[Tok_, A_]] =
            if (label == null && p != null) result(this) else for (p <- _p.optimised[Cont, Tok, A]; op <- _op.optimised[Cont, Tok, A => A]) yield
            {
                if (label == null)
                {
                    this.p = p
                    this.op = op
                    this.size = p.size + op.size + 2
                    this
                }
                else ChainPost(p, op)
            }
        override def findLetsAux[Cont[_, _]](implicit seen: Set[Parsley[_, _]], state: LetFinderState, ops: ContOps[Cont]): Cont[Unit, Unit] =
            for (_ <- _p.findLets; _ <- _op.findLets) yield ()
        override def optimise = op match
        {
            case _: Pure[A => A] => throw new Exception("left chain given parser which consumes no input")
            case _: MZero[Tok] => p
            case _ => this
        }
        override def codeGen[Cont[_, _]](implicit instrs: InstrBuffer, state: CodeGenState, ops: ContOps[Cont]): Cont[Unit, Unit] =
        {
            val body = state.freshLabel()
            val handler = state.freshLabel()
            p.codeGen >>
            {
                instrs += new instructions.InputCheck(handler)
                instrs += new instructions.Label(body)
                op.codeGen |>
                {
                    instrs += new instructions.Label(handler)
                    instrs += new instructions.ChainPost(body)
                }
            }
        }
    }
    private [parsley] final class ChainPre[Tok, A](_p: =>Parsley[Tok, A], _op: =>Parsley[Tok, A => A]) extends Parsley[Tok, A]
    {
        private [ChainPre] var p: Parsley[Tok, A] = _
        private [ChainPre] var op: Parsley[Tok, A => A] = _
        override def preprocess[Cont[_, _], Tok_ <: Tok, A_ >: A](implicit seen: Set[Parsley[_, _]], sub: SubMap, label: UnsafeOption[String], ops: ContOps[Cont]): Cont[Parsley[_, _], Parsley[Tok_, A_]] =
            if (label == null && p != null) result(this) else for (p <- _p.optimised[Cont, Tok, A]; op <- _op.optimised[Cont, Tok, A => A]) yield
            {
                if (label == null)
                {
                    this.p = p
                    this.op = op
                    this.size = p.size + op.size + 2
                    this
                }
                else ChainPre(p, op)
            }
        override def findLetsAux[Cont[_, _]](implicit seen: Set[Parsley[_, _]], state: LetFinderState, ops: ContOps[Cont]): Cont[Unit, Unit] =
            for (_ <- _p.findLets; _ <- _op.findLets) yield ()
        override def optimise = op match
        {
            case _: Pure[A => A] => throw new Exception("right chain given parser which consumes no input")
            case _: MZero[Tok] => p
            case _ => this
        }
        override def codeGen[Cont[_, _]](implicit instrs: InstrBuffer, state: CodeGenState, ops: ContOps[Cont]): Cont[Unit, Unit] =
        {
            val body = state.freshLabel()
            val handler = state.freshLabel()
            instrs += new instructions.InputCheck(handler)
            instrs += new instructions.Label(body)
            op.codeGen >>
            {
                instrs += new instructions.Label(handler)
                instrs += new instructions.ChainPre(body)
                p.codeGen |>
                (instrs += instructions.Apply)
            }
        }
    }
    private [parsley] final class Chainl[Tok, A](_p: =>Parsley[Tok, A], _op: =>Parsley[Tok, (A, A) => A]) extends Parsley[Tok, A]
    {
        private [Chainl] var p: Parsley[Tok, A] = _
        private [Chainl] var op: Parsley[Tok, (A, A) => A] = _
        override def preprocess[Cont[_, _], Tok_ <: Tok, A_ >: A](implicit seen: Set[Parsley[_, _]], sub: SubMap, label: UnsafeOption[String], ops: ContOps[Cont]): Cont[Parsley[_, _], Parsley[Tok_, A_]] =
            if (label == null && p != null) result(this) else for (p <- _p.optimised[Cont, Tok, A]; op <- _op.optimised[Cont, Tok, (A, A) => A]) yield
            {
                if (label == null)
                {
                    this.p = p
                    this.op = op
                    this.size = p.size*2 + op.size + 2
                    this
                }
                else Chainl(p, op)
            }
        override def findLetsAux[Cont[_, _]](implicit seen: Set[Parsley[_, _]], state: LetFinderState, ops: ContOps[Cont]): Cont[Unit, Unit] =
            for (_ <- _p.findLets; _ <- _op.findLets) yield ()
        override def codeGen[Cont[_, _]](implicit instrs: InstrBuffer, state: CodeGenState, ops: ContOps[Cont]): Cont[Unit, Unit] =
        {
            val body = state.freshLabel()
            val handler = state.freshLabel()
            p.codeGen >>
            {
                instrs += new instructions.InputCheck(handler)
                instrs += new instructions.Label(body)
                op.codeGen >>
                p.codeGen |>
                {
                    instrs += new instructions.Label(handler)
                    instrs += new instructions.Chainl(body)
                }
            }
        }
    }
    private [parsley] final class Chainr[Tok, A](_p: =>Parsley[Tok, A], _op: =>Parsley[Tok, (A, A) => A]) extends Parsley[Tok, A]
    {
        private [Chainr] var p: Parsley[Tok, A] = _
        private [Chainr] var op: Parsley[Tok, (A, A) => A] = _
        override def preprocess[Cont[_, _], Tok_ <: Tok, A_ >: A](implicit seen: Set[Parsley[_, _]], sub: SubMap, label: UnsafeOption[String], ops: ContOps[Cont]): Cont[Parsley[_, _], Parsley[Tok_, A_]] =
            if (label == null && p != null) result(this) else for (p <- _p.optimised[Cont, Tok, A]; op <- _op.optimised[Cont, Tok, (A, A) => A]) yield
            {
                if (label == null)
                {
                    this.p = p
                    this.op = op
                    this.size = p.size + op.size + 3
                    this
                }
                else Chainr(p, op)
            }
        override def findLetsAux[Cont[_, _]](implicit seen: Set[Parsley[_, _]], state: LetFinderState, ops: ContOps[Cont]): Cont[Unit, Unit] =
            for (_ <- _p.findLets; _ <- _op.findLets) yield ()
        override def codeGen[Cont[_, _]](implicit instrs: InstrBuffer, state: CodeGenState, ops: ContOps[Cont]): Cont[Unit, Unit]=
        {
            val body = state.freshLabel()
            val handler = state.freshLabel()
            instrs += new instructions.InputCheck(handler)
            instrs += new instructions.Label(body)
            p.codeGen >>
            {
                instrs += new instructions.InputCheck(handler)
                op.codeGen |>
                {
                    instrs += new instructions.Label(handler)
                    instrs += new instructions.Chainr(body)
                }
            }
        }
    }
    private [parsley] final class SepEndBy1[Tok, A, B](_p: =>Parsley[Tok, A], _sep: =>Parsley[Tok, B]) extends Parsley[Tok, List[A]]
    {
        private [SepEndBy1] var p: Parsley[Tok, A] = _
        private [SepEndBy1] var sep: Parsley[Tok, B] = _
        override def preprocess[Cont[_, _], Tok_ <: Tok, L >: List[A]](implicit seen: Set[Parsley[_, _]], sub: SubMap, label: UnsafeOption[String], ops: ContOps[Cont]): Cont[Parsley[_, _], Parsley[Tok_, L]] =
            if (label == null && p != null) result(this) else for (p <- _p.optimised[Cont, Tok, A]; sep <- _sep.optimised[Cont, Tok, B]) yield
            {
                if (label == null)
                {
                    this.p = p
                    this.sep = sep
                    this.size = p.size + sep.size + 3
                    this
                }
                else SepEndBy1(p, sep)
            }
        override def findLetsAux[Cont[_, _]](implicit seen: Set[Parsley[_, _]], state: LetFinderState, ops: ContOps[Cont]): Cont[Unit, Unit] =
            for (_ <- _p.findLets; _ <- _sep.findLets) yield ()
        override def codeGen[Cont[_, _]](implicit instrs: InstrBuffer, state: CodeGenState, ops: ContOps[Cont]): Cont[Unit, Unit] =
        {
            val body = state.freshLabel()
            val handler = state.freshLabel()
            instrs += new instructions.InputCheck(handler)
            instrs += new instructions.Label(body)
            p.codeGen >>
            {
                instrs += new instructions.InputCheck(handler)
                sep.codeGen |>
                {
                    instrs += new instructions.Label(handler)
                    instrs += new instructions.SepEndBy1(body)
                }
            }
        }
    }
    private [parsley] final class ManyUntil[Tok, A](_body: Parsley[Tok, Any]) extends Parsley[Tok, List[A]]
    {
        private [ManyUntil] var body: Parsley[Tok, Any] = _
        override def preprocess[Cont[_, _], Tok_ <: Tok, L >: List[A]](implicit seen: Set[Parsley[_, _]], sub: SubMap, label: UnsafeOption[String], ops: ContOps[Cont]): Cont[Parsley[_, _], Parsley[Tok_, L]] =
            if (label == null && body != null) result(this) else for (body <- _body.optimised[Cont, Tok, Any]) yield
            {
                if (label == null)
                {
                    this.body = body
                    this.size = body.size + 2
                    this
                }
                else ManyUntil(body)
            }
        override def findLetsAux[Cont[_, _]](implicit seen: Set[Parsley[_, _]], state: LetFinderState, ops: ContOps[Cont]): Cont[Unit, Unit] = _body.findLets
        override def codeGen[Cont[_, _]](implicit instrs: InstrBuffer, state: CodeGenState, ops: ContOps[Cont]): Cont[Unit, Unit] =
        {
            val start = state.freshLabel()
            val loop = state.freshLabel()
            instrs += new instructions.PushFallthrough(loop)
            instrs += new instructions.Label(start)
            body.codeGen |>
            {
                instrs += new instructions.Label(loop)
                instrs += new instructions.ManyUntil(start)
            }
        }
    }
    private [parsley] final class Ternary[Tok, A](_b: =>Parsley[Tok, Boolean], _p: =>Parsley[Tok, A], _q: =>Parsley[Tok, A]) extends Parsley[Tok, A]
    {
        private [Ternary] var b: Parsley[Tok, Boolean] = _
        private [Ternary] var p: Parsley[Tok, A] = _
        private [Ternary] var q: Parsley[Tok, A] = _
        override def preprocess[Cont[_, _], Tok_ <: Tok, A_ >: A](implicit seen: Set[Parsley[_, _]], sub: SubMap, label: UnsafeOption[String], ops: ContOps[Cont]): Cont[Parsley[_, _], Parsley[Tok_, A_]] =
            if (label == null && b != null) result(this) else for (b <- _b.optimised[Cont, Tok, Boolean]; p <- _p.optimised[Cont, Tok, A]; q <- _q.optimised[Cont, Tok, A]) yield
            {
                if (label == null)
                {
                    this.b = b
                    this.p = p
                    this.q = q
                    this.size = b.size + p.size + q.size + 2
                    this
                }
                else Ternary(b, p, q)
            }
        override def findLetsAux[Cont[_, _]](implicit seen: Set[Parsley[_, _]], state: LetFinderState, ops: ContOps[Cont]): Cont[Unit, Unit] =
            for (_ <- _b.findLets; _ <- _p.findLets; _ <- _q.findLets) yield ()
        override def optimise = b match
        {
            case Pure(true) => p
            case Pure(false) => q
            case _ => this
        }
        override def codeGen[Cont[_, _]](implicit instrs: InstrBuffer, state: CodeGenState, ops: ContOps[Cont]): Cont[Unit, Unit] =
        {
            val success = state.freshLabel()
            val end = state.freshLabel()
            b.codeGen >>
            {
                instrs += new instructions.If(success)
                q.codeGen >>
                {
                    instrs += new instructions.Jump(end)
                    instrs += new instructions.Label(success)
                    p.codeGen |>
                    (instrs += new instructions.Label(end))
                }
            }
        }
    }
    private [parsley] final class NotFollowedBy[Tok, A](_p: =>Parsley[Tok, A], val expected: UnsafeOption[String] = null) extends Parsley[Tok, Nothing]
    {
        private [NotFollowedBy] var p: Parsley[Tok, A] = _
        override def preprocess[Cont[_, _], Tok_ <: Tok, N >: Nothing](implicit seen: Set[Parsley[_, _]], sub: SubMap, label: UnsafeOption[String], ops: ContOps[Cont]): Cont[Parsley[_, _], Parsley[Tok_, N]] =
            if (label == null && p != null) result(this) else for (p <- _p.optimised[Cont, Tok, A]) yield
            {
                if (label == null)
                {
                    this.p = p
                    this.size = p.size + 2
                    this
                }
                else NotFollowedBy(p, label)
            }
        override def findLetsAux[Cont[_, _]](implicit seen: Set[Parsley[_, _]], state: LetFinderState, ops: ContOps[Cont]): Cont[Unit, Unit] = _p.findLets
        override def codeGen[Cont[_, _]](implicit instrs: InstrBuffer, state: CodeGenState, ops: ContOps[Cont]): Cont[Unit, Unit] =
        {
            val handler = state.freshLabel()
            instrs += new instructions.PushHandler(handler)
            p.codeGen |>
            {
                instrs += new instructions.Label(handler)
                instrs += new instructions.NotFollowedBy(expected)
            }
        }
    }
    private [parsley] final class Eof(val expected: UnsafeOption[String] = null) extends Parsley[Any, Nothing]
    {
        override def preprocess[Cont[_, _], Tok_ <: Any, N >: Nothing](implicit seen: Set[Parsley[_, _]], sub: SubMap, label: UnsafeOption[String], ops: ContOps[Cont]): Cont[Parsley[_, _], Parsley[Tok_, N]] =
        {
            if (label == null) result(this)
            else result(new Eof(label))
        }
        override def findLetsAux[Cont[_, _]](implicit seen: Set[Parsley[_, _]], state: LetFinderState, ops: ContOps[Cont]): Cont[Unit, Unit] = result(())
        override def codeGen[Cont[_, _]](implicit instrs: InstrBuffer, state: CodeGenState, ops: ContOps[Cont]): Cont[Unit, Unit] =
        {
            result(instrs += new instructions.Eof(expected))
        }
    }
    private [parsley] object Line extends Parsley[Any, Int]
    {
        override def preprocess[Cont[_, _], Tok_ <: Any, I >: Int](implicit seen: Set[Parsley[_, _]], sub: SubMap, label: UnsafeOption[String], ops: ContOps[Cont]): Cont[Parsley[_, _], Parsley[Tok_, I]] = result(this)
        override def findLetsAux[Cont[_, _]](implicit seen: Set[Parsley[_, _]], state: LetFinderState, ops: ContOps[Cont]): Cont[Unit, Unit] = result(())
        override def codeGen[Cont[_, _]](implicit instrs: InstrBuffer, state: CodeGenState, ops: ContOps[Cont]): Cont[Unit, Unit] =
        {
            result(instrs += instructions.Line)
        }
    }
    private [parsley] object Col extends Parsley[Any, Int]
    {
        override def preprocess[Cont[_, _], Tok_ <: Any, I >: Int](implicit seen: Set[Parsley[_, _]], sub: SubMap, label: UnsafeOption[String], ops: ContOps[Cont]): Cont[Parsley[_, _], Parsley[Tok_, I]] = result(this)
        override def findLetsAux[Cont[_, _]](implicit seen: Set[Parsley[_, _]], state: LetFinderState, ops: ContOps[Cont]): Cont[Unit, Unit] = result(())
        override def codeGen[Cont[_, _]](implicit instrs: InstrBuffer, state: CodeGenState, ops: ContOps[Cont]): Cont[Unit, Unit] =
        {
            result(instrs += instructions.Col)
        }
    }
    private [parsley] final class Get[S](v: Var) extends Parsley[Any, S]
    {
        override def preprocess[Cont[_, _], Tok_ <: Any, S_ >: S](implicit seen: Set[Parsley[_, _]], sub: SubMap, label: UnsafeOption[String], ops: ContOps[Cont]): Cont[Parsley[_, _], Parsley[Tok_, S_]] = result(this)
        override def findLetsAux[Cont[_, _]](implicit seen: Set[Parsley[_, _]], state: LetFinderState, ops: ContOps[Cont]): Cont[Unit, Unit] = result(())
        override def codeGen[Cont[_, _]](implicit instrs: InstrBuffer, state: CodeGenState, ops: ContOps[Cont]): Cont[Unit, Unit] =
        {
            result(instrs += new instructions.Get(v.v))
        }
    }
    private [parsley] final class Put[Tok, S](private [Put] val v: Var, _p: =>Parsley[Tok, S]) extends Parsley[Tok, Unit]
    {
        private [Put] var p: Parsley[Tok, S] = _
        override def preprocess[Cont[_, _], Tok_ <: Tok, U >: Unit](implicit seen: Set[Parsley[_, _]], sub: SubMap, label: UnsafeOption[String], ops: ContOps[Cont]): Cont[Parsley[_, _], Parsley[Tok_, U]] =
            if (label == null && p != null) result(this) else for (p <- _p.optimised[Cont, Tok, S]) yield
            {
                if (label == null)
                {
                    this.p = p
                    this.size = p.size + 1
                    this
                }
                else Put(v, p)
            }
        override def findLetsAux[Cont[_, _]](implicit seen: Set[Parsley[_, _]], state: LetFinderState, ops: ContOps[Cont]): Cont[Unit, Unit] = _p.findLets
        override def codeGen[Cont[_, _]](implicit instrs: InstrBuffer, state: CodeGenState, ops: ContOps[Cont]): Cont[Unit, Unit] =
        {
            p.codeGen |>
            (instrs += new instructions.Put(v.v))
        }
    }
    private [parsley] final class Modify[S](v: Var, f: S => S) extends Parsley[Any, S]
    {
        override def preprocess[Cont[_, _], Tok_ <: Any, S_ >: S](implicit seen: Set[Parsley[_, _]], sub: SubMap, label: UnsafeOption[String], ops: ContOps[Cont]): Cont[Parsley[_, _], Parsley[Tok_, S_]] = result(this)
        override def findLetsAux[Cont[_, _]](implicit seen: Set[Parsley[_, _]], state: LetFinderState, ops: ContOps[Cont]): Cont[Unit, Unit] = result(())
        override def codeGen[Cont[_, _]](implicit instrs: InstrBuffer, state: CodeGenState, ops: ContOps[Cont]): Cont[Unit, Unit] =
        {
            result(instrs += new instructions.Modify(v.v, f))
        }
    }
    private [parsley] final class Local[Tok, S, A](private [Local] val v: Var, _p: =>Parsley[Tok, S], _q: =>Parsley[Tok, A]) extends Parsley[Tok, A]
    {
        private [Local] var p: Parsley[Tok, S] = _
        private [Local] var q: Parsley[Tok, A] = _
        override def preprocess[Cont[_, _], Tok_ <: Tok, A_ >: A](implicit seen: Set[Parsley[_, _]], sub: SubMap, label: UnsafeOption[String], ops: ContOps[Cont]): Cont[Parsley[_, _], Parsley[Tok_, A_]] =
            if (label == null && p != null) result(this) else for (p <- _p.optimised[Cont, Tok, S]; q <- _q.optimised[Cont, Tok, A]) yield
            {
                if (label == null)
                {
                    this.p = p
                    this.q = q
                    this.size = p.size + q.size + 2
                    this
                }
                else Local(v, p, q)
            }
        override def findLetsAux[Cont[_, _]](implicit seen: Set[Parsley[_, _]], state: LetFinderState, ops: ContOps[Cont]): Cont[Unit, Unit] =
            for (_ <- _p.findLets; _ <- _q.findLets) yield ()
        override def codeGen[Cont[_, _]](implicit instrs: InstrBuffer, state: CodeGenState, ops: ContOps[Cont]): Cont[Unit, Unit] =
        {
            p.codeGen >>
            {
                instrs += new instructions.LocalEntry(v.v)
                q.codeGen |>
                (instrs += new instructions.LocalExit(v.v))
            }
        }
    }
    private [parsley] final class ErrorRelabel[Tok, +A](p: =>Parsley[Tok, A], msg: String) extends Parsley[Tok, A]
    {
        override def preprocess[Cont[_, _], Tok_ <: Tok, A_ >: A](implicit seen: Set[Parsley[_, _]], sub: SubMap, label: UnsafeOption[String], ops: ContOps[Cont]): Cont[Parsley[_, _], Parsley[Tok_, A_]] =
        {
            if (label == null) p.optimised(seen, sub, msg, ops)
            else p.optimised
        }
        override def findLetsAux[Cont[_, _]](implicit seen: Set[Parsley[_, _]], state: LetFinderState, ops: ContOps[Cont]): Cont[Unit, Unit] = p.findLets
        override def optimise = throw new Exception("Error relabelling should not be in optimisation!")
        override def codeGen[Cont[_, _]](implicit instrs: InstrBuffer, state: CodeGenState, ops: ContOps[Cont]): Cont[Unit, Unit] = throw new Exception("Error relabelling should not be in code gen!")
    }
    private [parsley] final class Debug[Tok, A](_p: =>Parsley[Tok, A], name: String, break: Breakpoint) extends Parsley[Tok, A]
    {
        private [Debug] var p: Parsley[Tok, A] = _
        override def preprocess[Cont[_, _], Tok_ <: Tok, A_ >: A](implicit seen: Set[Parsley[_, _]], sub: SubMap, label: UnsafeOption[String], ops: ContOps[Cont]): Cont[Parsley[_, _], Parsley[Tok_, A_]] =
            if (label == null && p != null) result(this) else for (p <- _p.optimised[Cont, Tok, A]) yield
            {
                if (label == null)
                {
                    this.p = p
                    this.size = p.size + 2
                    this
                }
                else Debug(p, name, break)
            }
        override def findLetsAux[Cont[_, _]](implicit seen: Set[Parsley[_, _]], state: LetFinderState, ops: ContOps[Cont]): Cont[Unit, Unit] = _p.findLets
        override def codeGen[Cont[_, _]](implicit instrs: InstrBuffer, state: CodeGenState, ops: ContOps[Cont]): Cont[Unit, Unit] =
        {
            val handler = state.freshLabel()
            instrs += new instructions.LogBegin(handler, name, (break eq EntryBreak) || (break eq FullBreak))
            p.codeGen |>
            {
                instrs += new instructions.Label(handler)
                instrs += new instructions.LogEnd(name, (break eq ExitBreak) || (break eq FullBreak))
            }
        }
    }

    private [DeepEmbedding] object Pure
    {
        def unapply[A](self: Pure[A]): Option[A] = Some(self.x)
    }
    private [DeepEmbedding] object <*>
    {
        def apply[Tok, A, B](pf: Parsley[Tok, A=>B], px: Parsley[Tok, A]): <*>[Tok, A, B] =
        {
            val res = new <*>[Tok, A, B](pf, px)
            res.pf = pf
            res.px = px
            res.size = pf.size + px.size + 1
            res
        }
        def unapply[Tok, A, B](self: <*>[Tok, A, B]): Option[(Parsley[Tok, A=>B], Parsley[Tok, A])] = Some((self.pf, self.px))
    }
    private [DeepEmbedding] object <|>
    {
        def apply[Tok, A, B](p: Parsley[Tok, A], q: Parsley[Tok, B]): <|>[Tok, A, B] =
        {
            val res = new <|>[Tok, A, B](p, q)
            res.p = p
            res.q = q
            res.size = p.size + q.size + 1
            res
        }
        def unapply[Tok, A, B](self: <|>[Tok, A, B]): Option[(Parsley[Tok, A], Parsley[Tok, B])] = Some((self.p, self.q))
    }
    private [DeepEmbedding] object >>=
    {
        def apply[Tok, A, B](p: Parsley[Tok, A], f: A => Parsley[Tok, B], expected: UnsafeOption[String]): >>=[Tok, A, B] =
        {
            val res = new >>=[Tok, A, B](p, f, expected)
            res.p = p
            res.size = p.size + 3
            res
        }
        def unapply[Tok, A, B](self: >>=[Tok, A, B]): Option[(Parsley[Tok, A], A => Parsley[Tok, B])] = Some((self.p, self.f))
    }
    private [DeepEmbedding] object Cont
    {
        def unapply[Tok, A, B](self: Cont[Tok, A, B]): Option[(Parsley[Tok, A], Parsley[Tok, B])] = Some((self.discard, self.result))
    }
    private [DeepEmbedding] object *>
    {
        def apply[Tok, A, B](p: Parsley[Tok, A], q: Parsley[Tok, B]): *>[Tok, A, B] =
        {
            val res = new *>[Tok, A, B](p, q)
            res.p = p
            res.q = q
            res.size = p.size + q.size + 1
            res
        }
        def unapply[Tok, A, B](self: *>[Tok, A, B]): Option[(Parsley[Tok, A], Parsley[Tok, B])] = Some((self.p, self.q))
    }
    private [DeepEmbedding] object <*
    {
        def apply[Tok, A, B](p: Parsley[Tok, A], q: Parsley[Tok, B]): <*[Tok, A, B] =
        {
            val res = new <*[Tok, A, B](p, q)
            res.p = p
            res.q = q
            res.size = p.size + q.size + 1
            res
        }
        def unapply[Tok, A, B](self: <*[Tok, A, B]): Option[(Parsley[Tok, A], Parsley[Tok, B])] = Some((self.p, self.q))
    }
    private [DeepEmbedding] object Attempt
    {
        def apply[Tok, A](p: Parsley[Tok, A]): Attempt[Tok, A] =
        {
            val res = new Attempt[Tok, A](p)
            res.p = p
            res.size = p.size + 2
            res
        }
        def unapply[Tok, A](self: Attempt[Tok, A]): Option[Parsley[Tok, A]] = Some(self.p)
    }
    private [DeepEmbedding] object Look
    {
        def apply[Tok, A](p: Parsley[Tok, A]): Look[Tok, A] =
        {
            val res = new Look[Tok, A](p)
            res.p = p
            res.size = p.size + 3
            res
        }
    }
    private [DeepEmbedding] object Subroutine
    {
        def apply[Tok, A](p: Parsley[Tok, A], expected: UnsafeOption[String]): Subroutine[Tok, A] =
        {
            val res = new Subroutine[Tok, A](p, expected)
            res.p = p
            res
        }
        def unapply[Tok, A](self: Subroutine[Tok, A]): Option[Parsley[Tok, A]] = Some(self.p)
    }
    private [DeepEmbedding] object CharTok
    {
        def unapply[Tok](self: CharTok[Tok]): Option[Tok] = Some(self.t)
    }
    private [DeepEmbedding] object StringTok
    {
        def unapply(self: StringTok): Option[String] = Some(self.s)
    }
    private [DeepEmbedding] object Satisfy
    {
        def unapply[Tok](self: Satisfy[Tok]): Option[Tok => Boolean] = Some(self.f)
    }
    private [DeepEmbedding] object Lift2
    {
        def apply[Tok, A, B, C](f: (A, B) => C, p: Parsley[Tok, A], q: Parsley[Tok, B]): Lift2[Tok, A, B, C] =
        {
            val res = new Lift2[Tok, A, B, C](f, p, q)
            res.p = p
            res.q = q
            res.size = p.size + q.size + 1
            res
        }
        def unapply[Tok, A, B, C](self: Lift2[Tok, A, B, C]): Option[((A, B) => C, Parsley[Tok, A], Parsley[Tok, B])] = Some((self.f, self.p, self.q))
    }
    private [DeepEmbedding] object Lift3
    {
        def apply[Tok, A, B, C, D](f: (A, B, C) => D, p: Parsley[Tok, A], q: Parsley[Tok, B], r: Parsley[Tok, C]): Lift3[Tok, A, B, C, D] =
        {
            val res = new Lift3[Tok, A, B, C, D](f, p, q, r)
            res.p = p
            res.q = q
            res.r = r
            res.size = p.size + q.size + r.size + 1
            res
        }
        def unapply[Tok, A, B, C, D](self: Lift3[Tok, A, B, C, D]): Option[((A, B, C) => D, Parsley[Tok, A], Parsley[Tok, B], Parsley[Tok, C])] = Some((self.f, self.p, self.q, self.r))
    }
    private [DeepEmbedding] object FastFail
    {
        def apply[Tok, A](p: Parsley[Tok, A], msggen: A => String, expected: UnsafeOption[String]): FastFail[Tok, A] =
        {
            val res = new FastFail[Tok, A](p, msggen, expected)
            res.p = p
            res.size = p.size + 1
            res
        }
    }
    private [DeepEmbedding] object FastUnexpected
    {
        def apply[Tok, A](p: Parsley[Tok, A], msggen: A => String, expected: UnsafeOption[String]): FastUnexpected[Tok, A] =
        {
            val res = new FastUnexpected[Tok, A](p, msggen, expected)
            res.p = p
            res.size = p.size + 1
            res
        }
    }
    private [DeepEmbedding] object Ensure
    {
        def apply[Tok, A](p: Parsley[Tok, A], pred: A => Boolean, expected: UnsafeOption[String]): Ensure[Tok, A] =
        {
            val res = new Ensure[Tok, A](p, pred, expected)
            res.p = p
            res.size = p.size + 1
            res
        }
    }
    private [DeepEmbedding] object Guard
    {
        def apply[Tok, A](p: Parsley[Tok, A], pred: A => Boolean, msg: String, expected: UnsafeOption[String]): Guard[Tok, A] =
        {
            val res = new Guard[Tok, A](p, pred, msg, expected)
            res.p = p
            res.size = p.size + 1
            res
        }
    }
    private [DeepEmbedding] object FastGuard
    {
        def apply[Tok, A](p: Parsley[Tok, A], pred: A => Boolean, msggen: A => String, expected: UnsafeOption[String]): FastGuard[Tok, A] =
        {
            val res = new FastGuard[Tok, A](p, pred, msggen, expected)
            res.p = p
            res.size = p.size + 1
            res
        }
    }
    private [DeepEmbedding] object Many
    {
        def apply[Tok, A](p: Parsley[Tok, A]): Many[Tok, A] =
        {
            val res = new Many[Tok, A](p)
            res.p = p
            res.size = p.size + 2
            res
        }
    }
    private [DeepEmbedding] object SkipMany
    {
        def apply[Tok, A](p: Parsley[Tok, A]): SkipMany[Tok, A] =
        {
            val res = new SkipMany[Tok, A](p)
            res.p = p
            res.size = p.size + 2
            res
        }
    }
    private [DeepEmbedding] object ChainPost
    {
        def apply[Tok, A](p: Parsley[Tok, A], op: Parsley[Tok, A => A]): ChainPost[Tok, A] =
        {
            val res = new ChainPost[Tok, A](p, op)
            res.p = p
            res.op = op
            res.size = p.size + op.size + 2
            res
        }
    }
    private [DeepEmbedding] object ChainPre
    {
        def apply[Tok, A](p: Parsley[Tok, A], op: Parsley[Tok, A => A]): ChainPre[Tok, A] =
        {
            val res = new ChainPre[Tok, A](p, op)
            res.p = p
            res.op = op
            res.size = p.size + op.size + 2
            res
        }
    }
    private [DeepEmbedding] object Chainl
    {
        def apply[Tok, A](p: Parsley[Tok, A], op: Parsley[Tok, (A, A) => A]): Chainl[Tok, A] =
        {
            val res = new Chainl[Tok, A](p, op)
            res.p = p
            res.op = op
            res.size = p.size*2 + op.size + 2
            res
        }
    }
    private [DeepEmbedding] object Chainr
    {
        def apply[Tok, A](p: Parsley[Tok, A], op: Parsley[Tok, (A, A) => A]): Chainr[Tok, A] =
        {
            val res = new Chainr[Tok, A](p, op)
            res.p = p
            res.op = op
            res.size = p.size + op.size + 3
            res
        }
    }
    private [DeepEmbedding] object SepEndBy1
    {
        def apply[Tok, A, B](p: Parsley[Tok, A], sep: Parsley[Tok, B]): SepEndBy1[Tok, A, B] =
        {
            val res = new SepEndBy1[Tok, A, B](p, sep)
            res.p = p
            res.sep = sep
            res.size = p.size + sep.size + 3
            res
        }
    }
    private [parsley] object ManyUntil
    {
        object Stop
        def apply[Tok, A](body: Parsley[Tok, Any]): ManyUntil[Tok, A] =
        {
            val res = new ManyUntil[Tok, A](body)
            res.body = body
            res.size = body.size + 2
            res
        }
    }
    private [DeepEmbedding] object Ternary
    {
        def apply[Tok, A](b: Parsley[Tok, Boolean], p: Parsley[Tok, A], q: Parsley[Tok, A]): Ternary[Tok, A] =
        {
            val res = new Ternary[Tok, A](b, p, q)
            res.b = b
            res.p = p
            res.q = q
            res.size = b.size + p.size + q.size + 1
            res
        }
    }
    private [DeepEmbedding] object NotFollowedBy
    {
        def apply[Tok, A](p: Parsley[Tok, A], expected: UnsafeOption[String]): NotFollowedBy[Tok, A] =
        {
            val res = new NotFollowedBy[Tok, A](p, expected)
            res.p = p
            res.size = p.size + 2
            res
        }
    }
    private [DeepEmbedding] object Put
    {
        def apply[Tok, S](v: Var, p: Parsley[Tok, S]): Put[Tok, S] =
        {
            val res = new Put[Tok, S](v, p)
            res.p = p
            res.size = p.size + 1
            res
        }
    }
    private [DeepEmbedding] object Local
    {
        def apply[Tok, S, A](v: Var, p: Parsley[Tok, S], q: Parsley[Tok, A]): Local[Tok, S, A] =
        {
            val res = new Local[Tok, S, A](v, p, q)
            res.p = p
            res.q = q
            res.size = p.size + q.size + 2
            res
        }
    }
    private [DeepEmbedding] object Debug
    {
        def apply[Tok, A](p: Parsley[Tok, A], name: String, break: Breakpoint): Debug[Tok, A] =
        {
            val res = new Debug[Tok, A](p, name, break)
            res.p = p
            res.size = p.size + 2
            res
        }
    }
}

object TokTest
{
    import Parsley._
    def main(args: Array[String]): Unit =
    {
        val p: Parsley[Int, (Int, Int)] = oneOf((0 to 10).toSet) ? "1 to 10" *> pos
        println(runParser(p, Array(7), Some((x: Int) => (1, 1 + ((Math.log(x) + 1)/Math.log(10)).toInt))))
        println(runParser(p,  Array.empty[Int], Some((_: Int) => (100, 100))))
    }
}