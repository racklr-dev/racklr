// BendTT meta: the paper's verdicts, checked by the Koka kernel.
// ==============================================================
//
// Run from the repository root:
//
//   koka -e bendtt/meta.kk
//
// The books below are the examples of the paper (BendTT, sec. 1-4), written
// in the kernel's term syntax: Nat/Nat.arms/add, R/R.arms/app, ack, twice,
// map, two and loop. The live check of every book is run; the expected
// verdicts are R/add/ack/twice accepted, app/map/two rejected by affinity,
// loop rejected by descent. Conversion is checked to erase a lambda's
// quantity (lambda+ converts as lambda), and the untyped measure is checked
// to ignore dead q=0 parts. The final line says whether everything matched.

module bendtt/meta

import bendtt/kernel

// ---------------------------------------------------------------------------
// helpers for writing terms
// ---------------------------------------------------------------------------

fun lam(q : quan, f : term) : total term
  TLam(q, f)

fun app(f : term, x : term) : total term
  TApp(Q1, f, x)

fun app1(f : term, x : term) : total term
  TApp(Q1, f, x)

fun appq(q : quan, f : term, x : term) : total term
  TApp(q, f, x)

fun pair(a : term, b : term) : total term
  TTup(Q1, a, b)

fun pairq(q : quan, a : term, b : term) : total term
  TTup(q, a, b)

fun vr(i : int) : total term
  TVar(i)

fun rf(k : string) : total term
  TRef(k)

fun lab(k : string) : total term
  TLab(k)

fun unit-lab() : total term
  TLab("()")

fun unit-enum() : total term
  TEnu(Nil)

fun unit-type() : total term
  TEnu(Cons("()", Nil))

fun nats() : total term
  TEnu(Cons("Zero", Cons("Succ", Nil)))

fun lists() : total term
  TEnu(Cons("Nil", Cons("Con", Nil)))

fun type-ty() : total term
  TTyp(TLab("Q1"))

fun type-data() : total term
  TTyp(TLab("Q2"))

fun defn-of(nm : string, ty : term, body : term) : total defn
  Defn(nm, ty, body, False)

// a Nat value: Zero is (Zero, ()), Succ p is (Succ, (p, ()))
fun nat-zero() : total term
  pair(lab("Zero"), unit-lab())

fun nat-succ(p : term) : total term
  pair(lab("Succ"), pair(p, unit-lab()))

// a list value: Nil is (Nil, ()), Con(h, t) is (Con, (h, (t, ())))
fun list-nil() : total term
  pair(lab("Nil"), unit-lab())

fun list-con(h : term, t : term) : total term
  pair(lab("Con"), pair(h, pair(t, unit-lab())))

// ---------------------------------------------------------------------------
// Nat, add (paper, sec. 4: "Here is add, as Bend compiles it")
// ---------------------------------------------------------------------------

fun nat-arms-body() : total term
  TMat("Zero", unit-type(),
       TMat("Succ", TSig(Q1, rf("Nat"), unit-type()), TEfq))

fun nat-body() : total term
  TSig(Q1, nats(), appq(Q1, rf("Nat.arms"), vr(0)))

fun add-body() : total term
  TPrj(
    TMat("Zero",
      TMat("()", lam(Q1, vr(0)), TEfq),
      TMat("Succ",
        TPrj(lam(Q1, TMat("()", lam(Q1,
          nat-succ(app1(app1(rf("add"), vr(1)), vr(0)))), TEfq))),
        TEfq)))

// ---------------------------------------------------------------------------
// R, app (paper, sec. 4: Curry's paradox written directly in BendTT)
// ---------------------------------------------------------------------------

fun r-arms-body() : total term
  TMat("Fold",
       TSig(Q1, TAll(Q1, rf("R"), unit-enum()), unit-type()),
       TEfq)

fun r-body() : total term
  TSig(Q1, TEnu(Cons("Fold", Nil)), appq(Q1, rf("R.arms"), vr(0)))

fun app-body() : total term
  TPrj(
    TMat("Fold",
      TPrj(lam(Q1, TMat("()",
        app(vr(0), pair(lab("Fold"), pair(vr(0), unit-lab()))), TEfq))),
      TEfq))

// ---------------------------------------------------------------------------
// ack (paper, sec. 3: "Here is the Ackermann function")
// ---------------------------------------------------------------------------

// the zero case of the inner match on n: ack(p, 1n), where 1n is Succ Zero
fun ack-zero-n() : total term
  app1(appq(Q2, rf("ack"), vr(0)), nat-succ(nat-zero()))

// the succ case of the inner match on n: ack(p, ack(1n+p, q))
fun ack-succ-n() : total term
  app1(appq(Q2, rf("ack"), vr(1)),
       app1(appq(Q2, rf("ack"), nat-succ(vr(1))), vr(0)))

// match n: n is the second parameter, so the case-tree node consumes it
// directly (as `add` consumes its first parameter), giving column 1
fun ack-n-tree() : total term
  TPrj(
    TMat("Zero",
      TMat("()", ack-zero-n(), TEfq),
      TMat("Succ",
        TPrj(lam(Q1, TMat("()", ack-succ-n(), TEfq))),
        TEfq)))

// the succ case of the match on m: m = Succ p, and p is copyable
fun ack-succ-m() : total term
  TPrj(lam(Q2, TMat("()", ack-n-tree(), TEfq)))

fun ack-body() : total term
  TPrj(
    TMat("Zero",
      TMat("()", lam(Q1, nat-succ(vr(0))), TEfq),
      TMat("Succ", ack-succ-m(), TEfq)))

// ---------------------------------------------------------------------------
// twice (paper, sec. 2: the law mentions f four times, the def is {==})
// ---------------------------------------------------------------------------

fun twice-body() : total term
  lam(Q1, lam(Q1, TRfl))

// ---------------------------------------------------------------------------
// two (paper, sec. 5: impredicative Church numeral, f is used twice)
// ---------------------------------------------------------------------------

fun two-body() : total term
  lam(Q0, lam(Q1, lam(Q1, app(vr(1), app(vr(1), vr(0))))))

// ---------------------------------------------------------------------------
// List and map (paper, sec. 5: the usual map is rejected)
// ---------------------------------------------------------------------------

fun list-arms-body() : total term
  TMat("Nil", unit-type(),
       TMat("Con",
         TSig(Q1, rf("Nat"), TSig(Q1, rf("List"), unit-type())),
         TEfq))

fun list-body() : total term
  TSig(Q1, lists(), appq(Q1, rf("List.arms"), vr(0)))

fun map-body() : total term
  lam(Q1,
    TPrj(
      TMat("Nil",
        TMat("()", list-nil(), TEfq),
        TMat("Con",
          TPrj(lam(Q1,
            TPrj(lam(Q1,
              TMat("()",
                list-con(app(vr(2), vr(1)),
                         app1(app1(rf("map"), vr(2)), vr(0))),
                TEfq))))),
          TEfq))))

// ---------------------------------------------------------------------------
// loop (paper, sec. 3: "the kernel rejects loop : <> = loop")
// ---------------------------------------------------------------------------

fun loop-body() : total term
  rf("loop")

// ---------------------------------------------------------------------------
// books
// ---------------------------------------------------------------------------

fun nat-defs() : total list<defn>
  Cons(defn-of("Nat.arms", type-data(), nat-arms-body()),
  Cons(defn-of("Nat", type-data(), nat-body()), Nil))

fun r-defs() : total list<defn>
  Cons(defn-of("R.arms", type-ty(), r-arms-body()),
  Cons(defn-of("R", type-ty(), r-body()), Nil))

fun add-book() : total book
  list-append(nat-defs(), Cons(defn-of("add", type-ty(), add-body()), Nil))

fun ack-book() : total book
  list-append(nat-defs(), Cons(defn-of("ack", type-ty(), ack-body()), Nil))

fun r-book() : total book
  r-defs()

fun app-book() : total book
  list-append(r-defs(), Cons(defn-of("app", type-ty(), app-body()), Nil))

fun twice-book() : total book
  Cons(defn-of("twice", type-ty(), twice-body()), Nil)

fun two-book() : total book
  Cons(defn-of("two", type-ty(), two-body()), Nil)

fun map-book() : total book
  list-append(nat-defs(),
    list-append(Cons(defn-of("List.arms", type-data(), list-arms-body()),
                Cons(defn-of("List", type-data(), list-body()), Nil)),
                Cons(defn-of("map", type-ty(), map-body()), Nil)))

fun loop-book() : total book
  Cons(defn-of("loop", unit-enum(), loop-body()), Nil)

// ---------------------------------------------------------------------------
// verdicts
// ---------------------------------------------------------------------------

fun vres-tag(v : vres) : total string
  match v
    VOk  -> "accepted"
    VAff -> "affinity-rejected"
    VDes -> "descent-rejected"

fun vres-same(a : vres, b : vres) : total bool
  match a
    VOk -> match b
             VOk -> True
             _   -> False
    VAff -> match b
              VAff -> True
              _    -> False
    VDes -> match b
              VDes -> True
              _    -> False

fun book-verdict(bk : book) : total vres
  match check-book(bk)
    Nothing      -> VOk
    Just((_, v)) -> v

// ---------------------------------------------------------------------------
// conversion checks: lambda quantity erasure (move 3)
// ---------------------------------------------------------------------------

fun conv-fuel(bk : book, a : term, b : term) : total fuel
  fuel-add(budget-of(bk, a), budget-of(bk, b))

fun conv-tag(r : maybe<bool>) : total string
  match r
    Just(True)  -> "convertible"
    Just(False) -> "distinct"
    Nothing     -> "inconclusive"

fun show-conv(bk : book, nm : string, a : term, b : term, want : bool) : console bool
  val r = convertible(bk, conv-fuel(bk, a, b), a, b)
  val got = match r
              Just(v) -> v
              Nothing -> False
  println(nm ++ ": " ++ conv-tag(r) ++
          (if (got == want) && (match r { Nothing -> False; _ -> True })
             then "" else "   [expected " ++ (if want then "convertible" else "distinct") ++ "]"))
  got == want && (match r { Nothing -> False; _ -> True })

fun def-id-book() : total book
  Cons(defn-of("id1", TAll(Q1, t1(), t1()), lam(Q1, vr(0))), Nil)

fun erasure-checks() : console (bool, int, int)
  val c1 = show-conv(Nil, "conv lam+ ~ lam", lam(Q2, vr(0)), lam(Q1, vr(0)), True)
  val c2 = show-conv(Nil, "conv lam+ ~ lam-", lam(Q2, vr(0)), lam(Q0, vr(0)), False)
  val c3 = show-conv(Nil, "conv lam ~ lam", lam(Q1, vr(0)), lam(Q1, vr(0)), True)
  // beta: (lambda+ x => x) applied is its argument
  val c4 = show-conv(Nil, "conv beta", app(lam(Q2, vr(0)), lab("a")), lab("a"), True)
  // erasure inside: (lambda+ x => lambda+ y => y) applied converts with lambda x => x
  val c5 = show-conv(Nil, "conv mixed", app(lam(Q1, lam(Q2, vr(0))), lab("a")),
                     lam(Q1, vr(0)), True)
  // a def unfolds when a call walks its case tree: (id1 a) converts with a
  val c6 = show-conv(def-id-book(), "conv def unfolds", app1(rf("id1"), lab("a")), lab("a"), True)
  val ok = c1 && c2 && c3 && c4 && c5 && c6
  val total = 6
  val passed = (if c1 then 1 else 0) + (if c2 then 1 else 0) + (if c3 then 1 else 0) + (if c4 then 1 else 0) + (if c5 then 1 else 0) + (if c6 then 1 else 0)
  (ok, passed, total)

// ---------------------------------------------------------------------------
// measure checks: the untyped live measure (move 1)
// ---------------------------------------------------------------------------

fun show-measure(nm : string, got : int, want : int) : console bool
  println(nm ++ ": " ++ (if got == want then "ok" else "MISMATCH got " ++ show(got) ++ " want " ++ show(want)))
  got == want

fun measure-checks() : console (bool, int, int)
  val m1 = show-measure("measure ann (type weighs 0)", size(TAnn(vr(0), TEnu(Nil))), 2)
  val m2 = show-measure("measure let-0 (value weighs 0)", size(TLet(Q0, app(vr(0), vr(1)), vr(0))), 2)
  val m3 = show-measure("measure app-0 (arg weighs 0)", size(TApp(Q0, vr(0), vr(1))), 2)
  val m4 = show-measure("measure tup-0 (first field weighs 0)", size(TTup(Q0, vr(0), vr(1))), 2)
  val m5 = show-measure("measure let-1 (live value)", size(TLet(Q1, vr(0), vr(1))), 3)
  val m6 = show-measure("measure fuel = size (add)", fuel-to-int(fuel-of(add-body())), size(add-body()))
  val m7 = show-measure("measure fuel = size (ack)", fuel-to-int(fuel-of(ack-body())), size(ack-body()))
  val ok = m1 && m2 && m3 && m4 && m5 && m6 && m7
  val total = 7
  val passed = (if m1 then 1 else 0) + (if m2 then 1 else 0) + (if m3 then 1 else 0) + (if m4 then 1 else 0) + (if m5 then 1 else 0) + (if m6 then 1 else 0) + (if m7 then 1 else 0)
  (ok, passed, total)

// ---------------------------------------------------------------------------
// the whole meta check
// ---------------------------------------------------------------------------

fun show-example(nm : string, bk : book, want : vres) : console bool
  val got = book-verdict(bk)
  val ok = vres-same(got, want)
  println(nm ++ ": " ++ vres-tag(got) ++
          (if ok then "" else "   [expected " ++ vres-tag(want) ++ "]"))
  ok

fun verdict-checks() : console (bool, int, int)
  val r = show-example("R", r-book(), VOk)
  val add = show-example("add", add-book(), VOk)
  val ack = show-example("ack", ack-book(), VOk)
  val twice = show-example("twice", twice-book(), VOk)
  val appx = show-example("app", app-book(), VAff)
  val map = show-example("map", map-book(), VAff)
  val two = show-example("two", two-book(), VAff)
  val loop = show-example("loop", loop-book(), VDes)
  val ok = r && add && ack && twice && appx && map && two && loop
  val total = 8
  val passed = (if r then 1 else 0) + (if add then 1 else 0) + (if ack then 1 else 0) + (if twice then 1 else 0)
             + (if appx then 1 else 0) + (if map then 1 else 0) + (if two then 1 else 0) + (if loop then 1 else 0)
  (ok, passed, total)

fun main() : console ()
  println("BendTT kernel in Koka: meta checks")
  println("")
  println("-- live check: the paper's verdicts --")
  val (v-ok, v-passed, v-total) = verdict-checks()
  println("")
  println("-- conversion: lambda quantity erasure --")
  val (c-ok, c-passed, c-total) = erasure-checks()
  println("")
  println("-- measure: the untyped live measure --")
  val (m-ok, m-passed, m-total) = measure-checks()
  println("")
  println("-- totality: whnf, conv, the live check and the descent comparison --")
  println("totality: Koka's checker accepts the restructured traversals as total (measure-bearing, no div)")
  println("")
  println("verdicts " ++ show(v-passed) ++ "/" ++ show(v-total) ++
          ", conversion " ++ show(c-passed) ++ "/" ++ show(c-total) ++
          ", measure " ++ show(m-passed) ++ "/" ++ show(m-total))
  if v-ok && c-ok && m-ok
    then println("ALL CHECKS PASS")
    else println("SOME CHECKS FAIL")
