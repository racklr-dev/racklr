// BendTT kernel in Koka (Koka 3.2.9).
// ====================================
//
// A learn-and-expand of the BendTT kernel (bend2/bendtt.lean) in Koka, not a
// line-for-line port. The Lean file holds the theory, the claims and the
// proofs; Koka cannot state the dependent Prop claims (subject reduction,
// progress, the empty-type argument, adequacy), so those stay Lean-only.
// What this file does provide:
//
//   1. The untyped live measure `size` of a term (dead q=0 parts weigh 0),
//      exactly as in bendtt.lean:2935, plus a total head evaluator whose
//      recursion is structurally accepted by Koka's totality checker (a
//      terminal `fuel` value derived from the measure; no `div`, no
//      `pretend-decreasing`, no `unsafe`).
//   2. The live check: affinity plus the descent relation, aligned with
//      the Lean model (bendtt.lean:1318) and, on elaborated terms,
//      field-for-field with bend2's `term_descend` (bend2/bend.ts:3253).
//   3. Conversion with the lambda quantity erasure: two lambdas of one
//      liveness convert, so a lambda of quantity 2 converts as a lambda of
//      quantity 1 (`Quan.lin`, bendtt.lean:170 and Term.parts at 9xx).
//
// Everything in this file is a total function: Koka's checker verifies that
// the evaluator and the live check are terminating by a structural measure
// (the `fuel` argument below), which is the machine-checked part of the
// termination-of-live-evaluation argument. The paper's proof that live
// evaluation fits the measure (the label measure of the Lean termination
// section) is a dependent claim and remains in Lean.

module bendtt/kernel

// ---------------------------------------------------------------------------
// Quantities (bendtt.lean:159-200)
// ---------------------------------------------------------------------------

pub type quan
  Q0
  Q1
  Q2

// a quantity is live unless it is erased
pub fun quan-live(q : quan) : total bool
  match q
    Q0 -> False
    _  -> True

// a lambda's quantity as conversion sees it: q=2 only says how a lambda
// runs (it copies), so a lambda+ converts as a lambda (bendtt.lean:167)
pub fun quan-lin(q : quan) : total quan
  match q
    Q2 -> Q1
    _  -> q

// the quantity of a pair's first field, when the pair is used at q
// (bendtt.lean:176)
pub fun quan-fld(r : quan, q : quan) : total quan
  match r
    Q0 -> Q0
    Q1 -> q
    Q2 -> Q2

// a q binder allows n live uses (bendtt.lean:200)
pub fun quan-allows(q : quan, n : int) : total bool
  match q
    Q0 -> n == 0
    Q1 -> n <= 1
    Q2 -> True

// the text mark of a quantity
pub fun quan-mark(q : quan) : total string
  match q
    Q0 -> "-"
    Q1 -> ""
    Q2 -> "+"

// ---------------------------------------------------------------------------
// Terms (bendtt.lean:80-103)
// ---------------------------------------------------------------------------

pub type term
  TVar(i : int)
  TRef(k : string)
  TAnn(x : term, ty : term)
  TLet(q : quan, v : term, f : term)
  TTyp(q : term)
  TMin(a : term, b : term)
  TAll(q : quan, dom : term, cod : term)
  TLam(q : quan, f : term)
  TApp(q : quan, f : term, x : term)
  TSig(q : quan, dom : term, cod : term)
  TTup(q : quan, a : term, b : term)
  TPrj(h : term)
  TEnu(ks : list<string>)
  TLab(k : string)
  TMat(k : string, h : term, m : term)
  TEfq
  TEql(a : term, b : term, ty : term)
  TRfl
  TRwt(e : term, p : term, f : term)

// the labels of a quantity (bendtt.lean:196)
pub val qs : list<string> = ["Q0", "Q1", "Q2"]

// Type and Data
pub fun t1() : total term
  TTyp(TLab("Q1"))

pub fun t2() : total term
  TTyp(TLab("Q2"))

// the kind a q binder's domain needs, inside a type of kind K (bendtt.lean:190)
pub fun kindof(q : quan, kind : term) : total term
  match q
    Q0 -> t1()
    Q1 -> kind
    Q2 -> t2()

// the meet of two quantities, as far as they are known: Q2 is its identity
// and Q0 absorbs it, on either side (bendtt.lean:203)
pub fun qmin(a : term, b : term) : total term
  match a
    TLab(k) ->
      match k
        "Q2" -> b
        "Q0" -> TLab("Q0")
        _    -> match b
                  TLab(j) -> if k == j then TLab(k) else TMin(a, b)
                  _       -> TMin(a, b)
    _ -> match b
           TLab("Q2") -> a
           TLab("Q0") -> TLab("Q0")
           _          -> TMin(a, b)

// a lambda or a lambda-match: a node of a case tree, which takes an argument
pub fun takes(t : term) : total bool
  match t
    TLam(_, _)     -> True
    TPrj(_)        -> True
    TMat(_, _, _)  -> True
    TEfq           -> True
    _              -> False

// a node of a case tree: a lambda, a lambda-match, or a spine of one on a
// variable (bendtt.lean:294)
pub fun node(t : term) : total bool
  match t
    TApp(_, f, TVar(_)) -> takes(unspine-head(f))
    _                   -> takes(t)

// the head of a spine
pub fun unspine-head(t : term) : total term
  match t
    TApp(_, f, _) -> unspine-head(f)
    _             -> t

// a former whose parts may bind a variable (bendtt.lean:288)
pub fun binds(t : term) : total bool
  match t
    TAll(_, _, _) -> True
    TLam(_, _)    -> True
    TSig(_, _, _) -> True
    TRwt(_, _, _) -> True
    _             -> False

// an argument as it enters a type: a lambda goes annotated
pub fun arg(x : term, ty : term) : total term
  if takes(x) then TAnn(x, ty) else x

// the spine of a head applied to arguments (bendtt.lean:255)
pub fun spine(t : term, xs : list<(quan, term)>) : total term
  match xs
    Nil            -> t
    Cons((q, x), r) -> spine(TApp(q, t, x), r)

// the head and the arguments of a spine, innermost first (bendtt.lean:259)
pub fun unspine(t : term, xs : list<(quan, term)>) : total (term, list<(quan, term)>)
  match t
    TApp(q, f, x) -> unspine(f, Cons((q, x), xs))
    _             -> (t, xs)

// ---------------------------------------------------------------------------
// Renaming and substitution (bendtt.lean:225-337)
// ---------------------------------------------------------------------------

pub fun ren(r : int -> int, t : term) : total term
  match t
    TVar(i)        -> TVar(r(i))
    TRef(_)        -> t
    TAnn(x, ty)    -> TAnn(ren(r, x), ren(r, ty))
    TLet(q, v, f)  -> TLet(q, ren(r, v), ren(fn(i) up-ren(r, i), f))
    TTyp(q)        -> TTyp(ren(r, q))
    TMin(a, b)     -> TMin(ren(r, a), ren(r, b))
    TAll(q, dom, cod) -> TAll(q, ren(r, dom), ren(fn(i) up-ren(r, i), cod))
    TLam(q, f)     -> TLam(q, ren(fn(i) up-ren(r, i), f))
    TApp(q, f, x)  -> TApp(q, ren(r, f), ren(r, x))
    TSig(q, dom, cod) -> TSig(q, ren(r, dom), ren(fn(i) up-ren(r, i), cod))
    TTup(q, a, b)  -> TTup(q, ren(r, a), ren(r, b))
    TPrj(h)        -> TPrj(ren(r, h))
    TEnu(_)        -> t
    TLab(_)        -> t
    TMat(k, h, m)  -> TMat(k, ren(r, h), ren(r, m))
    TEfq           -> t
    TEql(a, b, ty) -> TEql(ren(r, a), ren(r, b), ren(r, ty))
    TRfl           -> t
    TRwt(e, p, f)  -> TRwt(ren(r, e), ren(fn(i) up-ren(fn(j) up-ren(r, j), i), p), ren(r, f))

fun up-ren(r : int -> int, i : int) : total int
  if i == 0 then 0 else r(i - 1) + 1

pub fun sub(s : int -> term, t : term) : total term
  match t
    TVar(i)        -> s(i)
    TRef(_)        -> t
    TAnn(x, ty)    -> TAnn(sub(s, x), sub(s, ty))
    TLet(q, v, f)  -> TLet(q, sub(s, v), sub(fn(i) up-sub(s, i), f))
    TTyp(q)        -> TTyp(sub(s, q))
    TMin(a, b)     -> TMin(sub(s, a), sub(s, b))
    TAll(q, dom, cod) -> TAll(q, sub(s, dom), sub(fn(i) up-sub(s, i), cod))
    TLam(q, f)     -> TLam(q, sub(fn(i) up-sub(s, i), f))
    TApp(q, f, x)  -> TApp(q, sub(s, f), sub(s, x))
    TSig(q, dom, cod) -> TSig(q, sub(s, dom), sub(fn(i) up-sub(s, i), cod))
    TTup(q, a, b)  -> TTup(q, sub(s, a), sub(s, b))
    TPrj(h)        -> TPrj(sub(s, h))
    TEnu(_)        -> t
    TLab(_)        -> t
    TMat(k, h, m)  -> TMat(k, sub(s, h), sub(s, m))
    TEfq           -> t
    TEql(a, b, ty) -> TEql(sub(s, a), sub(s, b), sub(s, ty))
    TRfl           -> t
    TRwt(e, p, f)  -> TRwt(sub(s, e), sub(fn(i) up-sub(fn(j) up-sub(s, j), i), p), sub(s, f))

fun up-sub(s : int -> term, i : int) : total term
  if i == 0 then TVar(0) else ren(fn(j) j + 1, s(i - 1))

// substitute the value v for variable 0 (bendtt.lean:449)
pub fun inst(f : term, v : term) : total term
  sub(fn(i) if i == 0 then v else TVar(i - 1), f)

// an environment lists, innermost first, the values a case tree's lambdas
// bound (bendtt.lean:454)
pub fun env-sub(e : list<term>, i : int) : total term
  match e
    Nil            -> TVar(i)
    Cons(x, rest)  -> if i == 0 then x else env-sub(rest, i - 1)

// ---------------------------------------------------------------------------
// The untyped live measure (bendtt.lean:2935)
// ---------------------------------------------------------------------------
//
// The size of a term's live part. Dead positions weigh 0: the first field of
// a q=0 pair, the value of a q=0 let, and the argument of a q=0 application.
// Types, annotations and the rest are not consulted; this is the measure that
// needs no types.

pub fun size(t : term) : total int
  match t
    TAnn(x, _)       -> 1 + size(x)
    TLet(q, v, f)    -> (if quan-live(q) then size(v) else 0) + size(f) + 1
    TLam(_, f)       -> 1 + size(f)
    TApp(q, f, x)    -> size(f) + (if quan-live(q) then size(x) else 0) + 1
    TTup(q, a, b)    -> (if quan-live(q) then size(a) else 0) + size(b) + 1
    TPrj(h)          -> 1 + size(h)
    TMat(_, h, m)    -> 1 + size(h) + size(m)
    TRwt(e, _, f)    -> 1 + size(e) + size(f)
    _                -> 1

// ---------------------------------------------------------------------------
// Defs and books (bendtt.lean:104-118)
// ---------------------------------------------------------------------------

pub struct defn
  nm : string
  ty : term
  v : term
  o : bool

pub alias book = list<defn>

// the first def of a name wins; the index orders a def's live calls
pub fun book-index(bk : book, k : string) : total maybe<int>
  book-index-at(bk, k, 0)

fun book-index-at(bk : book, k : string, i : int) : total maybe<int>
  match bk
    Nil           -> Nothing
    Cons(d, rest) -> if d.nm == k then Just(i) else book-index-at(rest, k, i + 1)

pub fun book-get(bk : book, k : string) : total maybe<defn>
  match bk
    Nil           -> Nothing
    Cons(d, rest) -> if d.nm == k then Just(d) else book-get(rest, k)

pub fun book-length(bk : book) : total int
  match bk
    Nil           -> 0
    Cons(_, rest) -> 1 + book-length(rest)

// ---------------------------------------------------------------------------
// Structural equality (Koka cannot compare an inductive type structurally
// without an instance, so the kernel carries its own total term equality)
// ---------------------------------------------------------------------------

pub fun quan-eq(a : quan, b : quan) : total bool
  match a
    Q0 -> match b
            Q0 -> True
            _  -> False
    Q1 -> match b
            Q1 -> True
            _  -> False
    Q2 -> match b
            Q2 -> True
            _  -> False

fun string-list-eq(xs : list<string>, ys : list<string>) : total bool
  match xs
    Nil -> match ys
             Nil -> True
             _   -> False
    Cons(x, xr) -> match ys
                     Nil -> False
                     Cons(y, yr) -> x == y && string-list-eq(xr, yr)

pub fun term-eq(a : term, b : term) : total bool
  match a
    TVar(i) -> match b
                 TVar(j) -> i == j
                 _ -> False
    TRef(k) -> match b
                 TRef(j) -> k == j
                 _ -> False
    TAnn(x, ty) -> match b
                     TAnn(y, u) -> term-eq(x, y) && term-eq(ty, u)
                     _ -> False
    TLet(q, v, f) -> match b
                       TLet(p, w, g) -> quan-eq(q, p) && term-eq(v, w) && term-eq(f, g)
                       _ -> False
    TTyp(q) -> match b
                 TTyp(p) -> term-eq(q, p)
                 _ -> False
    TMin(x, y) -> match b
                    TMin(u, v) -> term-eq(x, u) && term-eq(y, v)
                    _ -> False
    TAll(q, dom, cod) -> match b
                           TAll(p, d, e) -> quan-eq(q, p) && term-eq(dom, d) && term-eq(cod, e)
                           _ -> False
    TLam(q, f) -> match b
                    TLam(p, g) -> quan-eq(q, p) && term-eq(f, g)
                    _ -> False
    TApp(q, f, x) -> match b
                       TApp(p, g, y) -> quan-eq(q, p) && term-eq(f, g) && term-eq(x, y)
                       _ -> False
    TSig(q, dom, cod) -> match b
                           TSig(p, d, e) -> quan-eq(q, p) && term-eq(dom, d) && term-eq(cod, e)
                           _ -> False
    TTup(q, x, y) -> match b
                       TTup(p, u, v) -> quan-eq(q, p) && term-eq(x, u) && term-eq(y, v)
                       _ -> False
    TPrj(h) -> match b
                 TPrj(g) -> term-eq(h, g)
                 _ -> False
    TEnu(ks) -> match b
                  TEnu(js) -> string-list-eq(ks, js)
                  _ -> False
    TLab(k) -> match b
                 TLab(j) -> k == j
                 _ -> False
    TMat(k, h, m) -> match b
                       TMat(j, g, n) -> k == j && term-eq(h, g) && term-eq(m, n)
                       _ -> False
    TEfq -> match b
              TEfq -> True
              _ -> False
    TEql(x, y, ty) -> match b
                        TEql(u, v, w) -> term-eq(x, u) && term-eq(y, v) && term-eq(ty, w)
                        _ -> False
    TRfl -> match b
              TRfl -> True
              _ -> False
    TRwt(e, p, f) -> match b
                       TRwt(d, q, g) -> term-eq(e, d) && term-eq(p, q) && term-eq(f, g)
                       _ -> False

// ---------------------------------------------------------------------------
// The measure as a structural budget
// ---------------------------------------------------------------------------
//
// Koka 3.2.9's totality checker has no measure annotations and rejects mutual
// recursion, so the evaluator below is a single self-recursive function whose
// only decreasing argument is a `fuel` value: a Peano presentation of the
// untyped live measure `size` above. `fuel-of` builds that value structurally
// from the term (so the conversion from the integer measure is itself total
// and does not recurse on an int).

pub type fuel
  FZero
  FSucc(p : fuel)

pub fun fuel-of(t : term) : total fuel
  match t
    TAnn(x, _)     -> FSucc(fuel-of(x))
    TLet(q, v, f)  -> fuel-add(if quan-live(q) then fuel-of(v) else FZero, FSucc(fuel-of(f)))
    TLam(_, f)     -> FSucc(fuel-of(f))
    TApp(q, f, x)  -> fuel-add(fuel-of(f), fuel-add(if quan-live(q) then fuel-of(x) else FZero, FSucc(FZero)))
    TTup(q, a, b)  -> fuel-add(if quan-live(q) then fuel-of(a) else FZero, fuel-add(fuel-of(b), FSucc(FZero)))
    TPrj(h)        -> FSucc(fuel-of(h))
    TMat(_, h, m)  -> FSucc(fuel-add(fuel-of(h), fuel-of(m)))
    TRwt(e, _, f)  -> FSucc(fuel-add(fuel-of(e), fuel-of(f)))
    _              -> FSucc(FZero)

pub fun fuel-add(a : fuel, b : fuel) : total fuel
  match a
    FZero   -> b
    FSucc(p) -> FSucc(fuel-add(p, b))

pub fun fuel-to-int(f : fuel) : total int
  match f
    FZero   -> 0
    FSucc(p) -> 1 + fuel-to-int(p)

// ---------------------------------------------------------------------------
// Budget: the untyped measure, scaled and including the book
// ---------------------------------------------------------------------------
//
// The evaluator below is total for *every* budget; the budget only bounds how
// much work it may do. A Peano value derived from `size` is used, because an
// int recursion is not total in Koka. The budget has to cover more than the
// input term: unfolding a def copies its body, and the spine and environment
// add work, so the initial budget is K units (K = 6) for each constructor of
// the term and of the book's bodies. This is a bound, not the Lean label
// measure: if the budget runs out, evaluation is inconclusive (`Nothing`),
// never a partial normal form.

pub fun book-fuel(bk : book) : total fuel
  match bk
    Nil           -> FZero
    Cons(d, rest) -> fuel-add(fuel-of(d.v), book-fuel(rest))

// k copies of f, by recursion on k
pub fun fuel-scale(f : fuel, k : fuel) : total fuel
  match k
    FZero   -> FZero
    FSucc(p) -> fuel-add(f, fuel-scale(f, p))

pub fun fuel-six() : total fuel
  FSucc(FSucc(FSucc(FSucc(FSucc(FSucc(FZero))))))

pub fun budget-of(bk : book, t : term) : total fuel
  fuel-scale(fuel-add(fuel-of(t), book-fuel(bk)), fuel-six())

// ---------------------------------------------------------------------------
// Evaluator (restructured from Term.wnf/run/fire, bendtt.lean:861-947)
// ---------------------------------------------------------------------------
//
// One self-recursive machine: `EMWnf` reduces a head on a spine, `EMRun`
// walks a def's case tree. The `fire` rules are inlined into both, so no
// mutually recursive group remains for Koka to reject. The closed-term
// normalisation of q=2 Data values (Term.val) is the open-term, lazy case
// here: it is an implementation sharing optimisation, not a conversion rule.

pub type emode
  EMWnf
  EMRun

// a machine result: a term, "the case tree needs more arguments", or "the
// budget ran out". Exhaustion is a separate outcome so it can never be
// mistaken for a weak head normal form.
pub type eres
  ET(t : term)
  EN
  EO

pub fun ev(bk : book, fl : fuel, md : emode, t : term, e : list<term>,
           xs : list<(quan, term)>) : total eres
  match fl
    FZero -> EO
    FSucc(g) ->
      match md
        EMWnf ->
          match t
            TAnn(x, _) -> ev(bk, g, EMWnf, x, Nil, xs)
            TLet(_, v, f) -> ev(bk, g, EMWnf, inst(f, v), Nil, xs)
            TApp(q, f0, x) -> ev(bk, g, EMWnf, f0, Nil, Cons((q, x), xs))
            TRwt(ee, pp, ff) ->
              match ev(bk, g, EMWnf, ee, Nil, Nil)
                ET(TRfl) -> ev(bk, g, EMWnf, ff, Nil, xs)
                ET(e2)   -> ET(spine(TRwt(e2, pp, ff), xs))
                EN       -> ET(spine(TRwt(ee, pp, ff), xs))
                EO       -> EO
            TMin(a, b) ->
              match ev(bk, g, EMWnf, a, Nil, Nil)
                ET(a2) ->
                  match ev(bk, g, EMWnf, b, Nil, Nil)
                    ET(b2) -> ET(spine(qmin(a2, b2), xs))
                    EN     -> ET(spine(TMin(a2, b), xs))
                    EO     -> EO
                EN -> ET(spine(t, xs))
                EO -> EO
            TRef(kk) ->
              match book-get(bk, kk)
                Just(dd) ->
                  if dd.o then ET(spine(t, xs))
                  else match ev(bk, g, EMRun, dd.v, Nil, xs)
                         ET(t2) -> ev(bk, g, EMWnf, t2, Nil, Nil)
                         EN     -> ET(spine(t, xs))
                         EO     -> EO
                Nothing -> ET(spine(t, xs))
            TLam(_, f0) ->
              match xs
                Cons((_, x), rest) -> ev(bk, g, EMWnf, inst(f0, x), Nil, rest)
                Nil                -> ET(spine(t, Nil))
            TPrj(h) ->
              match xs
                Cons((q, x), rest) ->
                  match ev(bk, g, EMWnf, x, Nil, Nil)
                    ET(TTup(r, a, b)) ->
                      ev(bk, g, EMWnf, h, Nil, Cons((quan-fld(r, q), a), Cons((q, b), rest)))
                    ET(_) -> ET(spine(t, xs))
                    EN    -> ET(spine(t, xs))
                    EO    -> EO
                Nil -> ET(spine(t, Nil))
            TMat(kk, h, m) ->
              match xs
                Cons((q, x), rest) ->
                  match ev(bk, g, EMWnf, x, Nil, Nil)
                    ET(TLab(j)) ->
                      if j == kk
                        then ev(bk, g, EMWnf, h, Nil, rest)
                        else ev(bk, g, EMWnf, m, Nil, Cons((q, TLab(j)), rest))
                    ET(_) -> ET(spine(t, xs))
                    EN    -> ET(spine(t, xs))
                    EO    -> EO
                Nil -> ET(spine(t, Nil))
            _ -> ET(spine(t, xs))
        EMRun ->
          match t
            TApp(q, f0, TVar(v)) ->
              if takes(unspine-head(f0))
                then ev(bk, g, EMRun, f0, e, Cons((q, env-sub(e, v)), xs))
                else ET(spine(sub(fn(i) env-sub(e, i), TApp(q, f0, TVar(v))), xs))
            TLam(_, f0) ->
              match xs
                Cons((_, x), rest) -> ev(bk, g, EMRun, f0, Cons(x, e), rest)
                Nil                -> EN
            TPrj(h) ->
              match xs
                Cons((q, x), rest) ->
                  match ev(bk, g, EMWnf, x, Nil, Nil)
                    ET(TTup(r, a, b)) ->
                      ev(bk, g, EMRun, h, e, Cons((quan-fld(r, q), a), Cons((q, b), rest)))
                    ET(_) -> EN
                    EN    -> EN
                    EO    -> EO
                Nil -> EN
            TMat(kk, h, m) ->
              match xs
                Cons((q, x), rest) ->
                  match ev(bk, g, EMWnf, x, Nil, Nil)
                    ET(TLab(j)) ->
                      if j == kk
                        then ev(bk, g, EMRun, h, e, rest)
                        else ev(bk, g, EMRun, m, e, Cons((q, TLab(j)), rest))
                    ET(_) -> EN
                    EN    -> EN
                    EO    -> EO
                Nil -> EN
            _ ->
              if takes(t) then EN
              else ET(spine(sub(fn(i) env-sub(e, i), t), xs))

// the weak head normal form of a term, bounded by the measure-derived budget.
// `Nothing` means the budget ran out: the term is not known to be a normal
// form, and callers must treat the comparison as inconclusive.
pub fun whnf(bk : book, t : term) : total maybe<term>
  whnf-with(bk, budget-of(bk, t), t)

// the weak head normal form of a term under an explicit budget
pub fun whnf-with(bk : book, fl : fuel, t : term) : total maybe<term>
  match ev(bk, fl, EMWnf, t, Nil, Nil)
    ET(u) -> Just(u)
    _     -> Nothing

// ---------------------------------------------------------------------------
// Conversion (bendtt.lean:956-1010)
// ---------------------------------------------------------------------------

pub type ord
  OLt
  OEq
  OGt

pub fun ord-eq(a : ord, b : ord) : total bool
  match a
    OLt -> match b
             OLt -> True
             _   -> False
    OEq -> match b
             OEq -> True
             _   -> False
    OGt -> match b
             OGt -> True
             _   -> False

// the parts two weak heads must match on; none when the heads differ. A
// lambda+ matches a lambda of quantity 1: the quantity of a lambda says how
// it runs, so conversion erases q=2 to q=1 (bendtt.lean:167, 968)
pub fun parts(a : term, b : term) : total maybe<list<(term, term)>>
  match a
    TAll(q, dom, cod) -> match b
                           TAll(p, d, e) -> if quan-eq(q, p) then Just(Cons((dom, d), Cons((cod, e), Nil))) else Nothing
                           _ -> Nothing
    TLam(q, f) -> match b
                    TLam(p, g) -> if quan-eq(quan-lin(q), quan-lin(p)) then Just(Cons((f, g), Nil)) else Nothing
                    _ -> Nothing
    TApp(q, f, x) -> match b
                       TApp(p, g, y) -> if quan-eq(q, p) then Just(Cons((f, g), Cons((x, y), Nil))) else Nothing
                       _ -> Nothing
    TSig(q, dom, cod) -> match b
                           TSig(p, d, e) -> if quan-eq(q, p) then Just(Cons((dom, d), Cons((cod, e), Nil))) else Nothing
                           _ -> Nothing
    TTup(q, x, y) -> match b
                       TTup(p, u, v) -> if quan-eq(q, p) then Just(Cons((x, u), Cons((y, v), Nil))) else Nothing
                       _ -> Nothing
    TPrj(h) -> match b
                 TPrj(g) -> Just(Cons((h, g), Nil))
                 _ -> Nothing
    TTyp(q) -> match b
                 TTyp(p) -> Just(Cons((q, p), Nil))
                 _ -> Nothing
    TMin(x, y) -> match b
                    TMin(u, v) -> Just(Cons((x, u), Cons((y, v), Nil)))
                    _ -> Nothing
    TMat(k, h, m) -> match b
                       TMat(j, g, n) -> if k == j then Just(Cons((h, g), Cons((m, n), Nil))) else Nothing
                       _ -> Nothing
    TEql(x, y, ty) -> match b
                        TEql(u, v, w) -> Just(Cons((x, u), Cons((y, v), Cons((ty, w), Nil))))
                        _ -> Nothing
    TRwt(e, p, f) -> match b
                       TRwt(d, q, g) -> Just(Cons((e, d), Cons((p, q), Cons((f, g), Nil))))
                       _ -> Nothing
    _ -> if term-eq(a, b) then Just(Nil) else Nothing

pub fun list-append(xs : list<a>, ys : list<a>) : total list<a>
  match xs
    Nil           -> ys
    Cons(x, rest) -> Cons(x, list-append(rest, ys))

// a and b convert: process the current pair, then the pending worklist. The
// fuel decreases on every step, so Koka's checker accepts the machine as
// total; at 0 the conversion is inconclusive (false).
// `Nothing` means the budget ran out before the answer was known: the
// conversion is inconclusive, not "distinct".
pub fun conv(bk : book, fl : fuel, a : term, b : term,
             work : list<(term, term)>) : total maybe<bool>
  match fl
    FZero -> Nothing
    FSucc(g) ->
      if term-eq(a, b) then
        match work
          Nil            -> Just(True)
          Cons((x, y), r) -> conv(bk, g, x, y, r)
      else
        match ev(bk, g, EMWnf, a, Nil, Nil)
          ET(a1) -> match ev(bk, g, EMWnf, b, Nil, Nil)
                      ET(b1) ->
                        if term-eq(a1, b1) then
                          match work
                            Nil            -> Just(True)
                            Cons((x, y), r) -> conv(bk, g, x, y, r)
                        else
                          match parts(a1, b1)
                            Just(ps) -> match ps
                                          Nil -> match work
                                                   Nil            -> Just(True)
                                                   Cons((x, y), r) -> conv(bk, g, x, y, r)
                                          Cons((x, y), r) -> conv(bk, g, x, y, list-append(r, work))
                            Nothing -> Just(False)
                      _ -> Nothing
          _ -> Nothing

pub fun convertible(bk : book, fl : fuel, a : term, b : term) : total maybe<bool>
  conv(bk, fl, a, b, Nil)

// ---------------------------------------------------------------------------
// Live check: affinity and descent (bendtt.lean:1240-1465)
// ---------------------------------------------------------------------------
//
// The live check reads no types. A binder's variable is used live at most as
// often as its quantity allows (affinity); the two arms of a match add up. A
// live call must name an earlier def, or its own def with arguments that
// descend, compared field-for-field against the column's pattern exactly as
// bend2's `term_descend` (bend2/bend.ts:3253) decides: an erased column is
// equal, a variable is equal only to itself, a constructor compares its
// fields in turn (a dead or absent field is bigger), the rest of a pattern is
// tried only when the first field that may be bigger failed.

// a column pattern: the pieces the case tree split open, each a pair of its
// parts, and holes for the rest (bendtt.lean:1270)
pub type pat
  PHole
  PPair(a : pat, b : pat)

// a tag says a variable is the piece of column j at a path: each step,
// innermost first, takes a pair's first or second field (bendtt.lean:1262)
pub alias tag = (int, list<bool>)

// a guard carries a def's book, its own index, column liveness, var tags, and
// the labels its matches hit, with their columns and paths
pub struct guard
  gbook : book
  gself : int
  gcols : list<bool>
  gtags : list<maybe<tag>>
  ghits : list<(tag, string)>

pub type vres
  VOk
  VAff
  VDes

pub type dres
  DOk(o : ord)
  DBad(bad : list<bool>)
  DVisit(v : bool)

pub type dmode
  MCmp(pi : list<bool>, x : term)
  MFields(pi : list<bool>, z : term)
  MVisit(bad : list<bool>, rho : list<bool>, x : term)

// ---------------------------------------------------------------------------
// small total helpers
// ---------------------------------------------------------------------------

fun list-length(xs : list<a>) : total int
  match xs
    Nil           -> 0
    Cons(_, rest) -> 1 + list-length(rest)

fun list-get(xs : list<a>, i : int) : total maybe<a>
  match xs
    Nil           -> Nothing
    Cons(x, rest) -> if i <= 0 then Just(x) else list-get(rest, i - 1)

fun list-reverse(xs : list<a>) : total list<a>
  list-rev-onto(xs, Nil)

fun list-rev-onto(xs : list<a>, acc : list<a>) : total list<a>
  match xs
    Nil           -> acc
    Cons(x, rest) -> list-rev-onto(rest, Cons(x, acc))

fun path-eq(a : list<bool>, b : list<bool>) : total bool
  match a
    Nil -> match b
             Nil -> True
             _   -> False
    Cons(x, xr) -> match b
                     Nil -> False
                     Cons(y, yr) -> x == y && path-eq(xr, yr)

fun tag-eq(a : tag, b : tag) : total bool
  match a
    (i, p) -> match b
                (j, q) -> i == j && path-eq(p, q)

fun maybe-tag-eq(a : maybe<tag>, b : maybe<tag>) : total bool
  match a
    Nothing -> match b
                 Nothing -> True
                 _       -> False
    Just(x) -> match b
                 Just(y) -> tag-eq(x, y)
                 Nothing -> False

fun list-mem-hit(hs : list<(tag, string)>, t : tag, k : string) : total bool
  match hs
    Nil -> False
    Cons((h, j), rest) -> (tag-eq(h, t) && j == k) || list-mem-hit(rest, t, k)

fun vand(a : vres, b : vres) : total vres
  match a
    VOk -> b
    _   -> a

fun vneeds(ok : bool, r : vres) : total vres
  if ok then VOk else r

// ---------------------------------------------------------------------------
// guard operations (bendtt.lean:460-480, 1270-1330)
// ---------------------------------------------------------------------------

fun guard-bind(g : guard, o : maybe<tag>) : total guard
  g(gtags = Cons(o, g.gtags))

// the tag of the next argument of a case tree: a pending piece, or a new
// column of the given liveness
fun guard-next(g : guard, ps : list<maybe<tag>>, l : bool) : total (maybe<tag>, guard, list<maybe<tag>>)
  match ps
    Cons(p0, rest) -> (p0, g, rest)
    Nil ->
      val p = Just((list-length(g.gcols), Nil))
      (p, g(gcols = list-append(g.gcols, Cons(l, Nil))), Nil)

fun map-tag-step(p : maybe<tag>, b : bool) : total maybe<tag>
  match p
    Nothing  -> Nothing
    Just((j, pi)) -> Just((j, Cons(b, pi)))

fun tag-hit(p : maybe<tag>, k : string) : total list<(tag, string)>
  match p
    Nothing  -> Nil
    Just(t)  -> Cons((t, k), Nil)

// column j's pattern: split down to every variable the case tree bound in it
// and every label a match hit (bendtt.lean:1276)
fun guard-pat(g : guard, j : int) : total pat
  fold-paths(list-append(paths-of-tags(g.gtags, j), paths-of-hits(g.ghits, j)), PHole)

fun paths-of-tags(ts : list<maybe<tag>>, j : int) : total list<list<bool>>
  match ts
    Nil                  -> Nil
    Cons(Nothing, rest)  -> paths-of-tags(rest, j)
    Cons(Just((c, pi)), rest) -> if c == j then Cons(pi, paths-of-tags(rest, j)) else paths-of-tags(rest, j)

fun paths-of-hits(hs : list<(tag, string)>, j : int) : total list<list<bool>>
  match hs
    Nil -> Nil
    Cons(((c, pi), _), rest) -> if c == j then Cons(pi, paths-of-hits(rest, j)) else paths-of-hits(rest, j)

fun fold-paths(ps : list<list<bool>>, p : pat) : total pat
  match ps
    Nil           -> p
    Cons(pi, rest) -> fold-paths(rest, pat-ins(list-reverse(pi), p))

// p, split open down to the piece at pi (outermost step first)
fun pat-ins(pi : list<bool>, p : pat) : total pat
  match pi
    Nil -> p
    Cons(b, rest) ->
      match p
        PPair(x, y) -> if b then PPair(x, pat-ins(rest, y)) else PPair(pat-ins(rest, x), y)
        PHole       -> if b then PPair(PHole, pat-ins(rest, PHole)) else PPair(pat-ins(rest, PHole), PHole)

// x is the piece at pi of column j as the case tree saw it: the variable it
// bound there, or the label a match hit there (bendtt.lean:1284)
fun guard-at(g : guard, j : int, pi : list<bool>, x : term) : total bool
  match x
    TVar(v) ->
      match list-get(g.gtags, v)
        Just(mt) -> match mt
                      Just(t) -> tag-eq(t, (j, pi))
                      Nothing -> False
        Nothing -> False
    TLab(k) -> list-mem-hit(g.ghits, (j, pi), k)
    _       -> False

// ---------------------------------------------------------------------------
// the descent comparison, aligned with bend2's term_descend
// ---------------------------------------------------------------------------
//
// One self-recursive function with three modes, so no mutual-recursion group
// remains for Koka to reject; every recursive call is on a strict subpattern,
// which is what the totality checker needs. The modes are:
//
//   MCmp(pi, x)      how x compares to the piece at path pi of pattern p
//   MFields(pi, z)   the right-spine field walk: compare the fields of p with
//                    the tuple z, one by one
//   MVisit(bad, rho, x)  x is no bigger than another field than the one that
//                    failed: only a constructor's fields are tried, not its
//                    tag or the pairs that chain them

fun pat-run(g : guard, j : int, p : pat, md : dmode) : total dres
  match md
    MCmp(pi, x) ->
      if guard-at(g, j, pi, x) then DOk(OEq)
      else
        match p
          PHole -> DOk(OGt)
          PPair(a, b) ->
            match x
              TTup(q, x1, x2) ->
                match (if quan-live(q)
                         then pat-run(g, j, a, MCmp(Cons(False, pi), x1))
                         else if guard-at(g, j, Cons(False, pi), x1) then DOk(OEq) else DOk(OGt))
                  DOk(OEq) ->
                    match pat-run(g, j, b, MFields(Cons(True, pi), x2))
                      DOk(o) -> DOk(o)
                      DBad(bad) ->
                        match pat-run(g, j, b, MVisit(bad, Cons(True, pi), x))
                          DVisit(v) -> DOk(if v then OLt else OGt)
                          _         -> DOk(OGt)
                      DVisit(_) -> DOk(OGt)
                  DOk(OLt) -> DOk(OLt)
                  DOk(OGt) ->
                    match pat-run(g, j, b, MVisit(Cons(False, pi), Cons(True, pi), x))
                      DVisit(v) -> DOk(if v then OLt else OGt)
                      _         -> DOk(OGt)
                  DBad(_) -> DOk(OGt)
                  DVisit(_) -> DOk(OGt)
              _ ->
                match pat-run(g, j, b, MVisit(pi, Cons(True, pi), x))
                  DVisit(v) -> DOk(if v then OLt else OGt)
                  _         -> DOk(OGt)
    MFields(pi, z) ->
      if guard-at(g, j, pi, z) then DOk(OEq)
      else
        match p
          PHole -> DBad(pi)
          PPair(a, b) ->
            match z
              TTup(q, z1, z2) ->
                if quan-live(q) then
                  match pat-run(g, j, a, MCmp(Cons(False, pi), z1))
                    DOk(OEq) -> pat-run(g, j, b, MFields(Cons(True, pi), z2))
                    DOk(OLt) -> DOk(OLt)
                    DOk(OGt) -> DBad(Cons(False, pi))
                    DBad(bad) -> DBad(bad)
                    DVisit(_) -> DBad(pi)
                else if guard-at(g, j, Cons(False, pi), z1) then
                  pat-run(g, j, b, MFields(Cons(True, pi), z2))
                else
                  DBad(Cons(False, pi))
              _ -> DBad(pi)
    MVisit(bad, rho, x) ->
      match p
        PHole -> DVisit(False)
        PPair(a, b) ->
          if (not(path-eq(bad, Cons(False, rho))))
             && (not(ord-eq(cmp-ord(pat-run(g, j, a, MCmp(Cons(False, rho), x))), OGt)))
            then DVisit(True)
            else pat-run(g, j, b, MVisit(bad, Cons(True, rho), x))

fun cmp-ord(r : dres) : total ord
  match r
    DOk(o) -> o
    _      -> OGt

// how one argument compares to column j (bendtt.lean:1340)
pub fun arg-cmp(g : guard, j : int, a : (quan, term)) : total ord
  match a
    (q, x) ->
      match list-get(g.gcols, j)
        Just(lv) ->
          if lv != quan-live(q) then OGt
          else if quan-live(q) then cmp-ord(pat-run(g, j, guard-pat(g, j), MCmp(Nil, x)))
          else OEq
        Nothing -> OGt

// arguments compared left to right: equal until one is smaller (bendtt.lean:1348)
fun arg-descend(g : guard, j : int, xs : list<(quan, term)>) : total ord
  match xs
    Nil -> OEq
    Cons(x, rest) ->
      match arg-cmp(g, j, x)
        OEq -> arg-descend(g, j + 1, rest)
        o   -> o

// a live call names an earlier def, or its own def on a descent (bendtt.lean:1355)
pub fun called(g : guard, t : term) : total bool
  match unspine(t, Nil)
    (TRef(kk), xs) ->
      match book-index(g.gbook, kk)
        Just(idx) ->
          if idx < g.gself then True
          else if idx == g.gself then ord-eq(arg-descend(g, 0, xs), OLt)
          else False
        Nothing -> False
    _ -> True

// ---------------------------------------------------------------------------
// affinity and the case-tree walk
// ---------------------------------------------------------------------------

// the live uses of variable i in t (bendtt.lean:1250)
pub fun uses(t : term, i : int) : total int
  match t
    TVar(j) -> if i == j then 1 else 0
    TAnn(x, _) -> uses(x, i)
    TLet(q, v, f) -> (if quan-live(q) then uses(v, i) else 0) + uses(f, i + 1)
    TLam(_, f) -> uses(f, i + 1)
    TApp(q, f, x) -> uses(f, i) + (if quan-live(q) then uses(x, i) else 0)
    TTup(q, a, b) -> (if quan-live(q) then uses(a, i) else 0) + uses(b, i)
    TPrj(h) -> uses(h, i)
    TMat(_, h, m) -> uses(h, i) + uses(m, i)
    TRwt(e, _, f) -> uses(e, i) + uses(f, i)
    TMin(a, b) -> uses(a, i) + uses(b, i)
    _ -> 0

fun vneeds-allows(q : quan, n : int) : total vres
  if quan-allows(q, n) then VOk else VAff

fun vneeds-called(g : guard, t : term) : total vres
  if called(g, t) then VOk else VDes

// the live check of a term; top is false on the head of an App (bendtt.lean:1370)
pub fun live(g : guard, top : bool, t : term) : total vres
  match t
    TApp(q, f, x) ->
      vand(if top then vneeds-called(g, t) else VOk,
           vand(live(g, False, f), if quan-live(q) then live(g, True, x) else VOk))
    TRef(_) -> if top then vneeds-called(g, t) else VOk
    TAnn(x, _) -> live(g, True, x)
    TLet(q, v, f) ->
      vand(vneeds-allows(q, uses(f, 0)),
           vand(if quan-live(q) then live(g, True, v) else VOk,
                live(guard-bind(g, Nothing), True, f)))
    TLam(q, f) ->
      vand(vneeds-allows(q, uses(f, 0)), live(guard-bind(g, Nothing), True, f))
    TTup(q, a, b) ->
      vand(if quan-live(q) then live(g, True, a) else VOk, live(g, True, b))
    TPrj(h) -> live(g, True, h)
    TMat(_, h, m) -> vand(live(g, True, h), live(g, True, m))
    TRwt(e, _, f) -> vand(live(g, True, e), live(g, True, f))
    TMin(a, b) -> vand(live(g, True, a), live(g, True, b))
    _ -> VOk

// the live check of a def's case tree; ps are the pending arguments
// (bendtt.lean:1394)
pub fun tree(g : guard, ps : list<maybe<tag>>, t : term) : total vres
  match t
    TLam(q, f) ->
      match guard-next(g, ps, quan-live(q))
        (p, g2, ps2) -> vand(vneeds-allows(q, uses(f, 0)), tree(guard-bind(g2, p), ps2, f))
    TPrj(h) ->
      match guard-next(g, ps, True)
        (p, g2, ps2) -> tree(g2, Cons(map-tag-step(p, False), Cons(map-tag-step(p, True), ps2)), h)
    TMat(kk, h, m) ->
      match guard-next(g, ps, True)
        (p, g2, ps2) ->
          vand(tree(g2(ghits = list-append(tag-hit(p, kk), g2.ghits)), ps2, h),
               tree(g2, Cons(p, ps2), m))
    TEfq -> VOk
    TApp(_, f, TVar(v)) ->
      if takes(unspine-head(f))
        then tree(g, Cons(join-tag(list-get(g.gtags, v)), ps), f)
        else live(g, True, t)
    _ -> live(g, True, t)

fun join-tag(m : maybe<maybe<tag>>) : total maybe<tag>
  match m
    Just(mt) -> mt
    Nothing  -> Nothing

// the whole live check of one def body
pub fun def-live(bk : book, i : int, d : defn) : total vres
  tree(Guard(bk, i, Nil, Nil, Nil), Nil, d.v)

// check every def; the first failure names its def and rule
pub fun check-book(bk : book) : total maybe<(string, vres)>
  check-book-at(bk, 0, bk)

fun check-book-at(bk : book, i : int, rest : list<defn>) : total maybe<(string, vres)>
  match rest
    Nil -> Nothing
    Cons(d, more) ->
      match def-live(bk, i, d)
        VOk -> check-book-at(bk, i + 1, more)
        v   -> Just((d.nm, v))
