datatype fruit
  = Peach
  | Apple
  | Pear
  | Lemon
  | Fig;

datatype tree
  = Bud
  | Flat of fruit * tree
  | Split of tree * tree;

fun flat_only(Bud)
  = true
  | flat_only(Flat(f, t))
  = flat_only(t)
  | flat_only(Split(s, t))
  = false;

fun split_only(Bud)
  = true
  | split_only(Flat(f, t))
  = false
  | split_only(Split(s, t))
  = split_only(s) andalso split_only(t);

fun fruit_only(x)
  = not(split_only(x));

fun less_than(n:int, m:int)
  = (n < m);

fun larger_of(n, m)
  = if less_than(n, m)
    then m
    else n;

fun height(Bud)
  = 0
  | height(Flat(f, t))
  = 1 + height(t)
  | height(Split(s, t))
  = 1 + larger_of(height(s), height(t));

(* Cheated a bit on this one.
  I couldn't be bothered writing
  out all of the combinations. *)
fun eq_fruit(a:fruit, b:fruit)
  = (a = b);

fun subst_in_tree(n, a, Bud)
  = Bud
  | subst_in_tree(n, a, Flat(f, t))
  = if eq_fruit(f, a)
      then Flat(n, subst_in_tree(n, a, t))
      else Flat(f, subst_in_tree(n, a, t))
  | subst_in_tree(n, a, Split(s, t))
  = Split(
      subst_in_tree(n, a, s),
      subst_in_tree(n, a, t));

fun occurs(a, Bud)
  = 0
  | occurs(a, Flat(f, t))
  = if eq_fruit(a, f)
      then 1 + occurs(a, t)
      else occurs(a, t)
  | occurs(a, Split(s, t))
  = occurs(a, s) + occurs(a, t);

(* This one hurts my head a little *)
datatype 'a slist
  = Empty
  | Scons of (('a sexp) * ('a slist))
and 'a sexp
  = An_atom of 'a
  | A_slist of ('a slist);

fun occurs_in_slist(a, Empty)
  = 0
  | occurs_in_slist(a, Scons(se, sl))
  = occurs_in_sexp(a, se) + occurs_in_slist(a, sl)
and occurs_in_sexp(a, An_atom(b))
  = if eq_fruit(a, b)
    then 1
    else 0
  | occurs_in_sexp(a, A_slist(sl))
  = occurs_in_slist(a, sl);

(* Looks like se and sl in Scons(se, sl)
   are our old friends car and cdr *)
fun subst_in_slist(a, b, Empty)
  = Empty
  | subst_in_slist(a, b, Scons(se, sl))
  = Scons(
      subst_in_sexp(a, b, se),
      subst_in_slist(a, b, sl)
    )
and subst_in_sexp(a, b, An_atom(c))
  = if eq_fruit(b, c)
    then An_atom(a)
    else An_atom(b)
  | subst_in_sexp(a, b, A_slist(sl))
  = A_slist(subst_in_slist(a, b, sl));


(* Version 1 *)
(*
fun eq_fruit_in_atom(a, An_atom(b))
  = eq_fruit(a, b)
  | eq_fruit_in_atom(a, A_slist(b))
  = false

fun rem_from_slist(a, Empty)
  = Empty
  | rem_from_slist(a, Scons(se, sl))
  = if eq_fruit_in_atom(a, se)
    then rem_from_slist(a, sl)
    else Scons(
      rem_from_sexp(a, se),
      rem_from_slist(a, sl)
    )
and rem_from_sexp(a, An_atom(b))
  = An_atom(b)
  | rem_from_sexp(a, A_slist(sl))
  = A_slist(rem_from_slist(a, sl));
*)

(* Seems that we can avoid a separate
   'and' clause if we handle the
   corresponding types in the initial
   matching block. *)
fun rem_from_slist(a, Empty)
  = Empty
  | rem_from_slist(a, Scons(An_atom(b), sl))
  = if eq_fruit(a, b)
    then rem_from_slist(a, sl)
    else Scons(
      An_atom(b),
      rem_from_slist(a, sl))
  | rem_from_slist(a, Scons(A_slist(b), sl))
  = Scons(
      A_slist(rem_from_slist(a, b)),
      rem_from_slist(a, sl));

(* Hello, old friend *)
(*
fun rember(a, Empty)
  = Empty
  | rember(a, Cons_cell(An_atom(b), cdr))
  = if eq_fruit(a, b)
    then rember(a, cdr)
    else Cons_cell(
      An_atom(b),
      rember(a, cdr))
  | rember(a, Cons_cell(A_slist(b), cdr))
  = Cons_cell(
      A_slist(rember(a, b)),
      rember(a, cdr));
*)
