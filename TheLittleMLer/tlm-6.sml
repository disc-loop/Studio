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
