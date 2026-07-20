datatype 'a list
  = Empty
  | Cons of 'a * 'a list;

datatype orapl
  = Orange
  | Apple;

fun eq_orapl(Orange, Orange)
  = true
  | eq_orapl(Apple, Apple)
  = true
  | eq_orapl(one, another)
  = false;

fun subst_orapl(new, old, Empty)
  = Empty
  | subst_orapl(new, old, Cons(head, tail))
  = if eq_orapl(old, head)
    then Cons(new, subst_orapl(new, old, tail))
    else Cons(head, subst_orapl(new, old, tail));

fun subst(rel, new, old, Empty)
  = Empty
  | subst(rel, new, old, Cons(head, tail))
  = if rel(old, head)
    then Cons(new, subst(rel, new, old, tail))
    else Cons(head, subst(rel, new, old, tail));

(* Not in the book *)
fun map(f, Empty)
  = Empty
  | map(f, Cons(head, tail))
  = Cons(f(head), map(f, tail));

fun in_range((bottom, top), n)
  = (n > bottom andalso n < top);
