datatype 'a pizza = Bottom
  | Topping of ('a * ('a pizza));

datatype fish = Anchovy | Lox | Tuna;

fun eq_fish (Anchovy, Anchovy) = true
  | eq_fish (Lox, Lox) = true
  | eq_fish (Tuna, Tuna) = true
  | eq_fish (some_fish, some_other_fish) = false;

fun rem_fish (x, Bottom) = Bottom
  | rem_fish (x, Topping (t, p)) =
      if eq_fish (x, t)
      then rem_fish (x, p)
      else Topping (t, rem_fish (x, p));

fun eq_int (n:int, m:int) = (n = m);

fun rem_int (n, Bottom) = Bottom
  | rem_int (n, Topping (t, p)) =
      if eq_int (n, t)
      then rem_int (n, p)
      else Topping (t, rem_int (n, p));

fun subst_fish (x, y, Bottom) = Bottom
  | subst_fish (x, y, Topping (t, p)) =
      if eq_fish (y, t)
      then Topping (x, subst_fish (x, y, p))
      else Topping (t, subst_fish (x, y, p));

fun subst_int (x, y, Bottom) = Bottom
  | subst_int (x, y, Topping (t, p)) =
      if eq_int (y, t)
      then Topping (x, subst_int (x, y, p))
      else Topping (t, subst_int (x, y, p));

datatype num = Zero
 | One_more_than of num;

fun eq_num (Zero, Zero) = true
  | eq_num (One_more_than (n), Zero) = false
  | eq_num (Zero, One_more_than (m)) = false
  | eq_num (
      One_more_than (n),
      One_more_than (m)
    ) = eq_num (n, m)
