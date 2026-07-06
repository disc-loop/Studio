datatype 'a pizza =
    Bottom
  | Topping of ('a * ('a pizza));

datatype fish =
    Anchovy
  | Lox
  | Tuna;

fun eq_fish (Anchovy, Anchovy) = true
  | eq_fish (Lox, Lox) = true
  | eq_fish (Tuna, Tuna) = true
  | eq_fish (some_fish, some_other_fish) = false;

fun rem_fish (x, Bottom) =
    Bottom
  | rem_fish (x, Topping (t, p)) =
      if eq_fish (x, t)
      then rem_fish (x, p)
      else Topping (t, rem_fish (x, p));
