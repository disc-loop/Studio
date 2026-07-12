datatype chain
  = Link of (int * (int -> chain));

fun chain_item(n, Link(i, f))
  = if (n = 1)
    then i
    else chain_item(n-1, f(i));

fun is_prime(n)
  = has_no_divisors(n, n-1)
and has_no_divisors(n, c)
  = if (c = 1)
    then true
    else if (n mod c = 0)
         then false
         else has_no_divisors(n, c-1);

fun primes(n)
  = if is_prime(n+1)
    then Link(n+1, primes)
    else primes(n+1);
