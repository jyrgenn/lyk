// playing with prime numbers

package org.w21.lyk

// The list of primes known so far.
val primes = mutableListOf<Long>(2, 3)

fun addNextPrime(prime: Long) {
    primes.add(prime)
}

// Return the highes prime known so far.
fun highestPrime() = primes[primes.size - 1]

// Return true iff we have a divisor for n in primes.
fun haveDivisor(n: Long): Boolean {
    val limit = isqrt(n)

    for (prime in primes) {
        if (prime > limit) {
            return false
        }
        if (n % prime == 0L) {
            return true
        }
    }
    return false
}

// Grow the primes list by one.
fun growPrimes() {
    var candidate = highestPrime() + 2
    while (haveDivisor(candidate)) {
        candidate += 2
    }
    addNextPrime(candidate)
}

// Return a function that, on subsequent calls, returns prime numbers in
// sequence.
fun primeSeqFunc(): () -> Long {
    var nextIndex = 0
    return {
        while (primes.size <= nextIndex) {
            growPrimes()
        }
        primes[nextIndex++]
    }
}


// Print the prime factors of the argument, in sequence.
fun factor(n: Long): List<Long> {
    var current = n
    val nextPrime = primeSeqFunc()
    val result = mutableListOf<Long>()
    while (current > 1) {
        val prime = nextPrime()
        if (prime > isqrt(current)) {
            break
        }
        while (current % prime == 0L && current > 1L) {
            result.add(prime)
            current /= prime
        }
    }
    if (current > 1) {
        result.add(current)
    }
    return result
}

// Return true iff n is a prime number.
fun isPrime(n: Long): Boolean {
    if (n <= 1) {
        return false
    }
    val nextPrime = primeSeqFunc()
    val limit = isqrt(n)
    while (true) {
        val prime = nextPrime()
        if (prime > limit) {
            return true
        }
        if (n % prime == 0L) {
            return false
        }
    }
}

// Return the first prime number greater than n.
fun nextPrimeAfter(n: Long): Long {
    if (n < 2L) {
        return 2L
    }
    if (n == 2L) {
        return 3L
    }
    var next = n + 1 + n % 2
    while (true) {
        if (isPrime(next)) {
            return next
        }
    }
}

/// builtin factor
/// fun     bi_factor
/// std     int
/// key     
/// opt     
/// rest    
/// ret     factor-list
/// special no
/// doc {
/// Return the list of prime factors of `int`.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_factor(args: LObject, kwArgs: Map<LSymbol, LObject>,
              suppp: Map<LSymbol, Boolean>): LObject {
    val primes = factor(longArg(arg1(args)))
    return collectedList {
        for (prime in primes) {
            it.add(makeNumber(prime))
        }
    }
}

/// builtin prime-number-p
/// fun     bi_prime_number_p
/// std     integer
/// key     
/// opt     
/// rest    
/// ret     t/nil
/// special no
/// doc {
/// Return true iff `integer` is a prime number.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_prime_number_p(args: LObject, kwArgs: Map<LSymbol, LObject>,
                      suppp: Map<LSymbol, Boolean>): LObject {
    return bool2ob(isPrime(longArg(arg1(args))))
}

/// builtin next-prime
/// fun     bi_next_prime
/// std     integer
/// key     
/// opt     
/// rest    
/// ret     prime
/// special no
/// doc {
/// Return the next prime greater than `integer`.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_next_prime(args: LObject, kwArgs: Map<LSymbol, LObject>,
                  suppp: Map<LSymbol, Boolean>): LObject {
    return makeNumber(nextPrimeAfter(longArg(arg1(args))))
}

/// builtin known-primes
/// fun     bi_known_primes
/// std     
/// key     
/// opt     
/// rest    
/// ret     prime-list
/// special no
/// doc {
/// Return a list of the consecutive prime numbers known so far.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_known_primes(args: LObject, kwArgs: Map<LSymbol, LObject>,
                    suppp: Map<LSymbol, Boolean>): LObject {
    return collectedList{
        for (prime in primes) {
            it.add(makeNumber(prime))
        }
    }
}

