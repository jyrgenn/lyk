// integer factorisation

package org.w21.lyk


val primes = mutableListOf<Long>(2, 3)

fun addNextPrime(n: Long) {
    primes.add(n)
}

fun highestPrime() = primes[primes.size-1]

fun haveDivisor(n: Long): Boolean {
    val limit = isqrt(n)

    for (p in 0 ..< primes.size) {
        val prime = primes[p]
        if (prime > limit) {
            break
        }
        if (n % prime == 0L) {
            return true
        }
    }
    return false
}

fun growPrimes() {
    var candidate = highestPrime() + 2
    while (haveDivisor(candidate)) {
        candidate += 2
    }
    addNextPrime(candidate)
}

fun makePrimeSeq(): () -> Long {
    var nextIndex = 0
    return {
        while (primes.size <= nextIndex) {
            growPrimes()
        }
        primes[nextIndex++]
    }
}

fun factorise(n: Long): List<Long> {
    var current = n
    val nextPrime = makePrimeSeq()
    val result = mutableListOf<Long>()
    while (true) {
        val p = nextPrime()
        val limit = isqrt(current)
        if (p > limit) {
            break
        }
        while (current > 1L && current % p == 0L) {
            result.add(p)
            current /= p
        }
        if (current == 1L) {
            break
        }
    }
    if (current > 1) {
        result.add(current)
    }
    return result
}

/// builtin factorise
/// fun     bi_factorise
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
fun bi_factorise(args: LObject, kwArgs: Map<LSymbol, LObject>): LObject {
    val primes = factorise(longArg(arg1(args), "factorise"))
    return collectedList {
        for (prime in primes) {
            it.add(makeNumber(prime))
        }
    }
}
