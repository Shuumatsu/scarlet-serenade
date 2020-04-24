const memoize = f => {
    const mem = {}
    return arg => {
        if (mem[arg]) return mem[arg]
        mem[arg] = f(arg)
        return mem[arg]
    }
}

const fact = memoize(n => (n <= 1 ? 1 : n * fact(n - 1)))

const c = (n, k) => {
    let curr = 1
    for (let i = 0; i < k; i += 1) {
        curr *= n - i
    }
    return curr / fact(k)
}

const r = n => c(n, 3) * c(365, 1)

console.log(r(365))
