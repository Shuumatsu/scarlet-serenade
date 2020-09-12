function x<A>() {
    return <B>(prop: { a: A; b: B }) => void {}
}
x<number>()({ a: 1, b: 'b' })

interface Foo {
    value: number
}

function increment(foo: Foo) {
    foo.value++
}

increment({
    value: 1,
    bar: 'bar',
}) // Fails: Object literal may only specify known properties, and 'bar' does not exist in type 'Foo'.

increment({
    value: 1,
    bar: 'bar',
} as Foo) // Workaround

const obj = {
    value: 1,
    bar: 'bar',
}
increment(obj) // Also works.

type A<T> = { done: boolean; rest: A<T> }

const len = (a: { done: boolean; rest: any }) => {
    if (a.done == true) return 1
    return 1 + len(a.rest)
}
