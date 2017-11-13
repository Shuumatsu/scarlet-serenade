
import { reduce, forEachObjIndexed } from 'ramda'

export const searchParamsToObject = searchParamsString => {
    const searchParams = new URLSearchParams(searchParamsString)
    const entries = [...searchParams.entries()]

    // no duplicate key
    return reduce((accu, entry) => {
        const key = entry[0]
        const value = entries[1]
        return { ...accu, [key]: value }
    }, {}, entries)
}

export const ObjectToSearchParams = object => {
    const searchParams = new URLSearchParams()
    forEachObjIndexed((value, key) => {
        searchParams.append(key, value)
    }, object)

    return searchParams.toString()
}
