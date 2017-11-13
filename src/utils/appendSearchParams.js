import { forEachObjIndexed, endsWith } from 'ramda'

export default (url, params) => {
    const urlSearchParams = new URLSearchParams()
    forEachObjIndexed((value, key) => {
        urlSearchParams.append(key, value)
    }, params)

    return endsWith('?', url) ?
        `${url}${urlSearchParams.toString()}` :
        `${url}?${urlSearchParams.toString()}`
}
