import { mapObjIndexed, prop, compose } from 'ramda'

export default (key, selectors) => mapObjIndexed(
    (selector, selectorKey) => compose(selector, prop(key)),
    selectors
)