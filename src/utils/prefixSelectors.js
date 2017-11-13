import { reduce, keys, prop, compose } from 'ramda'
import { camelize } from 'humps'

export default (domain, selectors) => reduce((accu, selectorKey) => ({
    ...accu,
    [
    camelize([domain, selectorKey].join('_'))
    ]: compose(selectors[selectorKey], prop(domain))
}), {}, keys(selectors))
