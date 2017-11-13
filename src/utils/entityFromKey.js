import { path, curry, compose } from 'ramda'

const entitiyFromKey = curry(
    (entitiesSelector, kind, key) => compose(
        path([kind, key]),
        entitiesSelector
    )
)

export default entitiyFromKey
