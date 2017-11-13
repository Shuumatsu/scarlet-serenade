import { curry } from 'ramda'

export default curry((taskName, behavior) =>
    (...args) =>
        `${taskName}-${behavior(...args)}`
)