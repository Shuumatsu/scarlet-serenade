import renderWorker from './render.worker'
import { memoizeWith, identity } from 'ramda'

const getWorker = memoizeWith(
    identity,
    () => new renderWorker()
)

export default getWorker