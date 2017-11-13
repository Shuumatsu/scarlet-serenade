import { propOr, identity } from 'ramda'

const createAttrChangeHandler = handlers =>
    (name, oldValue, newValue) =>
        propOr(identity, name, handlers)(name, oldValue, newValue)

export default createAttrChangeHandler