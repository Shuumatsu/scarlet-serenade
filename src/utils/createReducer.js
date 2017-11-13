import { propOr, identity } from 'ramda'

const createReducer = (initialState, handlers) =>
    (state = initialState, action) =>
        propOr(identity, action.type, handlers)(state, action)

export default createReducer

