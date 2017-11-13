import { githubEntityIdentifier } from '../actions'
import { prop, isNil, flatten, contains, mergeDeepRight } from 'ramda'

export const selectorKey = 'entities'

const entitiesReducer = (state = {}, action) => {
    const { type } = action
    if (!contains(githubEntityIdentifier, flatten([type])))
        return state

    const entities = prop('entities', action)

    return isNil(entities) ?
        state :
        mergeDeepRight(state, entities)
}

export default entitiesReducer
