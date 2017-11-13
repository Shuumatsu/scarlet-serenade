import { isNil, contains, flatten } from 'ramda'
import { taskIdentifier } from '../actions'

export const selectorKey = 'tasks'

const tasksReducer = (state = {}, action) => {
    const { type, taskKey } = action
    if (isNil(taskKey) ||
        !contains(taskIdentifier, flatten([type])))
        return state

    const { error, status, result } = action

    return {
        ...state, [taskKey]: {
            status, error, result, taskKey
        }
    }
}

export default tasksReducer
