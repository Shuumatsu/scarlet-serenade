import createReducer from '../utils/createReducer'
import { drawerState } from '../actions'

export const selectorKey = 'app'

const drawerStateReducer = (state, action) => ({
    ...state, state: action.state
})

const appReducer = createReducer({}, {
    [drawerState]: drawerStateReducer
})

export default appReducer
