import { combineReducers } from 'redux'
import entitiesReducer from './entities'
import appReducer from './app'
import tasksReducer from './tasks'

export default combineReducers({
    entities: entitiesReducer,
    app: appReducer,
    tasks: tasksReducer
})

