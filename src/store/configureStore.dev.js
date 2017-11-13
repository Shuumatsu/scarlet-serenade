import { createStore, applyMiddleware } from 'redux'
import thunk from 'redux-thunk'
import logger from 'redux-logger'
import github from '../middlewares/github'
import rootReducer from '../reducers/index'

const configureStore = preloadedState => {
    const store = createStore(
        rootReducer,
        preloadedState,
        applyMiddleware(thunk, github, logger)
    )

    // Hot Module Replacement API
    if (module.hot)
        module.hot.accept('../reducers', () => store.replaceReducer(rootReducer))

    return store
}

export default configureStore
