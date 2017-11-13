import 'normalize.css'
import React from 'react'
import { Provider } from 'react-redux'
import Modal from './components/Modal'
import RootRouter from './routes/RootRouter'
import configureStore from './store/configureStore'
import { ThemeManager, ThemeProvider } from './utils/themer'
import themes from './themes'

const store = configureStore()
const themeManager = new ThemeManager(themes)

export default () => (
    <Provider store={store}>
        <ThemeProvider themeManager={themeManager} >
            <RootRouter />
            <Modal />
        </ThemeProvider>
    </Provider>
)
