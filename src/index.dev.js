import React from 'react'
import ReactDOM from 'react-dom'
import App from './App'

const root = document.querySelector('#root')
const render = () => ReactDOM.render(<App />, root)

if (module.hot)
    module.hot.accept('./App', () => requestAnimationFrame(render))

render()

