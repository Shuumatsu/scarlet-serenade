import React, { PureComponent } from 'react'
import PropTypes from 'prop-types'
import { autobind } from 'core-decorators'
import { path, omit, forEach, compose, curry } from 'ramda'

const themeManagers = new WeakMap()

@autobind
export class ThemeManager {

    constructor(themes = []) {
        const callbacks = new Map()
        themeManagers.set(this, callbacks)
        this.themes = themes
        this.activeTheme = themes[0] || {}
    }

    subscribe(callback) {
        const key = Symbol()
        const callbacks = themeManagers.get(this)
        callbacks.set(key, callback)

        return {
            unSubscribe: () => callbacks.delete(key)
        }
    }

    update(activeTheme) {
        this.activeTheme = activeTheme
        compose(
            forEach(callback => callback(activeTheme)),
            Array.from
        )(themeManagers.get(this))
    }
}

export class ThemeProvider extends PureComponent {

    static childContextTypes = {
        themeManager: PropTypes.instanceOf(ThemeManager)
    }

    getChildContext() {
        return { themeManager: this.props.themeManager }
    }

    render() {
        return path(['props', 'children'], this)
    }
}

export const withTheme = curry((themeManager, Wrapped) => class extends Wrapped {

    state = { theme: {} }

    get themeManager() {
        return themeManager
    }

    @autobind
    setTheme(theme) {
        this.setState({ theme })
    }

    componentDidMount() {
        this.setTheme(this.themeManager.activeTheme)
        this.subscription = this.themeManager.subscribe(this.setTheme)
    }

    componentWillUnmount() {
        this.subscription.unSubscribe()
    }

    render() {
        const theme = path(['state', 'theme'], this)
        const propsToPassedDown = omit(['theme'], this.props)
        return (
            <Wrapped theme={theme} {...propsToPassedDown} />
        )
    }
})

export const connectTheme = Wrapped => class extends withTheme(null, Wrapped) {

    static contextTypes = {
        themeManager: PropTypes.instanceOf(ThemeManager)
    }

    get themeManager() {
        return this.context.themeManager
    }
}

