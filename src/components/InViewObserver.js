import React, { PureComponent } from 'react'
import PropTypes from 'prop-types'

const callbacks = new WeakMap()

const io = new IntersectionObserver((entries, observer) => {
    entries.forEach(entry => {
        console.log(entry)
        if (entry.isIntersecting) {
            const target = entry.target
            const callback = callbacks.get(target)
            typeof callback === 'function' && callback(entry)
        }
    })
}, { threshold: 0.25 })

export default class extends PureComponent {

    static propTypes = {
        handler: PropTypes.func.isRequired
    }

    componentDidMount() {
        callbacks.set(this.dom, this.props.handler)
        io.observe(this.dom)
    }

    componentWillUnmount() {
        io.unobserve(this.dom)
    }

    componentWillReceiveProps(nextProps) {
        callbacks.set(this.dom, this.props.handler)
    }

    render() {
        return (
            <div ref={dom => this.dom = dom}>
                {this.props.children}
            </div>
        )
    }
}

