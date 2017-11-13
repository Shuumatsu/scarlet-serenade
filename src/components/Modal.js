import React, { PureComponent } from 'react'
import { createPortal } from 'react-dom'

const mountPoint = document.querySelector('#modal')

export default class extends PureComponent {

    render() {
        return createPortal((
            <h1>Modal</h1>
        ), mountPoint)
    }
}