import React, { PureComponent } from 'react'
import MarkdownRenderer from './MarkdownRenderer'
import styled from 'styled-components'
import PropTypes from 'prop-types'
import { withRouter } from 'react-router-dom'
import { toSvg } from 'feather-icons'
import { connectTheme } from '../utils/themer'

export default class extends PureComponent {

    static propTypes = {
        comment: PropTypes.object.isRequired
    }

    render() {
        return (

        )
    }
}