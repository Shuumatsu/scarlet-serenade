import React, { PureComponent } from 'react'
import styled from 'styled-components'
import PropTypes from 'prop-types'
import { withRouter } from 'react-router-dom'
import { toSvg } from 'feather-icons'
import { connectTheme } from '../utils/themer'

const Container = styled.button`
  background-color: transparent;
`

const innerHTML = {
    __html: toSvg('arrow-left', {})
}

@withRouter
@connectTheme
export default class extends PureComponent {

    static propTypes = {
        match: PropTypes.object.isRequired,
        location: PropTypes.object.isRequired,
        history: PropTypes.object.isRequired
    }

    back = () => {
        const { history } = this.props

        const action = history.length > 0 ?
            history.goBack :
            history.push('/')

        action()
    }

    render() {
        return (
            <Container
                onClick={this.back}
                dangerouslySetInnerHTML={innerHTML} />
        )
    }
}

