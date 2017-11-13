import React, { PureComponent } from 'react'
import styled from 'styled-components'
import PropTypes from 'prop-types'
import { toSvg } from 'feather-icons'
import { connectTheme } from '../utils/themer'

const Container = styled.button`
  background-color: transparent;
`

const innerHTML = {
    __html: toSvg('menu', {})
}

@connectTheme
export default class extends PureComponent {

    static propTypes = {
        menuAction: PropTypes.func
    }

    render() {

        const menuAction = this.props.menuAction || (() => { })

        return (
            <Container
                onClick={menuAction}
                dangerouslySetInnerHTML={innerHTML} />
        )
    }
}

