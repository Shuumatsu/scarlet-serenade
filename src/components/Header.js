import React, { PureComponent } from 'react'
import styled from 'styled-components'
import { path } from 'ramda'
import { connectTheme } from '../utils/themer'
import Back from './Back'
import Menu from './Menu'

const Container = connectTheme(styled.header`
    background-color: ${path(['theme', 'colors', 'background'])}
`)



export default class extends PureComponent {

    render() {
        const { base } = this.props
        return (
            <Container>
                {base ?
                    <Menu /> :
                    <Back />}
                header.
            </Container>
        )
    }
}
