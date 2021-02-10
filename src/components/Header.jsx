import { withPrefix } from 'gatsby'
import React from 'react'
import styled from 'styled-components'
import plate from '../styles/plate'

const Container = styled.header`
    width: 100%;
    padding: 8px;
`

const Nav = styled.a`
    color: ${plate['text-color']};
    font-family: ${plate['sketchy-font-family']};
`

const Header = () => (
    <Container>
        <h1>
            <Nav href={withPrefix('/')}>Scarlet Serenade</Nav>
        </h1>
    </Container>
)

export default Header
