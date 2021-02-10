import React from 'react'
import { Helmet } from 'react-helmet'
import styled, { createGlobalStyle } from 'styled-components'
import Header from './Header'
import plate from '../styles/plate'

const GlobalStyle = createGlobalStyle`
    html,
    body,
    #___gatsby {
        height: 100%;
        width: 100%;

        color: ${plate['text-color']};
        background-color: ${plate['background-color']};
    }

    #gatsby-focus-wrapper {
        display: grid;
        grid-template-rows: auto 1fr;
        min-height: 100%;
        width: 100%;
    }
`

const Container = styled.main`
    width: 100%;
    padding: 8px;
`

const AppFrame = ({ children }) => (
    <>
        <Helmet>
            <title>Scarlet Serenade</title>
        </Helmet>
        <GlobalStyle></GlobalStyle>
        <Header></Header>
        <Container>{children}</Container>
    </>
)

export default AppFrame
