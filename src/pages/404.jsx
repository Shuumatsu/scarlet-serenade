import React from 'react'
import { Helmet } from 'react-helmet'
import styled from 'styled-components'
import AppFrame from '../components/AppFrame'
import plate from '../styles/plate'

const Message = styled.h1`
    padding: 8px;
    text-align: center;

    font-family: ${plate['sketchy-font-family']};
`

// markup
const NotFound = ({ data }) => (
    <>
        <Helmet>
            <title>Scarlet Serenade - 404</title>
        </Helmet>
        <AppFrame>
            <Message>404 Not Found</Message>
        </AppFrame>
    </>
)

export default NotFound
