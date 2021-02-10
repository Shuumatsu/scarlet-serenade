import { graphql } from 'gatsby'
import React from 'react'
import { Helmet } from 'react-helmet'
import rehypeReact from 'rehype-react'
import styled from 'styled-components'
import AppFrame from '../components/AppFrame'
import CodeBlock from '../components/CodeBlock'
import Math from '../components/Math'
import plate from '../styles/plate'
import PostLayout from './PostStyle'

const Container = styled.main`
    width: 100%;
    padding: 8px;

    * {
        :not(h1, h2, h3, h4, h5, h6),
        :not(h1 *, h2 *, h3 *, h4 *, h5 *, h6 *) {
        }

        pre {
        }
    }
`

const renderAst = new rehypeReact({
    createElement: React.createElement,
    components: {
        pre: props => props.children,
        code: ({ inline, ...props }) => {
            const Component = inline ? 'code' : CodeBlock
            return <Component {...props}></Component>
        },
        math: props => <Math {...props}></Math>,
    },
}).Compiler

export const pageQuery = graphql`
    query($slug: String!) {
        unistMarkdown(slug: { eq: $slug }) {
            hast
            mdast
            html
            frontmatter
        }
    }
`

const Title = styled.h1`
    text-align: center;

    font-family: ${plate['sketchy-font-family']};
`

const Layout = ({ data: { unistMarkdown } }) => {
    const { frontmatter } = unistMarkdown

    return (
        <>
            <PostLayout></PostLayout>
            <Helmet>
                <title>{`Scarlet Serenade - ${frontmatter.title}`}</title>
            </Helmet>
            <AppFrame>
                <Title>{frontmatter.title}</Title>
                <Container>{renderAst(unistMarkdown.hast)}</Container>
            </AppFrame>
        </>
    )
}

export default Layout
