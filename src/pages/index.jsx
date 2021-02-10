import { graphql, withPrefix } from 'gatsby'
import React from 'react'
import styled from 'styled-components'
import AppFrame from '../components/AppFrame'
import plate from '../styles/plate'

export const query = graphql`
    query {
        allUnistMarkdown {
            edges {
                node {
                    frontmatter
                    id
                    slug
                }
            }
        }
    }
`

const Post = styled.a`
    font-family: ${plate['sketchy-font-family']};
    font-weight: bold;

    white-space: nowrap;
    overflow: hidden;
    text-overflow: ellipsis;

    text-decoration: none;

    color: ${plate['text-color']};
    box-shadow: 0 -6px ${plate['purple-color-light']} inset;
`

// markup
const IndexPage = ({ data }) => (
    <AppFrame>
        <ul>
            {data.allUnistMarkdown.edges.map(({ node: { slug, id, frontmatter } }) => (
                <li>
                    <Post key={id} className="nav" href={withPrefix(slug)}>
                        {frontmatter.title}
                    </Post>
                </li>
            ))}
        </ul>
    </AppFrame>
)

export default IndexPage
