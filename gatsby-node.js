const path = require('path')
const { createFilePath } = require('gatsby-source-filesystem')
const { NodeType } = require('./plugins/gatsby-transformer-unimd/constants')

exports.setFieldsOnGraphQLNodeType = async ({ type, getNode }, options) => {
    if (type.name == NodeType)
        return {
            slug: {
                type: 'String',
                resolve: async node => {
                    const fp = createFilePath({ node, getNode, basePath: 'posts' })
                    return path.join('/posts', fp)
                },
            },
        }

    return {}
}

exports.createPages = async ({ graphql, actions }) => {
    const { createPage } = actions

    const { data } = await graphql(`
        query {
            allUnistMarkdown {
                edges {
                    node {
                        slug
                    }
                }
            }
        }
    `)

    for (const { node } of data.allUnistMarkdown.edges) {
        createPage({
            path: node.slug,
            component: path.resolve('./src/templates/post-layout.jsx'),
            // Data passed to context is available in page queries as GraphQL variables.
            context: { slug: node.slug },
        })
    }
}
