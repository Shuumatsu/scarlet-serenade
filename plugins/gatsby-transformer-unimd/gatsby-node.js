const matter = require('gray-matter')
const path = require('path')
const { selectAll } = require('hast-util-select')
const process = require('process')
const slash = require('slash')
const visit = require('unist-util-visit')
const h = require('hastscript')
const isRelative = require('is-relative-url')
const fromMarkdown = require('mdast-util-from-markdown')
const toHast = require('mdast-util-to-hast')
const toHTML = require('hast-util-to-html')
const directiveSyntax = require('micromark-extension-directive')
const directive = require('mdast-util-directive')
const mathSyntax = require('micromark-extension-math')
const math = require('mdast-util-math')

const { NodeType } = require('./constants')

exports.onCreateNode = async ({ node, actions, loadNodeContent, createContentDigest, getNode }, options) => {
    const { createNode, createParentChildLink } = actions

    const content = await loadNodeContent(node)

    const transformed = {
        id: `${node.id} - md`,
        parent: node.id,
        internal: { content, contentDigest: createContentDigest(content), type: NodeType },
    }

    createNode(transformed)
    createParentChildLink({ parent: node, child: transformed })
}

const parse = async content => {
    const encoding = 'utf8'

    const extensions = [directiveSyntax(), mathSyntax]
    const mdastExtensions = [directive.fromMarkdown, math.fromMarkdown]
    const options = { extensions, mdastExtensions }

    return fromMarkdown(content, encoding, options)
}

const translate = tree => {
    // directive translation
    visit(tree, ['textDirective', 'leafDirective', 'containerDirective'], node => {
        const data = node.data || (node.data = {})
        const hast = h(node.name, node.attributes)

        data.hName = hast.tagName
        data.hProperties = hast.properties
    })

    // code & inlineCode translation
    visit(tree, ['code'], node => {
        const data = node.data || (node.data = {})
        const hast = h(node.name, { ...node.attributes, inline: false })

        data.hProperties = hast.properties
    })
    visit(tree, ['inlineCode'], node => {
        const data = node.data || (node.data = {})
        const hast = h(node.name, { ...node.attributes, inline: true })

        data.hProperties = hast.properties
    })

    // math & inlineMath translation
    visit(tree, ['math'], node => {
        const data = node.data || (node.data = {})
        const hast = h('math', { ...node.attributes, inline: false })

        data.hName = hast.tagName
        data.hProperties = hast.properties
    })
    visit(tree, ['inlineMath'], node => {
        const data = node.data || (node.data = {})
        const hast = h('math', { ...node.attributes, inline: true })

        data.hProperties = hast.properties
        data.hName = hast.tagName
    })
}

const operate = async (node, { pathPrefix, fileNodes }) => {
    const { data, content } = matter(node.internal.content)

    const mdast = await parse(content)
    translate(mdast)

    const hast = toHast(mdast)
    for (const img of selectAll('img', hast)) {
        if (!isRelative(img.properties.src)) continue

        const parent = fileNodes.find(file => file.id == node.parent)
        const imagePath = slash(path.resolve(parent.dir, img.properties.src))
        const imageNode = fileNodes.find(file => file.absolutePath == imagePath)
        const name = `${imageNode.internal.contentDigest}/${imageNode.name}.${imageNode.extension}`

        img.properties.src = slash(path.join(pathPrefix || '/', 'static', name))
    }

    const html = toHTML(hast)

    return { mdast, hast, html, frontmatter: data }
}

exports.setFieldsOnGraphQLNodeType = async ({ type, getNodesByType, pathPrefix }, options) => {
    if (type.name != NodeType) return {}

    const fileNodes = getNodesByType('File')

    return {
        mdast: {
            type: 'JSON',
            resolve: async node => {
                const res = await operate(node, { fileNodes, pathPrefix })
                return res.mdast
            },
        },
        hast: {
            type: 'JSON',
            resolve: async node => {
                const res = await operate(node, { fileNodes, pathPrefix })
                return res.hast
            },
        },
        html: {
            type: 'String',
            resolve: async node => {
                const res = await operate(node, { fileNodes, pathPrefix })
                return res.html
            },
        },
        frontmatter: {
            type: 'JSON',
            resolve: async node => {
                const res = await operate(node, { fileNodes, pathPrefix })
                return res.frontmatter
            },
        },
    }
}

exports.unstable_shouldOnCreateNode = ({ node }) => node.internal.mediaType === 'text/markdown'
