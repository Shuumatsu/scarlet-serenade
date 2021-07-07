import sizeOf from 'image-size'
import fs from 'fs'
import matter from 'gray-matter'
import toHTML from 'hast-util-to-html'
import h from 'hastscript'
import directive from 'mdast-util-directive'
import fromMarkdown from 'mdast-util-from-markdown'
import math from 'mdast-util-math'
import toHast from 'mdast-util-to-hast'
import directiveSyntax from 'micromark-extension-directive'
import gfmSyntax from 'micromark-extension-gfm'
import gfm from 'mdast-util-gfm'
import mathSyntax from 'micromark-extension-math'
import path from 'path'
import process from 'process'
import isRelative from 'is-relative-url'
import visit from 'unist-util-visit'
import { promisify } from 'util'

const projectDirectory = process.cwd()
const postsDirectory = path.join(projectDirectory, 'posts')
const staticDirectory = path.join(projectDirectory, 'public')

const locate = id => {
    const location = `${path.join(postsDirectory, ...id)}.md`
    return fs.existsSync(location) ? location : path.join(postsDirectory, ...id, 'index.md')
}
const slugify = id => path.join('/posts', ...id)

export const getPostsData = async () => {
    const h = async (dirname, partial) => {
        const ret = []

        const names = await promisify(fs.readdir)(dirname)
        for (const name of names) {
            if (name.startsWith("_")) continue
            const fullPath = path.join(dirname, name)

            const stats = await promisify(fs.lstat)(fullPath)

            if (stats.isDirectory()) {
                ret.push(...(await h(fullPath, [...partial, name])))
            } else if (name.endsWith('.md')) {
                const res = promisify(fs.readFile)(fullPath).then(body => {
                    const { data, content } = matter(body)

                    const id = name == 'index.md' ? partial : [...partial, name.replace(/\.md$/, '')]

                    return {
                        id,
                        ctime: stats.ctime.getTime() / 1000,
                        mtime: stats.mtime.getTime() / 1000,
                        slug: slugify(id),
                        frontmatter: data,
                    }
                })
                ret.push(res)
            }
        }

        return ret
    }

    const posts = await h(path.join(projectDirectory, 'posts'), [])

    const ret = await Promise.all(posts)
    ret.sort((a, b) => b.mtime - a.mtime)

    return ret
}

export const getAllPostIds = async () => {
    const posts = await getPostsData()
    return posts.map(post => ({ params: { id: post.id } }))
}

const parse = content => {
    const encoding = 'utf8'

    const extensions = [gfmSyntax(), directiveSyntax(), mathSyntax]
    const mdastExtensions = [gfm.fromMarkdown, directive.fromMarkdown, math.fromMarkdown]
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

export const getPostData = async id => {
    const location = locate(id)

    const body = await promisify(fs.readFile)(location, 'utf8')
    const { data, content } = matter(body)

    const mdast = parse(content)
    translate(mdast)

    const slug = slugify(id)
    visit(mdast, ['image'], node => {
        if (isRelative(node.url)) {
            const src = path.join(location, '..', node.url)
            const dest = path.join(staticDirectory, slug, node.url)

            fs.mkdirSync(path.dirname(dest), { recursive: true })
            fs.copyFileSync(src, dest)

            const dimensions = sizeOf(src)

            const data = node.data || (node.data = {})
            const hast = h('img', {
                ...node.attributes,
                src: path.join('/', path.relative(staticDirectory, dest)),
                height: dimensions.height / 2,
                width: dimensions.width / 2,
            })
            data.hProperties = hast.properties
        }
    })

    const hast = toHast(mdast)

    const html = toHTML(hast)

    return { slug, id, html, hast, mdast, content, frontmatter: data }
}
