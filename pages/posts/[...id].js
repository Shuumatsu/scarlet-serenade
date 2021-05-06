import Head from 'next/head'
import Image from 'next/image'
import Link from 'next/link'
import * as React from 'react'
import { useEffect } from 'react'
import rehype from 'rehype-react'
import CodeBlock from '../../components/CodeBlock'
import Indent from '../../components/Indent'
import Equation from '../../components/Math'
import { getAllPostIds, getPostData } from '../../lib/posts'
import typography from '../../lib/post_typography'
import theme from '../../lib/theme'

export const getStaticPaths = async () => {
    const paths = await getAllPostIds()
    return { paths, fallback: false }
}

export const getStaticProps = async ({ params }) => {
    const post = await getPostData(params.id)
    return { props: { post } }
}

const renderAst = new rehype({
    createElement: React.createElement,
    components: {
        pre: props => props.children,
        code: ({ inline, ...props }) => {
            const Component = inline ? 'code' : CodeBlock
            return <Component {...props}></Component>
        },
        math: props => <Equation {...props}></Equation>,
        img: props => <Image layout="intrinsic" {...props}></Image>,
        indent: props => <Indent {...props}></Indent>,
    },
}).Compiler

const Post = ({ post }) => {
    useEffect(() => typography.injectStyles(), [])

    const { frontmatter, hast } = post

    return (
        <>
            <Head>
                <title>{`Scarlet Serenade - ${frontmatter.title}`}</title>
            </Head>
            <main>
                <h1 className="title">
                    <Link href={post.slug}>{frontmatter.title}</Link>
                </h1>
                <section className="container">{renderAst(hast)}</section>
            </main>

            <style jsx>{`
                .title {
                    text-align: center;
                    color: ${theme['text-color']};
                    font-family: ${theme['sketchy-font-family']};
                }

                .container {
                    width: 100%;
                }
            `}</style>
        </>
    )
}

export default Post
