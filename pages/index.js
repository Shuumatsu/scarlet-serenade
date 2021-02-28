import Link from 'next/link'
import { useEffect } from 'react'
import typography from '../lib/app_typography'
import { getPostsData } from '../lib/posts'
import theme from '../lib/theme'

typography.injectStyles()

export const getStaticProps = async () => {
    const allPostsData = await getPostsData()
    return { props: { allPostsData } }
}

const App = ({ allPostsData }) => {
    useEffect(() => typography.injectStyles(), [])

    return (
        <>
            <ul>
                {allPostsData.map(post => (
                    <li key={post.slug}>
                        <Link href={post.slug}>
                            <a className="post">{post.frontmatter.title}</a>
                        </Link>
                    </li>
                ))}
            </ul>
            <style jsx>
                {`
                    li {
                        margin-bottom: 1em;
                    }
                    .post {
                        font-family: ${theme['sketchy-font-family']};
                        font-weight: bold;
                        font-size: 16px;

                        color: ${theme['text-color']};
                        box-shadow: 0 -6px ${theme['purple-color-light']} inset;
                    }
                `}
            </style>
        </>
    )
}

export default App
