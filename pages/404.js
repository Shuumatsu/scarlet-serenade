import Head from 'next/head'
import { useEffect } from 'react'
import typography from '../lib/app_typography'
import theme from '../lib/theme'

const NotFound = () => {
    useEffect(() => typography.injectStyles(), [])

    return (
        <>
            <Head>
                <title>Scarlet Serenade - 404</title>
            </Head>
            <h1 className="message">404 Not Found</h1>
            <style jsx>{`
                .message {
                    padding: 8px;
                    text-align: center;

                    font-family: ${theme['sketchy-font-family']};
                }
            `}</style>
        </>
    )
}

export default NotFound
