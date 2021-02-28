import '@fontsource/noto-sans-sc'
import '@openfonts/architects-daughter_latin'
import Head from 'next/head'
import 'normalize.css'
import Header from '../components/Header'
import theme from '../lib/theme'
import '../styles/fonts/excalidraw/fontface.css'
import '../styles/fonts/Google Sans/fontface.css'
import '../styles/fonts/iosevka/fontface.css'
import '../styles/globals.css'

const emphasize = (selector, color) => `
    ${selector} {
        color: ${color};
        position: relative;
        padding-bottom: 2px;
    }

    ${selector}::after {
        content: '';
        height: 2px;
        background: ${color};
        width: 0;
        position: absolute;
        bottom: 0;
        left: 0;
    }

    ${selector}:hover::after {
        width: 100%;
        animation-duration: 0.8s;
        animation-name: expand;
        animation-iteration-count: 1;
    }

    @keyframes expand {
        0% {
            left: 50%;
            width: 10%;
        }
        100% {
            left: 0;
            width: 100%;
        }
    }
`

const AppFrame = ({ Component, pageProps }) => (
    <>
        <Head>
            <title>Scarlet Serenade</title>
        </Head>
        <Header></Header>
        <main className="container">
            <Component {...pageProps} />
        </main>
        <style jsx>{`
            .container {
                width: 100%;
                padding: 8px;
            }
        `}</style>
        <style jsx global>
            {`
                body {
                    color: ${theme['text-color']};
                }

                .nav {
                    color: ${theme['text-color-light']};
                    border-bottom: 1px solid ${theme['purple-color-light']};
                    box-shadow: inset 0 -7px 0 ${theme['purple-color-light']};
                    transition: all 0.25s ease-in-out;
                    padding: 2px 2px 0 2px;
                }

                .nav:hover {
                    background-color: ${theme['purple-color-light']};
                }

                ${emphasize('.yellow', theme['yellow-color'])}
                ${emphasize('.red', theme['red-color'])}
                ${emphasize('.blue-light', theme['blue-color-light'])}
            `}
        </style>
    </>
)

export default AppFrame
