import Link from 'next/link'
import * as React from 'react'
import theme from '../lib/theme'

const Header = () => (
    <>
        <header className="container">
            <h1>
                <Link href={'/'}>
                    <a className="title">Scarlet Serenade</a>
                </Link>
            </h1>
        </header>
        <style jsx>{`
            .container {
                width: 100%;
                padding: 8px;
            }

            .title {
                color: ${theme['text-color']};
                font-family: ${theme['sketchy-font-family']};
            }
        `}</style>
    </>
)

export default Header
