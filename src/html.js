import React from 'react'

const HTML = ({ htmlAttributes, headComponents, bodyAttributes, preBodyComponents, postBodyComponents, body }) => (
    <html {...htmlAttributes}>
        <head>
            <meta charSet="utf-8" />
            <meta httpEquiv="x-ua-compatible" content="ie=edge" />
            <meta name="viewport" content="width=device-width, initial-scale=1" />
            {headComponents}
        </head>
        <body {...bodyAttributes}>
            {preBodyComponents}
            <div key="body" id="___gatsby" dangerouslySetInnerHTML={{ __html: body }} />
            {postBodyComponents}
        </body>
    </html>
)

export default HTML
