import React from 'react'
import katex from 'katex'
import 'katex/dist/katex.css'

const Math = ({ inline, children, ...props }) => {
    const rendered = katex.renderToString(children[0], { displayMode: !inline, output: 'html', throwOnError: false })

    if (inline) {
        return <span data-raw={children[0]} dangerouslySetInnerHTML={{ __html: rendered }} {...props}></span>
    }
    return <div data-raw={children[0]} dangerouslySetInnerHTML={{ __html: rendered }} {...props}></div>
}

export default Math
