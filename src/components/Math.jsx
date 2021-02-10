import React from 'react'
import katex from 'katex'
import 'katex/dist/katex.css'

const Math = ({ inline, children, ...props }) => {
    const rendered = katex.renderToString(children[0], { displayMode: !inline, output: 'html' })

    if (inline) {
        return <span dangerouslySetInnerHTML={{ __html: rendered }} {...props}></span>
    }
    return <figure dangerouslySetInnerHTML={{ __html: rendered }} {...props}></figure>
}

export default Math
