import React from 'react'
import katex from 'katex'
import 'katex/dist/katex.css'

const Math = ({ inline, children, ...props }) => {
    const rendered = katex.renderToString(children[0], { displayMode: !inline, output: 'html', throwOnError: false })

    if (inline) {
        return <span dangerouslySetInnerHTML={{ __html: rendered }} {...props}></span>
    }
    return <div dangerouslySetInnerHTML={{ __html: rendered }} {...props}></div>
}

export default Math
