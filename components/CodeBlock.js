import * as hljs from 'highlight.js'
import 'highlight.js/styles/solarized-light.css'
import * as React from 'react'

const CodeBlock = ({ children, className, ...rest }) => {
    const language = className ? className.replace(/language-/, '') : ''
    const content =
        language != ''
            ? hljs.highlight(children[0], { language, ignoreIllegals: true }).value
            : hljs.highlightAuto(children[0]).value

    return <pre dangerouslySetInnerHTML={{ __html: content }}></pre>
}

export default CodeBlock
