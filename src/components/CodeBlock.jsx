import hljs from 'highlight.js'
import React from 'react'
import 'highlight.js/styles/solarized-light.css'
import styled from 'styled-components'

const Figure = styled.figure`
    font-family: iosevka;
`

const CodeBlock = ({ children, className, ...rest }) => {
    const language = className ? className.replace(/language-/, '') : ''
    const content = language != '' ? hljs.highlight(language, children[0]).value : hljs.highlightAuto(children[0]).value

    return <pre dangerouslySetInnerHTML={{ __html: content }}></pre>
}

export default CodeBlock
