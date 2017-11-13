import { highlightAuto } from 'highlight.js'
import marked from 'marked'

marked.setOptions({
    highlight: code => highlightAuto(code).value
})

global.addEventListener('message', evt => {
    const toRender = evt.data
    const renderd = marked(toRender)
    global.postMessage(renderd)
})
