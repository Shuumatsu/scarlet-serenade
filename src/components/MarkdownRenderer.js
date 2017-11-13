import { identity, propOr } from 'ramda'
import { autobind } from 'core-decorators'
import createTemplate from '../utils/createTemplate'
import reflectToAttr from '../utils/reflectToAttr'
import getWorker from '../utils/getRenderWorker'

const getInstance = createTemplate`
    <style>
        :host {
            display: block;
            contain: content;
            display: flex;
        }

        #content #loading {
            height: 100%;
            width: 100%;
            flex: 1;
        }

        .hide {
            display: none;
        }

        #loading {
            position: absolute;
            left: 50%;
            top: 50%;
            transform: translate(-50%, -50%);
        }

        #content {
        }
    </style>
    <div id="loading">Loading...</div>
    <div id="content"></div>
`

export const tagName = 'markdown-renderer'

const attrs = ['raw']
@reflectToAttr(attrs)
class MarkdownRenderer extends HTMLElement {

    constructor() {
        super()

        const shadowRoot = this.attachShadow({ mode: 'open' })
        const instance = getInstance()
        shadowRoot.innerHTML = instance.innerHTML

        const worker = getWorker()
        worker.addEventListener('message', this.onRendered)

        this.contentDiv = shadowRoot.querySelector('#content')
        this.loadingDiv = shadowRoot.querySelector('#loading')
    }

    @autobind
    attributeChangedCallback(name, oldValue, newValue) {
        const handlers = {
            raw: () => {
                this.render(newValue)
            }
        }

        propOr(identity, name, handlers)()
    }

    @autobind
    render(raw) {
        this.contentDiv.classList.add('hide')
        this.loadingDiv.classList.remove('hide')
        const worker = getWorker()
        worker.postMessage(raw || '')
    }

    @autobind
    onRendered(evt) {
        const rendererd = evt.data
        this.contentDiv.innerHTML = rendererd
        this.contentDiv.classList.remove('hide')
        this.loadingDiv.classList.add('hide')
    }

}

customElements.define(
    tagName,
    MarkdownRenderer,
    { extends: 'div' }
)

export default MarkdownRenderer
