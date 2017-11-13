import createTemplate from '../utils/createTemplate'

const getInstance = createTemplate`
    <style>
        :host {
            display: block;
            contain: content;
        }

        #content {
        }
    </style>
    <slot id="content"></slot>
`

export const tagName = 'markdown-rendered'

class Marked extends HTMLElement {

    constructor() {
        super()

        const shadowRoot = this.attachShadow({ mode: 'open' })
        const instance = getInstance()
        shadowRoot.innerHTML = instance.innerHTML
    }

}

customElements.define(
    tagName,
    Marked,
    { extends: 'div' }
)

export default Marked
