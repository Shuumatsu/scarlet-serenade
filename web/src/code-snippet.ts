import hljs from 'highlight.js'
import style from '!!lit-css-loader!highlight.js/styles/github.css'
import { customElement, html, LitElement, property, css } from 'lit-element'
import { styleMap } from 'lit-html/directives/style-map'
import { unsafeHTML } from 'lit-html/directives/unsafe-html.js'

@customElement('code-snippet')
export class CodeSnippet extends LitElement {
    @property() rendered = ''
    @property({ reflect: true }) block = false

    static get styles() {
        return [
            style,
            css`
                :host {
                    display: inline-block;
                }
                :host([block='true']) {
                    display: block;
                }
            `,
        ]
    }

    slotchange = (event: Event) => {
        const slot = this.shadowRoot?.querySelector('slot')
        const code = slot?.assignedNodes()[0].textContent
        if (code) this.rendered = hljs.highlightAuto(code).value
    }

    render() {
        const rstyle = this.rendered ? '' : styleMap({ display: 'none' })
        const pstyle = this.rendered ? styleMap({ display: 'none' }) : ''

        const slot = html`<slot style=${style} @slotchange=${this.slotchange}></slot>`
        const content = html`<code>${unsafeHTML(this.rendered)}</code>`

        if (this.block) {
            return html`
                <pre style=${rstyle}>${content}</pre>
                <pre style=${pstyle}>${slot}</pre>
            `
        }
        return html`
            <span style=${rstyle}>${content}</span>
            <span style=${pstyle}>${slot}</span>
        `
    }
}
