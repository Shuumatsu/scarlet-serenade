import hljs from 'highlight.js'
import styles from '!!raw-loader!highlight.js/styles/github.css'
import { customElement, html, LitElement, property, unsafeCSS } from 'lit-element'
import { styleMap } from 'lit-html/directives/style-map'
import { unsafeHTML } from 'lit-html/directives/unsafe-html.js'

@customElement('code-snippet')
export class CodeSnippet extends LitElement {
    @property() rendered = ''
    @property({ reflect: true }) block = false

    static get styles() {
        return [unsafeCSS(styles)]
    }

    slotchange = (event: Event) => {
        const slot = this.shadowRoot?.querySelector('slot')
        const code = slot?.assignedNodes()[0].textContent
        if (code) this.rendered = hljs.highlightAuto(code).value
    }

    render() {
        const rstyle = this.rendered ? '' : styleMap({ display: 'none' })
        const pstyle = this.rendered ? styleMap({ display: 'none' }) : ''
        if (this.block) {
            return html`
                <pre style=${rstyle}><code>${unsafeHTML(this.rendered)}</code></pre>
                <pre style=${pstyle}><code><slot @slotchange=${this.slotchange}></slot></code></pre>
            `
        }
        return html`
            <span style=${rstyle}><code>${unsafeHTML(this.rendered)}</code></span>
            <span style=${pstyle}
                ><code><slot @slotchange=${this.slotchange}></slot></code
            ></span>
        `
    }
}
