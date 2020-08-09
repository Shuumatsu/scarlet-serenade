import { LitElement, html, property, customElement, unsafeCSS } from 'lit-element'
import { unsafeHTML } from 'lit-html/directives/unsafe-html.js'
import { styleMap } from 'lit-html/directives/style-map'
import katex from 'katex'
import 'katex/dist/katex.css'
import styles from 'raw-loader!katex/dist/katex.css'

console.log(styles)

@customElement('mathjax-panel')
export class MathjaxPanel extends LitElement {
    @property() rendered = ''
    @property({ reflect: true }) block = false

    static get styles() {
        return [unsafeCSS(styles)]
    }

    slotchange = (event: Event) => {
        const slot = this.shadowRoot?.querySelector('slot')
        const math = slot?.assignedNodes()[0].textContent
        if (math) {
            try {
                this.rendered = katex.renderToString(math, { displayMode: this.block, output: 'html' })
            } catch (error) {
                console.error(`failed to render math equation: ${math}`)
            }
        }
    }

    render() {
        const rstyle = this.rendered ? "" : styleMap({ display: 'none' })
        const pstyle = this.rendered ? styleMap({ display: 'none' }) : ""
        if (this.block) {
            return html`
                <div style=${rstyle}>${unsafeHTML(this.rendered)}</div>
                <pre style=${pstyle}><div><slot @slotchange=${this.slotchange}></slot></div></pre>
            `
        }
        return html`
            <span style=${rstyle}>${unsafeHTML(this.rendered)}</span>
            <pre style=${pstyle}><span><slot @slotchange=${this.slotchange}></slot></span></pre>
        `
    }
}
