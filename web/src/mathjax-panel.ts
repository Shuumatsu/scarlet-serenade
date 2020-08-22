import style from '!!lit-css-loader!katex/dist/katex.css'
import katex from 'katex'
// https://github.com/Polymer/lit-element/issues/1053#issuecomment-677973159
import 'katex/dist/katex.css'
import { customElement, html, LitElement, property } from 'lit-element'
import { styleMap } from 'lit-html/directives/style-map'
import { unsafeHTML } from 'lit-html/directives/unsafe-html.js'

@customElement('mathjax-panel')
export class MathjaxPanel extends LitElement {
    @property() rendered = ''
    @property({ reflect: true }) block = false

    static get styles() {
        return [style]
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
        const rstyle = this.rendered ? '' : styleMap({ display: 'none' })
        const pstyle = this.rendered ? styleMap({ display: 'none' }) : ''
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
